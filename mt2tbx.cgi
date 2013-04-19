#!/usr/local/bin/perl

# Postprocess MultiTerm XML to TBX

#   Copyright 2012 LTAC Global
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

# programmer policy
use strict;
use warnings;

# CGI libraries/configuration
use CGI qw(:standard -private_tempfiles -tabindex);
use CGI::Carp 'fatalsToBrowser';
$CGITempFile::TMPDIRECTORY = '../user_files/upload';
$CGI::POST_MAX = 3e6;

# data-processing libraries
use JSON; # to read mapping
use XML::Rules; # to map MultiTerm data to TBX
use Encode; # to write TBX

# share the 'print' command between CGI and data processing
use SelectSaver; # it beats adding a filehandle argument to every print

# divert data processing warnings so CGI can display them
my $warnings_cache;
local $SIG{__WARN__} = sub {$warnings_cache .= (li($_[0]) . "\n")};

# globals, yay
my $enc; # now always eq 'utf-8-strict' but you may change that
my ($parser, $enc_name); # used by certain subs called by parser rules
my ($dialect_name, $xcs_name, $categorial, $queue_orders, $misc); # mapping



### Main code ###

# talk to the user
print header(
	-type => 'application/xhtml+xml',
	-charset => 'UTF-8',
);
print start_html(
	-title => 'MultiTerm-to-TBX Processor',
	-encoding => 'UTF-8',
);
print_form();
$enc = param('output_encoding');
my $error = cgi_error();
process_file() if param('mapping') and param('input');
print_warnings() if $warnings_cache;
print_footer();
print end_html;



### CGI subroutines ###

sub print_form {
	print h1('Map MultiTerm XML to TBX');
	print start_multipart_form, "\n";
	print h3('Upload files:'), "\n";
	print p('MultiTerm XML file'), br(), filefield('input'), br(), "\n";
	print p('Mapping file'), br(), filefield('mapping'), br(), "\n";
	print hidden('output_encoding', 'utf-8-strict'), "\n";
		# in future might also accept qr/UTF-(16|32)(BE|LE)?/
		# by putting radio buttons here
		# but beware extra BOMs from writing UTF-16/32 piecemeal
		# plan to cache the output and encode it all at once instead
	# print hidden('indent_with', "\t"), "\n";
		# possible future radio buttons to configure indent
	print p(submit( -label => 'Submit' ));
	print end_form, "\n\n";
	print p('Output and logs will be encoded in UTF-8. They will appear garbled if viewed in any other character encoding.');
}

sub process_file {

	# load mapping
	my $mapping = upload('mapping'); # maybe lightweight filehandle
	if (defined $mapping) {
		$mapping = $mapping->handle; # true IO::Handle
		$mapping = do {local $/; <$mapping>}; # JSON text
		$mapping = JSON->new->utf8->relaxed->decode($mapping); # data
		(
			$dialect_name, $xcs_name, $categorial, 
			$queue_orders, $misc
		) = @$mapping; # unpacked where rest of program can see it
		review_mapping();
	} else {
		print $error || em('unknown problem uploading mapping');
		return;
	}

	# load input
	my $in = upload('input');
	if (defined $in) {
		$in = $in->handle;
	} else {
		print $error || em('unknown problem uploading input');
		return;
	}

	# set up output
	my $dir = '../user_files/download';
	use File::Temp;
	my $out = File::Temp->new(
		DIR => $dir,
		SUFFIX => '.tbx',
	);
	$out->unlink_on_destroy(undef); # stays around after script
	chmod 0644, $out; # world-readable
	binmode($out, ":encoding($enc)"); # multilingual text

	# set up parser to process data
	$parser = XML::Rules->new(
		style => 'parse',
		rules => [read_rules()],
		stripspaces => 3|8,
		custom_escape => \&custom_escape,
		skip_xml_version => 1,
	);
	# $parser->{pad}{indent} = param('indent_with'); # const
	$parser->{pad}{indent} = "\t"; # const

	# misc preparation
	$enc_name = find_encoding($enc)->mime_name;

	# do ye some processin'
	{ my $saver = SelectSaver->new($out); # print to $out for now
		print <<PROLOGUE;
<?xml version="1.0" encoding="$enc_name"?>
<!DOCTYPE martif SYSTEM "TBXcoreStructV02.dtd">
PROLOGUE
		$parser->parse($in);
	} # destruction of $saver restores printing to STDOUT 

	# provide output
	print hr(), "\n";
	print p( 
		a({href => "$out"}, 'Download output immediately.'), 
		' Files will be discarded after 10-15 minutes.'
	), "\n";

}

sub print_warnings {
	print hr(), "\n";
	print h3('Warnings'), "\n";
	print ul("\n", $warnings_cache), "\n";
}

sub print_footer {
	print hr();
	print p('&copy; 2012 LTAC Global. This website is not affiliated with SDL plc. Trademarks are property of their owners. Tautologies are in the public domain.'), br();
	print a({href => '/'}, 'TBX Convert Home Page');
}



### Processing subroutines ###

# contents in order:
#
# review_mapping
#	import_teasp_template
#		validate_target (stub function)
# 		parse_out_xml (with own $parser)
#			fallback_parse (with regexes)
# 		import_substitution
# 			subst_from_hash
# 			subst_by_name
# 		import_placement (with %placement)
#	validate_queue_order
# custom_escape
# read_rules
#	several rules make reference to mapping via file-scoped variables:
#	^mtf rule refers to $dialect_name, $xcs_name
#	descripGrp rule refers to $categorial
#	concept/language/termGrp refer to $queue_orders via a subroutine
#	remaining subs up are called via ...Grp rules
# dismantle
# discover_level
# new_elements
# 	deep_copy
# create_unknown
# assemble_termComp
# assemble_termCompList
# bundle (refers to file-scoped $parser)
# 	start_bundle
#	notes_and_sources
# 	validate_bundling (with %valid_bundle; stub-like thus far)
# content_to_array
# execute_queue_orders
# 	drain_queues (calls bundle())
# remove_empty_queues
# queues_to_termCompLists
# prepend_auxInfo
# sweep_up
# append_unhandled

sub review_mapping {
	# review $categorial and $queue_orders from file scope	
	# maintenance: factor any lengthy checks out into subs
	
	# walk %$categorial looking for trouble
	while (my ($level, $datcats) = each %$categorial) {
		while (my ($user_datcat, $maps) = each %$datcats) {
			# @$maps contains mostly TEASPs, brief descriptions 
			# of how to turn certain data into valid TBX

			# disassemble for inspection
			my $key_list = shift @$maps;
				# @$maps now contains all special TEASPS
			my $default_teasp = shift @$key_list;
				# key list has corresponding special groups

			# do the numbers match up?
			my $special_groups = @$key_list;
			my $special_templates = @$maps;
			$special_groups == $special_templates or die "invalid mapping at $level level, $user_datcat: $special_groups special groups and $special_templates special templates\n";

			# loop over all TEASPs, validating and preprocessing
			my $teasp_desc = "default TEASP";
			my $n = 1;
			for my $teasp ($default_teasp, @$maps) {
				import_teasp_template(
					$teasp, $teasp_desc, 
					$user_datcat, $level
				);
				$teasp_desc = "special TEASP #$n"; $n++;
			}
			
			# any other checks on a user datcat here
			
			# reassemble for use
			unshift @$key_list, $default_teasp;
			unshift @$maps, $key_list;
		}
		
		# any other per-level checks here
	}
	
	# review category-bundling orders here
	while (my ($container, $list) = each %$queue_orders) {
		die "no list of category-bundling orders for $container\n"
			if ref $list ne 'ARRAY';
		validate_queue_order($_) for @$list;
		die "non-term-level order targets term-specific queue\n"
			if (
				$container ne 'termGrp' && 
				grep /^(?:termNote|termCompList)$/, 
					map {$_->[2]} @$list
			);
	}
}

sub import_teasp_template {
	my ($teasp, $teasp_desc, $user_datcat, $level) = @_;

	$teasp->[0] = validate_target($teasp->[0], $level);
	$teasp->[1] = parse_out_xml($teasp->[1]); # "element/attribute"
	$teasp->[2] = import_substitution(
		$teasp->[2], $teasp_desc, $user_datcat, $level
	);
	$teasp->[3] = import_placement(
		$teasp->[3], $teasp_desc, $user_datcat, $level
	);
	
	# any other per-teasp checks here
}

sub validate_target {
	return shift; # STUB
	# my ($target, $level) = @_;
	# points to potentially validate:
	# that targets don't step on reserved names
	# that termNote, termCompList are only targeted on term level
}

{ my $parser; # not the main one, a simple one for reading templates in

	BEGIN {
		$parser = XML::Rules->new(
			style => 'parse',
			stripspaces => 3|8,
			rules => [ _default => sub { [@_[0,1]] } ],
		);
	}

	sub parse_out_xml {
		my $element = shift;
		return $element if ref $element eq 'ARRAY'; # parsed already
		my $parse;
		{ local $@; # don't die with an unhelpful message
			$parse = eval { $parser->parse($element) };
		}
		if (not defined $parse) {
			$parse = fallback_parse($element);
			my $reconstruction = $parser->toXML($parse);
			$element = tt(custom_escape($element));
			$reconstruction = tt(custom_escape($reconstruction)); 
			warn "invalid XML in mapping: $element assumed to mean $reconstruction\n";
		}
		return $parse;
	}

} # end scope of template-reading parser 

{ my ($xml_name, $att_value); # regexes to dissect modestly ill-formed XML

	BEGIN {
		$xml_name = qr/[^\s<>=\/]+/; # broader than in the XML Spec
		$att_value = qr/ '(?>[^'<]*)' | "(?>[^"<]*)" /x; 
	}
	
	sub fallback_parse {
		my $element = shift;
		my ($tag, %attrs);
		for ($element) {
			($tag) = /($xml_name)/;
			%attrs = / ($xml_name) \s*=\s* ($att_value) /xg;
		}
		for (values %attrs) { s/^['"]//; s/['"]$//; }
		return [$tag, \%attrs];
	}

} # end scope of XML-dissecting regexes

sub import_substitution {
	# build coderef for proposed substitution
	my ($proposed, $teasp_desc, $user_datcat, $level) = @_;
	my $coderef;
	my $subst_type = ref $proposed;
	if ($subst_type eq 'HASH') {
		# user provided a list of substitutions
		# close coderef around hash of substitutions
		$coderef = subst_from_hash($proposed);
	} elsif ($subst_type eq '') {
		# user requested a named substitution
		$coderef = subst_by_name($proposed, $user_datcat);
	}
	if (not defined $coderef) {
		# ill-formed TEASP template or unknown subst name
		warn "invalid substitution '$proposed' at $level level, $user_datcat, $teasp_desc\n";
		$coderef = sub {
			"<!--ERROR: invalid substitution specified for content '$_[0]' of this element-->"
		};
	}
	return $coderef;
}

sub subst_from_hash {
	return sub { shift; } if 0 == keys %{$_[0]}; # empty hash: no-op
	my $corresp = shift;
	return sub {
		my $content = shift;
		$content = $corresp->{$content}
			if exists $corresp->{$content};
			 # if not, no change
		return $content;
	};
}

sub subst_by_name {
	my ($named_subst, $user_datcat) = @_;
	if (0) {
		die "false is true, hope the universe is paraconsistent\n";
	} elsif ($named_subst eq 'camel case') {
		return sub {
			my $value = shift;
			$value =~ s/ +([^ ])/\u$1/g;
			return $value;
		};
	} elsif ($named_subst eq 'category tag') {
		return sub { "[$user_datcat]: $_[0]" };
	} elsif ($named_subst eq 'lowercase') {
		return sub { lc shift; };
		# should use feature 'unicode_strings'
		# but can't rely on that before Perl 5.13
	} elsif ($named_subst eq 'null') {
		return sub { shift; };
	} else {
		# no such subst, you're out of luck
		return undef; # import_substitution will see this and warn
	}
	# maintenance: if the if-elsif chain gets too long, consider refactoring to a hash lookup.
	# But the hash has to provide a factory sub, which you call to define the substitution coderef, so that the substitution can close over $user_datcat (or any other variables so desired).
	# To be enclosed, these must be passed to the factory, and be read into lexicals inside it.
	# And in that case, remove subst_by_name() and just look up and call the factory inline in import_substitution(), and make a little scope around import_substitution to confine the hash (with a BEGIN to initialize it).
}

{ my %placement;

	BEGIN { 
		%placement = (
			content => sub {
				# my ($content, $attrhash) = @_;
				$_[1]{_content} = $_[0];
			},
			target => sub {
				# my ($content, $attrhash) = @_;
				$_[1]{target} = $_[0];
			},
			type => sub {
				$_[1]{type} = $_[0];
			},
			null => sub {},
		);
	}
	
	sub import_placement {
		# get coderef for proposed placement
		my ($proposed, $teasp_desc, $user_datcat, $level) = @_;
		my $coderef;
		unless ($coderef = $placement{$proposed}) {
			warn "invalid placement '$proposed' at $level level, $user_datcat, $teasp_desc\n";
			$coderef = sub {
				# my ($content, $attrhash) = @_;
				$_[1]->{_content} = "<!--ERROR: cannot place content '$_[0]' in location '$proposed' of this element-->";
			};
		} 
		return $coderef;
	}

} # end scope of %placement

{ my ($illegal_queue, $reserved); # regexps describing restricted targets

	BEGIN {
		$illegal_queue = qr/^(id | target | type | datatype | 
				xml:lang | username | date | 
				[CGPSTU]link | _content)$/x;
		$reserved = qr/^(_content | auxInfo | termNote | 
				termCompList | unhandled)$/x;
	}

	sub validate_queue_order {
		my $order = shift;
		die "ill-formed category-bundling order\n" 
			if ref $order ne 'ARRAY';
		die "invalid category-bundling order '@$order'\n" 
			if 3 != @$order;
		die "category-bundling order uses reserved name\n" 
			if grep /$illegal_queue/, @$order;
		die "category-bundling order drains reserved queue\n" 
			if grep /$reserved/, @$order[0,1];
	}

} # end scope of ($illegal_queue, $reserved)

# this sub kludges around XML::Rules's entity-escaping behavior
# in order to let me insert XML comments into the output text
# (it also allows CDATA and PIs because I had the regex handy)
sub custom_escape {
	return '' unless defined $_[0] and $_[0] ne '';
	my $do_not_escape = qr/
		<!-- (?: [^-] | -[^-] )* -->	# comment
	|	<!\[CDATA\[ .*? ]]>		# CDATA section
	|	<\? [^\s<>=\/]+ .*? \?>	# Processing Instruction
	/sx;
	my @pieces = split /($do_not_escape)/, $_[0];
	# even indices hold plain text and should be escaped
	# odd indices hold comments etc. and should not
	my $i = 0;
	foreach (@pieces) {
		next if $i++ % 2;
		s/&/&amp;/sg;
		s/</&lt;/sg;
		s/>/&gt;/sg;
		s/"/&quot;/sg;
	}
	return join '', @pieces;
}

sub read_rules {(
    # listing of handler routines to be run, 
    # mtf tags have special behavior
        # print TBX header, footer respectively
        # XML::Rules keeps <mtf> in memory but not all its children
        # so file can be arbitrarily long
    # conceptGrp contains code to print itself and all contents
    	# thus no other elements have code to print
        # they return a value (usually print-ready) to their containing element
    # one can write default handlers that just print tags through
    	# but not needed here
    	# and they force you to write lists of '^element' => 'handle',
    	# for every self-printing element and every element it contains
    # several rules refer to $parser from file scope
    	# if you're an anti-global purist, you can change it to $_[4]
    	# but good luck doing the same in bundle()
    	
	'^mtf' => sub { # printing handler
		# format and print TBX boilerplate header
		my $tbx_header = <<HEADER;
<martif type="$dialect_name" xml:lang="en">
	<martifHeader>
		<fileDesc>
			<sourceDesc>
				<p>auto-converted from MultiTerm XML</p>
			</sourceDesc>
		</fileDesc>
		<encodingDesc>
			<p type="DCSName">$xcs_name</p>
		</encodingDesc>
	</martifHeader>
	<text>
		<body>
HEADER
		$tbx_header =~ s/\t/$parser->{pad}{indent}/g;
		print $tbx_header;
		$parser->{pad}{level} = 3;
	},

	mtf => sub { # printing handler
		# print closing tags
		my $tbx_footer = <<'FOOTER';
		</body>
	</text>
</martif>
FOOTER
		$tbx_footer =~ s/\t/$parser->{pad}{indent}/g;
		print $tbx_footer;
		$parser->{pad}{level} = 0;
	},

 	conceptGrp => sub { # printing handler, also prints its contents
 		&execute_queue_orders;
		&prepend_auxInfo;
		sweep_up($_[1], 'id', '_content');
		append_unhandled($_[1]);
		# print yourself and all your children, with correct indents
		print $parser->{pad}{indent} x $parser->{pad}{level}; # base
		print $parser->toXML(
			'termEntry', # new element name
			$_[1], # attrhash
			0, # include closing tag
			$parser->{pad}{indent}, # indent with this
			$parser->{pad}{indent} x $parser->{pad}{level} # base
		    ), 
		    "\n";
	},

	concept => sub {
		return ('id' => "_$_[1]->{_content}");
		# id attr values must start with letter or underscore
	},

	system => '', # ignore

	languageGrp => sub {
		&execute_queue_orders;
		&prepend_auxInfo;
		sweep_up($_[1], 'xml:lang', '_content');
		append_unhandled($_[1]);
		# raw-type return of now-proper structure
		return [ 'langSet', $_[1] ]; # into conceptGrp's _content
	},

	language => sub { ('xml:lang' => $_[1]->{lang}) }, # promote

 	termGrp => sub {
		&execute_queue_orders;
		remove_empty_queues($_[1]);
		# presume other user-defined queues contain term components
		queues_to_termCompLists($_[1]);
		# build ntig (if necessary) or tig (if possible) by
		# plundering reserved queues: _content (contains term), 
		# auxInfo, termNote, and termCompLists
		# (maintenance: 'delete' returns undef for nonexistent keys,
		# but strict 'refs' makes @{undef} fatal; thus the || [] )
		my $elem;
		if (
			exists $_[1]{termCompList}
			or grep { $_->[0] eq 'termNoteGrp' }
				@{$_[1]{termNote}}
		) {
			# construct and return an ntig
			$elem = 'ntig';
			$_[1]{_content} = [
				[ 'termGrp', { _content => [ 
					@{$_[1]{_content} || []},
					@{delete $_[1]{termNote} || []},
					@{delete $_[1]{termCompList} || []},
				]}],
				@{delete $_[1]{auxInfo} || []}, 
			];
			# (in TBX, termGrp is an intra-ntig structure)
		} else {
			# construct and return a tig
			$elem = 'tig';
			$_[1]{_content} = [
				@{$_[1]{_content} || []}, 
				@{delete $_[1]{termNote} || []},
				@{delete $_[1]{auxInfo} || []},
			];
		}
		append_unhandled($_[1]);
		# change label of element, return it
		return [ $elem, $_[1] ]; # into languageGrp's _content
	},

	term => sub { [ $_[0], $_[1] ] }, # into termGrp's _content

	descripGrp => sub { 
		# does the heavy lifting of turning user datcats into TBX
		# based on <descrip>'s type and content, and on mapping
		
		# prepare useful variables	
		my ($type, $attrs, @contents) = dismantle($_[1]); 
		# still in $_[1]: queues from nested <descripGrp>s
		# discover own structural level
		my $level = discover_level($_[2]);
		# look up potential TBX equivalents to user's data category
		my $templates = $categorial->{$level}{$type};
		# create holding area for TBX elements soon to be built
		my @return; # will be an even-numbered list

		# set up new TBX elements
		while (my $value = shift @contents) {
			my @teasps = new_elements($templates, $value); 
			# recover if nothing suits the level, type, or value
			@teasps = create_unknown($type, $value, $attrs)
				unless @teasps;
			# iterate over teasps to process and insert value
			for my $teasp (@teasps) {
				my ($target, $elem, $subst, $place) 
					= @$teasp;
				# adopt attributes unless template overrides 
				$elem->[1] = { %$attrs, %{$elem->[1]} };
				# insert (possibly modified) data value
				$place->(
					$subst->($value), # correct value
					$elem->[1] # into the new element
				); 
				# add queues from nested descripGrps
				my %queues = deep_copy(%{$_[1]});
				assemble_termComp($elem, \%queues) 
					if $elem->[0] eq 'termComp';
				assemble_termCompList($elem, \%queues)
					if $elem->[0] eq 'termCompList';
				my $aux = delete $queues{auxInfo};
				bundle($elem, @$aux) if defined $aux;
				sweep_up(\%queues);
				if (my $unhandled = $queues{unhandled}) {
					content_to_array($elem->[1]);
					push @{$elem->[1]{_content}}, 
						['unhandled', $unhandled];
				}
				push @return, $target, $elem;
			} # per-teasp loop
		} # per-value loop
		# and, at last:
		@return;
	}, # end of descripGrp handler

	descrip => 'pass', # type and (string) _content into descripGrp

	xref => sub {
		# copy own content into descrip parent
		return "$_[1]->{_content}" if $_[2]->[-1] eq 'descrip';
		# otherwise reform self into a TBX xref and return that
		$_[1]->{target} = delete $_[1]->{Tlink};
		[ @_[0..1] ]
	},

	transacGrp => sub {
		# pull prefabricated children from named slots to content
		for (qw[type username date]) {
			push @{$_[1]->{_content}}, delete $_[1]->{$_};
		}
		return ('@auxInfo' => [ $_[0], $_[1] ]);
	},

	transac => sub {
		return (
			# fabricate TBX-style transac and transacNote,
			# store them in named slots in transacGrp
			type => ['transac', {
				type => 'transactionType',
				_content => $_[1]->{type},
			}],
			username => ['transacNote', {
				type => 'responsibility',
				_content => $_[1]->{_content},
			}],
		);
	}, 

	date => sub { (date => [ @_[0..1] ]) }, # self to named slot

	'hi, foreign, bpt, ept, ph' => 'raw',
	# I don't think these even exist in MultiTerm XML but it doesn't hurt

)} # end of read_rules()

sub dismantle {
	# strip information out of <descripGrp>'s attribute hash
	my $h = shift;
	my $type = delete $h->{type};
	my @contents = split /\|/, (delete $h->{_content} || 'NO CONTENT');
		# maintenance: presumes content is string, not arrayref
	# separate out any other attributes; leave queues
	my %attrs; 
	for my $key (keys %$h) {
		$attrs{$key} = delete $h->{$key} if not ref $h->{$key};
	}
	return ($type, \%attrs, @contents);
}

sub discover_level { # does not alter its arguments
	# argument is stack of names of containing elements
	# we grep the structural ones out and subscript to get the last one
	my $level = (grep /concept|language|term/, @{+shift})[-1];
	$level =~ s/Grp$//; # and clean it up a little
	return $level;
}

sub new_elements { # does not alter its arguments
	# instantiate appropriate TBX elements from templates
	my ($templates, $user_value) = @_;
	# if there were no plausible templates, save time: 
	return () unless defined $templates;
	# find indices of all special groups containing the user's value
	my @applicable;
	GROUP: for my $i (1 .. $#$templates) { # skipping index 0
			# NB: .. returns () if $#$templates < 1
		for my $special_value (@{$templates->[0][$i]}) {
			if ($user_value eq $special_value) {
				push @applicable, $i; # group applies
				next GROUP; # can skip rest of values in it
			}
		}
	}
	# grab the templates with the same indices
	@applicable = @{$templates}[@applicable]; # slice slice baby
	# default if no special group matched
	@applicable = ($templates->[0][0]) unless @applicable;
	# then build a proper TEASP from each applicable template
	@applicable = deep_copy(@applicable); # new elem/attrs
	# and proceed to return them
	return @applicable;
}

sub deep_copy { # does not alter its arguments
	my @return;
	while (my $this = shift) {
		my $type = ref $this;
		if ($type eq '') {
			# not a reference
			push @return, $this;
		} elsif ($type eq 'ARRAY') {
			push @return, [ deep_copy(@$this) ];
		} elsif ($type eq 'HASH') {
			push @return, { deep_copy(%$this) };
		} elsif ($type eq 'SCALAR') {
			push @return, \deep_copy($$this);
		} else {
			# other kinds of refs, give up and shallow-copy
			# in this program, that's good enough
			push @return, $this;
		}
	}
	@return;
}

sub create_unknown { # does not alter its argument
	my ($type, $value, $attrs) = @_;
	# create an <unknown> element to save the data but trip the validator
	my $unknown = [ 'unknown', {
		%$attrs,
		type => $type,
		_content => $value,
	}];
	# and a TEASP to hold it
	return ([
		'@unhandled', $unknown, 
		sub {}, sub {} # no need to modify or place content
	]);
}

sub assemble_termComp {
	# may replace first argument, modify referent of second argument
	# my ($elem, $queues) = @_;
	my $termNote = delete $_[1]->{termNote};
	bundle($_[0], @$termNote) if defined $termNote;
}

sub assemble_termCompList {
	# should modify referents of both arguments
	my ($elem, $queues) = @_;
	my $auxInfo = delete $queues->{auxInfo} || [];
	my $termComp = delete $queues->{termComp} || [];
	warn "No term components in the $elem->[1]{type} list\n" 
		unless @$termComp;
	my @contents = (@$auxInfo, @$termComp);
	$elem->[1]{_content} = \@contents if @contents;
}

sub bundle {
	my ($primary, @secondary) = @_;
	# replaces $_[0] with bundle but does not mutate $primary
	# subsequent arguments are placed inside bundle
	# and are not mutated or replaced

	my $bundle = start_bundle($primary);

	# loop over secondaries, validating and inserting
	for my $secondary (@secondary) {	

		# fix up special case: sources for notes
		notes_and_sources($bundle, $secondary);

		# verify that secondary is valid in bundle's content model
		my $legal = validate_bundling($bundle->[0], $secondary->[0]);
			# value is either 1 or undef
		
		# prep commentized secondary if necessary
		if (not $legal) {
			warn "$secondary->[0] rendered as comment to group it with $primary->[0]\n";
			my $stringified = $parser->toXML(@$secondary);
			$secondary = "<!-- $stringified -->";
		}
		
		# maintenance: if you don't want to use $parser as a 'global' there, you'll have to pass it down to bundle() from anything that calls it -- from the descripGrp handler and from drain_queues. Messy.

		# insert secondary into bundle
		push @{$bundle->[1]{_content}}, $secondary;

	} # (per-secondary loop ends)

	$_[0] = $bundle;
}

sub start_bundle {
	my ($primary) = @_; # does not modify its argument
	my $name = $primary->[0];
	# different behaviors for different primaries, but 
	# return value is always a brand-new element
	# with a brand-new content array that we can safely mutate
	if ($name =~ /^(?:admin|descrip|termComp|termNote|transac)$/) {
		# can just form ...Grp element
		return [ "${name}Grp", { _content => [ $primary ]}];
	} elsif ($name eq 'ref' or $name eq 'xref') {
		# ref and xref have no ... Grp, need a dummy <descrip>
		return [ 'descripGrp', { _content => [
			['descrip', {
				type => 'otherBinaryData',
				_content => 'see next element'
			}],
			$primary
		]}];
	} elsif ($name eq 'note') {
		# <note> turns into a special kind of admin when bundled
		my $upgrade = [ 'admin', { %{$primary->[1]} } ];
		$upgrade->[1]{type} = 'annotatedNote';
		return ['adminGrp', { _content => [ $upgrade ]}];
	} elsif ($name =~ /Grp$|^(?:admin|descrip|transac)Note$/x) {
		# already a grouping element, or can't head a group at all
		# we prepare a copy so we can mutate the content array
		my ($duplicate) = deep_copy($primary);
		# (and promote the content to an array if it isn't already)
		content_to_array($duplicate->[1]);
		return $duplicate;
	} elsif ($name eq 'unknown') {
		# result of trying to process an unmapped datcat
		return [ 'unknownGrp', { _content => [$primary] } ];
	} else {
		# quando omni flunkus moritati
		warn "cannot group other elements with $name\n";
		return [ 'unhandled', { _content => [$primary] } ];
	}
} # end of start_bundle

sub notes_and_sources {
	my ($bundle, $secondary) = @_; # replaces second argument
	return unless (
		$bundle->[0] eq 'adminGrp' &&
		$bundle->[1]{_content}[0][1]{type} eq 'annotatedNote' &&
		$secondary->[0] eq 'admin' &&
		$secondary->[1]{type} eq 'source'
	);
	# only if the bundle is <adminGrp><admin type="annotatedNote">
	# and the secondary is <admin type="source">
	# do we accommodate TBX's special case behavior:
	$_[1] = [ 'adminNote', { 
		%{$secondary->[1]}, # copy attributes and content, but
		type => 'noteSource' # override original type
	}];
}

{ my %valid_bundle;

	BEGIN {
		%valid_bundle = (
			'adminGrp' => {map {$_ => 1} qw (
				adminNote note ref xref
			)},
			'descripGrp' => {map {$_ => 1} qw (
				admin adminGrp descripNote
				note ref transacGrp xref
			)},
			'termCompGrp' => {map {$_ => 1} qw (
				admin adminGrp note ref termNote 
				termNoteGrp transacGrp xref
			)},
			'termNoteGrp' => {map {$_ => 1} qw (
				admin adminGrp note ref transacGrp xref
			)},
			'transacGrp' => {map {$_ => 1} qw (
				date note ref transacNote xref
			)},
		);
	}

	sub validate_bundling { # stub, inline it if it stays stubby
		my ($group, $content) = @_; 
		return $valid_bundle{$group}{$content}; # can return undef
	}

} # end scope of %valid_bundle

sub content_to_array {	
	my $attrhash = shift; # modifies %$attrhash,
	my $content = $attrhash->{_content}; # specifically this key
	if (not defined $content) {
		$attrhash->{_content} = [];
	} elsif (ref $content ne 'ARRAY') {
		# promote it to an array
		$attrhash->{_content} = [$content];
	} # else be harmless; content is already an array	
}

sub execute_queue_orders {
	my ($container, $attrhash) = @_; # modifies %$attrhash
	# get appropriate orders for level (from file-scoped var)
	my $orders = $queue_orders->{$container};
	for my $order (@$orders) {
		my ($primary_name, $secondary_name, $target_name) = @$order;
		# resolve these to arrayrefs
		# (if they don't exist, use as arrayrefs will autovivify;
		# if they already exist as attributes, die cryptically)
		my $primary = \@{ $attrhash->{$primary_name} };
		my $secondary = \@{ $attrhash->{$secondary_name} };
		# issue warnings as needed
		if (1 < @$primary && 0 < @$secondary) {
	 		warn "user must verify that each $primary_name is bundled with its own $secondary_name\n";
	 		push @{$attrhash->{auxInfo}}, "\n<!-- user must verify/adjust alignment of $secondary_name(s) to ${primary_name}s -->";
	 	}
		if (@$primary < @$secondary) {
			warn "data produced more ${secondary_name}s than could be aligned to ${primary_name}s\n";
			# in this case sweep_up() will catch the extras
		}
		push @{$attrhash->{$target_name}}, 
			drain_queues($primary, $secondary);
			# may autovivify target array
		# if queues drain completely or were empty to begin with,
		# sweep_up() will get rid of them
	}
}

sub drain_queues {
	my ( $primary_queue, $secondary_queue ) = @_; # modifies both arrays
	my @return;
	# pair items from queues one-to-one as far as they go
	while (my $primary = shift @$primary_queue) {
		if (my $secondary = shift @$secondary_queue) {
			bundle($primary, $secondary);
		} # and if not, so be it. Either way:
		push @return, $primary;
	}
 	@return;
}

sub remove_empty_queues {
	my $attrhash = shift; # modifies %$attrihash
	for my $key (keys %$attrhash) {
		next unless ref $attrhash->{$key} eq 'ARRAY';
		next unless @{ $attrhash->{$key} } == 0;
		delete $attrhash->{$key};
	}
}

sub queues_to_termCompLists {
	my $attrhash = shift; # modifies %$attrhash
	for my $key (keys %$attrhash) {
		# don't process reserved queues
		next if $key =~ /^(?: _content | auxInfo | 
			termCompList | termNote | unhandled )$/x;
		next unless @{$attrhash->{$key}}; # shouldn't happen
		# sort queue contents
		my (@auxInfo, @termComps);
		for my $elem (@{ delete $attrhash->{$key} }) {
			if ($elem->[0] =~ /^termComp(?:Grp)?$/) {
				push @termComps, $elem;
			} else {
				push @auxInfo, $elem;
			}
		}
		warn "No term components in the $key list\n" 
			unless @termComps;
		# assemble <termCompList type=$key>
		push @{$attrhash->{termCompList}}, 
			['termCompList', {
				type => $key, 
				_content => [@auxInfo, @termComps],
			}]; 
	}
}

sub prepend_auxInfo {
	# modifies $_[1], an attrhash
	# maintenance: the '|| []' guard prevents non-strict @{+undef}
	$_[1]{_content} = [
		@{delete $_[1]{auxInfo} || []}, 
		@{$_[1]{_content} || []}
	];
}

sub sweep_up {
	# consolidate stray attrhash entries
	my $attrhash = shift; # modifies %$attrhash
	# remaining arguments are non-stray entries
	# remove empty queues from containing element's hash
	remove_empty_queues($attrhash);
	# put unhandled non-empty queues/attributes into 'unhandled'
	# hash should have only 'unhandled' and other keys as specified
	my %allowed = map {$_ => 1} ('unhandled', @_);
	# remove all other keys and push them into unhandled
	for my $key (grep {!$allowed{$_}} keys %$attrhash) {
		warn "incomplete mapping: elements targeted to $key are not handled by their container\n";
		push @{$attrhash->{unhandled}}, 
			[ $key, delete $attrhash->{$key} ];
	}
}

sub append_unhandled {
	# pull stray elements from element's 'unhandled' queue,
	# wrap them in <unhandled>, push that to end of _content
	my $attrhash = shift; # modifies %$attrhash
	content_to_array($attrhash);
	push @{$attrhash->{_content}}, 
		['unhandled', delete $attrhash->{unhandled}] 
		if $attrhash->{unhandled};
}


