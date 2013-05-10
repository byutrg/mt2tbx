##Sample Data for mt2tbx

The files in this directory are examples of converting MultiTerm XML files to TBX using mt2tbx. Each directory contains one or more XML files and an accompanying mapping to direct conversion in mt2tbx. Each also contains the output of mt2tbx, along with an appropriate XCS file for the generated TBX dialect. If the XCS is not present, then the file uses a standard dialect, for which validation files are available in a [separate repository](https://github.com/byutrg/TBX-Spec).

###MultiTerm Template Files

The mapping and XCS files in [MTTemplate][./MTTemplate] allow one to convert any MultiTerm termbase which was created using *only* default values and data categories. We expect them to be extremely useful, as they allow one to skip the entire process of creating a mapping file. If you constrain yourself to using MultiTerm's default termbase definitions, then converting to TBX is trivial.