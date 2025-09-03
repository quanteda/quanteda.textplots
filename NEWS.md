# quanteda.textplots 0.96

* Removes the dependency on **extrafont**, which was scheduled for archiving due to an unfixed failure of one of its reverse dependencies.

# quanteda.textplots 0.95

* Trims the C++ legacy compiler directives that are no longer needed, and were causing problems with the checks on CRAN.
* Removed broken Rd links causing problems with the checks on CRAN.

# quanteda.textplots 0.94.2 - 0.94.4

* Updates to maintain clean CRAN checks and to ensure consistency with minor changes in underlying packages.


# quanteda.textplots 0.94.1

* Test update for changes in upcoming Matrix 1.4-1.

# quanteda.textplots 0.94

* Further updates in preparation for quanteda v3.

# quanteda.textplots 0.93

* Updates fcm and kwic plot methods for quanteda v3.
* Removes dependency on data.table.

# quanteda.textplots 0.91

* Fixes some CRAN NOTEs caused by Namespaces in Imports field not being imported, and an undeclared package in an Rd object.

# quanteda.textplots 0.90

New version split from quanteda 2.1.2, moving the `textplot_*()` functions to a separate package as part of a modularisation of the quanteda family of packages.
