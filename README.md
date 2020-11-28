
# quanteda.textplots: plots and visualisation for quanteda

<!-- badges: start -->

[![CRAN
Version](https://www.r-pkg.org/badges/version/quanteda.textplots)](https://CRAN.R-project.org/package=quanteda.textplots)
[![](https://img.shields.io/badge/devel%20version-0.90-royalblue.svg)](https://github.com/quanteda/quanteda.textplots)
[![Downloads](https://cranlogs.r-pkg.org/badges/quanteda.textplots)](https://CRAN.R-project.org/package=quanteda.textplots)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/quanteda.textplots?color=orange)](https://CRAN.R-project.org/package=quanteda.textplots)
[![R build
status](https://github.com/quanteda/quanteda.textplots/workflows/R-CMD-check/badge.svg)](https://github.com/quanteda/quanteda.textplots/actions)
[![codecov](https://codecov.io/gh/quanteda/quanteda.textplots/branch/master/graph/badge.svg)](https://codecov.io/gh/quanteda/quanteda.textplots)
[![DOI](http://joss.theoj.org/papers/10.21105/joss.00774/status.svg)](https://doi.org/10.21105/joss.00774)
<!-- badges: end -->

## About

Contains the textstat functions formerly in **quanteda**. For more
details, see <https://quanteda.io>.

## How to Install

The normal way from CRAN, using your R GUI or

``` r
install.packages("quanteda.textplots") 
```

Or for the latest development version:

``` r
# devtools package required to install quanteda from Github 
devtools::install_github("quanteda/quanteda.textplots") 
```

Because this compiles some C++ and Fortran source code, you will need to
have installed the appropriate compilers.

**If you are using a Windows platform**, this means you will need also
to install the [Rtools](https://CRAN.R-project.org/bin/windows/Rtools/)
software available from CRAN.

**If you are using macOS**, you should install the [macOS
tools](https://cran.r-project.org/bin/macosx/tools/), namely the Clang
6.x compiler and the GNU Fortran compiler (as **quanteda.textplots**
requires gfortran to build). If you are still getting errors related to
gfortran, follow the fixes
[here](https://thecoatlessprofessor.com/programming/rcpp-rcpparmadillo-and-os-x-mavericks--lgfortran-and--lquadmath-error/).

## How to cite

Benoit, Kenneth, Kohei Watanabe, Haiyan Wang, Paul Nulty, Adam Obeng,
Stefan Müller, and Akitaka Matsuo. (2018) “[quanteda: An R package for
the quantitative analysis of textual
data](https://www.theoj.org/joss-papers/joss.00774/10.21105.joss.00774.pdf)”.
*Journal of Open Source Software*. 3(30), 774.
<https://doi.org/10.21105/joss.00774>.

For a BibTeX entry, use the output from `citation(package =
"quanteda.textplots")`.
