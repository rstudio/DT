# An R interface to the DataTables library

<!-- badges: start -->
[![R-CMD-check](https://github.com/rstudio/DT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/DT/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/rstudio/DT/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rstudio/DT?branch=main)
[![Downloads from the RStudio CRAN mirror](https://cranlogs.r-pkg.org/badges/DT)](https://cran.r-project.org/package=DT)
<!-- badges: end -->

This package provides a function `datatable()` to display R data via the [DataTables](https://datatables.net/) library (N.B. not to be confused with the **data.table** package).

## Installation

You may install the stable version from CRAN, or the development version using **remotes**:

```r
# install from CRAN
install.packages('DT')

# or the development version if necessary
# install.packages("pak")
pak::pak('rstudio/DT')

# then try DT::datatable(iris) as a hello world example
```

See the full documentation at <https://rstudio.github.io/DT/>. Please use [Github issues](https://github.com/rstudio/DT/issues) only if you want to file bug reports or feature requests, and you are expected to ask questions on [StackOverflow](https://stackoverflow.com/questions/tagged/dt) with at least the tags `r` and `dt`.

Please note that due to limited resources, this package is mostly in the maintenance-only mode. The current maintainer (Yihui Xie) does not have enough time or interest in developing any new features. The only priority is to fix important bugs. We welcome you to submit pull requests to implement new features or fix bugs, but cannot guarantee these pull requests will be reviewed or merged (we will try our best). Thanks for your understanding! BTW, you may also consider [other packages for making tables](https://bookdown.org/yihui/rmarkdown-cookbook/table-other.html), although **DT** has a few unique features that are unlikely to be available in other packages.
