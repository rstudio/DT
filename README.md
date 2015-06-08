# An R interface to the DataTables library

[![Build Status](https://travis-ci.org/rstudio/DT.svg)](https://travis-ci.org/rstudio/DT)

This package provides a function `datatable()` to display R data via the [DataTables](http://datatables.net/) library (N.B. not to be confused with the **data.table** package).

## Installation

You may install the stable version from CRAN, or the development version using **devtools**:

```r
# install from CRAN
install.packages('DT', repos = 'http://cran.rstudio.com')

# or the development version if necessary
devtools::install_github('rstudio/DT')

# then try DT::datatable(iris) as a hello world example
```

See the full documentation at <http://rstudio.github.io/DT>.
