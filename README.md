# An R interface to the DataTables library

[![Build Status](https://travis-ci.org/rstudio/DT.svg)](https://travis-ci.org/rstudio/DT)

This package provides a function `datatable()` to display R data via the [DataTables](http://datatables.net/) library (N.B. not to be confused with the **data.table** package).

## Installation

This package is not on CRAN yet, and you can install it with **devtools**:

```r
if (!requireNamespace('htmlwidgets') || packageVersion('htmlwidgets') <= '0.3.2')
  devtools::install_github('ramnathv/htmlwidgets')
devtools::install_github('rstudio/DT')
# then try DT::datatable(iris) as a hello world example
```

See the full documentation at <http://rstudio.github.io/DT>.
