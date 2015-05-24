# An R interface to the DataTables library

[![Build Status](https://travis-ci.org/rstudio/DT.svg)](https://travis-ci.org/rstudio/DT)

This package provides a function `datatable()` to display R data via the [DataTables](http://datatables.net/) library (N.B. not to be confused with the **data.table** package).

## Installation

This package is not on CRAN yet, and you can install it with **devtools**:

```r
devtools::install_github('rstudio/DT')
# then try DT::datatable(iris) as a hello world example
```

See the full documentation at <http://rstudio.github.io/DT>.

## Updating rstudio/DT fork

See <https://help.github.com/articles/syncing-a-fork/> and <https://help.github.com/articles/configuring-a-remote-for-a-fork/>

```
cd ~/gh/DT_fork
git remote add upstream git@github.com:rstudio/DT.git
git fetch upstream
git merge upstream/master
```

