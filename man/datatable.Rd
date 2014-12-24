% Please edit documentation in R/datatables.R
\name{datatable}
\alias{datatable}
\title{Create an HTML table widget using the DataTables library}
\usage{
datatable(data, id = NULL, options = list(), callback = "function(table) {}", container, 
    server = FALSE)
}
\arguments{
\item{data}{a data object (either a matrix or a data frame)}

\item{id}{the id for the table}

\item{options}{a list of initialization options (see
\url{http://datatables.net/reference/option/}); the character options
wrapped in \code{\link[htmlwidgets]{JS}()} will be treated as literal
JavaScript code instead of normal character strings}

\item{callback}{a JavaScript callback function to be applied to the
DataTables instance}

\item{container}{a sketch of the HTML table to be filled with data cells; by
default, it is generated from \code{htmltools::tags$table()} with a table
header consisting of the column names of the data}

\item{server}{whether to use server-side processing; if \code{TRUE}, you must
provide a server URL so that DataTables can send Ajax requests to retrieve
data from the server}
}
\description{
This function creates an HTML widget to display rectangular data (a matrix or
data frame) using the JavaScript library DataTables.
}
\examples{
library(DT)

# see the package vignette for examples
vignette('DT', package = 'DT')

# some boring edge cases for testing purposes
m = matrix(nrow = 0, ncol = 5, dimnames = list(NULL, letters[1:5]))
datatable(m)  # zero rows
datatable(as.data.frame(m))

m = matrix(1, dimnames = list(NULL, 'a'))
datatable(m)  # one row and one column
datatable(as.data.frame(m))

m = data.frame(a = 1, b = 2, c = 3)
datatable(m)
datatable(as.matrix(m))
}
