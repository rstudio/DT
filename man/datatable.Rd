% Please edit documentation in R/datatables.R
\name{datatable}
\alias{datatable}
\title{Create an HTML table widget using the DataTables library}
\usage{
datatable(data, options = list(), callback = "function(table) {}", colnames, container, 
    server = FALSE, escape = TRUE, extensions = NULL)
}
\arguments{
\item{data}{a data object (either a matrix or a data frame)}

\item{options}{a list of initialization options (see
\url{http://datatables.net/reference/option/}); the character options
wrapped in \code{\link[htmlwidgets]{JS}()} will be treated as literal
JavaScript code instead of normal character strings}

\item{callback}{a JavaScript callback function to be applied to the
DataTables instance; if extensions are used, a default callback will be
automatically set up to initialize the extensions, and you have to provide
a different different callback function if you want further customization
of the extensions, which will require you to learn more about DataTables
extensions and JavaScript}

\item{colnames}{if missing, the column names of the data; otherwise it can be
an unnamed character vector of names you want to show in the table header
instead of the default data column names; alternatively, you can provide a
\emph{named} numeric or character vector of the form \code{'newName1' = i1,
'newName2' = i2} or \code{c('newName1' = 'oldName1', 'newName2' =
'oldName2', ...)}, where \code{newName} is the new name you want to show in
the table, and \code{i} or \code{oldName} is the index of the current
column name}

\item{container}{a sketch of the HTML table to be filled with data cells; by
default, it is generated from \code{htmltools::tags$table()} with a table
header consisting of the column names of the data}

\item{server}{whether to use server-side processing; if \code{TRUE}, you must
provide a server URL so that DataTables can send Ajax requests to retrieve
data from the server}

\item{escape}{whether to escape HTML entities in the table: \code{TRUE} means
to escape the whole table, and \code{FALSE} means not to escape it;
alternatively, you can specify numeric column indices or column names to
indicate which columns to escape, e.g. \code{1:5} (the first 5 columns),
\code{c(1, 3, 4)}, or \code{c(-1, -3)} (all columns except the first and
third), or \code{c('Species', 'Sepal.Length')}}

\item{extensions}{a character vector of the names of the DataTables
extensions (\url{http://datatables.net/extensions/index})}
}
\description{
This function creates an HTML widget to display rectangular data (a matrix or
data frame) using the JavaScript library DataTables.
}
\note{
You are recommended to escape the table content for security reasons
  (e.g. XSS attacks) when using this function in Shiny or any other dynamic
  web applications.
}
\examples{
library(DT)

# see the package vignette for examples and the link to website
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

# dates
datatable(data.frame(
  date = seq(as.Date("2015-01-01"), by = "day", length.out = 5), x = 1:5
))
datatable(data.frame(x = Sys.Date()))
datatable(data.frame(x = Sys.time()))
}
