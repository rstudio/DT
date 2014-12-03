\name{datatable}
\alias{datatable}
\title{Create an HTML table widget using the DataTables library}
\usage{
datatable(data, id = NULL, options = list(order = list()), 
    callback = "function(table) {}")
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
}
\description{
This function creates an HTML widget to display rectangular data (a matrix or
data frame) using the JavaScript library DataTables.
}
\examples{
library(DT)
# DataTables' default is to sort the first column, and datatable() removed the
# default sorting
datatable(iris)

# align the first 4 columns to the right
datatable(iris, options = list(
  columnDefs = list(list(className = 'dt-right', targets = 0:3))
))

# sort column 2 (ascending) and 4 (descending)
datatable(mtcars, options = list(order = list(list(1, 'asc'), list(3, 'desc'))))

# the initComplete callback
datatable(iris, options = list(
  order = list(),
  initComplete = JS(
    "function(settings, json) {",
    "alert('Initialization complete!');",
    "}")
))

# if the data is wider than 6 chars, use first 6 + ...
datatable(iris, options = list(order = list(), columnDefs = list(list(
  targets = 4,
  render = JS(
    "function ( data, type, full, meta ) {",
    "return type === 'display' && data.length > 6 ?",
    "'<span title=\\"'+data+'\\">'+data.substr( 0, 6 )+'...</span>' : data;",
    "}")
))))

# display some big numbers, right-align them, and format the 3rd column
m = as.data.frame(matrix(rnorm(100, 1e5, 1e6), 20))
datatable(m, options = list(
  columnDefs = list(list(className = 'dt-right', targets = '_all')),
  rowCallback = JS(
    "function(row, data) {",
    "var num = '$' + data[2].toString().replace(/\\\\B(?=(\\\\d{3})+(?!\\\\d))/g, ',');",
    "$('td:eq(2)', row).html(num);",
    "}")
))


# some edge cases
m = matrix(nrow = 0, ncol = 5, dimnames = list(NULL, letters[1:5]))
datatable(m)  # zero rows
datatable(as.data.frame(m))

m = matrix(1, dimnames = list(NULL, 'a'))
datatable(m)  # one row and one column
datatable(as.data.frame(m))
}
