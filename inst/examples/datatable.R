# !formatR
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
  initComplete = I(c(
    "function(settings, json) {",
    "alert('Initialization complete!');",
    "}"))
))

# if the data is wider than 6 chars, use first 6 + ...
datatable(iris, options = list(order = list(), columnDefs = list(list(
  targets = 4,
  render = I(c(
    "function ( data, type, full, meta ) {",
    "return type === 'display' && data.length > 6 ?",
    "'<span title=\"'+data+'\">'+data.substr( 0, 6 )+'...</span>' : data;",
    "}"))
))))

# display some big numbers, right-align them, and format the 3rd column
m = as.data.frame(matrix(rnorm(100, 1e5, 1e6), 20))
datatable(m, options = list(
  columnDefs = list(list(className = 'dt-right', targets = '_all')),
  rowCallback = I(c(
    "function(row, data) {",
    "var num = '$' + data[2].toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');",
    "$('td:eq(2)', row).html(num);",
    "}"))
))


# some edge cases
m = matrix(nrow = 0, ncol = 5, dimnames = list(NULL, letters[1:5]))
datatable(m)  # zero rows
datatable(as.data.frame(m))

m = matrix(1, dimnames = list(NULL, 'a'))
datatable(m)  # one row and one column
datatable(as.data.frame(m))
