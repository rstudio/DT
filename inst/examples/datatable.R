# !formatR
library(DT)

# see the package vignette for examples
vignette('DT', package = 'DT')

# escape table content or not
m = matrix(c(
  '<b>Bold</b>', '<em>Emphasize</em>', '<a href="http://rstudio.com">RStudio</a>',
  '<a href="#" onclick="alert(\'Hello World\');">Hello</a>'
), 2)
colnames(m) = c('<span style="color:red">Column 1</span>', '<em>Column 2</em>')
datatable(m)
datatable(m, escape = FALSE)
datatable(m, escape = 1)
datatable(m, escape = 2)
datatable(m, escape = c(TRUE, FALSE))
colnames(m) = c('V1', 'V2')
datatable(m, escape = 'V1')

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

# DataTables extensions
datatable(
  cbind(ID = seq_len(nrow(iris)), iris), extensions = 'AutoFill',
  callback = 'function(table) {new $.fn.dataTable.AutoFill(table);}'
)

datatable(iris, extensions = 'ColReorder', options = list(dom = 'Rlfrtip'))

datatable(iris, extensions = 'ColVis', options = list(dom = 'C<"clear">lfrtip'))

m = as.data.frame(matrix(rnorm(100), 5))
m = cbind(ID = seq_len(nrow(m)), m)
datatable(
  m, extensions = 'FixedColumns',
  callback = 'function(table){new $.fn.dataTable.FixedColumns(table);}',
  options = list(
    paging = FALSE,
    scrollX = TRUE,
    scrollY = '300px',
    scrollCollapse = TRUE
  )
)

datatable(
  iris, options = list(pageLength = 50), extensions = 'FixedHeader',
  callback = 'function(table) {new $.fn.dataTable.FixedHeader(table);}'
)

datatable(
  iris, extensions = 'KeyTable',
  callback = 'function(table) {$.fn.dataTable.KeyTable(table);}'
)

datatable(iris, extensions = 'Responsive', options = list(responsive = TRUE))

m = matrix(runif(50000 * 4), ncol = 4, dimnames = list(NULL, letters[1:4]))
m = cbind(id = seq_len(nrow(m)), round(m, 2))
datatable(m, extensions = 'Scroller', options = list(
  deferRender = TRUE,
  dom = "frtiS",
  scrollY = 200,
  scrollCollapse = TRUE
))
