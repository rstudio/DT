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
