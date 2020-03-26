library(testit)

assert('appendColumnDefs() works', {
  (appendColumnDefs(list(), 1) %==% list(columnDefs = list(1)))
  (appendColumnDefs(list(a = 1), 2) %==% list(a = 1, columnDefs = list(2)))
  (appendColumnDefs(list(columnDefs = list(1)), 2) %==% list(columnDefs = list(1, 2)))
})
