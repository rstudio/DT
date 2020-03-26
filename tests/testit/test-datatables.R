library(testit)

assert('appendColumnDefs() works', {
  (appendColumnDefs(list(), 1) %==% list(columnDefs = list(1)))
  (appendColumnDefs(list(a = 1), 2) %==% list(a = 1, columnDefs = list(2)))
  (appendColumnDefs(list(columnDefs = list(1)), 2) %==% list(columnDefs = list(1, 2)))
})

assert('classNameDefinedColumns() works', {
  opt = list()
  (classNameDefinedColumns(opt, ncol = 2) %==% integer())
  # only count className defined
  opt = list(columnDefs = list(list(targets = '_all')))
  (classNameDefinedColumns(opt, ncol = 3) %==% integer())
  opt$columnDefs[[1]]$className = 'xxx'
  (classNameDefinedColumns(opt, ncol = 3) %==% 0:2)
  # negative is interpreted as right to left cols
  opt = list(columnDefs = list(list(targets = -(3:4), className = 'xxx')))
  (classNameDefinedColumns(opt, ncol = 5L) %==% 2:1)
  # not '_all' and not numeric will get ignored
  opt = list(columnDefs = list(list(targets = 'bbb', className = 'xxx')))
  (classNameDefinedColumns(opt, ncol = 5L) %==% integer())
})
