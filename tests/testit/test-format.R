library(testit)

# fix issue #785
assert('formatXXX() should throw clear errors when table is not valid', {
  # The implementation of formatDate is a little different from other formatting functions
  # So we test both of them
  (has_error(formatDate(list(x = 1L)), silent = TRUE))
  (has_error(formatCurrency(list(x = 1L)), silent = TRUE))
  out = try(formatDate(list(x = 1L)), silent = TRUE)
  (grepl('Invalid table', as.character(out), fixed = TRUE))
  out = try(formatCurrency(list(x = 1L)), silent = TRUE)
  (grepl('Invalid table', as.character(out), fixed = TRUE))
})

# fix issue #790
# should generate the render callback for each columns individually
assert('formatting functions should support vectorized input', {
  out = datatable(iris) %>% formatRound(1:2, digits = 1:2)
  defs = Filter(function(x) !is.null(x$render), out$x$options$columnDefs)
  (length(defs) %==% 2L)
  (defs[[1L]]$target %==% 1L)
  (grepl('DTWidget.formatRound(data, 1, 3', defs[[1L]]$render, fixed = TRUE))
  (defs[[2L]]$target %==% 2L)
  (grepl('DTWidget.formatRound(data, 2, 3', defs[[2L]]$render, fixed = TRUE))
})

# issue #799 #702
assert('formatStyle is chainable and unmatched CSS value should be left as it is', {
  out = datatable(
    data.frame(V1 = c('a', 'green', 'c', 'yellow'), V2 = c('1', '2', '3', '4'))
  ) %>% formatStyle(
    'V1',
    target = 'row',
    backgroundColor = styleEqual('a', 'red')
  ) %>% formatStyle(
    'V1',
    target = 'row',
    backgroundColor = styleEqual('c', 'green')
  )
  expect = JS(
    'function(row, data) {',
      'var value=data[1]; $(row).css({\'background-color\':value == "a" ? "red" : null});',
      'var value=data[1]; $(row).css({\'background-color\':value == "c" ? "green" : null});',
    '}'
  )
  (out$x$options$rowCallback %==% expect)
})

assert('styleValue returns raw value', {
  tbl = data.frame(
    COL = c("A", "B", "C"),
    COLOR = c("#DF9AC2", "#83BF9A", "#A2D485"),
    stringsAsFactors = FALSE
  )
  out = datatable(tbl) %>%
    formatStyle(columns = 1, valueColumns = 2, background = styleValue())
  expect = JS(
    'function(row, data) {',
    'var value=data[2]; $(this.api().cell(row, 1).node()).css({\'background\':value});',
    '}'
  )
  (out$x$options$rowCallback %==% expect)
})

# issue #831
assert('formatting functions allow named colname inputs', {
  x = datatable(mtcars)
  x = formatRound(x, c('mpg' = 1, 'cyl' = 2), mark = ".", dec.mark = ",")
  coldefs = x$x$options$columnDefs
  (names(coldefs) %==% NULL)
})
