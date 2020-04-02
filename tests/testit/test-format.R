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
