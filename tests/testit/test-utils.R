library(testit)

assert('dropNULL() works', {
  (dropNULL(list()) %==% list())
  (dropNULL(list(a = 1, b = NULL)) %==% list(a = 1))
})

assert('upperToDash() works', {
  (upperToDash('fontWeight') %==% 'font-weight')
  (upperToDash('backgroundColor') %==% 'background-color')
})

assert('captionString() works', {
  (captionString('title') %==% '<caption>title</caption>')
})

assert('coerceValue() works', {
  # integer
  (coerceValue(2.1, 1L) %==% 2L)
  # numeric
  (coerceValue(2L, 1.0) %==% 2.0)
  # character
  (coerceValue(2.1, 'a') %==% '2.1')
  # Date
  (coerceValue('2020-01-01', Sys.Date()) %==% as.Date('2020-01-01'))
  # POSIXct POSIXlt
  (coerceValue('2020-01-01T12:00:00Z', Sys.time()) %==%
      as.POSIXct('2020-01-01 12:00:00', tz = 'UTC'))
  (coerceValue('2020-01-01T12:00:00Z', as.POSIXlt(Sys.time())) %==%
      as.POSIXlt('2020-01-01 12:00:00', tz = 'UTC'))
  # factor
  (coerceValue('a', factor(levels = c('a', 'b'))) %==% 'a')
  has_warning(out <- coerceValue(c('a', 'c'), factor(levels = c('a', 'b'))))
  (out %==% c('a', NA_character_))
  # coerceValue() should not throw warings for characters #541
  !has_warning(coerceValue('a', 'b'))
  # warn unsupported datatype
  has_warning(coerceValue('b', list(1)))
})

assert('fromJSON() will never try to read from a URL', {
  out = try(fromJSON('https://a.b.c'), silent = TRUE)
  grepl('invalid char in json text', as.character(out), fixed = TRUE)
})
