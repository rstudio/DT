library(testit)

# Factors and strings are searched differently.
# Older versions of R don't have this set.
op <- options(stringsAsFactors = FALSE)

assert('searching integer values works', {
  x = seq(-2, 2)
  (setequal(doColumnSearch(x, '0 ... 2'), 3:5))
  (setequal(doColumnSearch(x, '-2 ... 0'), 1:3))
})

assert('searching numeric values works', {
  x = seq(-2, 2) / 10
  (setequal(doColumnSearch(x, '0 ... 0.15'), 3:4))
  (setequal(doColumnSearch(x, '-0.2 ... 0'), 1:3))
})

assert('searching dates works', {
  x = as.Date('2022-03-17') + seq(-1, 1)
  (setequal(doColumnSearch(x, '2022-03-16 ... 2022-03-17'), 1:2))
})

assert('searching factors works', {
  (setequal(doColumnSearch(factor(c('A', 'B')), '["A"]'), 1L))
  (setequal(doColumnSearch(factor(c('A', 'B')), '["B"]'), 2L))
  (setequal(doColumnSearch(factor(c('A', 'B')), '["A", "B"]'), 1:2))
})

assert('searching booleans works', {
  (setequal(doColumnSearch(c(TRUE, FALSE), '["true"]'), 1L))
  (setequal(doColumnSearch(c(TRUE, FALSE), '["false"]'), 2L))
  (setequal(doColumnSearch(c(TRUE, FALSE), '["true", "false"]'), 1:2))
})

assert('searching characters works', {
  (setequal(doColumnSearch(c('foo', 'food'), 'oo'), 1:2))
  (setequal(doColumnSearch(c('foo', 'food'), 'ood'), 2L))
})

assert('character search options work', {
  (setequal(doColumnSearch(c('foo', 'food'), 'foo.?', options = list(regex = TRUE)), 1:2))
  (setequal(doColumnSearch(c('foo', 'food'), 'foo.?', options = list(regex = FALSE)), integer(0)))
  (setequal(doColumnSearch(c('foo', 'Foo'), 'F', options = list(caseInsensitive = TRUE)), 1:2))
  (setequal(doColumnSearch(c('foo', 'Foo'), 'F', options = list(caseInsensitive = FALSE)), 2L))
})

assert('global search works', {
  tbl = data.frame(
    foo = c('foo', 'bar', 'baz'),
    bar = c('bar', 'baz', 'foo')
  )
  (setequal(doGlobalSearch(tbl, 'ba'), 1:3))
  (setequal(doGlobalSearch(tbl, 'bar'), 1:2))
  (setequal(doGlobalSearch(tbl, 'baz'), 2:3))
  (setequal(doGlobalSearch(tbl, 'f'), c(1L, 3L)))
})

assert('smart searching works', {
  tbl = data.frame(foo = c('foo bar baz', 'bar baz foo'))
  (setequal(doGlobalSearch(tbl, 'foo bar', options = list(smart = TRUE)), 1:2))
  (setequal(doGlobalSearch(tbl, 'foo bar', options = list(smart = FALSE)), 1L))
})

assert('empty search returns everything', {
  tbl = data.frame(foo = c('foo bar baz', 'bar baz foo'))
  (setequal(doGlobalSearch(tbl, ''), 1:2))
  (setequal(doGlobalSearch(tbl, NULL), 1:2))
  (setequal(doColumnSearch(tbl$foo, ''), 1:2))
  (setequal(doColumnSearch(tbl$foo, NULL), 1:2))
})


# We know from tests above that individual search components work.
# Here we make sure they are combined correctly for client queries.

# Helpers to create client queries that run without errors
clientQuery = function(data, global = globalQuery(), columns = list(columnQuery())) {
  columns = rep_len(columns, ncol(data))
  names(columns) = seq_len(ncol(data)) - 1
  list(
    draw = '0',
    start = '0',
    length = '10',
    escape = 'true',
    search = global,
    columns = columns
  )
}

globalQuery = function(value = '', regex = FALSE, caseInsensitive = TRUE, smart = TRUE) {
  list(
    value = value,
    regex = tolower(regex),
    caseInsensitive = tolower(caseInsensitive),
    smart = tolower(smart)
  )
}

columnQuery = function(value = '', searchable = TRUE, regex = FALSE) {
  list(
    searchable = tolower(searchable),
    search = list(value = value, regex = tolower(regex))
  )
}

assert('server-side search handler works', {
  tbl = data.frame(
    foo = c('foo', 'bar', 'baz'),
    bar = c('bar', 'baz', 'foo')
  )

  query = clientQuery(tbl)
  out = dataTablesFilter(tbl, query)
  (setequal(out$DT_rows_all, 1:3))

  query = clientQuery(tbl, globalQuery('bar'))
  out = dataTablesFilter(tbl, query)
  (setequal(out$DT_rows_all, 1:2))

  query = clientQuery(tbl, globalQuery('foo'))
  query$columns[[1]] = columnQuery('ba')
  out = dataTablesFilter(tbl, query)
  (setequal(out$DT_rows_all, 3L))

  query = clientQuery(tbl)
  query$columns[[1]] = columnQuery('b')
  query$columns[[2]] = columnQuery('a')
  out = dataTablesFilter(tbl, query)
  (setequal(out$DT_rows_all, 2L))
})

assert('server-side search handler skips unsearchable columns', {
  tbl = data.frame(
    foo = c('foo', 'bar', 'baz'),
    bar = c('bar', 'baz', 'foo')
  )

  query = clientQuery(tbl, globalQuery('bar'))
  query$columns[[1]] = columnQuery(searchable = FALSE)
  out = dataTablesFilter(tbl, query)
  (setequal(out$DT_rows_all, 1L))

  query = clientQuery(tbl)
  query$columns[[1]] = columnQuery('bar', searchable = FALSE)
  out = dataTablesFilter(tbl, query)
  (setequal(out$DT_rows_all, 1:3))

  query = clientQuery(tbl)
  query$columns[[1]] = columnQuery(searchable = FALSE)
  query$columns[[2]] = columnQuery('bar')
  out = dataTablesFilter(tbl, query)
  (setequal(out$DT_rows_all, 1L))
})

assert('server-side search handler ignores NULL search', {
  tbl = data.frame(
    foo = c('foo', 'bar', 'baz'),
    bar = c('bar', 'baz', 'foo')
  )

  query = clientQuery(tbl, NULL)
  out = dataTablesFilter(tbl, query)
  (setequal(out$DT_rows_all, 1:3))
})

# Restore stringsAsFactors
options(op)
