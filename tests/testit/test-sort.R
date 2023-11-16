library(testit)

# Factors and strings are searched differently.
# Older versions of R don't have this set.
op = options(stringsAsFactors = FALSE)

# Helpers to create client queries that run without errors
clientQuery = function(data, columns = lapply(names(data), columnQuery)) {
  columns = rep_len(columns, ncol(data))
  names(columns) = seq_len(ncol(data)) - 1
  list(
    draw = '0',
    start = '0',
    length = '10',
    escape = 'true',
    search = list(
      value = '',
      regex = 'false',
      caseInsensitive = 'true',
      smart = 'true'
    ),
    columns = columns,
    order = list()
  )
}

columnQuery = function(name = '', orderable = TRUE) {
  list(
    name = name,
    orderable = tolower(orderable),
    searchable = 'true',
    search = list(value = '', regex = 'false')
  )
}

orderQuery = function(column, dir = 'asc') {
  list(
    column = column,
    dir = dir
  )
}

assert('server-side sort handler works', {
  tbl = data.frame(
    foo = c('foo', 'bar', 'baz'),
    bar = c('bar', 'baz', 'foo')
  )

  query = clientQuery(tbl)
  out = dataTablesFilter(tbl, query)
  (out$data %==% unname(tbl))

  query = clientQuery(tbl)
  query$order[[1]] = orderQuery('0', 'asc')
  tbl_sort = tbl[order(tbl$foo), ]
  out = dataTablesFilter(tbl, query)
  (out$data %==% unname(tbl_sort))

  query = clientQuery(tbl)
  query$order[[1]] = orderQuery('1', 'desc')
  tbl_sort = tbl[order(tbl$bar, decreasing = TRUE), ]
  out = dataTablesFilter(tbl, query)
  (out$data %==% unname(tbl_sort))
})

assert('server-side sort handler works with re-ordered columns', {
  tbl = data.frame(
    foo = c('foo', 'bar', 'baz'),
    bar = c('bar', 'baz', 'foo')
  )

  query = clientQuery(tbl, lapply(c("bar", "foo"), columnQuery))
  # order is indexed against re-ordered columns
  query$order[[1]] = orderQuery('0', 'asc')
  tbl_sort = tbl[order(tbl$bar), ]
  out = dataTablesFilter(tbl, query)
  (out$data %==% unname(tbl_sort))
})

options(op)
