library(testit)

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
