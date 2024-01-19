library(testit)

assert('SearchBuilder condition evaluation works', {
  (sbEvaluateCondition('>', 'num', 1:2, 1) == c(FALSE, TRUE))
  (sbEvaluateCondition('between', 'num', 7, c(2, 4)) == FALSE)
  (sbEvaluateCondition('starts', 'string', 'foo', 'f') == TRUE)
  (sbEvaluateCondition('starts', 'string', factor('foo'), 'f') == TRUE)
})

assert('SearchBuilder logic evaluation works', {
  res = sbEvaluateSearch(
    list(
      logic = 'AND',
      criteria = list(
        list(condition = '<=', data = 'a', value = '4', type = 'num'),
        list(condition = '>=', data = 'a', value = '2', type = 'num')
      )
    ),
    data.frame(a = 1:9)
  )  
  (setequal(which(res), 2:4))
  res = sbEvaluateSearch(
    list(
      logic = 'OR',
      criteria = list(
        list(condition = '>', data = 'a', value = '4', type = 'num'),
        list(condition = '<', data = 'a', value = '2', type = 'num')
      )
    ),
    data.frame(a = 1:9)
  )  
  (setequal(which(res), c(1, 5:9)))
})

assert('SearchBuilder complex queries work', {
  res = sbEvaluateSearch(
    list(
      logic = 'OR',
      criteria = list(
        list(condition = '=', data = 'a', value = '7', type = 'num'),
        list(
          logic = 'AND',
          criteria = list(
            list(condition = '<=', data = 'a', value = '4', type = 'num'),
            list(condition = '>=', data = 'a', value = '2', type = 'num')
          )
        )
      )
    ),
    data.frame(a = 1:9)
  )  
  (setequal(which(res), c(2:4, 7)))
})
