# server-side processing for the SearchBuilder extension
# https://datatables.net/extensions/searchbuilder/

# returns NULL if the search doesn't contain any valid conditions
sbEvaluateSearch = function(search, data) {
  # https://datatables.net/reference/option/searchBuilder.preDefined
  stopifnot(search$logic %in% c('AND', 'OR'))
  Reduce(
    switch(search$logic, AND = `&`, OR = `|`),
    dropNULL(lapply(search$criteria, sbEvaluateCriteria, data))
  )
}

sbEvaluateCriteria = function(criteria, data) {
  # https://datatables.net/reference/option/searchBuilder.preDefined.criteria
  if ('logic' %in% names(criteria)) {
    # this is a sub-group
    sbEvaluateSearch(criteria, data)
  } else if (sbHasValidCondition(criteria)) {
    # this is a criteria
    cond = criteria$condition
    type = criteria$type
    x = data[[criteria$origData %||% criteria$data]]
    v = sbParseValue(sbExtractValue(criteria), type)
    sbEvaluateCondition(cond, type, x, v)
  } else {
    # skip evaluating invalid conditions
  }
}

sbHasValidCondition = function(criteria) {
  'condition' %in% names(criteria) && all(sapply(criteria, nzchar))
}

sbExtractValue = function(criteria) {
  if ('value2' %in% names(criteria)) {
    # array values are passed in a funny way to R
    c(criteria$value1, criteria$value2)
  } else {
    criteria$value
  }
}

sbParseValue = function(value, type) {
  # TODO: handle 'moment' and 'luxon' types mentioned in condition reference
  if (type %in% c('string', 'html')) {
    as.character(value)
  } else if (type %in% c('num', 'num-fmt', 'html-num', 'html-num-fmt')) {
    as.numeric(value)
  } else if (type %in% c('date')) {
    as.Date(value)
  } else {
    stop(sprintf('unsupported criteria type "%s"', type))
  }
}

sbEvaluateCondition = function(condition, type, x, value) {
  # https://datatables.net/reference/option/searchBuilder.preDefined.criteria.condition
  if (type %in% c('string', 'html')) {
    switch(
      condition,
      '!=' = x != value,
      '!null' = !is.na(x) & x != '',
      '=' = x == value,
      'contains' = grepl(value, x, fixed = TRUE),
      '!contains' = !grepl(value, x, fixed = TRUE),
      'ends' = endsWith(as.character(x), value),
      '!ends' = !endsWith(as.character(x), value),
      'null' = is.na(x) | x == '',
      'starts' = startsWith(as.character(x), value),
      '!starts' = !startsWith(as.character(x), value),
      stop(sprintf('unsupported condition "%s" for criteria type "%s"', condition, type))
    )
  } else if (type %in% c('num', 'num-fmt', 'html-num', 'html-num-fmt')) {
    switch(
      condition,
      '!=' = x != value,
      '!null' = !is.na(x),
      '<' = x < value,
      '<=' = x <= value,
      '=' = x == value,
      '>' = x > value,
      '>=' = x >= value,
      'between' = x >= value[1] & x <= value[2],
      '!between' = x < value[1] | x > value[2],
      'null' = is.na(x),
      stop(sprintf('unsupported condition "%s" for criteria type "%s"', condition, type))
    )
  } else if (type %in% c('date', 'moment', 'luxon')) {
    switch(
      condition,
      '!=' = x != value,
      '!null' = !is.na(x),
      '<' = x < value,
      '=' = x == value,
      '>' = x > value,
      'between' = x >= value[1] & x <= value[2],
      '!between' = x < value[1] | x > value[2],
      'null' = is.na(x),
      stop(sprintf('unsupported condition "%s" for criteria type "%s"', condition, type))
    )
  } else {
    stop(sprintf('unsupported criteria type "%s"', type))
  }
}
