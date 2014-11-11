#' Create an HTML table widget using the DataTables library
#'
#' This function creates an HTML widget to display rectangular data (a matrix or
#' data frame) using the JavaScript library DataTables.
#' @param data a data object (either a matrix or a data frame)
#' @param id the id for the table
#' @param options a list of initialization options (see
#'   \url{http://datatables.net/reference/option/}); the character options
#'   wrapped in \code{I()} will be treated as literal JavaScript code instead of
#'   normal character strings
#' @importFrom htmltools tags
#' @export
#' @example inst/examples/datatable.R
datatable = function(data, id = NULL, options = list(order = list())) {
  isDF = is.data.frame(data)
  if (isDF) data = as.data.frame(data) else {
    if (!is.matrix(data))
      stop("'data' must be either a matrix or a data frame")
    if (length(colnames(data)) != ncol(data))
      stop("The 'data' matrix must have column names")
  }
  # TODO: how to deal with row names?
  rownames(data) = NULL

  table = tags$table(id = id, tags$thead(tags$tr(lapply(colnames(data), tags$th))))

  data = fix_WAT(data)
  # do not use is.list() because is.list(data frame) is TRUE
  if (inherits(data, 'list')) isDF = FALSE else {
    data = if (isDF) unname(as.list(data)) else unname(data)
  }
  params = list(
    data = data, isDF = isDF, table = as.character(table),
    evals = options_evals(options), options = options_sanitize(options)
  )

  htmlwidgets::createWidget('datatables', params, package = 'DT')
}

# fix some WAT's in RJSONIO that I discovered in shiny:::dataTablesJSON()
fix_WAT = function(data) {
  # toJSON(list(x = matrix(1:2))) => {x: [ [1], [2] ]}, however,
  # toJSON(list(x = matrix(1))) => {x: [ 1 ]} (loss of dimension, shiny#429)
  if (length(data) && all(dim(data) == 1)) return(list(list(unname(data[1, 1]))))
  # toJSON(list(x = matrix(nrow = 0, ncol = 1))) => {"x": } (shiny#299)
  if (is.matrix(data) && nrow(data) == 0) return(list())
  data
}

# JSON elements that are character with the class AsIs will be evaluated
options_sanitize = function(options) {
  if (!is.list(options) || length(options) == 0) return(options)
  lapply(options, function(x) {
    if (is.list(x)) return(options_sanitize(x))
    if (is.character(x) && inherits(x, 'AsIs')) x = paste(x, collapse = '\n')
    x
  })
}

should_eval = function(options) {
  if (is.list(options)) {
    if ((n <- length(options)) == 0) return(FALSE)
    # use numeric indices as names (remember JS indexes from 0, hence -1 here)
    if (is.null(nms <- names(options))) nms = names(options) = seq_len(n) - 1L
    if (length(nms) != n || any(nms == ''))
      stop("'options' must be a fully named list, or have no names (NULL)")
    lapply(options, should_eval)
  } else {
    is.character(options) && inherits(options, 'AsIs')
  }
}

# Does this: list(foo = list(1, list(bar = I('function(){}')), 2)) => foo.2.bar
# Later on the JS side, we will split foo.2.bar to ['foo', '2', 'bar'] and
# evaluate the JSON object member. Note '2' (character) should have been 2
# (integer) but it does not seem to matter in JS: x[2] is the same as x['2']
# when all child members of x are unnamed, and ('2' in x) will be true even if x
# is an array without names. This is a little hackish.
options_evals = function(options) {
  evals = names(which(unlist(should_eval(options))))
  I(evals)  # need I() to prevent RJSONIO::toJSON() from converting it to scalar
}
