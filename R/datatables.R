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
    data = data, isDF = isDF, table = as.character(table), options = options
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
