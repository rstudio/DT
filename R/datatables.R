#' Create an HTML table widget using the DataTables library
#'
#' This function creates an HTML widget to display rectangular data (a matrix or
#' data frame) using the JavaScript library DataTables.
#' @param data a data object (either a matrix or a data frame)
#' @param id the id for the table
#' @param options a list of initialization options (see
#'   \url{http://datatables.net/reference/option/}); the character options
#'   wrapped in \code{\link[htmlwidgets]{JS}()} will be treated as literal
#'   JavaScript code instead of normal character strings
#' @param callback a JavaScript callback function to be applied to the
#'   DataTables instance
#' @param container a sketch of the HTML table to be filled with data cells; by
#'   default, it is generated from \code{htmltools::tags$table()} with a table
#'   header consisting of the column names of the data
#' @importFrom htmltools tags
#' @export
#' @example inst/examples/datatable.R
datatable = function(
  data, id = NULL, options = list(order = list()), callback = 'function(table) {}',
  container
) {
  isDF = is.data.frame(data)
  if (isDF) {
    data = as.data.frame(data)
    numc = unname(which(sapply(data, is.numeric)))
  } else {
    if (!is.matrix(data))
      stop("'data' must be either a matrix or a data frame")
    if (length(colnames(data)) != ncol(data))
      stop("The 'data' matrix must have column names")
    numc = if (is.numeric(data)) seq_len(ncol(data))
  }
  # TODO: how to deal with row names?
  rownames(data) = NULL

  # align numeric columns to the right
  if (length(numc))
    options = append_columnDefs(options, list(className = 'dt-right', targets = numc - 1))

  # make sure the table is _not_ ordered by default (change the DataTables defalt)
  if (is.null(options[['order']])) options$order = list()

  colnames = colnames(data)
  if (missing(container))
    container = tags$table(id = id, tags$thead(tags$tr(lapply(colnames, tags$th))))

  data = fix_WAT(data)
  # do not use is.list() because is.list(data frame) is TRUE
  if (inherits(data, 'list')) isDF = FALSE else {
    data = if (isDF) unname(as.list(data)) else unname(data)
  }
  params = list(
    data = data, isDF = isDF, container = as.character(container), options = options,
    callback = paste(callback, collapse = '\n'), colnames = colnames
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

append_columnDefs = function(options, def) {
  defs = options[['columnDefs']]
  if (is.null(defs)) defs = list()
  defs[[length(defs) + 1]] = def
  options$columnDefs = defs
  options
}
