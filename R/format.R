formatColumns = function(table, columns, template, ...) {
  if (inherits(columns, 'formula')) columns = all.vars(columns)
  attr = table$x
  attr$options$rowCallback = appendFormatter(
    attr$options$rowCallback, columns, attr$colnames, attr$rownames, template, ...
  )
  table$x = attr
  table
}

#' Format table columns
#'
#' Format numeric columns in a table as currency or percentages, or round
#' numbers to a specified number of decimal places.
#' @param table a table object created from \code{\link{datatable}()}
#' @param columns the indices of the columns to be formatted (can be character,
#'   numeric, or logical)
#' @param currency the currency symbol
#' @param interval put a marker after how many digits of the numbers
#' @param mark the marker after every \code{interval} decimals in the numbers
#' @param method the method(s) to convert a date to string in JavaScript; see
#'   \code{DT:::DateMethods} for a list of possible methods,
#'   \url{http://mzl.la/1xGe99W} for a full reference, and
#'   \url{http://rstudio.github.io/DT/functions.html} for examples
#' @export
#' @examples library(DT)
#' m = cbind(matrix(rnorm(120, 1e5, 1e6), 40), runif(40), rnorm(40, 100))
#' colnames(m) = head(LETTERS, ncol(m))
#' m
#'
#' # format the columns A and C as currency, and D as percentages
#' datatable(m) %>% formatCurrency(c('A', 'C')) %>% formatPercentage('D', 2)
#'
#' # the first two columns are Euro currency, and round column E to 3 decimal places
#' datatable(m) %>% formatCurrency(1:2, '\U20AC') %>% formatRound('E', 3)
formatCurrency = function(table, columns, currency = '$', interval = 3, mark = ',') {
  formatColumns(table, columns, tplCurrency, currency, interval, mark)
}

#' @export
#' @rdname formatCurrency
#' @param digits the number of decimal places to round to
formatPercentage = function(table, columns, digits = 0) {
  formatColumns(table, columns, tplPercentage, digits)
}

#' @export
#' @rdname formatCurrency
formatRound = function(table, columns, digits = 2) {
  formatColumns(table, columns, tplRound, digits)
}

#' @export
#' @rdname formatCurrency
formatDate = function(table, columns, method = 'toDateString') {
  formatColumns(table, columns, tplDate, method)
}

# turn character/logical indices to numeric indices
name2int = function(name, names) {
  if (is.numeric(name)) {
    return(if (all(name > 0)) name else seq_along(names)[name])
  }
  names = setNames(seq_along(names), names)
  unname(names[name])
}

appendFormatter = function(js, name, names, rownames = TRUE, template, ...) {
  js = if (length(js) == 0) c('function(row, data) {', '}') else {
    unlist(strsplit(as.character(js), '\n'))
  }
  i = name2int(name, names)
  if (is.character(name) || (is.numeric(name) && !rownames)) i = i - 1
  if (any(is.na(i))) stop(
    'You specified the columns: ', paste(name, collapse = ', '), ', ',
    'but the column names of the data are ', paste(names, collapse = ', ')
  )
  JS(append(
    js, after = 1,
    template(i, ...)
  ))
}

tplCurrency = function(cols, currency, interval, mark) {
  sprintf(
    "$('td:eq(%d)', row).html('%s' + data[%d].toString().replace(/\\B(?=(\\d{%d})+(?!\\d))/g, '%s'));",
    cols, currency, cols, interval, mark
  )
}

tplPercentage = function(cols, digits) {
  sprintf(
    "var d = parseFloat(data[%d]); $('td:eq(%d)', row).html(isNaN(d) ? '' : (d * 100).toFixed(%d) + '%%');",
    cols, cols, digits
  )
}

tplRound = function(cols, digits) {
  sprintf(
    "var d = parseFloat(data[%d]); $('td:eq(%d)', row).html(isNaN(d) ? '' : d.toFixed(%d));",
    cols, cols, digits
  )
}

tplDate = function(cols, method) {
  sprintf(
    "var d = new Date(data[%d]); $('td:eq(%d)', row).html(d['%s']());",
    cols, cols, method
  )
}

DateMethods = c(
  'toDateString', 'toISOString', 'toLocaleDateString', 'toLocaleString',
  'toLocaleTimeString', 'toString', 'toTimeString', 'toUTCString'
)
