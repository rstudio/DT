format_columns = function(table, columns, template, ...) {
  attr = table$x
  attr$options$rowCallback = append_formatter(
    attr$options$rowCallback, columns, attr$colnames, template, ...
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
#' @export
#' @examples library(DT)
#' m = cbind(matrix(rnorm(120, 1e5, 1e6), 40), runif(40), rnorm(40, 100))
#' colnames(m) = head(LETTERS, ncol(m))
#' m
#'
#' # format the columns A and C as currency, and D as percentages
#' datatable(m) %>% format_currency(c('A', 'C')) %>% format_percentage('D', 2)
#'
#' # the first two columns are Euro currency, and round column E to 3 decimal places
#' datatable(m) %>% format_currency(1:2, '\U20AC') %>% format_round('E', 3)
format_currency = function(table, columns, currency = '$', interval = 3, mark = ',') {
  format_columns(table, columns, tpl_currency, currency, interval, mark)
}

#' @export
#' @rdname format_currency
#' @param digits the number of decimal places to round to
format_percentage = function(table, columns, digits = 0) {
  format_columns(table, columns, tpl_percentage, digits)
}

#' @export
#' @rdname format_currency
format_round = function(table, columns, digits = 2) {
  format_columns(table, columns, tpl_round, digits)
}

# turn character/logical indices to numeric indices
name2int = function(name, names) {
  if (is.numeric(name)) {
    return(if (all(name > 0)) name else seq_along(names)[name])
  }
  names = setNames(seq_along(names), names)
  unname(names[name])
}

append_formatter = function(js, name, names, template, ...) {
  js = if (length(js) == 0) c('function(row, data) {', '}') else {
    unlist(strsplit(as.character(js), '\n'))
  }
  i = name2int(name, names) - 1
  JS(append(
    js, after = 1,
    template(i, ...)
  ))
}

tpl_currency = function(cols, currency, interval, mark) {
  sprintf(
    "$('td:eq(%d)', row).html('%s' + data[%d].toString().replace(/\\B(?=(\\d{%d})+(?!\\d))/g, '%s'));",
    cols, currency, cols, interval, mark
  )
}

tpl_percentage = function(cols, digits) {
  sprintf(
    "$('td:eq(%d)', row).html((data[%d] * 100).toFixed(%d) + '%%');",
    cols, cols, digits
  )
}

tpl_round = function(cols, digits) {
  sprintf(
    "$('td:eq(%d)', row).html((data[%d] + 0).toFixed(%d));",
    cols, cols, digits
  )
}
