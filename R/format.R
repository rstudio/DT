formatColumns = function(table, columns, template, ...) {
  if (inherits(columns, 'formula')) columns = all.vars(columns)
  x = table$x
  colnames = base::attr(x, 'colnames', exact = TRUE)
  rownames = base::attr(x, 'rownames', exact = TRUE)
  x$options$rowCallback = appendFormatter(
    x$options$rowCallback, columns, colnames, rownames, template, ...
  )
  table$x = x
  table
}

#' Format table columns
#'
#' Format numeric columns in a table as currency (\code{formatCurrency()}) or
#' percentages (\code{formatPercentage()}), or round numbers to a specified
#' number of decimal places (\code{formatRound()}). The function
#' \code{formatStyle()} applies CSS styles to table cells by column.
#' @param table a table object created from \code{\link{datatable}()}
#' @param columns the indices of the columns to be formatted (can be character,
#'   numeric, logical, or a formula of the form \code{~ V1 + V2}, which is
#'   equivalent to \code{c('V1', 'V2')})
#' @param currency the currency symbol
#' @param interval put a marker after how many digits of the numbers
#' @param mark the marker after every \code{interval} decimals in the numbers
#' @param method the method(s) to convert a date to string in JavaScript; see
#'   \code{DT:::DateMethods} for a list of possible methods, and
#'   \url{http://mzl.la/1xGe99W} for a full reference
#' @references See \url{http://rstudio.github.io/DT/functions.html} for detailed
#'   documentation and examples.
#' @export
#' @examples # !formatR
#' library(DT)
#' m = cbind(matrix(rnorm(120, 1e5, 1e6), 40), runif(40), rnorm(40, 100))
#' colnames(m) = head(LETTERS, ncol(m))
#' m
#'
#' # format the columns A and C as currency, and D as percentages
#' datatable(m) %>% formatCurrency(c('A', 'C')) %>% formatPercentage('D', 2)
#'
#' # the first two columns are Euro currency, and round column E to 3 decimal places
#' datatable(m) %>% formatCurrency(1:2, '\U20AC') %>% formatRound('E', 3)
#'
#' # apply CSS styles to columns
#' datatable(iris) %>%
#'   formatStyle('Sepal.Length', fontWeight = styleInterval(5, c('bold', 'weight'))) %>%
#'   formatStyle('Sepal.Width',
#'     color = styleInterval(3.4, c('red', 'white')),
#'     backgroundColor = styleInterval(3.4, c('yellow', 'gray'))
#'   )
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

#' @param fontWeight the font weight, e.g. \code{'bold'} and \code{'normal'}
#' @param color the font color, e.g. \code{'red'} and \code{'#ee00aa'}
#' @param backgroundColor the background color of table cells
#' @param background the background of table cells
#' @param ... other CSS properties, e.g. \code{'border'}, \code{'font-size'},
#'   \code{'text-align'}, and so on; if you want to condition CSS styles on the
#'   cell values, you may use the helper functions such as
#'   \code{\link{styleInterval}()}; note the actual CSS property names are
#'   dash-separated, but you can use camelCase names in this function (otherwise
#'   you will have to use backticks to quote the names, e.g. \code{`font-size` =
#'   '12px'}), and this function will automatically convert camelCase names to
#'   dash-separated names (e.g. \code{'fontWeight'} will be converted to
#'   \code{'font-weight'} internally)
#' @export
#' @rdname formatCurrency
formatStyle = function(
  table, columns, fontWeight = NULL, color = NULL, backgroundColor = NULL,
  background = NULL, ...
) {
  styles = dropNULL(list(
    fontWeight = fontWeight, color = color, backgroundColor = backgroundColor,
    background = background, ...
  ))
  formatColumns(table, columns, tplStyle, styles)
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
    "var d = parseFloat(data[%d]); $(this.api().cell(row, %d).node()).html(isNaN(d) ? '' : '%s' + d.toString().replace(/\\B(?=(\\d{%d})+(?!\\d))/g, '%s'));",
    cols, cols, currency, interval, mark
  )
}

tplPercentage = function(cols, digits) {
  sprintf(
    "var d = parseFloat(data[%d]); $(this.api().cell(row, %s).node()).html(isNaN(d) ? '' : (d * 100).toFixed(%d) + '%%');",
    cols, cols, digits
  )
}

tplRound = function(cols, digits) {
  sprintf(
    "var d = parseFloat(data[%d]); $(this.api().cell(row, %s).node()).html(isNaN(d) ? '' : d.toFixed(%d));",
    cols, cols, digits
  )
}

tplDate = function(cols, method) {
  sprintf(
    "var d = new Date(data[%d]); $(this.api().cell(row, %s).node()).html(d['%s']());",
    cols, cols, method
  )
}

DateMethods = c(
  'toDateString', 'toISOString', 'toLocaleDateString', 'toLocaleString',
  'toLocaleTimeString', 'toString', 'toTimeString', 'toUTCString'
)

tplStyle = function(cols, styles) {
  if (length(styles) == 0) return()
  if (!is.list(styles)) stop("'styles' must be a list")
  JSclass = class(JS(''))
  # if a style is specified in JS(), do not single quote it (it is a JS expression)
  styles = vapply(styles, function(s) {
    isJS = inherits(s, JSclass)
    s = paste(s, collapse = '\n')
    if (isJS) s else sprintf("'%s'", s)
  }, FUN.VALUE = character(1))
  css = paste(sprintf("'%s':%s", upperToDash(names(styles)), styles), collapse = ',')
  sprintf(
    "var value=data[%s]; $(this.api().cell(row, %s).node()).css({%s});",
    cols, cols, css
  )
}

#' Conditional CSS styles
#'
#' A few helper functions for the \code{\link{formatStyle}()} function to
#' calculate CSS styles for table cells based on the cell values. Under the
#' hood, they just generate JavaScript and CSS code from the values specified in
#' R.
#'
#' The function \code{styleInterval()} maps intervals to CSS values. Its
#' argument \code{values} must be of length \code{n + 1} where \code{n =
#' length(cuts)}. The right-closed interval \samp{(cuts[i - 1], cuts[i]]} is
#' mapped to \samp{values[i]} for \samp{i = 2, 3, ..., n}; \samp{values[1]} is
#' for the interval \samp{(-Inf, cuts[1]]}, and \samp{values[n + 1]} is for
#' \samp{(cuts[n], +Inf)}. You can think of the order of \code{cuts} and
#' \code{values} using this diagram: \samp{-Inf -> values[1] -> cuts[1] ->
#' values[2] -> cuts[2] -> ... -> values[n] -> cuts[n] -> values[n + 1] ->
#' +Inf}.
#'
#' The function \code{styleEqual()} maps data values to CSS values in the
#' one-to-one manner, i.e. \code{values[i]} is used when the table cell value is
#' \code{levels[i]}.
#'
#' The function \code{styleColorBar()} can be used to draw background color bars
#' behind table cells in a column, and the width of bars is proportional to the
#' column values.
#' @param cuts a vector of cut points (sorted increasingly)
#' @param values a vector of CSS values
#' @export
styleInterval = function(cuts, values) {
  n = length(cuts)
  if (n != length(values) - 1)
    stop("length(cuts) must be equal to length(values) - 1")
  values = sprintf("'%s'", values)
  if (n == 0) return(values)
  if (!all(cuts == sort(cuts))) stop("'cuts' must be sorted increasingly")
  js = ''
  for (i in seq_len(n)) {
    js = paste0(js, sprintf('value <= %s ? %s : ', cuts[i], values[i]))
  }
  JS(paste0(js, values[n + 1]))
}

#' @param levels a character vector of data values to be mapped (one-to-one) to
#'   CSS values
#' @export
#' @rdname styleInterval
styleEqual = function(levels, values) {
  n = length(levels)
  if (n != length(values))
    stop("length(levels) must be equal to length(values)")
  if (n == 0) return("''")
  js = ''
  for (i in seq_len(n)) {
    js = paste0(js, sprintf("value == '%s' ? '%s' : ", levels[i], values[i]))
  }
  JS(paste0(js, "''"))
}

#' @param data the numeric vector to be represented as color bars (in fact, only
#'   its range, i.e. min and max, is needed here)
#' @param color the color of the bars
#' @export
#' @rdname styleInterval
styleColorBar = function(data, color) {
  rg = range(data, na.rm = TRUE, finite = TRUE)
  r1 = rg[1]; r2 = rg[2]; r = r2 - r1
  JS(sprintf(
    "isNaN(parseFloat(value)) || value == %s ? '' : 'linear-gradient(90deg, transparent ' + (%s - value)/%s * 100 + '%%, %s ' + (%s - value)/%s * 100 + '%%)'",
    r1, r2, r, color, r2, r
  ))
}
