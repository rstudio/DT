formatColumns = function(table, columns, template, ..., appendTo = c('columnDefs', 'rowCallback'), rows = NULL) {
  if (!inherits(table, 'datatables'))
    stop("Invalid table argument; a table object created from datatable() was expected")
  if (inherits(columns, 'formula')) columns = all.vars(columns)
  x = table$x
  colnames = base::attr(x, 'colnames', exact = TRUE)
  rownames = base::attr(x, 'rownames', exact = TRUE)
  appendTo = match.arg(appendTo)
  if (appendTo == 'columnDefs') {
    x$options$columnDefs = append(
      # must append to the front so that the later formatting
      # can override the previous formatting
      x$options$columnDefs, colFormatter(
        columns, colnames, rownames, rows, template, ...
      ), after = 0L
    )
  } else {
    x$options$rowCallback = appendFormatter(
      x$options$rowCallback, columns, colnames, rownames, template, ...
    )
  }
  table$x = x
  table
}

#' Format table columns
#'
#' Format numeric columns in a table as currency (\code{formatCurrency()}) or
#' percentages (\code{formatPercentage()}), or round numbers to a specified
#' number of decimal places (\code{formatRound()}), or a specified number
#' of significant figures (\code{formatSignif()}).  The function
#' \code{formatStyle()} applies CSS styles to table cells by column.
#' @param table a table object created from \code{\link{datatable}()}
#' @param columns the indices of the columns to be formatted (can be character,
#'   numeric, logical, or a formula of the form \code{~ V1 + V2}, which is
#'   equivalent to \code{c('V1', 'V2')})
#' @param currency the currency symbol
#' @param interval put a marker after how many digits of the numbers
#' @param mark the marker after every \code{interval} decimals in the numbers
#' @param dec.mark a character to indicate the decimal point
#' @param before whether to place the currency symbol before or after the values
#' @param zero.print a string to specify how zeros should be formatted.
#'   Useful for when many zero values exist. If \code{NULL}, keeps zero as it is.
#' @param rows an integer vector (starting from 1) to specify the only rows
#'   that the style applies to.
#'   By default, it's \code{NULL}, meaning all rows should be formatted. Note,
#'   \code{formatStyle()} doesn't support this argument and you should use
#'   \code{styleRow()} instead. In addition, this only works expected in the
#'   client-side processing mode, i.e., \code{server = FALSE}.
#' @references See \url{https://rstudio.github.io/DT/functions.html} for detailed
#'   documentation and examples.
#' @note The length of arguments other than \code{table} should be 1 or the same as
#'   the length of \code{columns}.
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
#' # render vapor pressure with only two significant figures.
#' datatable(pressure) %>% formatSignif('pressure',2)
#'
#' # apply CSS styles to columns
#' datatable(iris) %>%
#'   formatStyle('Sepal.Length', fontWeight = styleInterval(5, c('bold', 'weight'))) %>%
#'   formatStyle('Sepal.Width',
#'     color = styleInterval(3.4, c('red', 'white')),
#'     backgroundColor = styleInterval(3.4, c('yellow', 'gray'))
#'   )
formatCurrency = function(
  table, columns, currency = '$', interval = 3, mark = ',', digits = 2,
  dec.mark = getOption('OutDec'), before = TRUE, zero.print = NULL, rows = NULL
) {
  currency = gsub("'", "\\\\'", currency)
  mark = gsub("'", "\\\\'", mark)
  formatColumns(table, columns, tplCurrency, currency, interval, mark, digits, dec.mark, before, zero.print, rows = rows)
}

#' @export
#' @rdname formatCurrency
#' @param prefix string to put in front of the column values
#' @param suffix string to put after the column values
formatString = function(table, columns, prefix = '', suffix = '', rows = NULL) {
  formatColumns(table, columns, tplString, prefix, suffix, rows = rows)
}

#' @export
#' @rdname formatCurrency
#' @param digits the number of decimal places to round to
formatPercentage = function(
  table, columns, digits = 0, interval = 3, mark = ',', dec.mark = getOption('OutDec'), zero.print = NULL, rows = NULL
) {
  formatColumns(table, columns, tplPercentage, digits, interval, mark, dec.mark, zero.print, rows = rows)
}

#' @export
#' @rdname formatCurrency
formatRound = function(
  table, columns, digits = 2, interval = 3, mark = ',', dec.mark = getOption('OutDec'), zero.print = NULL, rows = NULL
) {
  formatColumns(table, columns, tplRound, digits, interval, mark, dec.mark, zero.print, rows = rows)
}

#' @export
#' @rdname formatCurrency
formatSignif = function(
  table, columns, digits = 2, interval = 3, mark = ',', dec.mark = getOption('OutDec'), zero.print = NULL, rows = NULL
) {
  formatColumns(table, columns, tplSignif, digits, interval, mark, dec.mark, zero.print, rows = rows)
}

#' @export
#' @rdname formatCurrency
#' @param method the method(s) to convert a date to string in JavaScript; see
#'   \code{DT:::DateMethods} for a list of possible methods, and
#'   \url{https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date}
#'   for a full reference
#' @param params a list parameters for the specific date conversion method,
#'   e.g., for the \code{toLocaleDateString()} method, your browser may support
#'   \code{params = list('ko-KR', list(year = 'numeric', month = 'long', day =
#'   'numeric'))}
formatDate = function(table, columns, method = 'toDateString', params = NULL, rows = NULL) {
  if (!inherits(table, 'datatables'))
    stop("Invalid table argument; a table object created from datatable() was expected")
  x = table$x
  if (x$filter != 'none') {
    if (inherits(columns, 'formula')) columns = all.vars(columns)
    colnames = base::attr(x, 'colnames', exact = TRUE)
    rownames = base::attr(x, 'rownames', exact = TRUE)
    if (is.null(params)) params = list()
    cols = sprintf("%d", name2int(columns, colnames, rownames))
    x$filterDateFmt = as.list(x$filterDateFmt)
    for (col in cols) x$filterDateFmt[[col]] = list(
      method = method, params = toJSON(params)
    )
    table$x = x
  }
  # the code above is used to ensure the date(time) filter displays the same format or
  # timezone as the column value
  formatColumns(table, columns, tplDate, method, params, rows = rows)
}

#' @param valueColumns indices of the columns from which the cell values are
#'   obtained; this can be different with the \code{columns} argument, e.g. you
#'   may style one column based on the values of a different column
#' @param target the target to apply the CSS styles to (the current cell or the
#'   full row)
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
  table, columns, valueColumns = columns, target = c('cell', 'row'),
  fontWeight = NULL, color = NULL, backgroundColor = NULL, background = NULL, ...
) {
  styles = dropNULL(list(
    fontWeight = fontWeight, color = color, backgroundColor = backgroundColor,
    background = background, ...
  ))
  formatColumns(table, columns, tplStyle, valueColumns, match.arg(target), styles,
                appendTo = 'rowCallback')
}

# turn character/logical indices to numeric indices
name2int = function(name, names, rownames, noerror = FALSE) {
  if (is.numeric(name)) {
    i = if (all(name >= 0)) name else seq_along(names)[name]
    if (!rownames) i = i - 1
    return(i)
  }
  i = unname(setNames(seq_along(names), names)[name]) - 1
  if (any(is.na(i))) {
    if (!noerror) stop(
      'You specified the columns: ', paste(name, collapse = ', '), ', ',
      'but the column names of the data are ', paste(names, collapse = ', ')
    )
    i = na.omit(i)
  }
  i
}

colFormatter = function(name, names, rownames = TRUE, rows = NULL, template, ...) {
  i = name2int(name, names, rownames)
  rowJS = ''
  if (length(rows)) {
    rows = as.integer(rows)
    rowJS = sprintf(' || $.inArray(meta.row + 1, %s) < 0', toJSON(rows))
  }
  # see https://datatables.net/reference/option/columns.render
  # #837 we only want to use the formatting for the "display" purpose
  js = sprintf("function(data, type, row, meta) {
    return type !== 'display'%s ? data : %s
  }", rowJS, template(...))
  Map(function(i, js) list(targets = i, render = JS(js)), i, js, USE.NAMES = FALSE)
}

appendFormatter = function(js, name, names, rownames = TRUE, template, ...) {
  js = if (length(js) == 0) {
    c('function(row, data, displayNum, displayIndex, dataIndex) {', '}')
  } else {
    unlist(strsplit(as.character(js), '\n'))
  }
  i = name2int(name, names, rownames, noerror = TRUE)
  JS(append(
    js, after = length(js) - 1,
    template(i, ..., names, rownames)
  ))
}

tplCurrency = function(currency, interval, mark, digits, dec.mark, before, zero.print, ...) {
  sprintf(
    "DTWidget.formatCurrency(data, %s, %d, %d, %s, %s, %s, %s);",
    jsValues(currency), digits, interval, jsValues(mark), jsValues(dec.mark),
    jsValues(before), jsValuesHandleNull(zero.print)
  )
}

tplString = function(prefix, suffix, ...) {
  sprintf(
    "DTWidget.formatString(data, %s, %s);",
    jsValues(prefix), jsValues(suffix)
  )
}

tplPercentage = function(digits, interval, mark, dec.mark, zero.print, ...) {
  sprintf(
    "DTWidget.formatPercentage(data, %d, %d, %s, %s, %s);",
    digits, interval, jsValues(mark), jsValues(dec.mark), jsValuesHandleNull(zero.print)
  )
}

tplRound = function(digits, interval, mark, dec.mark, zero.print, ...) {
  sprintf(
    "DTWidget.formatRound(data, %d, %d, %s, %s, %s);",
    digits, interval, jsValues(mark), jsValues(dec.mark), jsValuesHandleNull(zero.print)
  )
}

tplSignif = function(digits, interval, mark, dec.mark, zero.print, ...) {
  sprintf(
    "DTWidget.formatSignif(data, %d, %d, %s, %s, %s);",
    digits, interval, jsValues(mark), jsValues(dec.mark), jsValuesHandleNull(zero.print)
  )
}

tplDate = function(method, params, ...) {
  params = if (length(params) > 0) paste(',', toJSON(params)) else ''
  sprintf("DTWidget.formatDate(data, %s%s);", jsValues(method), params)
}

DateMethods = c(
  'toDateString', 'toISOString', 'toLocaleDateString', 'toLocaleString',
  'toLocaleTimeString', 'toString', 'toTimeString', 'toUTCString'
)

tplStyle = function(cols, valueCols, target, styles, ...) {
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
  valueCols = name2int(valueCols, ...)
  switch(
    target,
    cell = sprintf(
      "var value=data[%s]; $(this.api().cell(row, %s).node()).css({%s});",
      valueCols, cols, css
    ),
    row = sprintf(
      "var value=data[%s]; $(row).css({%s});",
      valueCols, css
    ),
    stop('Invalid target!')
  )
}

jsValues = function(x) {
  if (inherits(x, c("POSIXt", "POSIXct", "POSIXlt"))) {
    x = format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  } else if (inherits(x, "Date")) {
    x = format(x, "%Y-%m-%d")
  }
  vapply(x, jsonlite::toJSON, character(1), auto_unbox = TRUE, USE.NAMES = FALSE)
}

jsValuesHandleNull = function(x) {
  if (is.null(x)) {
    "null"
  } else {
    jsValues(x)
  }
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
#'
#' The function \code{styleValue()} uses the column value as the CSS values.
#'
#' The function \code{styleRow()} applies the CSS values based on Row Indexes.
#' This only works expected in the client-side processing mode, i.e., \code{server = FALSE}.
#'
#' @param cuts a vector of cut points (sorted increasingly)
#' @param values a vector of CSS values
#' @export
styleInterval = function(cuts, values) {
  n = length(cuts)
  if (n != length(values) - 1)
    stop("length(cuts) must be equal to length(values) - 1")
  values = jsValues(values)
  if (n == 0) return(values)
  if (!all(cuts == sort(cuts))) stop("'cuts' must be sorted increasingly")
  js = "isNaN(parseFloat(value)) ? '' : "  # missing or non-numeric values in data
  cuts = jsValues(cuts)
  for (i in seq_len(n)) {
    js = paste0(js, sprintf('value <= %s ? %s : ', cuts[i], values[i]))
  }
  JS(paste0(js, values[n + 1]))
}

#' @param levels a character vector of data values to be mapped (one-to-one) to
#'   CSS values
#' @param default a string or \code{NULL} used as the the default CSS value
#'   for values other than levels. If \code{NULL}, the CSS value of non-matched
#'   cells will be left unchanged.
#' @export
#' @rdname styleInterval
styleEqual = function(levels, values, default = NULL) {
  n = length(levels)
  if (length(values) == 1L) values <- rep(values, n)
  if (n != length(values))
    stop("length(levels) must be equal to length(values) when `values` is not a scalar")
  if (!is.null(default) && (!is.character(default) || length(default) != 1))
    stop("default must be null or a string")
  if (n == 0) return("''")
  # because the data will be transformed by escapeData(), we need to compare
  # the "escaped value" of the "levels" instead of the raw value
  if (is.character(levels) || is.factor(levels))
    levels = htmlEscape(levels)
  levels = jsValues(levels)
  values = jsValues(values)
  js = ''
  for (i in seq_len(n)) {
    js = paste0(js, sprintf("value == %s ? %s : ", levels[i], values[i]))
  }
  # set the css to null will leave the attribute as it is. Despite it's not
  # documented explicitly but the jquery test covers this behavior
  # https://github.com/jquery/jquery/commit/2ae872c594790c4b935a1d7eabdf8b8212fd3c3f
  default = jsValuesHandleNull(default)
  JS(paste0(js, default))
}

#' @export
#' @rdname styleInterval
styleValue = function() {
  JS('value')
}

#' @param data a numeric vector whose range will be used for scaling the
#' table data from 0-100 before being represented as color bars. A vector
#' of length 2 is acceptable here for specifying a range possibly wider or
#' narrower than the range of the table data itself.
#' @param color the color of the bars
#' @param angle a number of degrees representing the direction to fill the
#' gradient relative to a horizontal line and the gradient line, going
#' counter-clockwise. For example, 90 fills right to left and -90 fills
#' left to right.
#' @export
#' @rdname styleInterval
styleColorBar = function(data, color, angle=90) {
  rg = range(data, na.rm = TRUE, finite = TRUE)
  r1 = rg[1]; r2 = rg[2]; r = r2 - r1
  JS(sprintf(
    "isNaN(parseFloat(value)) || value <= %f ? '' : 'linear-gradient(%fdeg, transparent ' + Math.max(%f - value, 0)/%f * 100 + '%%, %s ' + Math.max(%f - value, 0)/%f * 100 + '%%)'",
    r1, angle, r2, r, color, r2, r
  ))
}

#' @param rows the Row Indexes (starting from 1) that applies the CSS style. It could
#' be an integer vector or a list of integer vectors, whose length must be equal to
#' the length of \code{values}, when \code{values} is not a scalar.
#' @rdname styleInterval
#' @export
styleRow = function(rows, values, default = NULL) {
  n = length(rows)
  if (length(values) == 1L) values <- rep(values, n)
  if (n != length(values))
    stop("length(rows) must be equal to length(values) when `values` is not a scalar")
  if (!is.null(default) && (!is.character(default) || length(default) != 1))
    stop("default must be null or a string")
  if (n == 0) return("''")
  values = jsValues(values)
  js = ''
  for (i in seq_len(n)) {
    row = as.integer(rows[[i]])
    # must use dataIndex + 1 as it's suppse
    js = paste0(js, sprintf("$.inArray(dataIndex + 1, [%s]) >= 0 ? %s : ", toString(row), values[i]))
  }
  default = jsValuesHandleNull(default)
  JS(paste0(js, default))
}
