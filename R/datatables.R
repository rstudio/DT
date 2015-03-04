#' Create an HTML table widget using the DataTables library
#'
#' This function creates an HTML widget to display rectangular data (a matrix or
#' data frame) using the JavaScript library DataTables.
#' @param data a data object (either a matrix or a data frame)
#' @param options a list of initialization options (see
#'   \url{http://datatables.net/reference/option/}); the character options
#'   wrapped in \code{\link[htmlwidgets]{JS}()} will be treated as literal
#'   JavaScript code instead of normal character strings
#' @param callback a JavaScript callback function to be applied to the
#'   DataTables instance
#' @param colnames if missing, the column names of the data; otherwise it can be
#'   an unnamed character vector of names you want to show in the table header
#'   instead of the default data column names; alternatively, you can provide a
#'   \emph{named} numeric or character vector of the form \code{'newName1' = i1,
#'   'newName2' = i2} or \code{c('newName1' = 'oldName1', 'newName2' =
#'   'oldName2', ...)}, where \code{newName} is the new name you want to show in
#'   the table, and \code{i} or \code{oldName} is the index of the current
#'   column name
#' @param container a sketch of the HTML table to be filled with data cells; by
#'   default, it is generated from \code{htmltools::tags$table()} with a table
#'   header consisting of the column names of the data
#' @param server whether to use server-side processing; if \code{TRUE}, you must
#'   provide a server URL so that DataTables can send Ajax requests to retrieve
#'   data from the server
#' @param escape whether to escape HTML entities in the table: \code{TRUE} means
#'   to escape the whole table, and \code{FALSE} means not to escape it;
#'   alternatively, you can specify numeric column indices or column names to
#'   indicate which columns to escape, e.g. \code{1:5} (the first 5 columns),
#'   \code{c(1, 3, 4)}, or \code{c(-1, -3)} (all columns except the first and
#'   third), or \code{c('Species', 'Sepal.Length')}
#' @note You are recommended to escape the table content for security reasons
#'   (e.g. XSS attacks) when using this function in Shiny or any other dynamic
#'   web applications.
#' @importFrom htmltools tags
#' @export
#' @example inst/examples/datatable.R
datatable = function(
  data, options = list(), callback = 'function(table) {}',
  colnames, container, server = FALSE, escape = TRUE, extensions = NULL
) {
  isDF = is.data.frame(data)
  if (isDF) {
    data = as.data.frame(data)
    numc = unname(which(sapply(data, is.numeric)))
  } else {
    if (!is.matrix(data))
      stop("'data' must be either a matrix or a data frame")
    if (length(base::colnames(data)) != ncol(data) && missing(container))
      stop("The 'data' matrix must have column names")
    numc = if (is.numeric(data)) seq_len(ncol(data))
  }
  # TODO: how to deal with row names?
  rownames(data) = NULL

  # align numeric columns to the right
  if (length(numc))
    options = appendColumnDefs(options, list(className = 'dt-right', targets = numc - 1))

  # make sure the table is _not_ ordered by default (change the DataTables default)
  if (is.null(options[['order']])) options$order = list()

  cn = base::colnames(data)
  if (missing(colnames)) {
    colnames = cn
  } else if (!is.null(names(colnames))) {
    # e.g. colnames = c('Sepal Width' = 'Sepal.Width' or 2) => make the 2nd
    # column name 'Sepal Width'
    i = convertIdx(colnames, cn)
    cn[i] = names(colnames)
    colnames = cn
  }

  if (missing(container))
    container = tags$table(tableHeader(colnames, 'head', escape))

  # in the server mode, we should not store the full data in JSON
  if (server) {
    data = NULL; isDF = FALSE
    options$serverSide = TRUE
  }

  # rstudio/DT#13: convert date/time to character
  if (isDF) for (j in seq_len(ncol(data))) {
    if (inherits(data[, j], 'Date')) {
      data[, j] = as.character(data[, j])
    } else if (inherits(data[, j], c('POSIXlt', 'POSIXct'))) {
      data[, j] = sub('(\\d{2})(\\d{2})$', '\\1:\\2', format(
        data[, j], '%Y-%m-%dT%H:%M:%OS6%z'
      ))
    }
  }
  data = escapeData(data, escape, colnames)
  data = fixWAT(data)
  # do not use is.list() because is.list(data frame) is TRUE
  if (inherits(data, 'list')) isDF = FALSE else if (isDF) {
    # see rstudio/DT#5 (list(1, 2) => [1, 2] but we really need [[1], [2]]; this
    # is fine: list(3:4, 5:6) => [[3, 4], [5, 6]])
    data = if (nrow(data) == 1) lapply(as.list(data), list) else as.list(data)
  }
  data = unname(data)

  params = list(
    data = data, isDF = isDF, container = as.character(container), options = options,
    callback = paste(callback, collapse = '\n'), colnames = cn
  )

  htmlwidgets::createWidget(
    'datatables', params, package = 'DT', width = '100%', height = 'auto',
    dependencies = lapply(extensions, extDependency)
  )
}

# fix some WAT's in RJSONIO that I discovered in shiny:::dataTablesJSON()
fixWAT = function(data) {
  # toJSON(list(x = matrix(1:2))) => {x: [ [1], [2] ]}, however,
  # toJSON(list(x = matrix(1))) => {x: [ 1 ]} (loss of dimension, shiny#429)
  if (length(data) && all(dim(data) == 1)) return(list(list(unname(data[1, 1]))))
  # toJSON(list(x = matrix(nrow = 0, ncol = 1))) => {"x": } (shiny#299)
  if (is.matrix(data) && nrow(data) == 0) return(list())
  data
}

appendColumnDefs = function(options, def) {
  defs = options[['columnDefs']]
  if (is.null(defs)) defs = list()
  defs[[length(defs) + 1]] = def
  options$columnDefs = defs
  options
}

# convert character indices to numeric
convertIdx = function(i, names, n = length(names), invert = FALSE) {
  if (!is.character(i)) return({
    if (invert) {
      if (is.numeric(i)) -i else if (is.logical(i)) !i else {
        stop('Indices must be either character, numeric, or logical')
      }
    } else i
  })
  if (is.null(names)) stop('The data must have column names')
  o = setNames(seq_len(n), names)
  i = o[i]
  if (any(is.na(i)))
    stop("Some column names in the 'escape' argument not found in data")
  if (invert) o[-i] else i
}

#' @importFrom htmltools HTML htmlEscape
escapeData = function(data, i, colnames) {
  if (is.null(data) || prod(dim(data)) == 0 || identical(i, FALSE)) return(data)
  i = convertIdx(i, colnames, ncol(data))
  # only escape character columns (no need to escape numeric or logical columns)
  if (is.list(data)) {
    data[i] = lapply(data[i], function(x) {
      if (is.character(x) || is.factor(x)) htmlEscape(x) else x
    })
  } else if (is.matrix(data)) {
    if (is.character(data)) data[, i] = htmlEscape(data[, i])
  } else stop('Wrong data for datatable()')
  data
}

escapeColNames = function(colnames, i) {
  if (isTRUE(i)) return(colnames)  # tags$th will escape them
  i = convertIdx(i, colnames, length(colnames), invert = TRUE)
  colnames = as.list(colnames)
  colnames[i] = lapply(colnames[i], HTML)
  colnames
}

#' Generate a table header or footer from column names
#'
#' A convenience function to generate a table header or footer given the column
#' names. It is basically a wrapper of \code{htmltools::tags$th} applied to the
#' column names.
#' @param names a character vector of the column names of the table (if it is an
#'   object with column names, its column names will be used instead)
#' @param type generate a header (\samp{<thead></thead>}) or footer
#'   (\samp{<tfoot></tfoot>})
#' @param escape whether to escape the names (see \code{\link{datatable}})
#' @return A tag object generated by \code{htmltools::tags}.
#' @export
#' @examples library(DT)
#' tableHeader(iris)  # or equivalently,
#' tableHeader(colnames(iris))
#' tableHeader(iris, 'foot')  # footer
#'
#' library(htmltools)
#' tags$table(tableHeader(iris, 'head'), tableHeader(iris, 'foot'))
tableHeader = function(names, type = c('head', 'foot'), escape = TRUE) {
  names2 = colnames(names)
  if (!is.null(names2)) names = names2
  type = match.arg(type)
  f = tags[[sprintf('t%s', type)]]
  f(tags$tr(lapply(escapeColNames(names, escape), tags$th)))
}

extPath = function(...) {
  system.file('htmlwidgets', 'lib', 'datatables-extensions', ..., package = 'DT')
}

extAll = function() {
  list.dirs(extPath(), FALSE, FALSE)
}

extDependency = function(extension) {
  # correct ExtName to extName just in case
  extension = sub('^(.)', '\\L\\1', extension, perl = TRUE)
  if (!(extension %in% extAll())) stop('The extension ', extension, 'does not exist')
  js = sprintf('dataTables.%s.min.js', extension)
  css = sprintf('dataTables.%s.min.css', extension)
  htmltools::htmlDependency(
    paste('datatables', extension, sep = '-'), DataTablesVersion, extPath(extension),
    script = js, stylesheet = css
  )
}

#' Copy the Flash SWF file from the TableTools extension
#'
#' This is a convenience function to copy the SWF file since the TableTools
#' extension depends on it.
#' @param dest the destination directory
#' @param pdf \code{TRUE} if you want to save the table as PDF
#'   (\file{copy_csv_xls_pdf.swf} will be copied); \code{FALSE} otherwise (use
#'   \file{copy_csv_xls.swf})
#' @references \url{http://datatables.net/extensions/tabletools}
#' @return A character string of the path of the SWF file, which may be used as
#'   the \code{sSwfPath} option for TableTools.
#' @export
copySWF = function(dest = '.', pdf = FALSE) {
  if (!file_test('-d', dest)) stop("'dest' must be a directory")
  swf = if (pdf) 'copy_csv_xls_pdf.swf' else 'copy_csv_xls.swf'
  file.copy(extPath(swf), dest, overwrite = TRUE)
  if (sub('/$', '', dest) == 'www') dest = sub('www/?', '', dest)
  file.path(dest, swf, fsep = '/')
}
