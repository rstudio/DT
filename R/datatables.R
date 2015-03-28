#' Create an HTML table widget using the DataTables library
#'
#' This function creates an HTML widget to display rectangular data (a matrix or
#' data frame) using the JavaScript library DataTables.
#' @param data a data object (either a matrix or a data frame)
#' @param options a list of initialization options (see
#'   \url{http://datatables.net/reference/option/}); the character options
#'   wrapped in \code{\link[htmlwidgets]{JS}()} will be treated as literal
#'   JavaScript code instead of normal character strings
#' @param class the CSS class(es) of the table; see
#'   \url{http://datatables.net/manual/styling/classes}
#' @param callback the body of a JavaScript callback function with the argument
#'   \code{table} to be applied to the DataTables instance (i.e. \code{table})
#' @param rownames \code{TRUE} (show row names) or \code{FALSE} (hide row names)
#'   or a character vector of row names; by default, the row names are displayed
#'   in the first column of the table if exist (not \code{NULL})
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
#' @param caption the table caption; a character vector or a tag object
#'   generated from \code{htmltools::tags$caption()}
#' @param filter whether/where to use column filters; \code{none}: no filters;
#'   \code{bottom/top}: put column filters at the bottom/top of the table; range
#'   sliders are used to filter numeric/date/time columns, select lists are used
#'   for factor columns, and text input boxes are used for character columns
#' @param server whether to use server-side processing; if \code{TRUE}, you must
#'   provide a server URL so that DataTables can send Ajax requests to retrieve
#'   data from the server
#' @param escape whether to escape HTML entities in the table: \code{TRUE} means
#'   to escape the whole table, and \code{FALSE} means not to escape it;
#'   alternatively, you can specify numeric column indices or column names to
#'   indicate which columns to escape, e.g. \code{1:5} (the first 5 columns),
#'   \code{c(1, 3, 4)}, or \code{c(-1, -3)} (all columns except the first and
#'   third), or \code{c('Species', 'Sepal.Length')}
#' @param extensions a character vector of the names of the DataTables
#'   extensions (\url{http://datatables.net/extensions/index}), or a named list
#'   of initialization options for the extensions (the names of the list are the
#'   names of extensions)
#' @note You are recommended to escape the table content for security reasons
#'   (e.g. XSS attacks) when using this function in Shiny or any other dynamic
#'   web applications.
#' @references See \url{http://rstudio.github.io/DT} for the full documentation.
#' @importFrom htmltools tags htmlDependency
#' @export
#' @example inst/examples/datatable.R
datatable = function(
  data, options = list(), class = 'display', callback = JS('return table;'),
  rownames, colnames, container, caption = NULL, filter = c('none', 'bottom', 'top'),
  server = FALSE, escape = TRUE, extensions = list()
) {

  # yes, we all hate it
  oop = options(stringsAsFactors = FALSE); on.exit(options(oop), add = TRUE)

  # deal with row names: rownames = TRUE or missing, use rownames(data)
  rn = if (missing(rownames) || isTRUE(rownames)) base::rownames(data) else {
    if (is.character(rownames)) rownames  # use custom row names
  }

  if (is.data.frame(data)) {
    data = as.data.frame(data)
    numc = unname(which(sapply(data, is.numeric)))
  } else {
    if (!is.matrix(data))
      stop("'data' must be either a matrix or a data frame")
    numc = if (is.numeric(data)) seq_len(ncol(data))
    data = as.data.frame(data)
  }
  if (length(rn)) {
    data = cbind(' ' = rn, data)
    numc = numc + 1  # move indices of numeric columns to the right by 1
    options = appendColumnDefs(options, list(orderable = FALSE, targets = 0))
  }

  # align numeric columns to the right
  if (length(numc))
    options = appendColumnDefs(options, list(className = 'dt-right', targets = numc - 1))

  # make sure the table is _not_ ordered by default (change the DataTables default)
  if (is.null(options[['order']])) options$order = list()
  # I do not see the point of "autoWidth: true" as the default in DataTables
  if (is.null(options[['autoWidth']])) options$autoWidth = FALSE

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
  # when rownames = TRUE, user may have only provided colnames for original
  # data, and we need to add a name for the first column, i.e. row names
  if (ncol(data) - length(colnames) == 1) colnames = c(' ', colnames)

  filter = match.arg(filter)
  if (missing(container)) {
    fRow = filterRow(data, length(rn) > 0, colnames, filter)
    container = tags$table(
      tableHead(colnames, 'head', escape, if (filter == 'top') fRow),
      if (filter == 'bottom') fRow,
      class = class
    )
    # use the first row in the header as the sorting cells when I put the
    # filters in the second row
    if (filter == 'top') options$orderCellsTop = TRUE
  }

  # in the server mode, we should not store the full data in JSON
  if (server) {
    data = NULL
    options$serverSide = TRUE
    if (is.null(options$processing)) options$processing = TRUE
    # if you generated the Ajax URL from dataTableAjax(), I'll configure type:
    # 'POST' and a few other options automatically
    if ('shiny' %in% loadedNamespaces() &&
        length(grep('^session/[a-z0-9]+/dataobj/', options$ajax$url))) {
      if (is.null(options$ajax$type)) options$ajax$type = 'POST'
      if (is.null(options$ajax$data)) options$ajax$data = JS(
        'function(d) {',
        sprintf(
          'd.search.caseInsensitive = %s;',
          tolower(!isFALSE(options$search$caseInsensitive))
        ),
        sprintf('d.escape = %s;', escapeToConfig(escape, colnames)),
        '}'
      )
    }
  }

  if (is.list(extensions)) {
    extOptions = extensions
    extensions = names(extensions)
  } else if (is.character(extensions)) {
    extOptions = setNames(vector('list', length(extensions)), extensions)
  } else stop("'extensions' must be either a character vector or a named list")

  # automatically configure options and callback for extensions
  if ('Responsive' %in% extensions) options$responsive = TRUE
  # these extensions need to be initialized via new $.fn.dataTable...
  extOptions = extOptions[intersect(extensions, extNew)]

  # rstudio/DT#13: convert date/time to character
  if (length(data)) for (j in seq_len(ncol(data))) {
    if (inherits(data[, j], 'Date')) {
      data[, j] = as.character(data[, j])
    } else if (inherits(data[, j], c('POSIXlt', 'POSIXct'))) {
      data[, j] = sub('(\\d{2})(\\d{2})$', '\\1:\\2', format(
        data[, j], '%Y-%m-%dT%H:%M:%OS6Z', tz = 'UTC'
      ))
    }
  }
  data = escapeData(data, escape, colnames)
  data = fixWAT(data)
  data = unname(data)

  # generate <caption></caption>
  if (is.character(caption)) caption = tags$caption(caption)
  caption = as.character(caption)

  if (!identical(class(callback), class(JS(''))))
    stop("The 'callback' argument only accept a value returned from JS()")
  params = list(
    data = data, container = as.character(container), options = options,
    callback = JS('function(table) {', callback, '}'),
    colnames = cn, rownames = length(rn) > 0, caption = caption, filter = filter
  )
  if (length(params$caption) == 0) params$caption = NULL
  if (length(extensions)) params$extensions = as.list(extensions)
  if (length(extOptions)) params$extOptions = extOptions

  deps = lapply(extensions, extDependency)
  if (filter != 'none') deps = c(deps, filterDependencies())
  htmlwidgets::createWidget(
    'datatables', params, package = 'DT', width = '100%', height = 'auto',
    dependencies = deps
  )
}

# fix some WAT's in RJSONIO that I discovered in shiny:::dataTablesJSON()
fixWAT = function(data) {
  # toJSON(list(x = matrix(1:2))) => {x: [ [1], [2] ]}, however,
  # toJSON(list(x = matrix(1))) => {x: [ 1 ]} (loss of dimension, shiny#429)
  if (length(data) && all(dim(data) == 1)) return(list(list(unname(data[1, 1]))))
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
  data[i] = lapply(data[i], function(x) {
    if (is.character(x) || is.factor(x)) htmlEscape(x) else x
  })
  data
}

escapeColNames = function(colnames, i) {
  if (isTRUE(i)) return(colnames)  # tags$th will escape them
  i = convertIdx(i, colnames, length(colnames), invert = TRUE)
  colnames = as.list(colnames)
  colnames[i] = lapply(colnames[i], HTML)
  colnames
}

escapeToConfig = function(escape, colnames) {
  if (isTRUE(escape)) return('true')
  if (isFALSE(escape)) return('false')
  if (!is.numeric(escape)) escape = convertIdx(escape, colnames)
  if (is.logical(escape)) escape = which(escape)
  sprintf('"%s"', paste(escape, collapse = ','))
}

#' Generate a table header or footer from column names
#'
#' Convenience functions to generate a table header (\samp{<thead></thead>}) or
#' footer (\samp{<tfoot></tfoot>}) given the column names. They are basically
#' wrappers of \code{htmltools::tags$th} applied to the column names.
#' @param names a character vector of the column names of the table (if it is an
#'   object with column names, its column names will be used instead)
#' @param escape whether to escape the names (see \code{\link{datatable}})
#' @return A tag object generated by \code{htmltools::tags}.
#' @export
#' @examples library(DT)
#' tableHeader(iris)  # or equivalently,
#' tableHeader(colnames(iris))
#' tableFooter(iris)  # footer
#'
#' library(htmltools)
#' tags$table(tableHeader(iris), tableFooter(iris))
tableHeader = function(names, escape = TRUE) {
  tableHead(names, 'head', escape)
}
#' @rdname tableHeader
#' @export
tableFooter = function(names, escape = TRUE) {
  tableHead(names, 'foot', escape)
}

tableHead = function(names, type = c('head', 'foot'), escape = TRUE, ...) {
  names2 = colnames(names)
  if (!is.null(names2)) names = names2
  type = match.arg(type)
  f = tags[[sprintf('t%s', type)]]
  f(tags$tr(lapply(escapeColNames(names, escape), tags$th)), ...)
}

#' @importFrom htmltools tagList
filterRow = function(data, rownames = TRUE, colnames, filter = 'none') {
  if (filter == 'none') return()
  tds = list()
  for (j in seq_len(ncol(data))) {
    if (j == 1 && rownames) {
      tds[[j]] = tags$td('')  # no filter for row names (may change in future)
      next
    }
    t = NULL
    d = data[, j]
    x = if (is.numeric(d) || is.Date(d)) {
      t = if (is.numeric(d)) 'number' else 'time'
      if (t == 'time') {
        # JavaScript does have the Date type like R (YYYY-mm-dd without time)
        if (inherits(d, 'Date')) {
          d = as.POSIXct(d); t = 'date'
        }
        d = as.numeric(d) * 1000  # use milliseconds for JavaScript
      }
      tags$div(
        style = 'display: none; position: absolute; width: 200px;',
        tags$div(
          `data-min` = min(d, na.rm = TRUE), `data-max` = max(d, na.rm = TRUE)
        ),
        tags$span(style = 'float: left;'), tags$span(style = 'float: right;')
      )
    } else if (is.factor(d)) {
      t = 'factor'
      tags$div(
        tags$select(
          multiple = 'multiple', style = 'width: 100%;',
          lapply(unique(d), function(x) tags$option(value = x, x))
        ),
        style = 'width: 100%; display: none;'
      )
    } else if (is.character(d)) {
      t = 'character'
      NULL
    }
    x = tagList(
      tags$div(
        class = 'form-group has-feedback', style = 'margin-bottom: auto;',
        tags$input(
          type = 'search', placeholder = 'All', class = 'form-control',
          style = 'width: 100%;'
        ),
        tags$span(class = 'glyphicon glyphicon-remove-circle form-control-feedback')
      ),
      x
    )
    tds[[j]] = tags$td(x, `data-type` = t, style = 'vertical-align: top;')
  }
  tr = tags$tr(tds)
  if (filter == 'top') tr else tags$tfoot(tr)
}

filterDependencies = function() {
  list(
    htmlDependency(
      'nouislider', '7.0.10', depPath('nouislider'),
      script = 'jquery.nouislider.min.js', stylesheet = 'jquery.nouislider.min.css'
    ),
    htmlDependency(
      'selectize', '0.12.0', depPath('selectize'),
      script = 'selectize.min.js', stylesheet = 'selectize.bootstrap3.css'
    )
  )
}

depPath = function(...) {
  system.file('htmlwidgets', 'lib', ..., package = 'DT')
}

extNew = c('AutoFill', 'FixedColumns', 'FixedHeader', 'KeyTable')

extPath = function(...) {
  depPath('datatables-extensions', ...)
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
  htmlDependency(
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

isFALSE = function(x) identical(x, FALSE)

is.Date = function(x) inherits(x, c('Date', 'POSIXlt', 'POSIXct'))
