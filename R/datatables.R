#' Create an HTML table widget using the DataTables library
#'
#' This function creates an HTML widget to display rectangular data (a matrix or
#' data frame) using the JavaScript library DataTables.
#' @param data a data object (either a matrix or a data frame)
#' @param options a list of initialization options (see
#'   \url{http://datatables.net/reference/option/}); the character options
#'   wrapped in \code{\link[htmlwidgets]{JS}()} will be treated as literal
#'   JavaScript code instead of normal character strings; you can also set
#'   options globally via \code{\link{options}(DT.options = list(...))}, and
#'   global options will be merged into this \code{options} argument if set
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
#'   for factor columns, and text input boxes are used for character columns; if
#'   you want more control over the styles of filters, you can provide a list to
#'   this argument of the form \code{list(position = 'top', clear = TRUE, plain
#'   = FALSE)}, where \code{clear} indicates whether you want the clear buttons
#'   in the input boxes, and \code{plain} means if you want to use Bootstrap
#'   form styles or plain text input styles for the text input boxes
#' @param escape whether to escape HTML entities in the table: \code{TRUE} means
#'   to escape the whole table, and \code{FALSE} means not to escape it;
#'   alternatively, you can specify numeric column indices or column names to
#'   indicate which columns to escape, e.g. \code{1:5} (the first 5 columns),
#'   \code{c(1, 3, 4)}, or \code{c(-1, -3)} (all columns except the first and
#'   third), or \code{c('Species', 'Sepal.Length')}
#' @param style the style name (\url{http://datatables.net/manual/styling/});
#'   currently only \code{'default'} and \code{'bootstrap'} are supported
#' @param selection the row selection mode (single or multiple selection or
#'   disable selection) when a table widget is rendered in a Shiny app
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
  escape = TRUE, style = 'default',
  selection = c('multiple', 'single', 'none'), extensions = list()
) {

  # yes, we all hate it
  oop = options(stringsAsFactors = FALSE); on.exit(options(oop), add = TRUE)

  options = modifyList(getOption('DT.options', list()), options)

  # deal with row names: rownames = TRUE or missing, use rownames(data)
  rn = if (missing(rownames) || isTRUE(rownames)) base::rownames(data) else {
    if (is.character(rownames)) rownames  # use custom row names
  }

  hideDataTable = FALSE
  if (is.null(data) || ncol(data) == 0) {
    data = data.frame(numeric(0))
    names(data) = " "
    hideDataTable = TRUE
  }

  if (is.data.frame(data)) {
    data = as.data.frame(data)
    numc = unname(which(vapply(data, is.numeric, logical(1))))
  } else {
    if (!is.matrix(data))
      stop("'data' must be either a matrix or a data frame")
    numc = if (is.numeric(data)) seq_len(ncol(data))
    data = as.data.frame(data)
  }
  if (length(rn)) {
    data = cbind(' ' = rn, data)
    numc = numc + 1  # move indices of numeric columns to the right by 1
  }

  # align numeric columns to the right
  if (length(numc)) options = appendColumnDefs(
    options, list(className = 'dt-right', targets = numc - 1)
  )

  # make sure the table is _not_ ordered by default (change the DataTables default)
  if (is.null(options[['order']])) options$order = list()
  # I do not see the point of "autoWidth: true" as the default in DataTables
  if (is.null(options[['autoWidth']])) options$autoWidth = FALSE
  # disable CSS classes for ordered columns
  if (is.null(options[['orderClasses']])) options$orderClasses = FALSE

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
  # do not order the first column if the name is empty (a column for row names)
  if (length(colnames) && colnames[1] == ' ')
    options = appendColumnDefs(options, list(orderable = FALSE, targets = 0))

  style = match.arg(style, list.files(depPath('datatables', 'css')))
  if (style == 'bootstrap') class = DT2BSClass(class)

  if (is.character(filter)) filter = list(position = match.arg(filter))
  filter = modifyList(list(position = 'none', clear = TRUE, plain = FALSE), filter)
  # HTML code for column filters
  filterHTML = as.character(filterRow(data, length(rn) > 0 && colnames[1] == ' ', filter))
  # use the first row in the header as the sorting cells when I put the filters
  # in the second row
  if (filter$position == 'top') options$orderCellsTop = TRUE
  if (missing(container)) {
    container = tags$table(tableHeader(colnames, escape), class = class)
  }

  # indices of columns that need to be escaped
  attr(options, 'escapeIdx') = escapeToConfig(escape, colnames)

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

  # generate <caption></caption>
  if (is.character(caption)) caption = tags$caption(caption)
  caption = as.character(caption)

  if (!identical(class(callback), class(JS(''))))
    stop("The 'callback' argument only accept a value returned from JS()")
  if (length(options$pageLength) && length(options$lengthMenu) == 0) {
    if (!isFALSE(options$lengthChange))
      options$lengthMenu = sort(unique(c(options$pageLength, 10, 25, 50, 100)))
    if (identical(options$lengthMenu, c(10, 25, 50, 100)))
      options$lengthMenu = NULL  # that is just the default
  }
  # if you use copy_csv_xls.swf, we should disable the pdf button in TableTools
  swf = options$tableTools$sSwfPath
  if (length(swf) == 1 && length(options$tableTools$aButtons) == 0) {
    if (basename(swf) == 'copy_csv_xls.swf')
      options$tableTools$aButtons = c('copy', 'csv', 'xls', 'print')
  }

  params = structure(list(
    data = data, container = as.character(container), options = options,
    callback = if (!missing(callback)) JS('function(table) {', callback, '}'),
    caption = caption, filter = filter$position
  ), colnames = cn, rownames = length(rn) > 0)
  if (length(params$caption) == 0) params$caption = NULL
  if (params$filter != 'none') params$filterHTML = filterHTML
  if (style != 'default') params$style = style
  if (length(extensions)) params$extensions = as.list(extensions)
  if (length(extOptions)) params$extOptions = extOptions
  if (inShiny()) params$selection = match.arg(selection)

  deps = list(htmlDependency(
    'datatables', DataTablesVersion, src = depPath('datatables', 'js'),
    script = 'jquery.dataTables.min.js'
  ))
  deps = c(deps, list(styleDependency(style)))
  deps = c(deps, lapply(extensions, extDependency))
  if (params$filter != 'none') deps = c(deps, filterDependencies())
  if (isTRUE(options$searchHighlight))
    deps = c(deps, list(pluginDependency('searchHighlight')))

  htmlwidgets::createWidget(
    'datatables', if (hideDataTable) NULL else params,
    package = 'DT', width = '100%', height = 'auto',
    dependencies = deps, preRenderHook = function(instance) {

      data = instance[['x']][['data']]

      # 1.5Mb is just an arbitrary size from my experiments
      # TODO: Move this to widget_data override, or preRenderHook
      if (object.size(data) > 1.5e6 && getOption('DT.warn.size', TRUE))
        warning(
          'It seems your data is too big for client-side DataTables. You may ',
          'consider server-side processing: http://rstudio.github.io/DT/server.html'
        )

      data = escapeData(data, escape, colnames)
      data = unname(data)
      instance$x$data = data

      instance
    }
  )
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
filterRow = function(
  data, rownames = TRUE,
  filter = list(position = 'none', clear = TRUE, plain = FALSE)
) {
  if (filter$position == 'none') return()
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
      if (is.integer(d)) t = 'integer'
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
    } else if (is.factor(d) || is.logical(d)) {
      if (is.logical(d)) {
        t = 'logical'
        d = c('true', 'false', if (any(is.na(d))) 'na')
      } else {
        t = 'factor'
        d = sort(unique(d))
      }
      tags$div(
        tags$select(
          multiple = 'multiple', style = 'width: 100%;',
          lapply(d, function(x) tags$option(value = x, x))
        ),
        style = 'width: 100%; display: none;'
      )
    } else if (is.character(d)) {
      t = 'character'
      NULL
    }
    clear = filter$clear
    input = if (filter$plain) {
      tags$div(
        style = 'margin-bottom: auto;',
        tags$input(
          type = if (clear) 'search' else 'text', placeholder = 'All',
          style = 'width: 100%;'
        )
      )
    } else {
      tags$div(
        class = if (clear) 'form-group has-feedback' else 'form-group',
        style = 'margin-bottom: auto;',
        tags$input(
          type = 'search', placeholder = 'All', class = 'form-control',
          style = 'width: 100%;'
        ),
        if (clear) tags$span(
          class = 'glyphicon glyphicon-remove-circle form-control-feedback'
        )
      )
    }
    x = tagList(input, x)
    tds[[j]] = tags$td(x, `data-type` = t, style = 'vertical-align: top;')
  }
  tags$tr(tds)
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
  if (dest == '') dest = '.'
  file.path(dest, swf, fsep = '/')
}

styleDependency = function(style) {
  d = depPath('datatables', 'css', style)
  htmlDependency(
    paste('datatables', style, sep = '-'), DataTablesVersion, src = d,
    script = list.files(d, '[.]min[.]js$'), stylesheet = list.files(d, '[.]css$')
  )
}

# translate DataTables classes to Bootstrap table classes
DT2BSClass = function(class) {
  class = unlist(strsplit(class, '\\s+'))
  if ('display' %in% class)
    class = unique(c('stripe', 'hover', 'row-border', 'order-column', class))
  BSclass = c(
    'cell-border' = 'table-bordered', 'compact' = 'table-condensed',
    'hover' = 'table-hover', 'stripe' = 'table-striped'
  )
  class = c(
    BSclass[intersect(class, names(BSclass))],
    grep('^table-', class, value = TRUE)
  )
  class = unique(c('table', class))
  paste(class, collapse = ' ')
}

pluginDependency = function(plugin) {
  d = depPath('datatables-plugins', plugin)
  htmlDependency(
    paste('datatables', plugin, sep = '-'), DataTablesVersion, src = d,
    script = list.files(d, '[.]js$'), stylesheet = list.files(d, '[.]css$')
  )
}
