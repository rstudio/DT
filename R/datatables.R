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
#' @param width,height Width/Height in pixels (optional, defaults to automatic
#'   sizing)
#' @param elementId An id for the widget (a random string by default).
#' @param fillContainer \code{TRUE} to configure the table to automatically fill
#'   it's containing element. If the table can't fit fully into it's container
#'   then vertical and/or horizontal scrolling of the table cells will occur.
#' @param autoHideNavigation \code{TRUE} to automatically hide navigational UI
#'   when the number of total records is less than the page size.
#' @param selection the row/column selection mode (single or multiple selection
#'   or disable selection) when a table widget is rendered in a Shiny app;
#'   alternatively, you can use a list of the form \code{list(mode = 'multiple',
#'   selected = c(1, 3, 8), target = 'row')} to pre-select rows; the element
#'   \code{target} in the list can be \code{'column'} to enable column
#'   selection, or \code{'row+column'} to make it possible to select both rows
#'   and columns (click on the footer to select columns), or \code{'cell'} to
#'   select cells
#' @param extensions a character vector of the names of the DataTables
#'   extensions (\url{https://datatables.net/extensions/index})
#' @param plugins a character vector of the names of DataTables plug-ins
#'   (\url{https://rstudio.github.io/DT/plugins.html}).  Note that only those
#'   plugins supported by the \code{DT} package can be used here.
#' @param editable \code{FALSE} to disable the table editor, or \code{TRUE} (or
#'   \code{"cell"}) to enable editing a single cell. Alternatively, you can set
#'   it to \code{"row"} to be able to edit a row, or \code{"column"} to edit a
#'   column, or \code{"all"} to edit all cells on the current page of the table.
#'   In all modes, start editing by doubleclicking on a cell. This argument can
#'   also be a list of the form \code{list(target = TARGET, disable =
#'   list(columns = INDICES))}, where \code{TARGET} can be \code{cell},
#'   \code{row}, \code{column}, or \code{all}, and \code{INDICES} is an integer
#'   vector of column indices. Use the list form if you want to disable editing
#'   certain columns.
#' @note You are recommended to escape the table content for security reasons
#'   (e.g. XSS attacks) when using this function in Shiny or any other dynamic
#'   web applications.
#' @references See \url{https://rstudio.github.io/DT} for the full
#'   documentation.
#' @importFrom htmltools tags htmlDependency
#' @export
#' @example inst/examples/datatable.R
datatable = function(
  data, options = list(), class = 'display', callback = JS('return table;'),
  rownames, colnames, container, caption = NULL, filter = c('none', 'bottom', 'top'),
  escape = TRUE, style = 'default', width = NULL, height = NULL, elementId = NULL,
  fillContainer = getOption('DT.fillContainer', NULL),
  autoHideNavigation = getOption('DT.autoHideNavigation', NULL),
  selection = c('multiple', 'single', 'none'), extensions = list(), plugins = NULL,
  editable = FALSE
) {

  # yes, we all hate it
  oop = base::options(stringsAsFactors = FALSE); on.exit(base::options(oop), add = TRUE)

  options = modifyList(
    getOption('DT.options', list()),
    if (is.function(options)) options() else options
  )

  # https://github.com/rstudio/DT/issues/658
  # https://datatables.net/reference/option/buttons says options$buttons accept
  # a character vector, a boolean or an object
  # But a scalar string or a boolean will display all the default buttons without
  # correct dependencies.
  btnConfig = options[['buttons']]
  if (is.logical(btnConfig)) {
    # $.fn.dataTable.Buttons.defaults
    btnDefaults = c('copy', 'excel', 'csv', 'pdf', 'print')
    btnConfig = if (isTRUE(btnConfig)) btnDefaults else character()
  }
  options[['buttons']] = as.list(btnConfig)

  params = list()
  attr(params, "TOJSON_ARGS") = getOption("DT.TOJSON_ARGS")

  if (crosstalk::is.SharedData(data)) {
    params$crosstalkOptions = list(key = data$key(), group = data$groupName())
    data = data$data(withSelection = FALSE, withFilter = TRUE, withKey = FALSE)
  }

  # deal with row names: rownames = TRUE or missing, use rownames(data)
  rn = if (missing(rownames) || isTRUE(rownames)) base::rownames(data) else {
    if (is.character(rownames)) rownames  # use custom row names
  }

  hideDataTable = FALSE
  if (is.null(data) || identical(ncol(data), 0L)) {
    data = matrix(ncol = 0, nrow = NROW(data))
    hideDataTable = TRUE
  } else if (length(dim(data)) != 2) {
    str(data)
    stop("'data' must be 2-dimensional (e.g. data frame or matrix)")
  }

  if (is.data.frame(data)) {
    data = as.data.frame(data)
    numc = unname(which(vapply(data, is.numeric, logical(1))))
  } else {
    if (!is.matrix(data)) stop(
      "'data' must be either a matrix or a data frame, and cannot be ",
      classes(data), ' (you may need to coerce it to matrix or data frame)'
    )
    numc = if (is.numeric(data)) seq_len(ncol(data))
    data = as.data.frame(data)
  }
  if (!is.null(rn)) {
    data = cbind(' ' = rn, data)
    numc = numc + 1  # move indices of numeric columns to the right by 1
  }

  # align numeric columns to the right
  if (length(numc)) {
    # if the `className` of the column has already been defined by the user,
    # we should not touch it
    undefined_numc = setdiff(numc - 1, classNameDefinedColumns(options, ncol(data)))
    if (length(undefined_numc)) options = appendColumnDefs(
      options, list(className = 'dt-right', targets = undefined_numc)
    )
  }

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

  style = match.arg(tolower(style), DTStyles())
  if (style == 'bootstrap') class = DT2BSClass(class)
  if (style != 'default') params$style = style

  # add class for fillContainer if necessary
  if (isTRUE(fillContainer)) class = paste(class, 'fill-container')

  if (is.character(filter)) filter = list(position = match.arg(filter))
  filter = modifyList(list(position = 'none', clear = TRUE, plain = FALSE), filter)
  # HTML code for column filters
  filterHTML = as.character(filterRow(data, !is.null(rn) && colnames[1] == ' ', filter))
  # use the first row in the header as the sorting cells when I put the filters
  # in the second row
  if (filter$position == 'top') options$orderCellsTop = TRUE
  params$filter = filter$position
  if (filter$position != 'none') params$filterHTML = filterHTML

  if (missing(container)) {
    container = tags$table(tableHeader(colnames, escape), class = class)
  } else {
    params$class = class
  }

  # indices of columns that need to be escaped
  attr(options, 'escapeIdx') = escapeToConfig(escape, colnames)

  if (is.list(extensions)) {
    extensions = names(extensions)
  } else if (!is.character(extensions)) {
    stop("'extensions' must be either a character vector or a named list")
  }
  params$extensions = if (length(extensions)) as.list(extensions)

  # automatically configure options and callback for extensions
  if ('Responsive' %in% extensions) options$responsive = TRUE

  params$caption = captionString(caption)

  if (isTRUE(editable)) editable = 'cell'
  if (is.character(editable)) editable = list(target = editable, disable = list(columns = NULL))
  if (is.list(editable)) params$editable = editable

  if (!identical(class(callback), class(JS(''))))
    stop("The 'callback' argument only accept a value returned from JS()")
  if (length(options$pageLength) && length(options$lengthMenu) == 0) {
    if (!isFALSE(options$lengthChange))
      options$lengthMenu = sort(unique(c(options$pageLength, 10, 25, 50, 100)))
    if (identical(options$lengthMenu, c(10, 25, 50, 100)))
      options$lengthMenu = NULL  # that is just the default
  }

  # record fillContainer and autoHideNavigation
  if (!is.null(fillContainer)) params$fillContainer = fillContainer
  if (!is.null(autoHideNavigation)) params$autoHideNavigation = autoHideNavigation

  params = structure(modifyList(params, list(
    data = data, container = as.character(container), options = options,
    callback = if (!missing(callback)) JS('function(table) {', callback, '}')
  )), colnames = cn, rownames = length(rn) > 0)
  # selection parameters in shiny (or crosstalk)
  if (inShiny() || length(params$crosstalkOptions)) {
    if (is.character(selection)) {
      selection = list(mode = match.arg(selection))
    }
    selection = modifyList(
      list(mode = 'multiple', selected = NULL, target = 'row'), selection
    )
    # for compatibility with DT < 0.1.22 ('selected' could be row names)
    if (grepl('^row', selection$target) && is.character(selection$selected) && length(rn)) {
      selection$selected = match(selection$selected, rn)
    }
    params$selection = selection
  }

  deps = list(DTDependency(style))
  deps = c(deps, unlist(
    lapply(extensions, extDependency, style, options),
    recursive = FALSE
  ))
  if (params$filter != 'none') deps = c(deps, filterDependencies())
  if (isTRUE(options$searchHighlight))
    deps = c(deps, list(pluginDependency('searchHighlight')))
  if (length(plugins))
    deps = c(deps, lapply(plugins, pluginDependency))
  deps = c(deps, crosstalk::crosstalkLibs())

  # force width and height to NULL for fillContainer
  if (isTRUE(fillContainer)) {
    width = NULL
    height = NULL
  }

  htmlwidgets::createWidget(
    'datatables', if (hideDataTable) NULL else params,
    package = 'DT', width = width, height = height, elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      knitr.figure = FALSE, knitr.defaultWidth = "100%", knitr.defaultHeight = "auto"
    ),
    dependencies = deps, preRenderHook = function(instance) {

      data = instance[['x']][['data']]

      # 1.5Mb is just an arbitrary size from my experiments
      if (object.size(data) > 1.5e6 && getOption('DT.warn.size', TRUE))
        warning(
          'It seems your data is too big for client-side DataTables. You may ',
          'consider server-side processing: https://rstudio.github.io/DT/server.html'
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

classNameDefinedColumns = function(options, ncol) {
  defs = options[['columnDefs']]
  cols = integer()
  for (def in defs) {
    if (!is.null(def[['className']])) {
      col = def[['targets']]
      if (is.numeric(col)) {
        col[col < 0] = col[col < 0] + ncol
      } else if ("_all" %in% col) {
        col = seq_len(ncol) - 1
      } else {
        col = integer()
      }
    }
    cols = c(cols, col)
  }
  unique(cols)
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
  decimals = function(x) {
    x = abs(na.omit(x))
    if (length(x) == 0) return()
    i = 0L
    while (i < 15 && any(round(x, i) != x)) i = i + 1L
    if (i > 0L) i
  }
  for (j in seq_len(ncol(data))) {
    if (j == 1 && rownames) {
      tds[[j]] = tags$td('')  # no filter for row names (may change in future)
      next
    }
    t = NULL
    d = data[, j]
    x = if (is.numeric(d) || is.Date(d)) {
      t = if (is.numeric(d)) {
        if (is.integer(d)) 'integer' else 'number'
      } else 'time'
      if (t == 'time') {
        # JavaScript does have the Date type like R (YYYY-mm-dd without time)
        if (inherits(d, 'Date')) {
          d = as.POSIXct(d); t = 'date'
        }
        d = as.numeric(d) * 1000  # use milliseconds for JavaScript
      }
      suppressWarnings({
        d1 = min(d, na.rm = TRUE)
        d2 = max(d, na.rm = TRUE)
      })
      dec = decimals(d)
      if (!is.null(dec)) {
        d1 = floor(d1 * 10^dec) / 10^dec
        d2 = ceiling(d2 * 10^dec) / 10^dec
      }
      if (is.finite(d1) && is.finite(d2) && d2 > d1) tags$div(
        style = 'display: none; position: absolute; width: 200px;',
        tags$div(`data-min` = d1, `data-max` = d2, `data-scale` = dec),
        tags$span(style = 'float: left;'), tags$span(style = 'float: right;')
      ) else {
        t = 'disabled'
        NULL
      }
    } else if (is.factor(d) || is.logical(d)) {
      if (length(unique(d)) <= 1) {
        t = 'disabled'
      } else if (is.logical(d)) {
        t = 'logical'
        d = c('true', 'false', if (any(is.na(d))) 'na')
      } else {
        t = 'factor'
        d = sort(unique(d))
      }
      if (t != 'disabled') tags$div(
        tags$select(
          multiple = 'multiple', style = 'width: 100%;',
          `data-options` = native_encode(jsonlite::toJSON(as.character(d)))
        ),
        style = 'width: 100%; display: none;'
      )
    } else if (is.character(d)) {
      t = if (length(unique(d)) <= 1) 'disabled' else 'character'
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

depName = function(style = 'default', ...) {
  tolower(paste(c(..., if (style != 'default') c('-', style)), collapse = ''))
}

DTStyles = function() {
  r = '^dataTables[.]([^.]+)[.]min[.]css$'
  x = list.files(depPath('datatables', 'css'), r)
  c('default', gsub(r, '\\1', x))
}

extPath = function(...) {
  depPath('datatables-extensions', ...)
}

extAll = function() {
  list.dirs(extPath(), FALSE, FALSE)
}

extDependency = function(extension, style, options) {
  if (!(extension %in% extAll())) stop('The extension ', extension, ' does not exist')
  src = extPath(extension)
  ext = sub('^(.)', '\\L\\1', extension, perl = TRUE)
  buttonDeps = NULL
  if (extension == 'Buttons') {
    buttons = listButtons(options)
    buttonDeps = extraDependency(
      c(if ('excel' %in% buttons) 'jszip', if ('pdf' %in% buttons) 'pdfmake'),
      extension, 'js'
    )
    js = c(
      sprintf('dataTables.%s.min.js', ext),
      sprintf('buttons.%s.min.js', c('flash', 'html5', 'colVis', 'print'))
    )
  } else js = sprintf('dataTables.%s.min.js', ext)
  if (style != 'default') js = c(js, sprintf('%s.%s.min.js', ext, style))
  css = sprintf('%s.%s.min.css', ext, if (style == 'default') 'dataTables' else style)
  js = file.path('js', js); css = file.path('css', css)
  in_dir(src, {
    js = existing_files(js); css = existing_files(css)
  })
  deps = htmlDependency(
    depName(style, 'dt-ext-', extension), DataTablesVersion, src,
    script = js, stylesheet = css, all_files = FALSE
  )
  append(buttonDeps, list(deps))
}

# whether a button was configured in the options
listButtons = function(options) {
  config = options[['buttons']]
  if (is.null(config)) return()
  if (is.character(config)) return(config)
  if (is.list(config)) return(unlist(lapply(config, function(cfg) {
    if (is.character(cfg)) return(cfg)
    if (is.list(cfg)) {
      extend = cfg$extend
      return(if (extend != 'collection') extend else listButtons(cfg))
    }
  })))
  stop('Options for DataTables extensions must be a boolean, a character vector or a list')
}

extraDepData = list(
  jszip = list(script = 'jszip.min.js'),
  pdfmake = list(script = c('pdfmake.min.js', 'vfs_fonts.js'))
)

extraDependency = function(names = NULL, ...) {
  lapply(names, function(name) {
    htmlDependency(
      name, DataTablesVersion, extPath(...),
      script = extraDepData[[name]][['script']], all_files = FALSE
    )
  })
}

# core JS and CSS dependencies of DataTables
DTDependency = function(style) {
  js = 'jquery.dataTables.min.js'
  if (style == 'default') {
    # patch the default style
    css = c('jquery.dataTables.min.css', 'jquery.dataTables.extra.css')
  } else {
    js = c(js, sprintf('dataTables.%s.min.js', style))
    css = sprintf('dataTables.%s.min.css', style)
    # patch the Bootstrap style
    if (style == 'bootstrap') css = c(css, 'dataTables.bootstrap.extra.css')
  }
  htmlDependency(
    depName(style, 'dt-core'), DataTablesVersion, src = depPath('datatables'),
    script = file.path('js', js), stylesheet = file.path('css', css),
    all_files = FALSE
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
  if (!dir.exists(d)) warning(
    "Could not find plugin '", plugin, "'.  ',
    'See https://rstudio.github.io/DT/plugins.html for a list of supported plugins."
  )
  htmlDependency(
    paste0('dt-plugin-', tolower(plugin)), DataTablesVersion, src = d,
    script = list.files(d, '[.]js$'), stylesheet = list.files(d, '[.]css$')
  )
}
