#' Create an HTML table widget using the DataTables library
#'
#' This function creates an HTML widget to display rectangular data (a matrix or
#' data frame) using the JavaScript library DataTables.
#' @param data a data object (either a matrix or a data frame)
#' @param options a list of initialization options (see
#'   \url{https://datatables.net/reference/option/}); the character options
#'   wrapped in \code{\link[htmlwidgets]{JS}()} will be treated as literal
#'   JavaScript code instead of normal character strings; you can also set
#'   options globally via \code{\link{options}(DT.options = list(...))}, and
#'   global options will be merged into this \code{options} argument if set
#' @param class the CSS class(es) of the table; see
#'   \url{https://datatables.net/manual/styling/classes}
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
#'   you want more control over the styles of filters, you can provide a named
#'   list to this argument; see Details for more
#' @param escape whether to escape HTML entities in the table: \code{TRUE} means
#'   to escape the whole table, and \code{FALSE} means not to escape it;
#'   alternatively, you can specify numeric column indices or column names to
#'   indicate which columns to escape, e.g. \code{1:5} (the first 5 columns),
#'   \code{c(1, 3, 4)}, or \code{c(-1, -3)} (all columns except the first and
#'   third), or \code{c('Species', 'Sepal.Length')}; since the row names take
#'   the first column to display, you should add the numeric column indices by
#'   one when using \code{rownames}
#' @param style either \code{'auto'}, \code{'default'}, \code{'bootstrap'}, or
#'   \code{'bootstrap4'}. If \code{'auto'}, and a **bslib** theme is
#'   currently active, then bootstrap styling is used in a way that "just works"
#'   for the active theme. Otherwise,
#'   \href{https://datatables.net/manual/styling/classes}{DataTables
#'   \code{'default'} styling} is used. If set explicitly to \code{'bootstrap'}
#'   or \code{'bootstrap4'}, one must take care to ensure Bootstrap's HTML
#'   dependencies (as well as Bootswatch themes, if desired) are included on the
#'   page. Note, when set explicitly, it's the user's responsibility to ensure
#'   that only one unique `style` value is used on the same page, if multiple
#'   DT tables exist, as different styling resources may conflict with each other.
#' @param width,height Width/Height in pixels (optional, defaults to automatic
#'   sizing)
#' @param elementId An id for the widget (a random string by default).
#' @param fillContainer \code{TRUE} to configure the table to automatically fill
#'   it's containing element. If the table can't fit fully into it's container
#'   then vertical and/or horizontal scrolling of the table cells will occur.
#' @param autoHideNavigation \code{TRUE} to automatically hide navigational UI
#'   (only display the table body) when the number of total records is less
#'   than the page size. Note, it only works on the client-side processing mode
#'   and the `pageLength` option should be provided explicitly.
#' @param lazyRender \code{FALSE} to render the table immediately on page load,
#'   otherwise delay rendering until the table becomes visible.
#' @param selection the row/column selection mode (single or multiple selection
#'   or disable selection) when a table widget is rendered in a Shiny app;
#'   alternatively, you can use a list of the form \code{list(mode = 'multiple',
#'   selected = c(1, 3, 8), target = 'row', selectable = c(-2, -3))} to
#'   pre-select rows and control the selectable range; the element
#'   \code{target} in the list can be \code{'column'} to enable column
#'   selection, or \code{'row+column'} to make it possible to select both rows
#'   and columns (click on the footer to select columns), or \code{'cell'} to
#'   select cells. See details section for more info.
#' @param extensions a character vector of the names of the DataTables
#'   extensions (\url{https://datatables.net/extensions/index})
#' @param plugins a character vector of the names of DataTables plug-ins
#'   (\url{https://rstudio.github.io/DT/plugins.html}).  Note that only those
#'   plugins supported by the \code{DT} package can be used here. You can see
#'   the available plugins by calling \code{DT:::available_plugins()}
#' @param editable \code{FALSE} to disable the table editor, or \code{TRUE} (or
#'   \code{"cell"}) to enable editing a single cell. Alternatively, you can set
#'   it to \code{"row"} to be able to edit a row, or \code{"column"} to edit a
#'   column, or \code{"all"} to edit all cells on the current page of the table.
#'   In all modes, start editing by doubleclicking on a cell. This argument can
#'   also be a list of the form \code{list(target = TARGET, disable =
#'   list(columns = INDICES))}, where \code{TARGET} can be \code{"cell"},
#'   \code{"row"}, \code{"column"}, or \code{"all"}, and \code{INDICES} is an
#'   integer vector of column indices. Use the list form if you want to disable
#'   editing certain columns. You can also restrict the editing to accept only
#'   numbers by setting this argument to a list of the form \code{list(target =
#'   TARGET, numeric = INDICES)} where \code{INDICES} can be the vector of the
#'   indices of the columns for which you want to restrict the editing to
#'   numbers or \code{"all"} to restrict the editing to numbers for all columns.
#'   If you don't set \code{numeric}, then the editing is restricted to numbers
#'   for all numeric columns; set \code{numeric = "none"} to disable this
#'   behavior. It is also possible to edit the cells in text areas, which are
#'   useful for large contents. For that, set the \code{editable} argument to a
#'   list of the form \code{list(target = TARGET, area = INDICES)} where
#'   \code{INDICES} can be the vector of the indices of the columns for which
#'   you want the text areas, or \code{"all"} if you want the text areas for
#'   all columns. Of course, you can request the numeric editing for some
#'   columns and the text areas for some other columns by setting
#'   \code{editable} to a list of the form \code{list(target = TARGET, numeric
#'   = INDICES1, area = INDICES2)}. Finally, you can edit date cells with a
#'   calendar with \code{list(target = TARGET, date = INDICES)}; the target
#'   columns must have the \code{Date} type. If you don't set \code{date} in
#'   the \code{editable} list, the editing with the calendar is automatically
#'   set for all \code{Date} columns.
#' @details \code{selection}:
#'   \enumerate{
#'     \item The argument could be a scalar string, which means the selection
#'       \code{mode}, whose value could be one of  \code{'multiple'} (the default),
#'       \code{'single'} and \code{'none'} (disable selection).
#'     \item When a list form is provided for this argument, only parts of the
#'       "full" list are allowed. The default values for non-matched elements are
#'       \code{list(mode = 'multiple', selected = NULL, target = 'row',
#'       selectable = NULL)}.
#'     \item \code{target} must be one of \code{'row'}, \code{'column'},
#'       \code{'row+column'} and \code{'cell'}.
#'     \item \code{selected} could be \code{NULL} or "indices".
#'     \item \code{selectable} could be \code{NULL}, \code{TRUE}, \code{FALSE}
#'       or "indices", where \code{NULL} and \code{TRUE} mean all the table is
#'       selectable. When \code{FALSE}, it means users can't select the table
#'       by the cursor (but they could still be able to select the table via
#'       \code{\link{dataTableProxy}}, specifying \code{ignore.selectable = TRUE}).
#'       If "indices", they must be all positive or non-positive values. All
#'       positive "indices" mean only the specified ranges are selectable while all
#'       non-positive "indices" mean those ranges are \emph{not} selectable.
#'       The "indices"' format is specified below.
#'     \item The "indices"' format of \code{selected} and \code{selectable}:
#'       when \code{target} is \code{'row'} or \code{'column'}, it should be a plain
#'       numeric vector; when \code{target} is \code{'row+column'}, it should be a
#'       list, specifying \code{rows} and \code{cols} respectively, e.g.,
#'       \code{list(rows = 1, cols = 2)}; when \code{target} is \code{'cell'},
#'       it should be a 2-col \code{matrix}, where the two values of each row
#'       stand for the row and column index.
#'     \item Note that DT has its own selection implementation and doesn't
#'       use the Select extension because the latter doesn't support the
#'       server-side processing mode well. Please set this argument to
#'       \code{'none'} if you really want to use the Select extension.
#'   }
#'   \code{options$columnDefs}:
#'   \enumerate{
#'     \item \code{columnDefs} is an option that provided by the DataTables library
#'       itself, where the user can set various attributes for columns. It must be
#'       provided as a list of list, where each sub-list must contain a vector named 'targets',
#'       specifying the applied columns, i.e.,
#'       \code{list(list(..., targets = '_all'), list(..., targets = c(1, 2)))}
#'     \item \code{columnDefs$targets} is a vector and should be one of:
#'       \itemize{
#'         \item 0 or a positive integer: column index counting from the left.
#'         \item A negative integer: column index counting from the right.
#'         \item A string: the column name. Note, it must be the names of the
#'           original data, not the ones that (could) be changed via param \code{colnames}.
#'         \item The string "_all": all columns (i.e. assign a default).
#'       }
#'     \item When conflicts happen, e.g., a single column is defined for some property
#'       twice but with different values, the value that defined earlier takes the priority.
#'       For example, \code{list(list(visible=FALSE, target=1), list(visible=TRUE, target=1))}
#'       results in a table whose first column is \emph{invisible}.
#'     \item See \url{https://datatables.net/reference/option/columnDefs} for more.
#'   }
#'   \code{filter}:
#'   \enumerate{
#'     \item \code{filter} can be used to position and customize column filters.
#'       A scalar string value defines the position, and must be one of \code{'none'}
#'       (the default), \code{'bottom'} and \code{'top'}. A named list can be used
#'       for further control. In the named list form:
#'     \item \code{$position} is a string as described above. It defaults to \code{'none'}.
#'     \item \code{$clear} is a logical value indicating if clear
#'       buttons should appear in input boxes. It defaults to \code{TRUE}.
#'     \item \code{$plain} is a logical value indicating if plain styling
#'       should be used for input boxes instead of Bootstrap styling. It
#'       defaults to \code{FALSE}.
#'     \item \code{$vertical} is a logical value indicating if slider
#'       widgets should be oriented vertically rather than horizontally.
#'       It defaults to \code{FALSE}.
#'     \item \code{$opacity} is a numeric value between 0 and 1 used to set
#'       the level of transparency of slider widgets. It defaults to \code{1}.
#'     \item \code{$settings} is a named list used to directly pass configuration
#'       for initializing filter widgets in JavaScript.
#'       \itemize{
#'          \item The \code{$select} element is passed to the select widget, and
#'            \code{$slider} is passed to the slider widget.
#'          \item Valid values depend on the settings accepted by the underlying
#'            JavaScript libraries, \href{https://selectize.dev/}{Selectize}
#'            and \href{https://refreshless.com/nouislider/}{noUiSlider}.
#'            Please note that the versions bundled with DT are currently quite old,
#'            so accepted settings may not match their most recent documentation.
#'          \item These settings can override values set by DT, so specifying
#'            a setting already in use may break something. Use with care.
#'       }
#'   }
#' @note You are recommended to escape the table content for security reasons
#'   (e.g. XSS attacks) when using this function in Shiny or any other dynamic
#'   web applications.
#' @references See \url{https://rstudio.github.io/DT/} for the full
#'   documentation.
#' @importFrom htmltools tags htmlDependency
#' @export
#' @example inst/examples/datatable.R
datatable = function(
  data, options = list(), class = 'display', callback = JS('return table;'),
  rownames, colnames, container, caption = NULL, filter = c('none', 'bottom', 'top'),
  escape = TRUE, style = 'auto', width = NULL, height = NULL, elementId = NULL,
  fillContainer = getOption('DT.fillContainer', NULL),
  autoHideNavigation = getOption('DT.autoHideNavigation', NULL),
  lazyRender = NULL,
  selection = c('multiple', 'single', 'none'), extensions = list(), plugins = NULL,
  editable = FALSE
) {

  # yes, we all hate it
  oop = base::options(stringsAsFactors = FALSE); on.exit(base::options(oop), add = TRUE)

  options = modifyList(
    getOption('DT.options', list()),
    if (is.function(options)) options() else options
  )

  # make sure the options will be a JS array when it is a character vector (of
  # length 1): https://github.com/rstudio/DT/issues/658
  if (is.character(btnOpts <- options[['buttons']]))
    options[['buttons']] = as.list(btnOpts)

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

  # convert the string targets; it must be defined here (not after), as it's supported to be
  # applied to the "original" column names, instead of the "modified“ ones, e.g., via the `colnames` arg
  options[["columnDefs"]] = colDefsTgtHandle(options[["columnDefs"]], base::colnames(data))

  # box scalar elements of list columns
  data = boxListColumnAtomicScalars(data)

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

  # enable column names for column reordering by default
  for (j in seq_len(ncol(data)))
    options = appendColumnDefs(options, list(name = names(data)[j], targets = j - 1))

  style = normalizeStyle(style)
  if (grepl('^bootstrap', style)) class = DT2BSClass(class)
  if (style != 'default') params$style = style

  # add class for fillContainer if necessary
  if (isTRUE(fillContainer)) class = paste(class, 'fill-container')

  if (is.character(filter)) filter = list(position = match.arg(filter))
  filter = modifyList(list(position = 'none', clear = TRUE, plain = FALSE, vertical = FALSE, opacity = 1), filter)
  # HTML code for column filters
  filterHTML = as.character(filterRow(data, !is.null(rn) && colnames[1] == ' ', filter))
  # use the first row in the header as the sorting cells when I put the filters
  # in the second row
  if (filter$position == 'top') options$orderCellsTop = TRUE
  params$filter = filter$position
  params$vertical = filter$vertical
  if (filter$position != 'none') params$filterHTML = filterHTML
  params$filterSettings = filter$settings

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
  if ('Responsive' %in% extensions && is.null(options$responsive)) {
    options$responsive = TRUE
  }

  params$caption = captionString(caption)

  if (isTRUE(editable)) editable = 'cell'
  if (is.character(editable))
    editable = list(target = editable, disable = list(columns = NULL))
  if (is.list(editable))
    params$editable = makeEditableField(editable, data, rn)

  if (!identical(class(callback), class(JS(''))))
    stop("The 'callback' argument only accept a value returned from JS()")
  if (length(options$pageLength) && length(options$lengthMenu) == 0) {
    if (!isFALSE(options$lengthChange))
      options$lengthMenu = sort(unique(c(options$pageLength, 10, 25, 50, 100)))
    if (identical(options$lengthMenu, c(10, 25, 50, 100)))
      options$lengthMenu = NULL  # that is just the default
  }
  if (!is.null(options[['search']]) && !is.list(options[['search']]))
    stop("The value of `search` in `options` must be NULL or a list")

  # record fillContainer and autoHideNavigation
  if (!is.null(fillContainer)) params$fillContainer = fillContainer
  if (!is.null(autoHideNavigation)) {
    if (isTRUE(autoHideNavigation) && length(options$pageLength) == 0L)
      warning("`autoHideNavigation` will be ignored if the `pageLength` option is not provided.",
              immediate. = TRUE)
    params$autoHideNavigation = autoHideNavigation
  }

  # record lazyRender
  if(isFALSE(lazyRender)) params$lazyRender = lazyRender

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
      list(mode = 'multiple', selected = NULL, target = 'row', selectable = NULL), selection,
      keep.null = TRUE # this is necessary otherwise the element may become undefined in JS
      # instead of the null value
    )
    # for compatibility with DT < 0.1.22 ('selected' could be row names)
    if (grepl('^row', selection$target) && is.character(selection$selected) && length(rn)) {
      selection$selected = match(selection$selected, rn)
    }
    params$selection = validateSelection(selection)
    # warn if the Select ext is used but selection is not set to none
    if ('Select' %in% extensions && selection$mode != 'none') warning(
      "The Select extension can't work properly with DT's own ",
      "selection implemention and is only recommended in the client mode. ",
      "If you really want to use the Select extension please set ",
      "`selection = 'none'`", immediate. = TRUE
    )
  }

  deps = DTDependencies(style)
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
      knitr.figure = FALSE, defaultWidth = '100%', defaultHeight = 'auto'
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

makeEditableField = function(x, data, rn) {
  for (i in c('numeric', 'area', 'date')) {
    xi = x[[i]]
    if (i == 'area' && is.null(xi)) next  # no default editable fields for textareas
    test = switch(
      i, numeric = is.numeric, date = function(x) inherits(x, 'Date')
    )
    make = function(z) {
      if (identical(z, 'none')) return(NULL)
      if (identical(z, 'all')) return(seq_along(data) - 1)
      if (is.null(z)) return(
        which(unname(vapply(data, test, logical(1)))) - 1
      )
      z - is.null(rn)
    }
    x[[i]] = as.list(make(xi))
  }
  x
}

validateSelection = function(x) {
  isRowColList = function(x) is.list(x) && all(names(x) %in% c('rows', 'cols'))
  is2ColMatrix = function(x) is.matrix(x) && ncol(x) == 2L
  validator = list(
    mode = function(x) {
      if (length(x$mode) != 1L || !x$mode %in% c('none', 'single', 'multiple'))
        "- `mode` must be one of 'none', 'single' and 'multiple'"
    },
    target = function(x) {
      if (length(x$target) != 1L || !x$target %in% c('row', 'column', 'row+column', 'cell'))
        "- `target` must be one of 'row', 'column', 'row+column' and 'cell'"
    },
    selected = function(x) {
      if (length(x$selected) == 0L)
        NULL
      else if (x$target == 'row+column' && !isRowColList(x$selected))
        "- when `target` is 'row+column', `selected` must be in the form of `list(rows = 1, cols = 2)`"
      else if (x$target == 'cell' && !is2ColMatrix(x$selected))
        "- when `target` is 'cell', `selected` must be a 2-col matrix"
    },
    selectable = function(x) {
      if (length(x$selectable) == 0L || is.logical(x$selectable))
        NULL
      else if (!sameSign(x$selectable, zero = -1L))
        "- selectable must be either all positive or all non-positive values"
      else if (x$target == 'row+column' && !isRowColList(x$selectable))
        "- when `target` is 'row+column', `selectable` must be in the form of `list(rows = 1, cols = 2)`"
      else if (x$target == 'cell' && !is2ColMatrix(x$selectable))
        "- when `target` is 'cell', `selectable` must be a 2-col matrix"
    }
  )
  err = lapply(names(validator), function(e) validator[[e]](x))
  err = unlist(err, use.names = FALSE)
  if (length(err))
    stop(paste0(c("the `selection` argument is incorrect:", err), collapse = '\n'), call. = FALSE)
  x
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
        col = seq_len(ncol) - 1L
      } else {
        col = integer()
      }
      cols = c(cols, col)
    }
  }
  unique(cols)
}

targetIdx = function(targets, names) {
  # return the js side idx which starts from zero
  unname(convertIdx(targets, names)) - 1L
}

colDefsTgtHandle = function(columnDefs, names) {
  convert = function(targets, names) {
    if (is.list(targets)) {
      lapply(targets, convert, names = names)
    } else if (is.character(targets)) {
      any_all = "_all" %in% targets
      if (any_all) {
        out = "_all"
      } else {
        out = targetIdx(targets, names)
      }
      out
    } else {
      targets
    }
  }
  error_msg = "options$columnDefs must be `NULL` or a list of sub-lists, where each sub-list must contain a `targets` element."
  check = function(x) {
    if (is.list(x)) {
      if (is.null(names(x))) return(lapply(x, check))
      if ('targets' %in% names(x)) {
        x[['targets']] = convert(x[['targets']], names)
        return(x)
      }
    }
    str(columnDefs)
    stop(error_msg, call. = FALSE)
  }
  lapply(columnDefs, check)
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
  if (is.null(data) || prod(dim(data)) == 0 || isFALSE(i)) return(data)
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

sameSign = function(x, zero = 0L) {
  if (length(x) == 0L) return(TRUE)
  if (is.list(x)) return(all(vapply(x, sameSign, TRUE, zero = zero)))
  sign = base::sign(x)
  sign[x == 0L] = base::sign(zero)
  length(unique(as.vector(sign))) == 1L
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

filterRow = function(
  data, rownames = TRUE,
  filter = list(position = 'none', clear = TRUE, plain = FALSE, vertical = FALSE, opacity = 1)
) {
  if (filter$position == 'none') return()

  filters = columnFilters(data)
  row = columnFilterRow(filters, options = filter)

  # no filter for row names (may change in future)
  if (rownames) {
    row$children[[1]][[1]] = tags$td('')
  }

  row
}

# calculate properties needed to represent a filter control in JavaScript
columnFilters = function(data) {
  decimals = function(x) {
    x = abs(na.omit(x))
    if (length(x) == 0) return()
    i = 0L
    while (i < 15 && any(round(x, i) != x)) i = i + 1L
    if (i > 0L) i
  }

  lapply(data, function(d) {
    type = NULL
    control = 'none'
    params = list()
    disabled = FALSE

    if (is.numeric(d) || is.Date(d)) {
      control = 'slider'
      type = if (is.numeric(d)) {
        if (is.integer(d)) 'integer' else 'number'
      } else 'time'

      # convert date/times to JavaScript format
      if (type == 'time') {
        # JavaScript does have the Date type like R (YYYY-mm-dd without time)
        if (inherits(d, 'Date')) {
          d = as.POSIXct(d); type = 'date'
        }
        d = as.numeric(d) * 1000  # use milliseconds for JavaScript
      }

      # find range
      suppressWarnings({
        d1 = min(d, na.rm = TRUE)
        d2 = max(d, na.rm = TRUE)
      })

      # find scale to avoid fp issues
      dec = decimals(d)
      if (!is.null(dec)) {
        d1 = floor(d1 * 10^dec) / 10^dec
        d2 = ceiling(d2 * 10^dec) / 10^dec
      }

      disabled = !(is.finite(d1) && is.finite(d2) && d2 > d1)
      if (disabled) {
        # still need some finite numbers for noUiSlider to not crash
        d1 = 0
        d2 = 1
      }

      params = list(min = d1, max = d2, scale = dec)
    } else if (is.factor(d) || is.logical(d)) {
      control = 'select'
      disabled = (length(unique(d)) <= 1)

      if (is.logical(d)) {
        type = 'logical'
        d = c('true', 'false', if (anyNA(d)) 'na')
      } else {
        type = 'factor'
        d = levels(d)
      }

      opts = native_encode(jsonlite::toJSON(as.character(d)))
      params = list(options = opts)
    } else if (is.character(d)) {
      type = 'character'
      disabled = (length(unique(d)) <= 1)
    }

    list(control = control, type = type, params = params, disabled = disabled)
  })
}

#' @importFrom htmltools tagList
columnFilterRow = function(filters, options = list()) {
  defaults = list(clear = TRUE, plain = FALSE, vertical = FALSE, opacity = 1)
  options = modifyList(defaults, options)

  tds = lapply(filters, function(f) {
    p = f$params

    # create HTML for the control element
    ctrl = if (f$control == 'slider') {
      tags$div(
        style = paste0('display: none;position: absolute;width: 200px;opacity: ', options$opacity),
        tags$div(`data-min` = p$min, `data-max` = p$max, `data-scale` = p$scale),
        if (options$vertical) tagList(
          tags$span(style = 'position: absolute; bottom: 0px; left: 15px;'),
          tags$span(style = 'display: none;', HTML('&nbsp;')),
          tags$span(style = 'position: absolute; top: 2px; left: 15px;')
        ) else tagList(
          tags$span(style = 'float: left;'),
          tags$span(style = 'float: right;')
        )
      )
    } else if (f$control == 'select') {
      tags$div(
        tags$select(
          multiple = 'multiple',
          style = 'width: 100%;',
          `data-options` = p$options
        ),
        style = 'width: 100%; display: none;'
      )
    }

    # create HTML for the search box input
    clear = options$clear
    input = if (options$plain) {
      tags$div(
        style = 'margin-bottom: auto;',
        tags$input(
          type = if (clear) 'search' else 'text', placeholder = 'All',
          style = 'width: 100%;',
          disabled = if (f$disabled) ""
        )
      )
    } else {
      tags$div(
        class = if (clear) 'form-group has-feedback' else 'form-group',
        style = 'margin-bottom: auto;',
        tags$input(
          type = 'search', placeholder = 'All', class = 'form-control',
          style = 'width: 100%;',
          disabled = if (f$disabled) ""
        ),
        if (clear) tags$span(
          class = 'glyphicon glyphicon-remove-circle form-control-feedback'
        )
      )
    }

    tags$td(tagList(input, ctrl), `data-type` = f$type, style = 'vertical-align: top;')
  })

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
    if (is.logical(buttons))
      buttons = if (buttons) c('copy', 'excel', 'csv', 'pdf', 'print')
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
  if (extension == 'DateTime') css = sprintf('dataTables.%s.min.css', ext)
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
  if (is.null(config) || is.character(config) || is.logical(config)) return(config)
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
  pdfmake = list(script = c('pdfmake.js', 'vfs_fonts.js'))
)

extraDependency = function(names = NULL, ...) {
  lapply(names, function(name) {
    htmlDependency(
      name, DataTablesVersion, extPath(...),
      script = extraDepData[[name]][['script']], all_files = FALSE
    )
  })
}

normalizeStyle = function(style) {
  style = tolower(style)
  if (!identical(style, 'auto')) {
    return(match.arg(style, DTStyles()))
  }
  if (system.file(package = 'bslib') == '') {
    return('default')
  }
  # This function should really be called inside preRenderHook() (if called anytime earlier,
  # we're running the risk of not knowing the true theme). Overall, I think that's ok in this
  # case since DT doesn't need to compile new Sass, it just needs to know whether or not
  # Bootstrap is relevant. If we run into problems in the future, let's move this to preRenderHook()
  # time, but that's going to be a non-trivial change to existing logic
  theme = bslib::bs_current_theme()
  if (is.null(theme)) {
    return('default')
  }

  bs_v = as.numeric(bslib::theme_version(theme)[1])
  if (length(bs_v) == 0) return('default')
  # TODO: If DT adds support for BS > 5, update this logic
  if (bs_v > 5) bs_v = 5
  style = paste0("bootstrap", if (bs_v > 3) bs_v)

  # Have style remember if bslib should be a dependency
  structure(style, bslib = TRUE)
}

# core JS and CSS dependencies of DataTables
DTDependencies = function(style) {
  js = 'jquery.dataTables.min.js'
  if (style == 'default') {
    # patch the default style
    css = c('jquery.dataTables.min.css', 'jquery.dataTables.extra.css')
  } else {
    js = c(js, sprintf('dataTables.%s.min.js', style))
    css = sprintf('dataTables.%s.min.css', style)
    # patch the Bootstrap style
    if (style == 'bootstrap') css = c(css, 'dataTables.bootstrap.extra.css')
    if (style == 'bootstrap4') css = c(css, 'dataTables.bootstrap4.extra.css')
  }
  c(
    list(jquerylib::jquery_core(), htmlDependency(
      depName(style, 'dt-core'),
      DataTablesVersion,
      src = depPath('datatables'),
      script = file.path('js', js),
      stylesheet = file.path('css', css),
      all_files = FALSE
    )),
    # This attribute may have been added in normalizeStyle(), and in that case,
    # a bslib theme is active/relevant, so add the Bootstrap HTML dependencies to
    # make sure the Bootstrap styles are present
    if (grepl('^bootstrap', style) && isTRUE(attr(style, 'bslib'))) {
      bslib::bs_theme_dependencies(bslib::bs_current_theme())
    }
  )
}

# translate DataTables classes to Bootstrap table classes
DT2BSClass = function(class) {
  class = if (is.list(class)) {
    names(which(unlist(class)))
  } else {
    unlist(strsplit(class, '\\s+'))
  }
  if ('display' %in% class)
    class = unique(c('stripe', 'hover', 'row-border', 'order-column', class))
  BSclass = c(
    'cell-border' = 'table-bordered', 'compact' = 'table-condensed',
    'hover' = 'table-hover', 'stripe' = 'table-striped'
  )
  # translate known default styling classes to BS table classes and keep
  # unknown classes as they are
  class = c(
    BSclass[intersect(class, names(BSclass))],
    setdiff(class, names(BSclass))
  )
  class = unique(c('table', class))
  paste(class, collapse = ' ')
}

# all the plugins' js/css files should be place under datatables-plugins/group/plugin
# the name is the path of the plugins relative to depPath('datatables-plugins')
available_plugins = function() {
  plugin_path = depPath('datatables-plugins')
  groups = list.dirs(plugin_path, full.names = FALSE, recursive = FALSE)
  plugins = lapply(
    groups,
    function(g) {
      out = list.dirs(file.path(plugin_path, g), full.names = FALSE, recursive = FALSE)
      setNames(out, file.path(g, out))
    }
  )
  unlist(plugins)
}

pluginDependency = function(plugin) {
  plugins = available_plugins()
  if (!plugin %in% plugins) stop(
    "Could not find plugin '", plugin, "'.  '",
    'Only the following plugins are supported by DT: ',
    paste0(plugins, collapse = ', ')
  )
  d = depPath('datatables-plugins', names(plugins)[plugins == plugin])
  htmlDependency(
    paste0('dt-plugin-', tolower(plugin)), DataTablesVersion, src = d,
    script = list.files(d, '[.]js$'), stylesheet = list.files(d, '[.]css$')
  )
}
