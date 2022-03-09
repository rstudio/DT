#' Helper functions for using DT in Shiny
#'
#' These two functions are like most \code{fooOutput()} and \code{renderFoo()}
#' functions in the \pkg{shiny} package. The former is used to create a
#' container for table, and the latter is used in the server logic to render the
#' table.
#' @inheritParams shiny::dataTableOutput
#' @param width the width of the table container
#' @param height the height of the table container
#' @references \url{https://rstudio.github.io/DT/shiny.html}
#' @export
#' @examples # !formatR
#' if (interactive()) {
#'   library(shiny)
#'   library(DT)
#'   shinyApp(
#'     ui = fluidPage(fluidRow(column(12, DTOutput('tbl')))),
#'     server = function(input, output) {
#'       output$tbl = renderDT(
#'         iris, options = list(lengthChange = FALSE)
#'       )
#'     }
#'   )
#' }
dataTableOutput = function(outputId, width = '100%', height = 'auto') {
  htmltools::attachDependencies(
    htmlwidgets::shinyWidgetOutput(
      outputId, 'datatables', width, height, package = 'DT'
    ),
    crosstalk::crosstalkLibs(),
    append = TRUE
  )
}

#' @export
#' @rdname dataTableOutput
DTOutput = dataTableOutput

#' @export
#' @rdname dataTableOutput
#' @inheritParams shiny::renderDataTable
#' @param expr an expression to create a table widget (normally via
#'   \code{\link{datatable}()}), or a data object to be passed to
#'   \code{datatable()} to create a table widget
#' @param server whether to use server-side processing. If \code{TRUE}, then the
#'   data is kept on the server and the browser requests a page at a time; if
#'   \code{FALSE}, then the entire data frame is sent to the browser at once.
#'   Highly recommended for medium to large data frames, which can cause
#'   browsers to slow down or crash. Note that if you want to use
#'   \code{renderDataTable} with \code{shiny::bindCache()}, this must be
#'   \code{FALSE}.
#' @param funcFilter (for expert use only) passed to the \code{filter} argument
#'   of \code{\link{dataTableAjax}()}
#' @param ... ignored when \code{expr} returns a table widget, and passed as
#'   additional arguments to \code{\link{datatable}()} when \code{expr} returns
#'   a data object
renderDataTable = function(
    expr, server = TRUE, env = parent.frame(), quoted = FALSE,
    funcFilter = dataTablesFilter, ...
  ) {
  if (!quoted) expr = substitute(expr)

  # TODO: this can be simplified after this htmlwidgets PR is merged
  # https://github.com/ramnathv/htmlwidgets/pull/122
  outputInfoEnv = new.env(parent = emptyenv())
  outputInfoEnv[["outputName"]] = NULL
  # jcheng 2018-12-17:
  # It's important to save the session, not just the outputName. It turns
  # out that if the datatable is defined in a module, the outputName is
  # the fully qualified name, and session is the top-level session (that
  # is, the outputName and session that is passed to the render function).
  # That's a fine combination, or alternatively it'd be fine if the name
  # was unqualified and the session was module-specific. But during my
  # commit to add async support, I broke this pairing, and used the (fully
  # qualified) outputName from the render function, and the session from
  # getDefaultReactiveDomain() (which is module specific), and this combo
  # is no good--you end up with the module prefix included twice in the
  # ajax URL. See issue #626 for the repro.
  outputInfoEnv[["session"]] = NULL

  exprFunc = shiny::exprToFunction(expr, env, quoted = TRUE)
  argFunc = shiny::exprToFunction(list(..., server = server), env, quoted = FALSE)
  widgetFunc = function() {
    opts = options(DT.datatable.shiny = TRUE); on.exit(options(opts), add = TRUE)
    instance = exprFunc()
    if (promises::is.promising(instance)) {
      promises::then(instance, processWidget)
    } else {
      processWidget(instance)
    }
  }
  processWidget = function(instance) {
    args = argFunc()
    server = args$server; args$server = NULL # the last element is `server`
    # which is only used in `renderDT()` not `datatable()`, the reason
    # of having it in `argFunc()` is we want `server` to be reactive
    if (!all(c('datatables', 'htmlwidget') %in% class(instance))) {
      instance = do.call(datatable, c(list(instance), args))
    } else if (length(args) != 0) {
      warning("renderDataTable ignores ... arguments when expr yields a datatable object; see ?renderDataTable")
    }

    # in the server mode, we should not store the full data in JSON
    if (server && !is.null(instance[['x']])) {
      if (!is.null(instance$x$crosstalkOptions$group)) {
        stop("Crosstalk only works with DT client mode: DT::renderDataTable({...}, server=FALSE)")
      }

      origData = instance[['x']][['data']]
      instance$x$data = NULL

      # register the data object in a shiny session
      options = instance[['x']][['options']]

      # autoHideNavigation won't work in the server mode
      if (isTRUE(instance[['x']][['autoHideNavigation']]))
        warning("`autoHideNavigation` only works with DT client mode and it will be ignored",
                immediate. = TRUE, call. = FALSE)

      # Normalize "ajax" argument; if we leave it a string then we have several
      # code paths that need to account for both string and list representations
      if (is.character(options[['ajax']])) {
        options$ajax = list(url = options$ajax)
      }

      if (is.null(options[['ajax']][['url']])) {
        url = sessionDataURL(outputInfoEnv[["session"]], origData, outputInfoEnv[["outputName"]], funcFilter)
        options$ajax$url = url
      }
      instance$x$options = fixServerOptions(options)

      # We need to warn the use of "Select" extension in the server-side processing
      # mode since right now there's no good way of supporting the server mode.
      # More specifically, the Select ext can't remember the cross-page selections
      # because the javascript implementation doesn't take the server mode into account.
      # Until that gets changed, we are not able to integrate the Select ext with DT's
      # own implementations.
      if ('Select' %in% as.character(instance$x$extensions)) warning(
        "The Select extension is not able to work with the server-side ",
        "processing mode properly. It's recommended to use the Select extension ",
        "only in the client-side processing mode (by setting `server = FALSE` ",
        "in `DT::renderDT()`) or use DT's own selection implementations (",
        "see the `selection` argument in ?DT::datatable).",
        immediate. = TRUE, call. = FALSE
      )
    }

    instance
  }

  renderFunc = htmlwidgets::shinyRenderWidget(
    widgetFunc(), dataTableOutput, environment(), FALSE
  )

  # The cacheHint arg is not present in Shiny < 1.6.0. Once that version is
  # very widely used, we can remove this if() statement.
  func = if ("cacheHint" %in% names(formals(shiny::markRenderFunction))) {
    # Can't cache with server-side processing
    cacheHint = if (server) FALSE else list(label = "renderDataTable", userExpr = expr)

    shiny::markRenderFunction(
      uiFunc = dataTableOutput,
      renderFunc = function(shinysession, name, ...) {
        domain = tempVarsPromiseDomain(outputInfoEnv, outputName = name, session = shinysession)
        removeTimestampFromSnapshot(name)
        promises::with_promise_domain(domain, renderFunc())
      },
      cacheHint = cacheHint
    )
  } else {
    shiny::markRenderFunction(
      uiFunc = dataTableOutput,
      renderFunc = function(shinysession, name, ...) {
        domain = tempVarsPromiseDomain(outputInfoEnv, outputName = name, session = shinysession)
        removeTimestampFromSnapshot(name)
        promises::with_promise_domain(domain, renderFunc())
      }
    )
  }

  func = shiny::snapshotPreprocessOutput(func, function(value) {
    # Looks for a string like this in the JSON:
    # "url":"session/2a2b834d90637a7559f3ebaba460ad10/dataobj/table?w=&nonce=aea032f33aedfd0e",
    # and removes it, so that the value isn't saved in test snapshots.
    gsub('"ajax"\\s*:\\s*\\{\\s*"url"\\s*:\\s*"[^"]*"\\s*,?', '"ajax":{', value)
  })

  shiny::registerInputHandler('DT.cellInfo', function(val, ...) {
    opts = options(stringsAsFactors = FALSE); on.exit(options(opts), add = TRUE)
    val = lapply(val, as.data.frame)
    do.call(rbind, val)
  }, TRUE)

  func
}

#' @export
#' @rdname dataTableOutput
renderDT = renderDataTable

getAll = function(x, env) {
  as.list(mget(x, env, ifnotfound = rep(list(NULL), times = length(x))))
}

setAll = function(lst, env) {
  mapply(names(lst), lst, FUN = function(name, val) {
    assign(name, val, env)
  })
  invisible()
}

removeTimestampFromSnapshot = function(name) {
  shiny::snapshotPreprocessInput(paste0(name, "_state"), function(value) {
    value$time <- NULL
    value
  })
}

# This promise domain is needed to set/unset temporary variables in
# a specific environment anytime a promise handler is invoked in the
# domain. This is used to pass the Shiny output name from where we
# know it (in the function(shinysession, name, ...) {...}) to where
# we don't know it, but need it (processWidget).
tempVarsPromiseDomain = function(env, ...) {
  force(env)
  vars = list(...)

  promises::new_promise_domain(
    wrapOnFulfilled = function(onFulfilled) {
      # force(onFulfilled)
      function(...) {
        old = getAll(names(vars), env)
        setAll(vars, env)
        on.exit({
          setAll(old, env)
        }, add = TRUE)

        onFulfilled(...)
      }
    },
    wrapOnRejected = function(onRejected) {
      # force(onRejected)
      function(...) {
        old = getAll(names(vars), env)
        setAll(vars, env)
        on.exit({
          setAll(old, env)
        }, add = TRUE)

        onRejected(...)
      }
    },
    wrapSync = function(expr) {
      old = getAll(names(vars), env)
      setAll(vars, env)
      on.exit({
        setAll(old, env)
      }, add = TRUE)

      force(expr)
    }
  )
}

#' Manipulate an existing DataTables instance in a Shiny app
#'
#' The function \code{dataTableProxy()} creates a proxy object that can be used
#' to manipulate an existing DataTables instance in a Shiny app, e.g. select
#' rows/columns, or add rows.
#' @param outputId the id of the table to be manipulated (the same id as the one
#'   you used in \code{\link{dataTableOutput}()})
#' @param session the Shiny session object (from the server function of the
#'   Shiny app)
#' @param deferUntilFlush whether an action should be carried out right away, or
#'   should be held until after the next time all of the outputs are updated
#' @note \code{addRow()} only works for client-side tables. If you want to use
#'   it in a Shiny app, make sure to use \code{renderDataTable(..., server =
#'   FALSE)}. Also note that the column filters (if used) of the table will not
#'   be automatically updated when a new row is added, e.g., the range of the
#'   slider of a column will stay the same even if you have added a value
#'   outside the range of the original data column.
#' @references \url{https://rstudio.github.io/DT/shiny.html}
#' @rdname proxy
#' @export
dataTableProxy = function(
  outputId, session = shiny::getDefaultReactiveDomain(), deferUntilFlush = TRUE
) {
  if (is.null(session))
    stop('dataTableProxy() must be called from the server function of a Shiny app')

  structure(list(
    id = session$ns(outputId), rawId = outputId, session = session,
    deferUntilFlush = deferUntilFlush
  ), class = 'dataTableProxy')
}

#' @param proxy a proxy object returned by \code{dataTableProxy()}
#' @param selected an integer vector of row/column indices, or a matrix of two
#'   columns (row and column indices, respectively) for cell indices; you may
#'   use \code{NULL} to clear existing selections
#' @param ignore.selectable when \code{FALSE} (the default), the "non-selectable"
#'   range specified by \code{selection = list(selectable= )} is respected, i.e.,
#'   you can't select "non-selectable" range. Otherwise, it is ignored.
#'
#' @rdname proxy
#' @export
selectRows = function(proxy, selected, ignore.selectable = FALSE) {
  invokeRemote(
    proxy, 'selectRows',
    list(I_null(as.integer(selected)), ignore.selectable)
  )
}

#' @rdname proxy
#' @export
selectColumns = function(proxy, selected, ignore.selectable = FALSE) {
  invokeRemote(
    proxy, 'selectColumns',
    list(I_null(as.integer(selected)), ignore.selectable)
  )
}

I_null = function(x) if (is.null(x)) list() else x

#' @rdname proxy
#' @export
selectCells = function(proxy, selected, ignore.selectable = FALSE) {
  invokeRemote(
    proxy, 'selectCells',
    list(selected, ignore.selectable)
  )
}

#' @param data a single row of data to be added to the table; it can be a matrix
#'   or data frame of one row, or a vector or list of row data (in the latter
#'   case, please be cautious about the row name: if your table contains row
#'   names, here \code{data} must also contain the row name as the first
#'   element)
#' @rdname proxy
#' @export
addRow = function(proxy, data, resetPaging = TRUE) {
  if ((is.matrix(data) || is.data.frame(data)) && nrow(data) != 1)
    stop("'data' must be of only one row")
  rn <- rownames(data); if (!is.null(rn)) rn <- I(rn)
  # must apply unname() after as.list() because a data.table object
  # can't be really unnamed. The names() attributes will be
  # preserved but with empty strings (see #760).
  invokeRemote(proxy, 'addRow', list(unname(as.list(data)), rn, resetPaging))
}

#' @rdname proxy
#' @export
clearSearch = function(proxy) {
  updateSearch(proxy, list(global = '', columns = ''))
}

#' @param page a number indicating the page to select
#' @rdname proxy
#' @export
selectPage = function(proxy, page) {
  invokeRemote(proxy, 'selectPage', list(page))
}

#' @param caption a new table caption (see the \code{caption} argument of
#'   \code{\link{datatable}()})
#' @rdname proxy
#' @export
updateCaption = function(proxy, caption) {
  invokeRemote(proxy, 'updateCaption', list(captionString(caption)))
}

#' @param keywords a list of two components: \code{global} is the global search
#'   keyword of a single character string (ignored if \code{NULL});
#'   \code{columns} is a character vector of the search keywords for all columns
#'   (when the table has one column for the row names, this vector of keywords
#'   should contain one keyword for the row names as well)
#' @rdname proxy
#' @export
updateSearch = function(proxy, keywords = list(global = NULL, columns = NULL)) {
  global = keywords$global
  if (is.null(global)) {
    keywords['global'] = list(NULL)
  } else {
    if (!is.character(global) || length(global) != 1)
      stop('keywords$global must be a character string')
  }
  columns = keywords$columns
  if (is.null(columns)) {
    keywords['columns'] = list(NULL)
  } else {
    if (is.character(columns)) {
      if (length(columns) == 0) stop(
        'The length of keywords$columns must be greater than zero if it is a character vector'
      )
    } else if (is.list(columns)) {
      if (any(sapply(columns, length) > 1)) stop(
        'keywords$columns should be a list of NULL or character strings'
      )
    } else stop('keywords$columns must be either a character vector or a list')
  }
  invokeRemote(proxy, 'updateSearch', list(keywords))
}

#' @param show a vector of column positions to show (the indexing starts at
#' 0, but if row.names are visible, they are the first column).
#' @rdname proxy
#' @export
showCols = function(proxy, show, reset = FALSE) {
  invokeRemote(proxy, 'showCols', list(show, reset))
}

#' @param hide a vector of column positions to hide
#' @param reset if \code{TRUE}, will only show/hide the columns indicated.
#' @rdname proxy
#' @export
hideCols = function(proxy, hide, reset = FALSE) {
  invokeRemote(proxy, 'hideCols', list(hide, reset))
}

#' @param order A numeric vector of column positions, starting from 0, and including
#' the row.names as a column, if they are include. Must contain a value
#' for all columns, regardless of whether they are visible or not. Also for
#' column reordering to work, the datatable must have extension 'ColReorder'
#' set as well as option 'colReordoer' set to TRUE).
#' @param origOrder Whether column reordering should be relative to the original
#' order (the default is to compare to current order)
#' @rdname proxy
#' @export
colReorder = function(proxy, order, origOrder = FALSE) {
  invokeRemote(proxy, 'colReorder', list(order, origOrder))
}


#' @param resetPaging whether to reset the paging position
#' @param clearSelection which existing selections to clear: it can be any
#'   combinations of \code{row}, \code{column}, and \code{cell}, or \code{all}
#'   for all three, or \code{none} to keep current selections (by default, all
#'   selections are cleared after the data is reloaded)
#' @note \code{reloadData()} only works for tables in the server-side processing
#'   mode, e.g. tables rendered with \code{renderDataTable(server = TRUE)}. The
#'   data to be reloaded (i.e. the one you pass to \code{dataTableAjax()}) must
#'   have exactly the same number of columns as the previous data object in the
#'   table.
#' @rdname proxy
#' @export
reloadData = function(
  proxy, resetPaging = TRUE, clearSelection = c('all', 'none', 'row', 'column', 'cell')
) {
  if ('all' %in% clearSelection) clearSelection = c('row', 'column', 'cell')
  invokeRemote(proxy, 'reloadData', list(resetPaging, clearSelection))
}

#' Replace data in an existing table
#'
#' Replace the data object of a table output and avoid regenerating the full
#' table, in which case the state of the current table will be preserved
#' (sorting, filtering, and pagination) and applied to the table with new data.
#' @param proxy a proxy object created by \code{dataTableProxy()}
#' @param data the new data object to be loaded in the table
#' @param ... other arguments to be passed to \code{\link{dataTableAjax}()}
#' @param resetPaging,clearSelection passed to \code{\link{reloadData}()}
#' @note When you replace the data in an existing table, please make sure the
#'   new data has the same number of columns as the current data. When you have
#'   enabled column filters, you should also make sure the attributes of every
#'   column remain the same, e.g. factor columns should have the same or fewer
#'   levels, and numeric columns should have the same or smaller range,
#'   otherwise the filters may never be able to reach certain rows in the data,
#'   unless you explicitly update the filters with \code{updateFilters()}.
#' @export
replaceData = function(proxy, data, ..., resetPaging = TRUE, clearSelection = 'all') {
  dataTableAjax(proxy$session, data, ..., outputId = proxy$rawId)
  reloadData(proxy, resetPaging, clearSelection)
}

#' @rdname replaceData
#' @export
updateFilters = function(proxy, data) {
  # calculate the values to be supplied to the filters based on column type
  new_lims = lapply(data, function(x) {
    if (inherits(x, c('numeric'))) {
      range(x)
    } else if (is.logical(x)) {
      c("true", "false", if (anyNA(x)) "na")
    } else if (is.factor(x)) {
      levels(x)
    } else if (inherits(x, c('Date'))) {
      as.numeric(as.POSIXct.Date(range(x))) * 1000
    } else if (inherits(x, 'POSIXt')) {
      round(as.numeric(range(x)), digits = 2) * 1000
    } else {
      stop('updateFilters() requires all columns to be one of the following classes: numeric, factor, logical, Date, POSIXt')
    }
  })

  # make sure JS gets an array, not an object
  new_lims = unname(new_lims)

  # ensure limits are always passed as JS arrays, not scalars
  new_lims = lapply(new_lims, as.list)

  # Trigger the JavaScript to update the filters
  invokeRemote(proxy, 'updateFilters', list(new_lims))
}

invokeRemote = function(proxy, method, args = list()) {
  if (!inherits(proxy, 'dataTableProxy'))
    stop('Invalid proxy argument; table proxy object was expected')

  msg = list(id = proxy$id, call = list(method = method, args = args))

  sess = proxy$session
  if (proxy$deferUntilFlush) {
    sess$onFlushed(function() {
      sess$sendCustomMessage('datatable-calls', msg)
    }, once = TRUE)
  } else {
    sess$sendCustomMessage('datatable-calls', msg)
  }
  proxy
}

shinyFun = function(name) getFromNamespace(name, 'shiny')

# Works around the fact that session$getCurrentOutputInfo() in Shiny through
# version 1.4 signals an error if there is no active output (the private field
# ShinySession$currentOutputName is NULL). Consider removing in the future
# sometime after https://github.com/rstudio/shiny/pull/2707 is released.
getCurrentOutputName = function(session) {
  tryCatch(session$getCurrentOutputInfo()[["name"]], error = function(e) NULL)
}

#' Register a data object in a shiny session for DataTables
#'
#' This function stores a data object in a shiny session and returns a URL that
#' returns JSON data based on DataTables Ajax requests. The URL can be used as
#' the \code{url} option inside the \code{ajax} option of the table. It is
#' basically an implementation of server-side processing of DataTables in R.
#' Filtering, sorting, and pagination are processed through R instead of
#' JavaScript (client-side processing).
#'
#' Normally you should not need to call this function directly. It is called
#' internally when a table widget is rendered in a Shiny app to configure the
#' table option \code{ajax} automatically. If you are familiar with
#' \pkg{DataTables}' server-side processing, and want to use a custom filter
#' function, you may call this function to get an Ajax URL.
#' @param session the \code{session} object in the shiny server function
#'   (\code{function(input, output, session)})
#' @param data a data object (will be coerced to a data frame internally)
#' @param rownames see \code{\link{datatable}()}; it must be consistent with
#'   what you use in \code{datatable()}, e.g. if the widget is generated by
#'   \code{datatable(rownames = FALSE)}, you must also use
#'   \code{dataTableAjax(rownames = FALSE)} here
#' @param filter (for expert use only) a function with two arguments \code{data}
#'   and \code{params} (Ajax parameters, a list of the form \code{list(search =
#'   list(value = 'FOO', regex = 'false'), length = 10, ...)}) that return the
#'   filtered table result according to the DataTables Ajax request
#' @param outputId the output ID of the table (the same ID passed to
#'   \code{dataTableOutput()}; if missing, an attempt to infer it from
#'   \code{session} is made. If it can't be inferred, a random id is
#'   generated.)
#' @references \url{https://rstudio.github.io/DT/server.html}
#' @return A character string (an Ajax URL that can be queried by DataTables).
#' @example inst/examples/ajax-shiny.R
#' @export
dataTableAjax = function(session, data, rownames, filter = dataTablesFilter, outputId) {

  oop = options(stringsAsFactors = FALSE); on.exit(options(oop), add = TRUE)

  if (missing(outputId)) outputId = getCurrentOutputName(session)
  # abuse tempfile() to obtain a random id unique to this R session
  if (is.null(outputId)) outputId = basename(tempfile(''))

  # deal with row names: rownames = TRUE or missing, use rownames(data)
  rn = if (missing(rownames) || isTRUE(rownames)) base::rownames(data) else {
    if (is.character(rownames)) rownames  # use custom row names
  }
  data = as.data.frame(data)  # think dplyr
  if (length(rn)) data = cbind(' ' = rn, data)

  sessionDataURL(session, data, outputId, filter)
}

sessionDataURL = function(session, data, id, filter) {

  toJSON = shinyFun('toJSON')
  httpResponse = shinyFun('httpResponse')

  filterFun = function(data, req) {
    # DataTables requests were sent via POST
    params = rawToChar(req$rook.input$read())
    # I don't think the browser would send out nonASCII strings, but keep it as it is
    Encoding(params) = 'UTF-8'
    # shiny::parseQueryString() calls httpuv::decodeURIComponent() internally and will handle encoding correctly
    params = shiny::parseQueryString(params, nested = TRUE)

    res = tryCatch(filter(data, params), error = function(e) {
      list(error = as.character(e))
    })

    jsonArgs = c(list(x = res, dataframe = 'rows'),
                 getOption('DT.TOJSON_ARGS', getOption('htmlwidgets.TOJSON_ARGS')))
    httpResponse(200, 'application/json', enc2utf8(do.call(toJSON, jsonArgs)))
  }

  session$registerDataObj(id, data, filterFun)
}

# filter a data frame according to the DataTables request parameters
dataTablesFilter = function(data, params) {
  n = nrow(data)
  q = params
  ci = q$search[['caseInsensitive']] == 'true'
  # users may be updating the table too frequently
  if (length(q$columns) != ncol(data)) return(list(
    draw = as.integer(q$draw),
    recordsTotal = n,
    recordsFiltered = 0,
    data = list(),
    DT_rows_all = seq_len(n),
    DT_rows_current = list()
  ))

  # global searching
  # for some reason, q$search might be NULL, leading to error `if (logical(0))`
  if (length(v <- q$search[['value']]) > 0) {
    if (!identical(q$search[['smart']], 'false')) {
      v = unlist(strsplit(gsub('^\\s+|\\s+$', '', v), '\\s+'))
    }
  }
  if (length(v) == 0) v = ''
  m = if ((nv <- length(v)) > 1) array(FALSE, c(dim(data), nv)) else logical(n)
  # TODO: this searching method may not be efficient and need optimization
  i = if (!identical(v, '')) {
    for (j in seq_len(ncol(data))) {
      if (q$columns[[j]][['searchable']] != 'true') next
      for (k in seq_len(nv)) {
        i0 = grep2(
          v[k], as.character(data[, j]), fixed = q$search[['regex']] == 'false',
          ignore.case = ci
        )
        if (nv > 1) m[i0, j, k] = TRUE else m[i0] = TRUE
      }
    }
    which(if (nv > 1) apply(m, 1, function(z) all(colSums(z) > 0)) else m)
  } else seq_len(n)

  # search by columns
  if (length(i)) for (j in names(q$columns)) {
    col = q$columns[[j]]
    # if the j-th column is not searchable or the search string is "", skip it
    if (col[['searchable']] != 'true') next
    if ((k <- col[['search']][['value']]) == '') next
    j = as.integer(j)
    dj = data[, j + 1]
    ij = if (is.numeric(dj) || is.Date(dj)) {
      which(filterRange(dj, k))
    } else if (is.factor(dj)) {
      which(dj %in% fromJSON(k))
    } else if (is.logical(dj)) {
      which(dj %in% as.logical(fromJSON(k)))
    } else {
      grep2(k, as.character(dj), fixed = col[['search']][['regex']] == 'false',
            ignore.case = ci)
    }
    i = intersect(ij, i)
    if (length(i) == 0) break
  }
  if (length(i) != n) data = data[i, , drop = FALSE]
  iAll = i  # row indices of filtered data

  # sorting
  oList = list()
  for (ord in q$order) {
    k = ord[['column']]  # which column to sort
    d = ord[['dir']]     # direction asc/desc
    if (q$columns[[k]][['orderable']] != 'true') next
    col = data[, as.integer(k) + 1]
    oList[[length(oList) + 1]] = (if (d == 'asc') identity else `-`)(
      if (is.numeric(col)) col else xtfrm(col)
    )
  }
  if (length(oList)) {
    i = do.call(order, oList)
    data = data[i, , drop = FALSE]
    iAll = iAll[i]
  }
  # paging
  if (q$length != '-1') {
    len = as.integer(q$length)
    # I don't know why this can happen, but see https://github.com/rstudio/DT/issues/164
    if (is.na(len)) {
      warning("The DataTables parameter 'length' is '", q$length, "' (invalid).")
      len = 0
    }
    i = seq(as.integer(q$start) + 1L, length.out = len)
    i = i[i <= nrow(data)]
    fdata = data[i, , drop = FALSE]  # filtered data
    iCurrent = iAll[i]
  } else {
    fdata = data
    iCurrent = iAll
  }

  if (q$escape != 'false') {
    k = seq_len(ncol(fdata))
    if (q$escape != 'true') {
      # q$escape might be negative indices, e.g. c(-1, -5)
      k = k[as.integer(strsplit(q$escape, ',')[[1]])]
    }
    for (j in k) if (maybe_character(fdata[, j])) fdata[, j] = htmlEscape(fdata[, j])
  }

  # TODO: if iAll is just 1:n, is it necessary to pass this vector to JSON, then
  # to R? When n is large, it may not be very efficient
  list(
    draw = as.integer(q$draw),
    recordsTotal = n,
    recordsFiltered = nrow(data),
    data = cleanDataFrame(fdata),
    DT_rows_all = iAll,
    DT_rows_current = iCurrent
  )
}

# when both ignore.case and fixed are TRUE, we use grep(ignore.case = FALSE,
# fixed = TRUE) to do lower-case matching of pattern on x; assume value = FALSE
grep2 = function(pattern, x, ignore.case = FALSE, fixed = FALSE, ...) {
  if (fixed && ignore.case) {
    pattern = tolower(pattern)
    x = tolower(x)
    ignore.case = FALSE
  }
  # when the user types in the search box, the regular expression may not be
  # complete before it is sent to the server, in which case we do not search
  if (!fixed && inherits(try(grep(pattern, '', perl = TRUE), silent = TRUE), 'try-error'))
    return(seq_along(x))
  # #749 if both fixed and perl are TRUE, the latter will be ignored by R with
  # an annoyed warning
  grep(pattern, x, ignore.case = ignore.case, fixed = fixed, perl = !fixed, ...)
}

# filter a numeric/date/time vector using the search string "lower ... upper"
filterRange = function(d, string) {
  if (!grepl('[.]{3}', string) || length(r <- strsplit(string, '[.]{3}')[[1]]) > 2)
    stop('The range of a numeric / date / time column must be of length 2')
  if (length(r) == 1) r = c(r, '')  # lower,
  r = gsub('^\\s+|\\s+$', '', r)
  r1 = r[1]; r2 = r[2]
  if (is.numeric(d)) {
    r1 = as.numeric(r1); r2 = as.numeric(r2)
  } else if (inherits(d, 'Date')) {
    if (r1 != '') r1 = as.Date(r1)
    if (r2 != '') r2 = as.Date(r2)
  } else {
    if (r1 != '') r1 = as.POSIXct(r1, tz = 'GMT', '%Y-%m-%dT%H:%M:%S')
    if (r2 != '') r2 = as.POSIXct(r2, tz = 'GMT', '%Y-%m-%dT%H:%M:%S')
  }
  if (r[1] == '') return(d <= r2)
  if (r[2] == '') return(d >= r1)
  d >= r1 & d <= r2
}

# treat factors as characters
maybe_character = function(x) {
  is.character(x) || is.factor(x)
}

# make sure we have a tidy data frame (no unusual structures in it)
cleanDataFrame = function(x) {
  x = unname(x)  # remove column names
  if (!is.data.frame(x)) return(x)
  for (j in seq_len(ncol(x))) {
    xj = x[, j]
    xj = unname(xj)  # remove names
    dim(xj) = NULL  # drop dimensions
    if (is.table(xj)) xj = c(xj)  # drop the table class
    x[[j]] = xj
  }
  unname(x)
}

fixServerOptions = function(options) {
  options$serverSide = TRUE
  if (is.null(options$processing)) options$processing = TRUE

  # if you generated the Ajax URL from dataTableAjax(), I'll configure type:
  # 'POST' and a few other options automatically
  if (!inShiny()) return(options)
  if (length(grep('^session/[a-z0-9]+/dataobj/', options$ajax$url)) == 0)
    return(options)

  if (is.null(options$ajax$type)) options$ajax$type = 'POST'
  if (is.null(options$ajax$data)) options$ajax$data = JS(
    'function(d) {',
    sprintf(
      'd.search.caseInsensitive = %s;',
      tolower(!isFALSE(options[['search']]$caseInsensitive))
    ),
    sprintf(
      'd.search.smart = %s;',
      tolower(!isFALSE(options[['search']]$smart))
    ),
    sprintf('d.escape = %s;', attr(options, 'escapeIdx', exact = TRUE)),
    'var encodeAmp = function(x) { x.value = x.value.replace(/&/g, "%26"); }',
    'encodeAmp(d.search);',
    '$.each(d.columns, function(i, v) {encodeAmp(v.search);});',
    '}'
  )
  options
}
