checkShinyVersion = function() {
  if (packageVersion('shiny') < '0.12.0') stop(
    'DT requires shiny >= 0.12.0. ',
    'Please install the latest version of shiny from CRAN: ',
    'update.packages(ask = FALSE)'
  )
}

#' Helper functions for using DT in Shiny
#'
#' These two functions are like most \code{fooOutput()} and \code{renderFoo()}
#' functions in the \pkg{shiny} package. The former is used to create a
#' container for table, and the latter is used in the server logic to render the
#' table.
#' @inheritParams shiny::dataTableOutput
#' @param width the width of the table container
#' @param height the height of the table container
#' @references \url{http://rstudio.github.io/DT/shiny.html}
#' @export
#' @examples # !formatR
#' if (interactive()) {
#'   library(shiny)
#'   shinyApp(
#'     ui = fluidPage(fluidRow(column(12, DT::dataTableOutput('tbl')))),
#'     server = function(input, output) {
#'       output$tbl = DT::renderDataTable(
#'         iris, options = list(lengthChange = FALSE)
#'       )
#'     }
#'   )
#' }
dataTableOutput = function(outputId, width = '100%', height = 'auto') {
  checkShinyVersion()
  htmlwidgets::shinyWidgetOutput(
    outputId, 'datatables', width, height, package = 'DT'
  )
}

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
#'   browsers to slow down or crash.
#' @param ... ignored when \code{expr} returns a table widget, and passed as
#'   additional arguments to \code{datatable()} when \code{expr} returns a data
#'   object
renderDataTable = function(expr, server = TRUE, env = parent.frame(), quoted = FALSE, ...) {
  checkShinyVersion()
  if (!quoted) expr = substitute(expr)

  # TODO: this can be simplified after this htmlwidgets PR is merged
  # https://github.com/ramnathv/htmlwidgets/pull/122
  currentSession = NULL
  currentOutputName = NULL

  exprFunc = shiny::exprToFunction(expr, env, quoted = TRUE)
  widgetFunc = function() {
    instance = exprFunc()
    if (!all(c('datatables', 'htmlwidget') %in% class(instance))) {
      instance = datatable(instance, ...)
    } else {
      if (length(list(...)) != 0) {
        warning("renderDataTable ignores ... arguments when expr yields a datatable object; see ?renderDataTable")
      }
    }

    # in the server mode, we should not store the full data in JSON
    if (server && !is.null(instance[['x']])) {
      origData = instance[['x']][['data']]
      instance$x$data = NULL

      # register the data object in a shiny session
      options = instance[['x']][['options']]

      # Normalize "ajax" argument; if we leave it a string then we have several
      # code paths that need to account for both string and list representations
      if (is.character(options[['ajax']])) {
        options$ajax = list(url = options$ajax)
      }

      if (is.null(options[['ajax']][['url']])) {
        url = sessionDataURL(currentSession, origData, currentOutputName, dataTablesFilter)
        options$ajax$url = url
      }
      instance$x$options = fixServerOptions(options)
    }

    instance
  }

  renderFunc = htmlwidgets::shinyRenderWidget(
    widgetFunc(), dataTableOutput, environment(), FALSE
  )

  shiny::markRenderFunction(dataTableOutput, function(shinysession, name, ...) {
    currentSession <<- shinysession
    currentOutputName <<- name
    on.exit({
      currentSession <<- NULL
      currentOutputName <<- NULL
    }, add = TRUE)

    renderFunc()
  })
}

shinyFun = function(name) getFromNamespace(name, 'shiny')

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
#' @references \url{http://rstudio.github.io/DT/server.html}
#' @return A character string (an Ajax URL that can be queried by DataTables).
#' @example inst/examples/ajax-shiny.R
#' @export
dataTableAjax = function(session, data, rownames, filter = dataTablesFilter) {

  oop = options(stringsAsFactors = FALSE); on.exit(options(oop), add = TRUE)

  # abuse tempfile() to obtain a random id unique to this R session
  id = basename(tempfile(''))

  # deal with row names: rownames = TRUE or missing, use rownames(data)
  rn = if (missing(rownames) || isTRUE(rownames)) base::rownames(data) else {
    if (is.character(rownames)) rownames  # use custom row names
  }
  data = as.data.frame(data)  # think dplyr
  if (length(rn)) data = cbind(' ' = rn, data)

  sessionDataURL(session, data, id, filter)
}

sessionDataURL = function(session, data, id, filter) {

  URLdecode = shinyFun('URLdecode')
  toJSON = shinyFun('toJSON')
  httpResponse = shinyFun('httpResponse')

  filterFun = function(data, req) {
    # DataTables requests were sent via POST
    params = URLdecode(rawToChar(req$rook.input$read()))
    Encoding(params) = 'UTF-8'
    # use system native encoding if possible (again, this grep(fixed = TRUE) bug
    # https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16264)
    params2 = iconv(params, 'UTF-8', '')
    if (!is.na(params2)) params = params2 else warning(
      'Some DataTables parameters contain multibyte characters ',
      'that do not work in current locale.'
    )
    params = shiny::parseQueryString(params, nested = TRUE)

    res = tryCatch(filter(data, params), error = function(e) {
      list(error = as.character(e))
    })
    httpResponse(200, 'application/json', enc2utf8(toJSON(res)))
  }

  session$registerDataObj(id, data, filterFun)
}

# filter a data frame according to the DataTables request parameters
dataTablesFilter = function(data, params) {
  n = nrow(data)
  q = params
  ci = q$search[['caseInsensitive']] == 'true'

  # global searching
  i = seq_len(n)
  # for some reason, q$search might be NULL, leading to error `if (logical(0))`
  if (isTRUE(q$search[['value']] != '')) {
    i0 = apply(data, 2, function(x) {
      grep2(q$search[['value']], as.character(x),
            fixed = q$search[['regex']] == 'false', ignore.case = ci)
    })
    i = intersect(i, unique(unlist(i0)))
  }

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
      which(dj %in% jsonlite::fromJSON(k))
    } else if (is.logical(dj)) {
      which(dj %in% as.logical(jsonlite::fromJSON(k)))
    } else {
      grep2(k, as.character(dj), fixed = col[['search']][['regex']] == 'false',
            ignore.case = ci)
    }
    i = intersect(ij, i)
    if (length(i) == 0) break
  }
  if (length(i) != n) data = data[i, , drop = FALSE]

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
  }
  # paging
  if (q$length != '-1') {
    i = seq(as.integer(q$start) + 1L, length.out = as.integer(q$length))
    i = i[i <= nrow(data)]
    fdata = data[i, , drop = FALSE]  # filtered data
  } else fdata = data

  fdata = unname(as.matrix(fdata))
  if (is.character(fdata) && q$escape != 'false') {
    if (q$escape == 'true') fdata = htmlEscape(fdata) else {
      k = as.integer(strsplit(q$escape, ',')[[1]])
      # use seq_len() in case escape = negative indices, e.g. c(-1, -5)
      for (j in seq_len(ncol(fdata))[k]) fdata[, j] = htmlEscape(fdata[, j])
    }
  }

  list(
    draw = as.integer(q$draw),
    recordsTotal = n,
    recordsFiltered = nrow(data),
    data = fdata
  )
}

# when both ignore.case and fixed are TRUE, we use grep(ignore.case = FALSE,
# fixed = TRUE) to do lower-case matching of pattern on x
grep2 = function(pattern, x, ignore.case = FALSE, fixed = FALSE, ...) {
  if (fixed && ignore.case) {
    pattern = tolower(pattern)
    x = tolower(x)
    ignore.case = FALSE
  }
  # when the user types in the search box, the regular expression may not be
  # complete before it is sent to the server, in which case we do not search
  if (!fixed && inherits(try(grep(pattern, ''), silent = TRUE), 'try-error'))
    return(seq_along(x))
  grep(pattern, x, ignore.case = ignore.case, fixed = fixed, ...)
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
    sprintf('d.escape = %s;', attr(options, 'escapeIdx', exact = TRUE)),
    '}'
  )
  options
}
