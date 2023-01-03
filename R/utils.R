`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

dropNULL = function(x) {
  if (length(x) == 0 || !is.list(x)) return(x)
  x[!unlist(lapply(x, is.null))]
}

isFALSE = function(x) identical(x, FALSE)

is.Date = function(x) inherits(x, c('Date', 'POSIXlt', 'POSIXct'))

# for CSS propertices: fontWeight -> font-weight, backgroundColor ->
# background-color, etc
upperToDash = function(x) {
  x = gsub('^(.)', '\\L\\1', x, perl = TRUE)
  x = gsub('([A-Z])', '-\\L\\1', x, perl = TRUE)
  x
}

inShiny = function() {
  getOption('DT.datatable.shiny', 'shiny' %in% loadedNamespaces())
}

in_dir = function(dir, expr) {
  owd = setwd(dir); on.exit(setwd(owd))
  expr
}

existing_files = function(x) x[file.exists(x)]

# generate <caption></caption>
captionString = function(caption) {
  if (is.character(caption)) caption = tags$caption(caption)
  caption = as.character(caption)
  if (length(caption)) caption
}

# 'box' list column atomic scalars so that the data are represented
# consistently (as arrays) in javascript, e.g.,
# data.frame(col = I(list(integer(), 1, 2:3))) --> [[], [1], [2, 3]]
# instead of [[], 1, [2, 3]]
boxAtomicScalarElements = function(x) {
  is_atomic = vapply(x, is.atomic, logical(1))
  if (all(is_atomic)) {
    is_scalar = lengths(x) == 1L
    x[is_scalar] = lapply(x[is_scalar], list)
  }
  x
}

boxListColumnAtomicScalars = function(x) {
  is_list = vapply(x, is.list, logical(1))
  x[is_list] = lapply(x[is_list], boxAtomicScalarElements)
  x
}

toJSON = function(...) {
  FUN = getFromNamespace('toJSON', 'htmlwidgets')
  FUN(...)
}

# I don't want txt to be treated as a file path in DT; it is always a string, so
# I use this length() == 2 hack to force jsonlite::fromJSON() to treat it as
# string (the second empty element doesn't matter)
fromJSON = function(txt, ...) {
  jsonlite::fromJSON(c(txt, ''), ...)
}

native_encode = function(x) {
  if (.Platform$OS.type == 'unix') return(x)
  x2 = enc2native(x)
  if (identical(enc2utf8(x2), x)) x2 else x
}

classes = function(x) paste(class(x), collapse = ', ')

#' Coerce a character string to the same type as a target value
#'
#' Create a new value from a character string based on an old value, e.g., if
#' the old value is an integer, call \code{as.integer()} to coerce the string to
#' an integer.
#'
#' This function only works with integer, double, date, time (\code{POSIXlt} or
#' \code{POSIXct}), and factor values. The date must be of the format
#' \code{\%Y-\%m-\%dT\%H:\%M:\%SZ}. The factor value must be in the levels of
#' \code{old}, otherwise it will be coerced to \code{NA}.
#' @param val A character string.
#' @param old An old value, whose type is the target type of \code{val}.
#' @export
#' @return A value of the same data type as \code{old} if possible.
#' @examples library(DT)
#' coerceValue('100', 1L)
#' coerceValue('1.23', 3.1416)
#' coerceValue('2018-02-14', Sys.Date())
#' coerceValue('2018-02-14T22:18:52Z', Sys.time())
#' coerceValue('setosa', iris$Species)
#' coerceValue('setosa2', iris$Species)  # NA
#' coerceValue('FALSE', TRUE)  # not supported
coerceValue = function(val, old) {
  if (is.integer(old)) return(as.integer(val))
  if (is.numeric(old)) return(as.numeric(val))
  if (is.character(old)) return(as.character(val))
  if (inherits(old, 'Date')) return(as.Date(val))
  if (inherits(old, c('POSIXlt', 'POSIXct'))) {
    val = strptime(val, '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC')
    if (inherits(old, 'POSIXlt')) return(val)
    return(as.POSIXct(val))
  }
  if (is.factor(old)) {
    i = val %in% levels(old)
    if (all(i)) return(val)
    warning(
      'New value(s) "', paste(val[!i], collapse = ', '),
      '" not in the original factor levels: "',
      paste(levels(old), collapse = ', '), '"; will be coerced to NA.'
    )
    val[!i] = NA
    return(val)
  }
  warning('The data type is not supported: ', classes(old))
  val
}

#' Edit a data object using the information from the editor in a DataTable
#'
#' When editing cells in a DataTable in a Shiny app, we know the row/column
#' indices and values of the cells that were edited. With these information, we
#' can update the data object behind the DataTable accordingly.
#' @param data The original data object used in the DataTable.
#' @param info The information about the edited cells. It should be obtained
#'   from \code{input$tableId_cell_edit} from Shiny, and is a data frame
#'   containing columns \code{row}, \code{col}, and \code{value}.
#' @param rownames Whether row names are displayed in the table.
#' @param proxy,resetPaging,... (Optional) If \code{proxy} is provided, it must
#'   be either a character string of the output ID of the table or a proxy
#'   object created from \code{\link{dataTableProxy}()}, and the rest of
#'   arguments are passed to \code{\link{replaceData}()} to update the data in a
#'   DataTable instance in a Shiny app.
#' @note For factor columns, new levels would be automatically added when necessary
#'   to avoid \code{NA} coercing.
#' @return The updated data object.
#' @export
editData = function(data, info, proxy = NULL, rownames = TRUE, resetPaging = FALSE, ...) {
  for (r in split(info, info$col)) {
    i = r$row; j = r$col + !rownames; v = r$value
    j = j[1]
    # the 0-th column is the row names in this case
    if (j == 0) {
      rownames(data)[i] = v
    } else {
      # allow add new factor levels
      if (is.factor(data[[j]]) && !all(v %in% levels(data[[j]]))) {
        levels(data[[j]]) <- unique(c(levels(data[[j]]), v))
      }
      data[i, j] = coerceValue(v, data[i, j, drop = TRUE])
    }
  }
  if (is.character(proxy)) proxy = dataTableProxy(proxy)
  if (inherits(proxy, 'dataTableProxy')) {
    replaceData(proxy, data, resetPaging = resetPaging, rownames = rownames, ...)
  }
  data
}
