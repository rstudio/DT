globals <- new.env(parent = emptyenv())
globals$sass_vars <- list()

#' Datatable theming variables
#'
#' Use `datatableThemeVariables()` to set Datatable SASS variables to influence their
#' default CSS styling. Use `datatableThemeVariablesClear()` to clear any currently
#' set variables.
#'
#' @param ... Variables to be set, using `name = value`; or, a single unnamed
#'   argument which is a named list of variables to set. To specify a name that
#'   uses characters that aren't valid for R identifiers, wrap that name in
#'   quotes, double quotes, or backticks.
#'
#' @return If no arguments are passed, a list of all set options is returned. If
#'   arguments are passed, a list containing the previous values of the newly
#'   set options is returned invisibly.
#'
#' @md
#' @export
#' @examples
#' library(htmltools)
#' # Color palette inspired by https://bootswatch.com/solar/
#' color <- "#002B36"
#' stroke <- "#839496"
#'
#' # Full list of SASS variables
#' # https://github.com/DataTables/DataTablesSrc/blob/master/css/jquery.dataTables.scss
#' datatableThemeVariables(
#'   "table-header-border" = color,
#'   "table-body-border" = color,
#'   "table-row-background" = color,
#'   "table-control-color" = stroke,
#'   "table-paging-button-active" = stroke,
#'   "table-paging-button-hover" = color,
#'   "table-shade" = stroke
#' )
#' browsable(
#'   tags$body(
#'     style = sprintf("background-color: %s; color: %s", color, stroke),
#'     datatable(mtcars)
#'   )
#' )
#'
datatableThemeVariables <- function(...) {
  args <- rlang::list2(...)
  arg_names <- names(args)

  # If called without args, this is just a read operation.
  if (length(args) == 0) {
    # Getter
    return(globals$sass_vars)
  }

  # If called with a single unnamed argument that's a list, then act
  # like it was a do.call.
  if (length(args) == 1 && is.null(arg_names) && is.list(args[[1]])) {
    args <- args[[1]]
    arg_names <- names(args)
  }

  if (is.null(arg_names) || !isTRUE(all(nzchar(arg_names), na.rm = FALSE))) {
    stop("All arguments to theme_variables() must be named")
  }

  old_opts <- globals$sass_vars

  globals$sass_vars[arg_names] <- args

  invisible(setNames(old_opts[arg_names], arg_names))
}

#' @rdname datatableThemeVariables
#' @export
datatableThemeVariablesClear <- function() {
  old_opts <- globals$sass_vars
  globals$sass_vars <- list()
  invisible(old_opts)
}
