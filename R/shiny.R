#' Helper functions for using DT in Shiny
#'
#' These two functions are like most \code{fooOutput()} and \code{renderFoo()}
#' functions in the \pkg{shiny} package. The former is used to create a
#' container for table, and the latter is used in the server logic to render the
#' table.
#' @inheritParams shiny::dataTableOutput
#' @param width the width of the table container
#' @param height the height of the table container
#' @export
#' @examples # !formatR
#' \donttest{library(shiny)
#' shinyApp(
#'   ui = fluidPage(fluidRow(column(12, DT::dataTableOutput('tbl')))),
#'   server = function(input, output) {
#'     output$tbl = DT::renderDataTable(DT::datatable(iris))
#'   }
#' )
#' }
dataTableOutput = function(outputId, width = '100%', height = 'auto') {
  htmlwidgets::shinyWidgetOutput(
    outputId, 'datatables', width, height, package = 'DT'
  )
}

#' @export
#' @rdname dataTableOutput
#' @inheritParams shiny::renderDataTable
#' @param expr an expression to create a table widget
#' @param ... currently ignored, with a warning message
renderDataTable = function(expr, env = parent.frame(), quoted = FALSE, ...) {
  if (length(list(...))) warning(
    "Arguments in addition to 'expr', 'env', and 'quoted' are ignored. ",
    "If you came from shiny::renderDataTable(), you may want to pass ",
    "these arguments to DT::datatable() instead."
  )
  if (!quoted) expr = substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, dataTableOutput, env, quoted = TRUE)
}

#' @export
appendCheckboxes = function(data, after = TRUE) {
  check = checkboxRows(data)
  if (after) cbind(data, ' ' = check) else cbind(' ' = check, data)
}

#' @export
checkboxRows = function(data) {
  sprintf(
    '<input data-row="%s" type="checkbox" class="DT checkboxRows" />',
    seq_len(nrow(data))
  )
}
