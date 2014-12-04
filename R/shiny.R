#' @export
dataTableOutput = function(outputId, width = '100%', height = 'auto') {
  htmlwidgets::shinyWidgetOutput(
    outputId, 'datatables', width, height, package = 'DT'
  )
}

#' @export
renderDataTable = function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr = substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, dataTableOutput, env, quoted = TRUE)
}
