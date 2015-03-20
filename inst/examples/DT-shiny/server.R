library(shiny)
library(DT)

shinyServer(function(input, output, session) {
  action = dataTableAjax(session, iris)
  widget = datatable(iris, server = TRUE, options = list(
    ajax = list(url = action)
  ))
  output$tbl = DT::renderDataTable(widget)
})
