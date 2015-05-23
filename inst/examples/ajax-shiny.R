# !formatR
library(DT)
DTApp = function(data, ..., options = list()) {
  library(shiny)
  shinyApp(
    ui = fluidPage(
      title = 'Server-side processing of DataTables',
      fluidRow(
        DT::dataTableOutput('tbl')
      )
    ),
    server = function(input, output, session) {
      options$ajax = list(url = dataTableAjax(session, data))
      # create a widget using an Ajax URL created above
      widget = datatable(data, server = TRUE, ..., options = options)
      output$tbl = DT::renderDataTable(widget)
    }
  )
}

if (interactive()) DTApp(iris)
if (interactive()) DTApp(iris, filter = 'top')
