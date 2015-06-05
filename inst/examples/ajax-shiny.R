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
      output$tbl = DT::renderDataTable({
        datatable(data, ..., options = options)
      }, server = TRUE)
    }
  )
}

if (interactive()) DTApp(iris)
if (interactive()) DTApp(iris, filter = 'top')
