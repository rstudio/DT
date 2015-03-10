library(shiny)

fluidPage(
  title = 'DataTables Information',
  fluidRow(
    column(6, DT::dataTableOutput('x1')),
    column(6, plotOutput('x2', height = 500))
  )
)
