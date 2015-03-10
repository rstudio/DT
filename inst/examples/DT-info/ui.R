library(shiny)

fluidPage(
  title = 'DataTables Information',
  fluidRow(
    column(6, DT::dataTableOutput('x1')),
    column(6, plotOutput('x2', height = 500))
  ),
  fluidRow(
    p(class = 'text-center', downloadButton('x3', 'Download Filtered Data'))
  )
)
