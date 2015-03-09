library(shiny)

fluidPage(
  title = 'Using Checkboxes to Select Table Rows',
  h1('Append a column of checkboxes'),
  fluidRow(
    column(6, DT::dataTableOutput('x1')),
    column(6, plotOutput('x2', height = 580))
  ),
  hr(),
  h1('Use checkboxes in place of row names'),
  fluidRow(
    column(9, DT::dataTableOutput('x3')),
    column(3, verbatimTextOutput('x4'))
  )
)
