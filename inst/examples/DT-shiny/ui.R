library(shiny)

shinyUI(fluidPage(
  title = 'Use the DT package in shiny',
  fluidRow(
    column(2),
    column(8, DT::dataTableOutput('tbl')),
    column(2)
  )
))
