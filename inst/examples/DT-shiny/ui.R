library(shiny)
if (packageVersion('shiny') <= '0.10.2.2') stop('This app requires shiny > 0.10.2.2')

shinyUI(fluidPage(
  title = 'Use the DT package in shiny',
  fluidRow(
    column(2),
    column(8, DT::dataTableOutput('tbl')),
    column(2)
  )
))
