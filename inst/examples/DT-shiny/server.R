library(shiny)
library(DT)

shinyServer(function(input, output, session) {
  output$tbl_a = DT::renderDataTable(iris)
  output$tbl_b = DT::renderDataTable(iris, server = TRUE)
})
