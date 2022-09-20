library(shiny)
library(DT)

fluidPage(
  tags$h4(
    "Click the button to delete the selected rows."
  ),
  br(),
  DTOutput("dtable")
)
