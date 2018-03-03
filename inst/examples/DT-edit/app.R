library(shiny)
library(DT)
shinyApp(
  ui = fluidPage(
    title = 'Double-click to edit table cells',
    titlePanel('Double-click to edit table cells'),
    DTOutput('x1')
  ),
  server = function(input, output, session) {
    d1 = iris

    output$x1 = renderDT(d1, selection = "none", editable = T, rownames = T, options = list(
      editType = list("0" = "text", "1" = "text", "2" = "text", "3" = "text", "4" = "select"),
      editAttribs = list("0" = list(placeholder = "Length"), "1" = list(placeholder = "Width"),
                         "2" = list(placeholder = "Length"), "3" = list(placeholder = "Width"),
                         "4" = list(options = c("setosa", "versicolor", "virginica")))
    ))

    proxy1 = dataTableProxy('x1')
    
    observeEvent(input$x1_cell_edit, {
      info = input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      d1[i, j] <<- DT::coerceValue(v, d1[i, j])
      replaceData(proxy1, d1, resetPaging = FALSE)  # important
    })
  }
)
