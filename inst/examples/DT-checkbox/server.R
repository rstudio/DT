library(shiny)
library(DT)

shinyServer(function(input, output) {
  output$x1 = DT::renderDataTable({
    datatable(
      appendCheckboxes(cars),
      options = list(
        columnDefs = list(list(orderable = FALSE, targets = 3)),
        autoWidth = FALSE
      ),
      escape = -4
    )
  })
  output$x2 = renderPlot({
    s = input$x1_selected
    par(mar = c(4, 4, 1, .1))
    plot(cars)
    if (length(s)) points(cars[s, , drop = FALSE], pch = 19, cex = 2)
  })
  output$x3 = DT::renderDataTable({
    datatable(
      iris,
      rownames = checkboxRows(iris),
      escape = -1
    )
  })
  output$x4 = renderPrint({
    s = input$x3_selected
    if (length(s)) {
      cat('These rows were selected:\n\n')
      cat(s, sep = ' ')
    }
  })
})
