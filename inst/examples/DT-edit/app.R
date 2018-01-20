library(shiny)
library(DT)
shinyApp(
  ui = fluidPage(
    title = 'Double-click to edit table cells',
    fluidRow(column(12, h1('Client-side processing'), hr(), DTOutput('x1'))),
    fluidRow(column(12, h1('Server-side processing'), hr(), DTOutput('x2'))),
    fluidRow(column(12, h1('Server-side processing (no row names)'), hr(), DTOutput('x3')))
  ),
  server = function(input, output, session) {
    d1 = iris
    d1$Date = Sys.time() + seq_len(nrow(d1))
    d2 = d3 = d1

    options(DT.options = list(pageLength = 5))

    output$x1 = renderDT(d1, selection = 'none', server = FALSE, editable = TRUE)
    output$x2 = renderDT(d2, selection = 'none', editable = TRUE)
    output$x3 = renderDT(d3, selection = 'none', rownames = FALSE, editable = TRUE)

    proxy2 = dataTableProxy('x2')

    observeEvent(input$x2_cell_edit, {
      info = input$x2_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      d2[i, j] <<- DT::coerceValue(v, d2[i, j])
      replaceData(proxy2, d2, resetPaging = FALSE)  # important
    })

    proxy3 = dataTableProxy('x3')

    observeEvent(input$x3_cell_edit, {
      info = input$x3_cell_edit
      str(info)
      i = info$row
      j = info$col + 1  # column index offset by 1
      v = info$value
      d3[i, j] <<- DT::coerceValue(v, d3[i, j])
      replaceData(proxy3, d3, resetPaging = FALSE, rownames = FALSE)
    })
  }
)
