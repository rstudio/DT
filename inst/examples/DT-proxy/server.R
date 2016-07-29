library(shiny)
library(DT)

shinyServer(function(input, output, session) {

  # using server = FALSE mainly for addRow(); server = TRUE works for
  # selectRows() and selectColumns()
  output$foo = DT::renderDataTable(
    iris, server = FALSE, selection = list(target = 'row+column')
  )

  proxy = dataTableProxy('foo')

  observeEvent(input$select1, {
    proxy %>% selectRows(as.numeric(input$rows))
  })

  observeEvent(input$select2, {
    proxy %>% selectColumns(input$col)
  })

  observeEvent(input$clear1, {
    proxy %>% selectRows(NULL)
  })

  observeEvent(input$clear2, {
    proxy %>% selectColumns(NULL)
  })

  observeEvent(input$add, {
    proxy %>% addRow(iris[sample(nrow(iris), 1), , drop = FALSE])
  })

  output$info = renderPrint({
    list(rows = input$foo_rows_selected, columns = input$foo_columns_selected)
  })

})
