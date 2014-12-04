library(shiny)
library(DT)

shinyServer(function(input, output, session) {
  action = session$registerDataObj('iris', iris, shiny:::dataTablesJSON)
  widget = datatable(iris, server = TRUE, options = list(
    processing = TRUE, ajax = list(
      url = action, type = 'POST', data = JS(
        'function(d) {',
        'd.search.caseInsensitive = false;',
        'd.escape = true;',
        '}'
      )
    )
  ))
  output$tbl = DT::renderDataTable(widget)
})
