library(shiny)
library(DT)

shinyServer(function(input, output, session) {

  mtcars2 = data.frame(
    name = rownames(mtcars), mtcars[, c('mpg', 'hp')],
    stringsAsFactors = FALSE
  )
  mtcars2$am = factor(mtcars$am, labels = c('automatic', 'manual'))
  mtcars2$date = Sys.Date() + seq_len(nrow(mtcars))

  action = dataTableAjax(session, mtcars2, rownames = FALSE)

  output$tbl = DT::renderDataTable({
    DT::datatable(
      mtcars2, filter = 'top', server = TRUE, rownames = FALSE,
      options = list(
        autoWidth = TRUE, ajax = list(url = action)
      )
    )
  })
})
