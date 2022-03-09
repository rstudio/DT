library(shiny)
library(DT)

tbl <- data.frame(
  int = -2:2,
  num1 = replace(seq(-2, 2) * 10, 1, Inf),
  num2 = seq(-2, 2) / 1,
  num3 = seq(-2, 2) / 10,
  date = Sys.Date() + seq(-2, 2),
  dttm = round(Sys.time()) + seq(-2, 2) * 3600,
  fct1 = factor(c("A", rep("B", 4)), levels = c("A", "B", "C")),
  fct2 = factor(c(rep("A", 4), "B"), levels = c("A", "B", "C")),
  bool = c(NA, TRUE, TRUE, FALSE, FALSE),
  chr = c("foo", "bar", "baz", "baz", "baz")
)

ui <- fluidPage(
  sliderInput("rows", "Slice rows", 0, 5, value = c(1, 4), step = 1),
  DTOutput("table_sliced"),
  tags$hr(),
  DTOutput("table_original"),
)

server <- function(input, output, session) {
  simple_dt <- function(data, caption) {
    datatable(data, caption = caption, filter = "top", options = list(dom = "t"))
  }

  # Render tables only once to begin with
  output$table_sliced <- renderDT(simple_dt(tbl, caption = "Sliced data"))
  output$table_original <- renderDT(simple_dt(tbl, caption = "Original data"))

  # Update table_sliced based on input slider row selection
  tbl_slice <- eventReactive(input$rows, {
    r1 <- input$rows[1]
    r2 <- input$rows[2]
    tbl[seq(r1, r2), ]
  })

  proxy <- dataTableProxy("table_sliced")
  observeEvent(tbl_slice(), {
    replaceData(proxy, tbl_slice())
    updateFilters(proxy, tbl_slice())
  })
}

shinyApp(ui, server)
