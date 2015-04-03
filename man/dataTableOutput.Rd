% Please edit documentation in R/shiny.R
\name{dataTableOutput}
\alias{dataTableOutput}
\alias{renderDataTable}
\title{Helper functions for using DT in Shiny}
\usage{
dataTableOutput(outputId, width = "100\%", height = "auto")

renderDataTable(expr, env = parent.frame(), quoted = FALSE, ...)
}
\arguments{
\item{outputId}{output variable to read the table from}

\item{width}{the width of the table container}

\item{height}{the height of the table container}

\item{expr}{an expression to create a table widget (normally via
\code{\link{datatable}()})}

\item{env}{The environment in which to evaluate \code{expr}.}

\item{quoted}{Is \code{expr} a quoted expression (with \code{quote()})? This
is useful if you want to save an expression in a variable.}

\item{...}{currently ignored, with a warning message}
}
\description{
These two functions are like most \code{fooOutput()} and \code{renderFoo()}
functions in the \pkg{shiny} package. The former is used to create a
container for table, and the latter is used in the server logic to render the
table.
}
\examples{
if (interactive()) {
  library(shiny)
  shinyApp(
    ui = fluidPage(fluidRow(column(12, DT::dataTableOutput('tbl')))),
    server = function(input, output) {
      output$tbl = DT::renderDataTable({
        DT::datatable(iris, options = list(lengthChange = FALSE))
      })
    }
  )
}
}
\references{
\url{http://rstudio.github.io/DT/shiny.html}
}
