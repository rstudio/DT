% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny.R
\name{dataTableOutput}
\alias{dataTableOutput}
\alias{DTOutput}
\alias{renderDataTable}
\alias{renderDT}
\title{Helper functions for using DT in Shiny}
\usage{
dataTableOutput(outputId, width = "100\%", height = "auto", fill = TRUE)

DTOutput(outputId, width = "100\%", height = "auto", fill = TRUE)

renderDataTable(
  expr,
  server = TRUE,
  env = parent.frame(),
  quoted = FALSE,
  funcFilter = dataTablesFilter,
  future = FALSE,
  outputArgs = list(),
  ...
)

renderDT(
  expr,
  server = TRUE,
  env = parent.frame(),
  quoted = FALSE,
  funcFilter = dataTablesFilter,
  future = FALSE,
  outputArgs = list(),
  ...
)
}
\arguments{
\item{outputId}{output variable to read the table from}

\item{width}{the width of the table container}

\item{height}{the height of the table container}

\item{fill}{passed to \code{htmlwidgets::\link{shinyWidgetOutput}()}, see
there for explanation (requires \pkg{htmlwidgets} > v1.5.4).}

\item{expr}{an expression to create a table widget (normally via
\code{\link{datatable}()}), or a data object to be passed to
\code{datatable()} to create a table widget}

\item{server}{whether to use server-side processing. If \code{TRUE}, then the
data is kept on the server and the browser requests a page at a time; if
\code{FALSE}, then the entire data frame is sent to the browser at once.
Highly recommended for medium to large data frames, which can cause
browsers to slow down or crash. Note that if you want to use
\code{renderDataTable} with \code{shiny::bindCache()}, this must be
\code{FALSE}.}

\item{env}{The parent environment for the reactive expression. By default,
this is the calling environment, the same as when defining an ordinary
non-reactive expression. If \code{expr} is a quosure and \code{quoted} is
\code{TRUE}, then \code{env} is ignored.}

\item{quoted}{If it is \code{TRUE}, then the \code{\link{quote}()}ed value of
\code{expr} will be used when \code{expr} is evaluated. If \code{expr} is a
quosure and you would like to use its expression as a value for
\code{expr}, then you must set \code{quoted} to \code{TRUE}.}

\item{funcFilter}{(for expert use only) passed to the \code{filter} argument
of \code{\link{dataTableAjax}()}}

\item{future}{whether the server-side filter function should be executed as a
future or as a standard synchronous function. If true, the future will be
evaluated according to the session's \link[future]{plan}.}

\item{outputArgs}{A list of arguments to be passed through to the implicit
call to \code{\link{dataTableOutput}()} when
\code{\link{renderDataTable}()} is used in an interactive R Markdown
document.}

\item{...}{ignored when \code{expr} returns a table widget, and passed as
additional arguments to \code{\link{datatable}()} when \code{expr} returns
a data object}
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
  library(DT)
  shinyApp(
    ui = fluidPage(fluidRow(column(12, DTOutput('tbl')))),
    server = function(input, output) {
      output$tbl = renderDT(
        iris, options = list(lengthChange = FALSE)
      )
    }
  )
}
}
\references{
\url{https://rstudio.github.io/DT/shiny.html}
}
