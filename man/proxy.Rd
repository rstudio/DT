% Please edit documentation in R/shiny.R
\name{dataTableProxy}
\alias{addRow}
\alias{clearSearch}
\alias{dataTableProxy}
\alias{reloadData}
\alias{selectCells}
\alias{selectColumns}
\alias{selectPage}
\alias{selectRows}
\title{Manipulate an existing DataTables instance in a Shiny app}
\usage{
dataTableProxy(outputId, session = shiny::getDefaultReactiveDomain(), 
    deferUntilFlush = TRUE)

selectRows(proxy, selected)

selectColumns(proxy, selected)

selectCells(proxy, selected)

addRow(proxy, data)

clearSearch(proxy)

selectPage(proxy, page)

reloadData(proxy, resetPaging = TRUE)
}
\arguments{
\item{outputId}{the id of the table to be manipulated (the same id as the one
you used in \code{\link{dataTableOutput}()})}

\item{session}{the Shiny session object (from the server function of the
Shiny app)}

\item{deferUntilFlush}{whether an action should be carried out right away, or
should be held until after the next time all of the outputs are updated}

\item{proxy}{a proxy object returned by \code{dataTableProxy()}}

\item{selected}{an integer vector of row/column indices, or a matrix of two
columns (row and column indices, respectively) for cell indices; you may
use \code{NULL} to clear existing selections}

\item{data}{a single row of data to be added to the table; it can be a matrix
or data frame of one row, or a vector or list of row data (in the latter
case, please be cautious about the row name: if your table contains row
names, here \code{data} must also contain the row name as the first
element)}

\item{page}{a number indicating the page to select}

\item{resetPaging}{whether to reset the paging position}
}
\description{
The function \code{datatableProxy()} creates a proxy object that can be used
to manipulate an existing DataTables instance in a Shiny app, e.g. select
rows/columns, or add rows.
}
\note{
\code{addRow()} only works for client-side tables. If you want to use
  it in a Shiny app, make sure to use \code{renderDataTable(..., server =
  FALSE)}.

\code{reloadData()} only works for tables in the server-side processing
  mode, e.g. tables rendered with \code{renderDataTable(server = TRUE)}.
}
\references{
\url{http://rstudio.github.io/DT/shiny.html}
}
