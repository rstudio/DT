% Please edit documentation in R/shiny.R
\name{replaceData}
\alias{replaceData}
\title{Replace data in an existing table}
\usage{
replaceData(proxy, data, ..., resetPaging = TRUE, clearSelection = "all")
}
\arguments{
\item{proxy}{a proxy object created by \code{dataTableProxy()}}

\item{data}{the new data object to be loaded in the table}

\item{...}{other arguments to be passed to \code{\link{dataTableAjax}()}}

\item{resetPaging, clearSelection}{passed to \code{\link{reloadData}()}}
}
\description{
Replace the data object of a table output and avoid regenerating the full
table, in which case the state of the current table will be preserved
(sorting, filtering, and pagination) and applied to the table with new data.
}
\note{
When you replace the data in an existing table, please make sure the
  new data has the same number of columns as the current data. When you have
  enabled column filters, you should also make sure the attributes of every
  column remain the same, e.g. factor columns should have the same or fewer
  levels, and numeric columns should have the same or smaller range,
  otherwise the filters may never be able to reach certain rows in the data.
}
