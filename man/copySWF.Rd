% Please edit documentation in R/datatables.R
\name{copySWF}
\alias{copySWF}
\title{Copy the Flash SWF file from the TableTools extension}
\usage{
copySWF(dest = ".", pdf = FALSE)
}
\arguments{
\item{dest}{the destination directory}

\item{pdf}{\code{TRUE} if you want to save the table as PDF
(\file{copy_csv_xls_pdf.swf} will be copied); \code{FALSE} otherwise (use
\file{copy_csv_xls.swf})}
}
\value{
A character string of the path of the SWF file, which may be used as
  the \code{sSwfPath} option for TableTools.
}
\description{
This is a convenience function to copy the SWF file since the TableTools
extension depends on it.
}
\references{
\url{http://datatables.net/extensions/tabletools}
}
