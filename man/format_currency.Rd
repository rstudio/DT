\name{format_currency}
\alias{format_currency}
\alias{format_percentage}
\alias{format_round}
\title{Format table columns}
\usage{
format_currency(table, columns, currency = "$", interval = 3, mark = ",")

format_percentage(table, columns, digits = 0)

format_round(table, columns, digits = 2)
}
\arguments{
\item{table}{a table object created from \code{\link{datatable}()}}

\item{columns}{the indices of the columns to be formatted (can be character,
numeric, or logical)}

\item{currency}{the currency symbol}

\item{interval}{put a marker after how many digits of the numbers}

\item{mark}{the marker after every \code{interval} decimals in the numbers}

\item{digits}{the number of decimal places to round to}
}
\description{
Format numeric columns in a table.
}
\section{Functions}{
\itemize{
\item \code{}: Format numbers as currency.

\item \code{}: Format numbers as percentages.

\item \code{}: Round numbers to a specified number of decimal places.
}}
\examples{
library(DT)
m = cbind(matrix(rnorm(120, 1e+05, 1e+06), 40), runif(40), rnorm(40, 100))
colnames(m) = head(LETTERS, ncol(m))
m

# format the columns A and C as currency, and D as percentages
datatable(m) \%>\% format_currency(c("A", "C")) \%>\% format_percentage("D", 2)

# the first two columns are Euro currency, and round column E to 3 decimal places
datatable(m) \%>\% format_currency(1:2, "\\U20AC") \%>\% format_round("E", 3)
}
