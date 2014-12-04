\name{formatCurrency}
\alias{formatCurrency}
\alias{formatPercentage}
\alias{formatRound}
\title{Format table columns}
\usage{
formatCurrency(table, columns, currency = "$", interval = 3, mark = ",")

formatPercentage(table, columns, digits = 0)

formatRound(table, columns, digits = 2)
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
Format numeric columns in a table as currency or percentages, or round
numbers to a specified number of decimal places.
}
\examples{
library(DT)
m = cbind(matrix(rnorm(120, 1e+05, 1e+06), 40), runif(40), rnorm(40, 100))
colnames(m) = head(LETTERS, ncol(m))
m

# format the columns A and C as currency, and D as percentages
datatable(m) \%>\% formatCurrency(c("A", "C")) \%>\% formatPercentage("D", 2)

# the first two columns are Euro currency, and round column E to 3 decimal places
datatable(m) \%>\% formatCurrency(1:2, "\\U20AC") \%>\% formatRound("E", 3)
}
