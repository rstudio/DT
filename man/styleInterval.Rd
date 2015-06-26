% Please edit documentation in R/format.R
\name{styleInterval}
\alias{styleColorBar}
\alias{styleEqual}
\alias{styleInterval}
\title{Conditional CSS styles}
\usage{
styleInterval(cuts, values)

styleEqual(levels, values)

styleColorBar(data, color, angle = 90)
}
\arguments{
\item{cuts}{a vector of cut points (sorted increasingly)}

\item{values}{a vector of CSS values}

\item{levels}{a character vector of data values to be mapped (one-to-one) to
CSS values}

\item{data}{a numeric vector whose range will be used for scaling the
table data from 0-100 before being represented as color bars. A vector
of length 2 is acceptable here for specifying a range possibly wider or
narrower than the range of the table data itself.}

\item{color}{the color of the bars}

\item{angle}{a number of degrees representing the direction to fill the
gradient relative to a horizontal line and the gradient line, going
counter-clockwise. For example, 90 fills right to left and -90 fills
left to right.}
}
\description{
A few helper functions for the \code{\link{formatStyle}()} function to
calculate CSS styles for table cells based on the cell values. Under the
hood, they just generate JavaScript and CSS code from the values specified in
R.
}
\details{
The function \code{styleInterval()} maps intervals to CSS values. Its
argument \code{values} must be of length \code{n + 1} where \code{n =
length(cuts)}. The right-closed interval \samp{(cuts[i - 1], cuts[i]]} is
mapped to \samp{values[i]} for \samp{i = 2, 3, ..., n}; \samp{values[1]} is
for the interval \samp{(-Inf, cuts[1]]}, and \samp{values[n + 1]} is for
\samp{(cuts[n], +Inf)}. You can think of the order of \code{cuts} and
\code{values} using this diagram: \samp{-Inf -> values[1] -> cuts[1] ->
values[2] -> cuts[2] -> ... -> values[n] -> cuts[n] -> values[n + 1] ->
+Inf}.

The function \code{styleEqual()} maps data values to CSS values in the
one-to-one manner, i.e. \code{values[i]} is used when the table cell value is
\code{levels[i]}.

The function \code{styleColorBar()} can be used to draw background color bars
behind table cells in a column, and the width of bars is proportional to the
column values.
}
