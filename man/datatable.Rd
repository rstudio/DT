% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datatables.R
\name{datatable}
\alias{datatable}
\title{Create an HTML table widget using the DataTables library}
\usage{
datatable(
  data,
  options = list(),
  class = "display",
  callback = JS("return table;"),
  rownames,
  colnames,
  container,
  caption = NULL,
  filter = c("none", "bottom", "top"),
  escape = TRUE,
  style = "auto",
  width = NULL,
  height = NULL,
  elementId = NULL,
  fillContainer = getOption("DT.fillContainer", NULL),
  autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
  selection = c("multiple", "single", "none"),
  extensions = list(),
  plugins = NULL,
  editable = FALSE
)
}
\arguments{
\item{data}{a data object (either a matrix or a data frame)}

\item{options}{a list of initialization options (see
\url{https://datatables.net/reference/option/}); the character options
wrapped in \code{\link[htmlwidgets]{JS}()} will be treated as literal
JavaScript code instead of normal character strings; you can also set
options globally via \code{\link{options}(DT.options = list(...))}, and
global options will be merged into this \code{options} argument if set}

\item{class}{the CSS class(es) of the table; see
\url{https://datatables.net/manual/styling/classes}}

\item{callback}{the body of a JavaScript callback function with the argument
\code{table} to be applied to the DataTables instance (i.e. \code{table})}

\item{rownames}{\code{TRUE} (show row names) or \code{FALSE} (hide row names)
or a character vector of row names; by default, the row names are displayed
in the first column of the table if exist (not \code{NULL})}

\item{colnames}{if missing, the column names of the data; otherwise it can be
an unnamed character vector of names you want to show in the table header
instead of the default data column names; alternatively, you can provide a
\emph{named} numeric or character vector of the form \code{'newName1' = i1,
'newName2' = i2} or \code{c('newName1' = 'oldName1', 'newName2' =
'oldName2', ...)}, where \code{newName} is the new name you want to show in
the table, and \code{i} or \code{oldName} is the index of the current
column name}

\item{container}{a sketch of the HTML table to be filled with data cells; by
default, it is generated from \code{htmltools::tags$table()} with a table
header consisting of the column names of the data}

\item{caption}{the table caption; a character vector or a tag object
generated from \code{htmltools::tags$caption()}}

\item{filter}{whether/where to use column filters; \code{none}: no filters;
\code{bottom/top}: put column filters at the bottom/top of the table; range
sliders are used to filter numeric/date/time columns, select lists are used
for factor columns, and text input boxes are used for character columns; if
you want more control over the styles of filters, you can provide a named
list to this argument; see Details for more}

\item{escape}{whether to escape HTML entities in the table: \code{TRUE} means
to escape the whole table, and \code{FALSE} means not to escape it;
alternatively, you can specify numeric column indices or column names to
indicate which columns to escape, e.g. \code{1:5} (the first 5 columns),
\code{c(1, 3, 4)}, or \code{c(-1, -3)} (all columns except the first and
third), or \code{c('Species', 'Sepal.Length')}; since the row names take
the first column to display, you should add the numeric column indices by
one when using \code{rownames}}

\item{style}{either \code{'auto'}, \code{'default'}, \code{'bootstrap'}, or
\code{'bootstrap4'}. If \code{'auto'}, and a **bslib** theme is
currently active, then bootstrap styling is used in a way that "just works"
for the active theme. Otherwise,
\href{https://datatables.net/manual/styling/classes}{DataTables
\code{'default'} styling} is used. If set explicitly to \code{'bootstrap'}
or \code{'bootstrap4'}, one must take care to ensure Bootstrap's HTML
dependencies (as well as Bootswatch themes, if desired) are included on the
page. Note, when set explicitly, it's the user's responsibility to ensure
that only one unique `style` value is used on the same page, if multiple
DT tables exist, as different styling resources may conflict with each other.}

\item{width, height}{Width/Height in pixels (optional, defaults to automatic
sizing)}

\item{elementId}{An id for the widget (a random string by default).}

\item{fillContainer}{\code{TRUE} to configure the table to automatically fill
it's containing element. If the table can't fit fully into it's container
then vertical and/or horizontal scrolling of the table cells will occur.}

\item{autoHideNavigation}{\code{TRUE} to automatically hide navigational UI
(only display the table body) when the number of total records is less
than the page size. Note, it only works on the client-side processing mode
and the `pageLength` option should be provided explicitly.}

\item{selection}{the row/column selection mode (single or multiple selection
or disable selection) when a table widget is rendered in a Shiny app;
alternatively, you can use a list of the form \code{list(mode = 'multiple',
selected = c(1, 3, 8), target = 'row', selectable = c(-2, -3))} to
pre-select rows and control the selectable range; the element
\code{target} in the list can be \code{'column'} to enable column
selection, or \code{'row+column'} to make it possible to select both rows
and columns (click on the footer to select columns), or \code{'cell'} to
select cells. See details section for more info.}

\item{extensions}{a character vector of the names of the DataTables
extensions (\url{https://datatables.net/extensions/index})}

\item{plugins}{a character vector of the names of DataTables plug-ins
(\url{https://rstudio.github.io/DT/plugins.html}).  Note that only those
plugins supported by the \code{DT} package can be used here. You can see
the available plugins by calling \code{DT:::available_plugins()}}

\item{editable}{\code{FALSE} to disable the table editor, or \code{TRUE} (or
\code{"cell"}) to enable editing a single cell. Alternatively, you can set
it to \code{"row"} to be able to edit a row, or \code{"column"} to edit a
column, or \code{"all"} to edit all cells on the current page of the table.
In all modes, start editing by doubleclicking on a cell. This argument can
also be a list of the form \code{list(target = TARGET, disable =
list(columns = INDICES))}, where \code{TARGET} can be \code{"cell"},
\code{"row"}, \code{"column"}, or \code{"all"}, and \code{INDICES} is an
integer vector of column indices. Use the list form if you want to disable
editing certain columns. You can also restrict the editing to accept only
numbers by setting this argument to a list of the form \code{list(target =
TARGET, numeric = INDICES)} where \code{INDICES} can be the vector of the
indices of the columns for which you want to restrict the editing to
numbers or \code{"all"} to restrict the editing to numbers for all columns.
If you don't set \code{numeric}, then the editing is restricted to numbers
for all numeric columns; set \code{numeric = "none"} to disable this
behavior. It is also possible to edit the cells in text areas, which are
useful for large contents. For that, set the \code{editable} argument to a
list of the form \code{list(target = TARGET, area = INDICES)} where
\code{INDICES} can be the vector of the indices of the columns for which
you want the text areas, or \code{"all"} if you want the text areas for
all columns. Of course, you can request the numeric editing for some
columns and the text areas for some other columns by setting
\code{editable} to a list of the form \code{list(target = TARGET, numeric
= INDICES1, area = INDICES2)}. Finally, you can edit date cells with a
calendar with \code{list(target = TARGET, date = INDICES)}; the target
columns must have the \code{Date} type. If you don't set \code{date} in
the \code{editable} list, the editing with the calendar is automatically
set for all \code{Date} columns.}
}
\description{
This function creates an HTML widget to display rectangular data (a matrix or
data frame) using the JavaScript library DataTables.
}
\details{
\code{selection}:
  \enumerate{
    \item The argument could be a scalar string, which means the selection
      \code{mode}, whose value could be one of  \code{'multiple'} (the default),
      \code{'single'} and \code{'none'} (disable selection).
    \item When a list form is provided for this argument, only parts of the
      "full" list are allowed. The default values for non-matched elements are
      \code{list(mode = 'multiple', selected = NULL, target = 'row',
      selectable = NULL)}.
    \item \code{target} must be one of \code{'row'}, \code{'column'},
      \code{'row+column'} and \code{'cell'}.
    \item \code{selected} could be \code{NULL} or "indices".
    \item \code{selectable} could be \code{NULL}, \code{TRUE}, \code{FALSE}
      or "indices", where \code{NULL} and \code{TRUE} mean all the table is
      selectable. When \code{FALSE}, it means users can't select the table
      by the cursor (but they could still be able to select the table via
      \code{\link{dataTableProxy}}, specifying \code{ignore.selectable = TRUE}).
      If "indices", they must be all positive or non-positive values. All
      positive "indices" mean only the specified ranges are selectable while all
      non-positive "indices" mean those ranges are \emph{not} selectable.
      The "indices"' format is specified below.
    \item The "indices"' format of \code{selected} and \code{selectable}:
      when \code{target} is \code{'row'} or \code{'column'}, it should be a plain
      numeric vector; when \code{target} is \code{'row+column'}, it should be a
      list, specifying \code{rows} and \code{cols} respectively, e.g.,
      \code{list(rows = 1, cols = 2)}; when \code{target} is \code{'cell'},
      it should be a 2-col \code{matrix}, where the two values of each row
      stand for the row and column index.
    \item Note that DT has its own selection implementation and doesn't
      use the Select extension because the latter doesn't support the
      server-side processing mode well. Please set this argument to
      \code{'none'} if you really want to use the Select extension.
  }
  \code{options$columnDefs}:
  \enumerate{
    \item \code{columnDefs} is an option that provided by the DataTables library
      itself, where the user can set various attributes for columns. It must be
      provided as a list of list, where each sub-list must contain a vector named 'targets',
      specifying the applied columns, i.e.,
      \code{list(list(..., targets = '_all'), list(..., targets = c(1, 2)))}
    \item \code{columnDefs$targets} is a vector and should be one of:
      \itemize{
        \item 0 or a positive integer: column index counting from the left.
        \item A negative integer: column index counting from the right.
        \item A string: the column name. Note, it must be the names of the
          original data, not the ones that (could) be changed via param \code{colnames}.
        \item The string "_all": all columns (i.e. assign a default).
      }
    \item When conflicts happen, e.g., a single column is defined for some property
      twice but with different values, the value that defined earlier takes the priority.
      For example, \code{list(list(visible=FALSE, target=1), list(visible=TRUE, target=1))}
      results in a table whose first column is \emph{invisible}.
    \item See \url{https://datatables.net/reference/option/columnDefs} for more.
  }
  \code{filter}:
  \enumerate{
    \item \code{filter} can be used to position and customize column filters.
      A scalar string value defines the position, and must be one of \code{'none'}
      (the default), \code{'bottom'} and \code{'top'}. A named list can be used
      for further control. In the named list form:
    \item \code{$position} is a string as described above. It defaults to \code{'none'}.
    \item \code{$clear} is a logical value indicating if clear
      buttons should appear in input boxes. It defaults to \code{TRUE}.
    \item \code{$plain} is a logical value indicating if plain styling
      should be used for input boxes instead of Bootstrap styling. It
      defaults to \code{FALSE}.
    \item \code{$vertical} is a logical value indicating if slider
      widgets should be oriented vertically rather than horizontally.
      It defaults to \code{FALSE}.
    \item \code{$opacity} is a numeric value between 0 and 1 used to set
      the level of transparency of slider widgets. It defaults to \code{1}.
    \item \code{$settings} is a named list used to directly pass configuration
      for initializing filter widgets in JavaScript.
      \itemize{
         \item The \code{$select} element is passed to the select widget, and
           \code{$slider} is passed to the slider widget.
         \item Valid values depend on the settings accepted by the underlying
           JavaScript libraries, \href{https://selectize.dev/}{Selectize}
           and \href{https://refreshless.com/nouislider/}{noUiSlider}.
           Please note that the versions bundled with DT are currently quite old,
           so accepted settings may not match their most recent documentation.
         \item These settings can override values set by DT, so specifying
           a setting already in use may break something. Use with care.
      }
  }
}
\note{
You are recommended to escape the table content for security reasons
  (e.g. XSS attacks) when using this function in Shiny or any other dynamic
  web applications.
}
\examples{
library(DT)

# see the package vignette for examples and the link to website
vignette('DT', package = 'DT')

# some boring edge cases for testing purposes
m = matrix(nrow = 0, ncol = 5, dimnames = list(NULL, letters[1:5]))
datatable(m)  # zero rows
datatable(as.data.frame(m))

m = matrix(1, dimnames = list(NULL, 'a'))
datatable(m)  # one row and one column
datatable(as.data.frame(m))

m = data.frame(a = 1, b = 2, c = 3)
datatable(m)
datatable(as.matrix(m))

# dates
datatable(data.frame(
  date = seq(as.Date("2015-01-01"), by = "day", length.out = 5), x = 1:5
))
datatable(data.frame(x = Sys.Date()))
datatable(data.frame(x = Sys.time()))
}
\references{
See \url{https://rstudio.github.io/DT/} for the full
  documentation.
}
