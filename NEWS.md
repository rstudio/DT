# CHANGES IN DT VERSION 0.2

NEW FEATURES

- The `selection` argument of `datatable()` can be used to specify a vector of row indices to be pre-selected (thanks, @maxmoro, #89).

- Column filters may be disabled individually using the `searchable` settings of columns, e.g. http://rstudio.github.io/DT/009-searchable.html (thanks, @GitChub, #101).

- `formatCurrency()` will round numbers to 2 decimal places by default, and it is configurable via the new argument `digits` (thanks, @mebaran, #100).

- In Shiny, `input$tableId_cell_clicked` gives the row and column indices of the currently clicked cell as well as its value as a list of the form `list(row = row_index, column = column_index, value = cell_value)`.

- Added a new argument `valueColumns` to `formatStyle()` so we can style a column based on the values of a different column (thanks, @zizaozi, #115). See http://rstudio.github.io/DT/010-style.html for examples.

- You can enable column selection by `datatable(..., selection = list(which = 'column'))` now. The indices of selected columns are available to Shiny as `input$tableId_columns_selected` (thanks, @DarioS, #117).

BUG FIXES

- Row selections are not preserved when column filters are enabled and clicked (thanks, @The-Dub, #97).

- Single row selection does not work for server-side tables (http://stackoverflow.com/q/30700143/559676).

- Missing dates are not rendered correctly with `formatDate()` (thanks, @studerus, #112)

- Missing values are mistakenly treated as 0 in `formatStyle()` (thanks, @studerus, #116)

- The thousands separator (e.g. a comma) in `formatCurrency()` should not be applied to the digits after the decimal point (thanks, @johnbaums, #116).

# CHANGES IN DT VERSION 0.1

- Initial CRAN release.
