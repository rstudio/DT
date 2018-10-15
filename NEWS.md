# CHANGES IN DT VERSION 0.5

## NEW FEATURES

- Added the Shiny async support (thanks, @jcheng5, #549; @alexturcan, #543).

- The filters of `Date` or `Datetime` columns now display the same format and timezone as the column content if `formatDate()` is applied on these columns (thanks, @shrektan, #522 #241).

- You can now show and hide columns from shiny using `showCols()` and `hideCols()`, and reorder the the display of columns using `colReorder()` (thanks, @gergness, #527).

- You can now set the default CSS value in `styleEqual()` by using the newe param `default` (thanks, @shrektan, #558, #546).

- The js callbacks generated from the `format*()` functions will be executed in sequential order. `formatString()` will change the cells based on the existing content rather than the raw data (thanks, @shrektan, #576).

- Added the DataTables plugin `ellipsis` (thanks, @jarauh, #603).

## BUG FIXES

- `styleInterval()` and `styleEqual()` now generates the correct callback for `Date` and `Datetime` values. (thanks, @shrektan, #500, #495).

- The `dt-right` class will no longer be added to numeric headers unexpectedly (thanks, @shrektan @carlganz @vnijs, #514 #512 #511 #476).

- The printing values of `NA` and `Inf` can be controlled by `getOption('DT.TOJSON_ARGS')` in the server-side processing mode now. (thanks, @shrektan, #536 #513).

- `styleEqual()`, `styleInterval()` and `styleColorBar()` now generate correct javascript values when `options(OutDec = ',')` (thanks, @shrektan @mteixido, #516 #515).

- Fixed a bug that `selectRows()` and `selectColumns()` behave erratically for scalar inputs or character inputs (thanks, @shrektan #528).

- The server-side processing mode now supports data with nested `data.frame`s in its columns (thanks, @shrektan, #530 #525).

- The `colReorder` extention now works with the column filters and the server-side processing mode (thanks @shrektan, #532 #531 #160).

- Fix the bug that `formatDate()` may display dates off by one day when method = "toLocaleDateString" (thanks, @shrektan @DevMui, #539 #538).

- Fix the bug that in IE11, the scroll bar of the filter will disappear when you try to click it (thanks, @shrektan, #557 #556).

- `coerceValue()` no longer throws warnings for class `charactor` (thanks, @shrektan, #541 #542).

- Fix the bug that using DT as a htmlwidget inside UI throws an error (thanks, @shrektan, #583).

- The RowReorder extension now works (thanks, @shrektan, #595 #592).

# CHANGES IN DT VERSION 0.4

## BUG FIXES

- A table will never appear again after `renderDT(NULL)` once (thanks, @jcheng5, #488).

- `updateSearch()` does not respect the search options `regex` and `caseInsensitive` (thanks, @mgirlich, #487).

# CHANGES IN DT VERSION 0.3

## NEW FEATURES

- It is possible to double-click to edit values in table cells now. See https://github.com/rstudio/DT/pull/480 for examples.

- Added functions `DTOutput()` and `renderDT()` as aliases of `dataTableOutput()` and `renderDataTable()`, respectively. This is because the latter often collide with functions of the same names in **shiny**. You are recommended to use `DTOutput()` and `renderDT()` in Shiny apps (unless you always use the `DT::` qualifier).

- Support using `Shift + Click` to select multiple rows (thanks, @shrektan, #475 #305 #481).

- Added an argument `params` to the `formatDate()` function so you can pass a list of parameters to the specific date conversion method (http://stackoverflow.com/q/38875173/559676).

- The arguments `interval`, `mark`, and `dec.mark` also work for functions `formatPercentage()`, `formatRound()`, and `formatSignif()` now (thanks, @paulofelipe, #128).

- Added the `RowGroup` extension (thanks, @ecortens, #466 #442).

## MAJOR CHANGES

- Searching in the server-side processing mode has enabled the "smart" mode by default (https://datatables.net/reference/option/search.smart). Previously this only works in the client-side processing mode. If you want to disable the smart filtering, you can set the initialization options in `datatable()` (e.g., `options = list(search = list(smart = FALSE))`). The smart filtering means spaces in the global search keyword in the table will have a special meaning: each returned record in the table should match _all_ of the words separated by spaces (thanks, @carlganz, #453; @keshavramaswamy, #391).

- For client-side tables in Shiny, the indices of selected rows (`input$tableId_rows_selected`) will include the rows that are not visible in the current view after filtering is applied. If you want to exclude the indices of such rows, you can take the intersection of `input$tableId_rows_selected` and `input$tableId_rows_all` (the latter is the indices of all rows after filtering is applied).

## MINOR CHANGES

- Upgraded DataTables from 1.10.12 to 1.10.16.

## BUG FIXES

- When the DataTables option `language.url` is used, the table cannot be successfully initialized (thanks, @lubomirskirka, #344)

- `styleEqual()` accepts `NA` in the `levels` argument now (thanks, @MalditoBarbudo, #355).

- `dataTableProxy()` does not work with Shiny modules (thanks, @fbreitwieser, #357).

- `replaceData()` does not work with Shiny modules (thanks, @ykeuter, @ssayols, @fbreitwieser, @carlganz, #359).

- `formatDate()` may display dates off by one day (thanks, @jdthorpe, #198).

- When a table is destroyed, all Shiny input values associated with the table should be emptied (thanks, @dracodoc, #457).

# CHANGES IN DT VERSION 0.2

## NEW FEATURES

- The `selection` argument of `datatable()` can be used to specify a vector of row indices to be pre-selected (thanks, @maxmoro, #89).

- Column filters may be disabled individually using the `searchable` settings of columns, e.g. http://rstudio.github.io/DT/009-searchable.html (thanks, @GitChub, #101).

- `formatCurrency()` will round numbers to 2 decimal places by default, and it is configurable via the new argument `digits` (thanks, @mebaran, #100).

- In Shiny, `input$tableId_cell_clicked` gives the row and column indices of the currently clicked cell as well as its value as a list of the form `list(row = row_index, col = column_index, value = cell_value)`.

- Added a new argument `valueColumns` to `formatStyle()` so we can style a column based on the values of a different column (thanks, @zizaozi, #115). See http://rstudio.github.io/DT/010-style.html for examples.

- You can enable column selection by `datatable(..., selection = list(target = 'column'))` now. The indices of selected columns are available to Shiny as `input$tableId_columns_selected` (thanks, @DarioS, #117).

- Row and column selections can be enabled simultaneously using `datatable(..., selection = list(target = 'row+column')`. Column selection is done via clicking on the table footer.

- Cell selection can be enabled via `datatable(..., selection = list(target = 'cell'))`. See http://rstudio.github.io/DT/shiny.html for more info.

- It is possible to update the data of a table without regenerating the whole table widget with the new function `replaceData()` now (#168, #208).

- Added a `width` argument to `datatable()` (thanks, @taiyun).

- Added a `plugins` argument to `datatable()` to support **DataTables** plugins. See http://rstudio.github.io/plugins/ for more information.

- Added a function `dataTableProxy()` to create a proxy object that can be used to manipulate a table instance after it has been rendered in a Shiny app. Currently supported methods include `selectRows()`, `selectColumns()` (#126), and `addRow()` (#129), etc.

- Added a function `selectCells()` to select table cells.

- Added a function `clearSearch()` to clear the filters.

- Added a function `selectPage()` to select a page in the table (thanks, @carlganz, #314).

- Added a function `updateCaption()` to update the table caption only (thanks, @johnpauls, #155).

- Added a function `updateSearch()` to change the search keywords of the global search box and individual column filters (thanks, @fbreitwieser, #262).

- When all values in a numeric column are missing (`NA`), the column filter will be disabled (http://stackoverflow.com/q/31323807/559676).

- Added an argument `dec.mark` to `formatCurrency()` to customize the character for the decimal point (thanks, @frajuegies, #128).

- Added an argument `before` to `formatCurrency()` to determine whether to place the currency symbol before or after the data values (thanks, @jrdnmdhl, #220).

- Added a `target` argument to `formatStyle()` to decide whether to style the cell or the full row (thanks, @peterlomas, #108).

- Added a `formatSignif()` function to format numbers to a specified number of significant digits (thanks, @shabbychef, #216).

- Added a `formatString()` function to format strings; currently it has two arguments `prefix` and `suffix`, and you may add strings before/after column values (thanks, @fbreitwieser, #279)

- Added the `elementId` argument to `datatable()` (#307).

## MAJOR CHANGES

- Upgraded the DataTables library to 1.10.12; there have been many changes from 1.10.7 to this version: http://datatables.net/blog/2015-08-13
    - The `extensions` argument of `datatable()` should be a character vector now; previously it can be a list of initialization options for extensions due to the inconsistent ways of initializing DataTables extensions; now the initialization options for all extensions can be set in the `options` argument.
    - The `copySWF()` function has been removed, since the `TableTools` extension has been removed.
    - The `ColVis` extension was removed and replaced but the `colvis` button in the `Buttons` extension.

- In the previous version, row names were used as row indices in the server-side processing mode, but numeric row indices were used in the client-side mode. Now we always use numeric row indices in both modes for the sake of consistency. These input values in Shiny will always be integers: `input$tableId_rows_current`, `input$tableId_rows_all`, and `input$tableId_rows_selected`.

- `formatCurrency()` puts the currency symbol after the minus sign now, e.g. previously you might see `$-20` but now it is displayed as `-$20` (#220).

## BUG FIXES

- Row selections are not preserved when column filters are enabled and clicked (thanks, @The-Dub, #97).

- Single row selection does not work for server-side tables (http://stackoverflow.com/q/30700143/559676).

- Missing dates are not rendered correctly with `formatDate()` (thanks, @studerus, #112)

- Missing values are mistakenly treated as 0 in `formatStyle()` (thanks, @studerus, #116)

- The thousands separator (e.g. a comma) in `formatCurrency()` should not be applied to the digits after the decimal point (thanks, @johnbaums, #116).

- The `class` argument does not work when a custom table `container` is used in `datatable()` (thanks, @DarioS, #138).

- The column filters for numeric columns (sliders) do not work well when the columns contain very small values or values with a large number of decimal places (thanks, @DarioS, #150).

- Searching for the ampersand `&` in the table does not work in Shiny.

- Searching for `+` in columns does not work in Shiny (thanks, @vnijs, #214).

- Fixed a bug that triggers a DataTables warning dialog box in the browser (thanks, @zross, https://github.com/WHOequity/who-heat/issues/229 and https://github.com/rstudio/shiny/issues/561)

- Factors will a huge number of levels may slow down the rendering of the table significantly (thanks, @vnijs, #169).

- Clicking links in table cells should not trigger row/column/cell selection (thanks, @daattali, #265).

- White spaces may be trimmed unexpectedly in select inputs when `dataTableOutput()` is present in a Shiny app (thanks, @Yang-Tang, #303).

- Respect column-wise `searchable` options when performing global searching (thanks, @aj2duncan, #311).

- Clear buttons do not work when column filters are pre-set (thanks, @nutterb, #319).

- Changes in column visibility should trigger changes in the table state, i.e. `input$tableId_state` (thanks, @MikeBadescu, #256).

# CHANGES IN DT VERSION 0.1

- Initial CRAN release.
