# CHANGES IN DT VERSION 0.24

- Fixed a bug with missing numeric values rendered as `0` (thanks, @tverbeke, #1000).

- Box scalar elements of 'list' columns so that, e.g., `data.frame(col = I(integer(), 1, 2:3))` is represented in JavaScript as `[[], [1], [2, 3]]` instead of `[[], 1, [2, 3]]` (thanks, @mtmorgan, #1005).

# CHANGES IN DT VERSION 0.23

- Make sure number inputs return numeric values when editing numeric table cells (thanks, @stla, #987).

# CHANGES IN DT VERSION 0.22

- Enhancements to `updateFilters()` (#971):
  - Added support for data with integer and character columns (@mikmart #972).
  - Fixed an issue with length 1 options causing filters not to update (@mikmart #973).
  - Fixed an issue with slider limits sometimes being incorrectly scaled (@mikmart #974).
  - Updates to factor and logical filters now match the initial render (@mikmart #975).
  - Disabled status is now also affected by updates (@mikmart #977).

- New functions `doColumnSearch()` and `doGlobalSearch()` let you do server-side searching with filter search strings. These are particularly useful in conjunction with `updateFilters()` for implementing dynamic limits for filter controls based on currently filtered data (@mikmart #982).

- Server-side searching is now faster when multiple filters are active (@mikmart).

- Fix a bug that the column filter didn't work for strings like "+" (@stephan-hutter @shrektan #980).

# CHANGES IN DT VERSION 0.21

## NEW FEATURES

- Add the `zero.print` argument to `formatPercentage()`, `formatCurrency()`, `formatSignif()` and `formatRound()`, which allows to control the format of zero values. It's useful when the data is "sparse" (thanks, @shrektan #953).

- `formatXXX()` functions now gain a new argument `rows` (starting from 1), which can be used to specify the rows that the "style" should be applied to. Note that it only works expected in the client-side processing mode, i.e., `server = FALSE`. `formatStyle()` is the only exception that doesn't have this argument and should use `styleRow()` instead (thanks, @jrecasens @shrektan #520).

- Added a new function `updateFilters` to allow users to update the filters of a DataTable object that has been updated by reference, such as with `replaceData()` (thanks, @giadasp #934, @wholmes105 #939).

## MAJOR CHANGES

- Now users can provide column names of the data to `options$columnDefs$targets`. Previously, it only supports column indexes or "_all" (thanks, @shrektan #948).

## MINOR CHANGES

- `styleRow()` and `styleEqual()` now allows a scalar `values` argument like other R functions, e.g., `styleRow(1:5, 'abcd')` is the same as `styleRow(1:5, rep('abcd', 5))`. It throws error that `the length(rows) must be equal to length(values)` in the previous version (thanks, @shrektan #955).

## BUG FIXES

- Fix the bug that `addRow()` doesn't support a list of `data` after R 3.4.0, where `structure(NULL, ...)` was deprecated (thanks, @stla @shrektan #959).

- Fix the bug that DT table can't vertically fill the container, e.g., RStudio IDE or FlexDashboard (thanks, @mbojan @shrektan #951).

- Fix the bug that DT failed to parse ajax request correctly, when special strings like "=" exist (thanks, @shrektan #965).

# CHANGES IN DT VERSION 0.20

## MAJOR CHANGES

- Upgraded the DataTables library to v1.11.3 (thanks, @shrektan #942, @michaelquinn32 #940).

# CHANGES IN DT VERSION 0.19

## NEW FEATUERS

- Add `vertical` and `opacity` option to `filter` which applies to the noUiSliders. This is useful with limited width DataTables to prevent x overflow (thanks, @alexvpickering, #919).

- Now the user is able to restrict the editing to accept only numbers by, e.g., `datatable(..., editable = list(target = "cell", numeric = 3:5))`. By default, the editing is restricted to numbers for all numeric columns; set `numeric = "none"` to disable this behavior (thanks, @stla, #860 #824).

- Now the user can request text areas for the editing by, e.g., `datatable(..., editable = list(target = "cell", area = 1:2))`. Text areas are useful for cells with large contents (thanks, @stla #860, @fabiangehring #671).

## MAJOR CHANGES

- Upgraded jQuery from v1.12.4 to v3.x imported from the **jquerylib** R package.

# CHANGES IN DT VERSION 0.18

## NEW FEATURES

- `datatable()`'s `style` argument now defaults to `'auto'`, which resolves to either `'bootstrap'` or `'bootstrap4'` when a `{bslib}` theme is relevant. If a `{bslib}` theme isn't relevant, `'auto'` resolves to the old default value of `'default'` (thanks, @cpsievert, #852).

- Add a new function `styleRow()`. It's useful when you want to apply CSS styles based on Row Indexes (thanks, @s-fleck, #894). 

## BUG FIXES

- `datatable(data)` and `datatable(data, fillContainer = TRUE)` now work as expected when statically rendered inside `flexdashboard::flex_dashboard()` (thanks, @cpsievert, #904).

# CHANGES IN DT VERSION 0.17

## NEW FEATURES

- Support the new datatables' extension [SearchBuilder](https://datatables.net/extensions/searchbuilder/). With this new extension, users can construct a complex search query by adding groups and conditions. Note, as the time of writing, this extension only works on the client-side processing mode (thanks, @stla, #875)

- Add a new plugin [diacritics-neutralise](https://datatables.net/plug-ins/filtering/type-based/diacritics-neutralise), which can be used for searching accented, _non-Latin_ characters with their unaccented counterparts. Note, it will only work in the client-side processing mode (thanks, @tyler-richardett, #887).

## MINOR CHANGES

- The `autoHideNavigation` argument now works with the default theme. In addition, the prerequisite is properly documented. Specifically speaking, it only works when the `pageLength` option is provided and is rendered in the client-side processing mode (thanks, @bhogan-mitre, #856). 

- When editing factor columns, `editData()` now automatically updates the factor levels if it's necessary (thanks, @aman-malik3010, #865).

## BUG FIXES

- Fix the issue that `addRow()` would fail when the proxy DT table contains no data (e.g., a zero-row data.frame) (thanks, @chalioui, #888).

- DT no longer overrides the `options$responsive` to `TRUE`. Thus, it enables users to provide customized options for the Responsive extension (thanks, @hdrab127, #885).


# CHANGES IN DT VERSION 0.16

## NEW FEATURES

- Add a new plugin [accent-neutralise](https://datatables.net/plug-ins/filtering/type-based/accent-neutralise), which can be used for searching accented characters with their unaccented counterparts. Note, it will only work in the client-side processing mode (#822).

- `addRow()` now has a new parameter `resetPaging`. By setting it to `FALSE`, we can keep the paging position after adding a row (thanks, @stanstrup, #853).

## MINOR CHANGES

- Upgrade the SearchPanes extension to v1.1.1 so that it can [display all the entries properly with the Scroller extension](https://datatables.net/forums/discussion/62807/searchpanes-button-filtering-value-only-show-10-from-all-available-values-with-scroller-extension) (thanks, @JonasMandel @stla, #820).

- The `class` argument now keeps user-defined classes with bootstrap themes (thanks, @mmuurr, #806).

- Now DT will throw a clear error message if the value of `search` provided in `datatables(..., options=)` is illegal (thanks, @realHenningLorenzen, #848).

## BUG FIXES

- Fix the issue that the sorting results may not be expected after formatting functions applied. This is a regression of PR #777 (thanks, @fernandofernandezgonzalez @shrektan, #837).

- `styleColorBar()` now displays correctly on Safari, when the data is greater than the upper limit, due to negative css percentage values (thanks, @Seyphaton, #843).

# CHANGES IN DT VERSION 0.15

## BUG FIXES

- Fix the issue that the formatting functions may throw error for named colname inputs (thanks, @jarauh @shrektan, #831).

- The `input$xxx_xxx_selected` values will always be reset to `NULL` or an empty list when the table gets reloaded (thanks, @stibu81 @shrektan, #828).

# CHANGES IN DT VERSION 0.14

## NEW FEATURES

- Now the user is able to control the selection range by `datatable(..., selection = list(selectable = 3:5))`, where positive and non-positive `selectable` means "enable" and "disable", respectively (thanks, @tomasreigl @shrektan, #201 #793).

- Added a function `styleValue()` to use column values as CSS values. When the user has a complex CSS rule to apply, storing the CSS values in the columns of the table and call this function is very convenient (thanks, @Mosk915 @shrektan, #801 #802).

- Clicking on a row or cell now always triggers a reactive event in Shiny (`input$tableId_row_last_clicked` or `input$tableId_cell_clicked`), even if the same row or cell is clicked multiple times (thanks @gadenbuie, #811).

## BUG FIXES

- Fix the issue that formatting functions don't support vectorized arguments any longer. This was a regression of PR #777 (thanks, @pbreheny @shrektan, #790).

- Fix the issue that `styleEqual()` incorrectly uses the column values as the default css values. This was a regression of PR #702 (thanks, @Chr96er @shrektan, #799)

## MINOR CHANGES

- The formatting functions now throw clearer error messages when called on non-datatables objects (thanks, @shrektan, #785).

- Now the `server` argument in `renderDT()` is reactive in Shiny (thanks, @shrektan, #794).

- Incorrect `selection` param now triggers a clear error message on the R side (thanks, @shrektan, #795).

# CHANGES IN DT VERSION 0.13

## BUG FIXES

- Fix a bug that on Windows, rmarkdown can't render a file that contains DT with PDF download button enabled (thanks, @mfherman @shrektan, #774)

- The formatting function will no longer throw errors for non-existing columns (thanks, @philibe @shrektan, #623).

- Fix a bug that `renderDT()` doesn't react to `...` arguments (thanks, @AlfTang @shrektan, #152).

- Now DT will always display black text in the RStudio IDE, even under a dark theme like "Cobalt" (thanks, @GegznaV @shrektan, #447 #767).

- Fix the issue that DT doesn't look well in pkgdown (thanks, @jayhesselberth @shrektan, #563).

## NEW FEATURES

- All the formatting functions except `formatStyle()` now support the `Responsive` plugin (thanks, @andirey @shrektan, #776 #777 #782)

# CHANGES IN DT VERSION 0.12

## NEW FEATURES

- Support the Select extension on the client-side processing mode (thanks, @shrektan, #744).

- Support [features/scrollResize plug-in](https://datatables.net/blog/2017-12-31) (thanks, @andrewkho @shrektan, #467 #754).

- Upgrade DataTables to 1.10.20 and support the new datatables extension [SearchPanes](https://datatables.net/blog/2020-01-13) (thanks, @shrektan, #756).

## BUG FIXES

- The server no longer throws warnings when conducting searching (thanks, @bquast @shrektan, #749).

- Fix a bug that the row generated by the RowGroup extension can't be used as the selector of the DataTables' callback event handlers (thanks, @wiktorowczarz @shrektan, #759).

- Fix a bug that `addRow()` can't add a `data.table` row to the proxy (thanks, @sifodia @shrektan, #760 #761).

- Now DT displays the column alignment and the selection style correctly under the bootstrap4 theme (thanks, @pjvandam @shrektan, #601 #765).

# CHANGES IN DT VERSION 0.11

## NEW FEATURES

- Support [pagination plug-ins](https://datatables.net/plug-ins/pagination/) (#734).

## BUG FIXES

- Highlighting when searching with regex (i.e., using `options = list(searchHighlight = TRUE, search = list(regex = TRUE))` for the table) works now (thanks, @lrasmus, #719).

- Updated the AutoFill extension (thanks, @stla @shrektan, #711).

## MINOR CHANGES

- In the server-side processing mode, filters now use Perl-compatible regular expressions (regexps), such as for lookround and negating assertions, see `help(regex)` or https://perldoc.perl.org/perlre.html. This may be most useful in columns (`DT::renderDataTable(filter = list(position = "top"), options = list(search = list(regex = TRUE))`), but also works in the global search (thanks, @rfhb, #727).

- For a factor column, the choices of the filter now use the factor levels (#728).

# CHANGES IN DT VERSION 0.10

## BUG FIXES

- Fix the issue that `styleEqual()` doesn't work expectedly for values contain special HTML character like `>` or `<` (thanks, @hjia222 #723).

# CHANGES IN DT VERSION 0.9

## NEW FEATURES

- `formatStyle()` with `styleEqual()` is now chainable by default (thanks, @e-kennedy #632).

## BUG FIXES

- Fix the issue that the first column can't be disabled from editing (thanks, @tsolloway #669, @haozhu233 #694).

- Fix the issue that the filter boxes are not anchored to the corresponding value columns when there are many columns (thanks, @philibe, #554).

- Column selection now works in `row+column` selection mode, when one of `scrollX` or `scrollY` is enabled (thanks, @akarslan #705).

## MINOR CHANGES

- Upgraded DataTables from 1.10.16 to 1.10.19.

# CHANGES IN DT VERSION 0.8

## BUG FIXES

- For `datatable()`, `options$buttons` now works as expected when providing a scalar string or a boolean value (thanks, @shrektan, #685 #658).

- Fix the issue that when parameters of the `formatXXX()` functions contain single quotes, they may lead to incorrect JavaScript code due to failing to escape the single quotes (thanks, @shrektan #683 #666, @lorenzwalthert #667).

# CHANGES IN DT VERSION 0.7

## BUG FIXES

- Bulk table editing doesn't work for the client-side processing mode without Shiny (e.g. a DataTable on a static HTML page).

# CHANGES IN DT VERSION 0.6

## NEW FEATURES

- The table editor has been enhanced: now the `editable` argument of `datatable()` can take four possible values, `cell` (or `TRUE` for backward compatibility), `row`, `column`, or `all`, which means you can edit a single cell a time, or a whole row or column, or all cells in the table. To trigger the editor, doubleclick on any cell. To submit the edit, hit `Ctrl + Enter` when multiple cells are being edited, or hit `Esc` to cancel the edit. See https://github.com/rstudio/DT/tree/main/inst/examples/DT-edit for comprehensive examples (thanks, @LukasK13 #509 and @mgirlich #493).

- Editing can be disabled on specified columns now: pass a list of the form `list(target = TARGET, disable = list(columns = INDICES))` to the `editable` argument of `datatable()`, where `TARGET` can be `'cell'`, `'row'`, `'column'`, or `'all'`, and `INDICES` is an integer vector of column indices (thanks, @opremicSebastian #657, @l-ts #550).

- Added a `funcFilter` argument to `DT::renderDT()` (thanks, @galachad, #638).

## BUG FIXES

- Fixed the issue where `replaceData()` didn't work when used in Shiny modules (thanks, @donarus, #628, #626).

- The JavaScript event `cell_edit` now always triggers a reactive event on the R side. Since `cell_edit` will only be triggered when the value shown on the table has been changed so it's almost always what user expects (thanks, @shrektan @stelmath, #647 #645).

- Fix the issue that the server-side search option doesn't handle exotic encoding correctly, because after httpuv v1.5.0, `shiny::parseQueryString()` always assumes the input is an UTF-8 encoded string (thanks, @shrektan @phileas-condemine, #656).

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
