# CHANGES IN DT VERSION 0.2

NEW FEATURES

- The `selection` argument of `datatable()` can be used to specify a vector of row indices to be pre-selected (thanks, @maxmoro, #89).

- Column filters may be disabled individually using the `searchable` settings of columns, e.g. http://rstudio.github.io/DT/009-searchable.html (thanks, @GitChub, #101).

BUG FIXES

- Row selections are not preserved when column filters are enabled and clicked (thanks, @The-Dub, #97).

- Single row selection does not work for server-side tables (http://stackoverflow.com/q/30700143/559676).

- Missing dates are not rendered correctly with formatDate() (thanks, @studerus, #112)

- Missing values are mistakenly treated as 0 in formatStyle() (thanks, @studerus, #116)

# CHANGES IN DT VERSION 0.1

- Initial CRAN release.
