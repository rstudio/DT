# CHANGES IN DT VERSION 0.2

NEW FEATURES

- The `selection` argument of `datatable()` can be used to specify a vector of row indices to be pre-selected (thanks, @maxmoro, #89).

BUG FIXES

- Row selections are not preserved when column filters are enabled and clicked (thanks, @The-Dub, #97).

- Single row selection does not work for server-side tables (http://stackoverflow.com/q/30700143/559676).

# CHANGES IN DT VERSION 0.1

- Initial CRAN release.
