dropNULL = function(x) {
  if (length(x) == 0 || !is.list(x)) return(x)
  x[!unlist(lapply(x, is.null))]
}

isFALSE = function(x) identical(x, FALSE)

is.Date = function(x) inherits(x, c('Date', 'POSIXlt', 'POSIXct'))

# for CSS propertices: fontWeight -> font-weight, backgroundColor ->
# background-color, etc
upperToDash = function(x) {
  x = gsub('^(.)', '\\L\\1', x, perl = TRUE)
  x = gsub('([A-Z])', '-\\L\\1', x, perl = TRUE)
  x
}

# not rigorous, but should work in most cases
inShiny = function() 'shiny' %in% loadedNamespaces()
