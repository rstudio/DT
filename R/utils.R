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

inShiny = function() getOption('DT.datatable.shiny', FALSE)

in_dir = function(dir, expr) {
  owd = setwd(dir); on.exit(setwd(owd))
  expr
}

existing_files = function(x) x[file.exists(x)]

# generate <caption></caption>
captionString = function(caption) {
  if (is.character(caption)) caption = tags$caption(caption)
  caption = as.character(caption)
  if (length(caption)) caption
}

toJSON = function(...) {
  FUN = getFromNamespace('toJSON', 'htmlwidgets')
  FUN(...)
}

native_encode = function(x) {
  if (.Platform$OS.type == 'unix') return(x)
  x2 = enc2native(x)
  if (identical(enc2utf8(x2), x)) x2 else x
}
