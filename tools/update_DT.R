owd = setwd('tools')
ver = DT:::DataTablesVersion
out = sprintf('DataTables-%s.zip', ver)
unlink('DataTables*', recursive = TRUE)
download.file(sprintf('http://datatables.net/releases/DataTables-%s.zip', ver), out, mode = 'wb')
unzip(out)

# base64 encode images into CSS
encode_img = function(css) {
  x = readLines(css)
  m = gregexpr('"?[.][.][^"]+?[.]png"?', x)
  regmatches(x, m) = lapply(regmatches(x, m), function(ps) {
    ps = gsub('^"|"$', '', ps)
    sapply(ps, knitr::image_uri)
  })
  writeLines(x, css)
}

in_dir = function(dir, expr) {
  owd = setwd(dir); on.exit(setwd(owd))
  expr
}

setwd(file.path(sprintf('DataTables-%s', ver), 'media'))
in_dir('css', encode_img('jquery.dataTables.min.css'))

dt_path = local({
  path = normalizePath('../../../inst/htmlwidgets/lib/datatables/')
  function(...) file.path(path, ...)
})
# copy required files to the R package
file.copy('css/jquery.dataTables.min.css', dt_path('css'), overwrite = TRUE)
file.copy('js/jquery.dataTables.min.js', dt_path('js'), overwrite = TRUE)
file.copy('../license.txt', dt_path(), overwrite = TRUE)

setwd('../extensions')

dir.create(dt_path('extensions'), showWarnings = FALSE)

# the responsive extension does not have the minified css for some reason
local({
  x = 'Responsive/css/dataTables.responsive.min.css'
  if (!file.exists(x)) file.copy(sub('min.css', 'css', x), x)
})

lapply(list.files(), function(Ext) {
  ext = sub('^(.)', '\\L\\1', Ext, perl = TRUE)
  in_dir(file.path(Ext, 'css'), encode_img(sprintf('dataTables.%s.min.css', ext)))
})
file.copy(
  list.files('.', '[.]min[.](css|js)$', recursive = TRUE),
  dt_path('extensions'), overwrite = TRUE
)

setwd(owd)
