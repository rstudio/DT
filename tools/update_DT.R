owd = setwd('tools')
ver = DT:::DataTablesVersion
out = sprintf('DataTables-%s.zip', ver)
unlink('DataTables*', recursive = TRUE)
download.file(sprintf('http://datatables.net/releases/DataTables-%s.zip', ver), out, mode = 'wb')
unzip(out)

# base64 encode images into CSS
encode_img = function(css) {
  x = readLines(css)
  m = gregexpr('("?)[.][.][^"]+?[.]png\\1', x)
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
file.copy(
  'css/jquery.dataTables.min.css', dt_path('css/default/'),
  overwrite = TRUE
)
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
file.copy(c(
  list.files('.', '[.]min[.](css|js)$', recursive = TRUE),
  file.path('TableTools', 'swf', c('copy_csv_xls.swf', 'copy_csv_xls_pdf.swf'))
), dt_path('extensions'), overwrite = TRUE)

in_dir(dt_path('extensions'), local({
  for (f in list.files('.', '[.]min[.](css|js)$')) {
    d = gsub('^dataTables.|.min.(css|js)$', '', f)
    if (!file_test('-d', d)) dir.create(d)
    file.rename(f, file.path(d, f))
  }
}))

in_dir(dt_path(), {
  unlink('../datatables-extensions', recursive = TRUE)
  file.rename('extensions', '../datatables-extensions')
})

setwd('../../Plugins/')
in_dir('integration/bootstrap/3/', {
  encode_img('dataTables.bootstrap.css')
  file.copy(
    c('dataTables.bootstrap.css', 'dataTables.bootstrap.min.js'),
    dt_path('css', 'bootstrap'), overwrite = TRUE
  )
})

in_dir('features/searchHighlight', {
  download.file('http://bartaz.github.io/sandbox.js/jquery.highlight.js', 'jquery.highlight.js')
  file.copy(
    c('dataTables.searchHighlight.css', 'jquery.highlight.js'),
    dt_path('..', 'datatables-plugins', 'searchHighlight'), overwrite = TRUE
  )
})

setwd(owd)
