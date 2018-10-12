owd = setwd('tools')
ver = DT:::DataTablesVersion
out = sprintf('DataTables-%s.zip', ver)
unlink('DataTables*', recursive = TRUE)
download.file(sprintf('http://datatables.net/releases/DataTables-%s.zip', ver), out, mode = 'wb')
unzip(out)

# base64 encode images into CSS
encode_img = function(css) {
  w = setwd(dirname(css)); on.exit(setwd(w), add = TRUE)
  css = basename(css)
  x = readLines(css)
  m = gregexpr('("?)[.][.][^"]+?[.]png\\1', x)
  regmatches(x, m) = lapply(regmatches(x, m), function(ps) {
    if (length(ps) == 0) return(ps)
    ps = gsub('^"|"$', '', ps)
    sapply(ps, knitr::image_uri)
  })
  writeLines(x, css)
}

# if foo.min.js exists, remove foo.js; similar thing to .css
keep_min = function(dir = '.') {
  x1 = list.files(dir, '[.](css|js)$', full.names = TRUE)
  x2 = gsub('[.](css|js)$', '.min.\\1', x1)
  if (length(x1) == 0) return()
  file.remove(x1[file.exists(x2)])
}

in_dir = DT:::in_dir

setwd(file.path(sprintf('DataTables-%s', ver), 'media'))
invisible({
  lapply(list.dirs('..'), keep_min)
  lapply(list.files('..', '[.]css$', recursive = TRUE, full.names = TRUE), encode_img)
})
unlink('css/jquery.dataTables_themeroller.css')
unlink('js/jquery.js')

dt_path = local({
  path = normalizePath('../../../inst/htmlwidgets/lib/datatables/')
  function(...) file.path(path, ...)
})
# copy required files to the R package
file.copy(
  list.files('css', '[.]css$', full.names = TRUE), dt_path('css/'),
  overwrite = TRUE
)
file.copy(
  list.files('js', '[.]js$', full.names = TRUE), dt_path('js'),
  overwrite = TRUE
)
file.copy('../license.txt', dt_path(), overwrite = TRUE)

setwd('../extensions')

extPath = dt_path('..', 'datatables-extensions')
unlink(extPath, recursive = TRUE)

# only keep css/ and js/ (plus swf/ for Buttons)
invisible(lapply(list.files(), function(ext) {
  dirs = file.path(ext, c('css', 'js', if (ext == 'Buttons') 'swf'))
  allf = list.files(ext, all.files = TRUE, full.names = TRUE, no.. = TRUE)
  unlink(allf[!(file_test('-d', allf) & (allf %in% dirs))], recursive = TRUE)
  if (ext == 'Buttons') {
    unlink(file.path(ext, 'css', '*.scss'))
    for (u in c(
      'https://cdnjs.cloudflare.com/ajax/libs/jszip/2.5.0/jszip.min.js',
      'https://raw.githubusercontent.com/bpampuch/pdfmake/0.1.18/build/pdfmake.min.js',
      'https://raw.githubusercontent.com/bpampuch/pdfmake/0.1.18/build/vfs_fonts.js'
    )) download.file(u, file.path(ext, 'js', basename(u)))
  }
  allf = list.files(ext, all.files = TRUE, recursive = TRUE, full.names = TRUE, no.. = TRUE)
  allf = grep('[.](css|js|swf)$', allf, value = TRUE, invert = TRUE)
  if (length(allf)) warning('These files may not be needed: ', paste(allf, collapse = ', '))
  NULL
}))

file.rename('../extensions', '../datatables-extensions')
file.copy('../datatables-extensions', dt_path('..'), overwrite = TRUE, recursive = TRUE)

setwd('../../Plugins/')
system2('git', 'pull origin master')

in_dir('features/searchHighlight', {
  download.file('http://bartaz.github.io/sandbox.js/jquery.highlight.js', 'jquery.highlight.js')
  file.copy(
    c('dataTables.searchHighlight.css', 'dataTables.searchHighlight.min.js', 'jquery.highlight.js'),
    dt_path('..', 'datatables-plugins', 'searchHighlight'), overwrite = TRUE
  )
  unlink('jquery.highlight.js')
})

file.copy(
  c('sorting/natural.js'),
  dt_path('..', 'datatables-plugins', 'natural'), overwrite = TRUE
)

file.copy(
  c('dataRender/ellipsis.js'),
  dt_path('..', 'datatables-plugins', 'ellipsis'), overwrite = TRUE
)

setwd(owd)
