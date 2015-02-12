owd = setwd('tools')
ver = '1.10.5'
out = sprintf('DataTables-%s.zip', ver)
unlink('DataTables*', recursive = TRUE)
download.file(sprintf('http://datatables.net/releases/DataTables-%s.zip', ver), out, mode = 'wb')
unzip(out)

# base64 encode images into CSS
setwd(file.path(sprintf('DataTables-%s', ver), 'media', 'css'))
x = readLines('jquery.dataTables.min.css')
m = gregexpr('"[.][.][^"]+?[.]png"', x)
regmatches(x, m) = lapply(regmatches(x, m), function(ps) {
  ps = gsub('^"|"$', '', ps)
  sapply(ps, knitr::image_uri)
})
writeLines(x, 'jquery.dataTables.min.css')
setwd('..')

# copy required files to the R package
file.copy('css/jquery.dataTables.min.css', '../../../inst/lib/datatables/css/', overwrite = TRUE)
file.copy('js/jquery.dataTables.min.js', '../../../inst/lib/datatables/js/', overwrite = TRUE)
file.copy('../license.txt', '../../../inst/lib/datatables/', overwrite = TRUE)

setwd(owd)
