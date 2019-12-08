
# note --------------------------------------------------------------------

# This script is going to update
# 1. datatables and its extentions' css and js files
# 2. datatables' plugins' js files
# 3. other dependencies like jquery.highlight.js

# steps -------------------------------------------------------------------

# 1. Create a folder "download" under the root of this project
# 2. Go to https://datatables.net/download/index , click all the extentions,
#    then go to Step 3 choose "minify" but not "concatenate". Put the files
#    in "download/DataTables"
# 3. Download all the content from https://github.com/DataTables/Plugins/ and
#    put them under folder "download/Plugins"
# 4. Update the value of "DataTablesVersion" in "package.R"
# 5. Run this script (note it will clean up the "download" folder afterwards
#    so you might want to backup those files in case)
# 6. Manually test all the apps in "inst/examples"
# 7. Rebuild the site

# param -------------------------------------------------------------------

dld_folder = './download'
dld_dt_path = function(...) {
  file.path(dld_folder, 'DataTables', ...)
}
dld_plugin_path = function(...) {
  file.path(dld_folder, 'Plugins', ...)
}

# utils -------------------------------------------------------------------

dt_path = function(...) {
  path = file.path('./inst/htmlwidgets/lib/datatables/', ...)
  path = normalizePath(path)
  if (!dir.exists(path)) dir.create(path)
  path
}

setwd = function(x) {
  if (!dir.exists(x)) dir.create(x)
  base::setwd(x)
}

in_dir = function(dir, expr) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  DT:::in_dir(dir, expr)
}

# base64 encode images into CSS
encode_img = function(css) {
  w = setwd(dirname(css)); on.exit(setwd(w), add = TRUE)
  css = basename(css)
  x = readLines(css)
  # match both "../images/xxx.png" and "images/xxx.png"
  m = gregexpr('("|\']?)(\\.\\.)?[^"\']+?[.]png\\1', x)
  regmatches(x, m) = lapply(regmatches(x, m), function(ps) {
    if (length(ps) == 0) return(ps)
    # replace the first and the last `"` with empty
    ps = gsub('^"|^\'|"$|\'$', '', ps)
    sapply(ps, knitr::image_uri)
  })
  writeLines(x, css)
  invisible()
}

# if foo.min.js exists, remove foo.js; similar thing to .css
keep_min = function(dir = dld_folder) {
  dirs = list.dirs(dir, recursive = FALSE)
  invisible(lapply(dirs, keep_min))
  x1 = list.files(dir, '[.](css|js)$', full.names = TRUE)
  x2 = gsub('[.](css|js)$', '.min.\\1', x1)
  if (length(x1) == 0) return()
  file.remove(x1[file.exists(x2)])
  invisible()
}

rm_version_number = function(dir = file.path(dld_folder, 'DataTables')) {
  dirs = list.dirs(dir, recursive = FALSE)
  pattern = '-\\d+[.]\\d+[.]\\d+$'
  dirs = dirs[grepl(pattern, dirs)]
  file.rename(dirs, gsub(pattern, '', dirs))
  invisible()
}

lib_path = function(...) {
  file.path('inst/htmlwidgets/lib', ...)
}

copy_js_css = function(from_dir, to_dir) {
  js_css_files = list.files(
    from_dir, pattern = '[.](css|js)$', recursive = TRUE
  )
  to_files = file.path(to_dir, js_css_files)
  # create the sub-folder if doesn't exist
  lapply(Filter(Negate(dir.exists), dirname(to_files)), function(dir) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  })
  file.copy(file.path(from_dir, js_css_files), to_files, overwrite = TRUE)
  invisible()
}

# clean up ----------------------------------------------------------------

# remove the version number attached in the subfolder of DataTables
rm_version_number()

# only keep min files
keep_min()

# replace the png files with base64 encode images
invisible(lapply(
  list.files(dld_folder, '[.]css$', recursive = TRUE, full.names = TRUE),
  encode_img
))

# put JSZip, pdfmake js files to Buttons because it depends on those files
local({
  jszip_files = list.files(
    dld_dt_path('JSZip'),
    pattern = '[.]js$',
    full.names = TRUE
  )
  pdfmake_files = list.files(
    dld_dt_path('pdfmake'),
    pattern = '[.]js$',
    full.names = TRUE
  )
  files = c(jszip_files, pdfmake_files)
  file.rename(
    files,
    file.path(dld_dt_path('Buttons'), basename(files))
  )
})

# copy files --------------------------------------------------------------

# copy DataTables
# dataTables.uikit.min.css, dataTables.uikit.min.js, dataTables.dataTables.min.js
# may be obsolete
# In addition, there's no license file bundled. Does this matter?
copy_js_css(dld_dt_path('DataTables'), lib_path('datatables'))

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
