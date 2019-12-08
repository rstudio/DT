
# todo --------------------------------------------------------------------

# 1. automate the download - maybe no need
# 2. the datatables license file is not in the bundle anymore - do we really
#    need that?
# 3. should not update jquery.highlight.js because it's now manually maintained

# note --------------------------------------------------------------------

# This script is going to update
# 1. datatables and its extentions' css and js files
# 2. datatables' plugins' js files

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

# utils -------------------------------------------------------------------

dld_folder = function() {
  './download'
}

dld_dt_path = function(...) {
  file.path(dld_folder(), 'DataTables', ...)
}

dld_plugin_path = function(...) {
  file.path(dld_folder(), 'Plugins', ...)
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
keep_min = function(dir) {
  dirs = list.dirs(dir, recursive = FALSE)
  invisible(lapply(dirs, keep_min))
  x1 = list.files(dir, '[.](css|js)$', full.names = TRUE)
  x2 = gsub('[.](css|js)$', '.min.\\1', x1)
  if (length(x1) == 0) return()
  file.remove(x1[file.exists(x2)])
  invisible()
}

# sometimes the bundle downloaded from datatables.net contains empty
# files (seems caused by the downloading set-up error) so we just
# remove those
rm_empty_files = function(dir) {
  files = list.files(dir, recursive = TRUE, full.names = TRUE)
  empty_files = files[file.size(files) == 0]
  unlink(empty_files)
}

rm_version_number = function(dir) {
  dirs = list.dirs(dir, recursive = FALSE)
  pattern = '-\\d+[.]\\d+[.]\\d+$'
  dirs = dirs[grepl(pattern, dirs)]
  file.rename(dirs, gsub(pattern, '', dirs))
  invisible()
}

lib_path = function(...) {
  file.path('inst/htmlwidgets/lib', ...)
}

lib_ext_path = function(...) {
  lib_path('datatables-extensions', ...)
}

lib_plugin_path = function(...) {
  lib_path('datatables-plugins', ...)
}

copy_js_css_swf = function(from_dir, to_dir) {
  js_css_files = list.files(
    from_dir, pattern = '[.](css|js|swf)$', recursive = TRUE
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

# remove the empty files if exists
rm_empty_files(dld_folder())

# only keep min files
keep_min(dld_folder())

# replace the png files with base64 encode images
invisible(lapply(
  list.files(dld_folder(), '[.]css$', recursive = TRUE, full.names = TRUE),
  encode_img
))

# must be placed after `encode_img` because the css files may contain
# images like "DataTables-1.10.20/images/sort_both.png"
# remove the version number attached in the subfolder of DataTables
rm_version_number(dld_dt_path())

# put JSZip, pdfmake js files to Buttons because it depends on those files
# but those files are placed separately from Buttons
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
    file.path(dld_dt_path('Buttons', 'js'), basename(files))
  )
  # so that all other folders except DataTables are extensions
  unlink(c(dld_dt_path('JSZip'), dld_dt_path('pdfmake')), recursive = TRUE)
})

# put all the plugins under a folder with the same name if it only consists
# a single js file
local({
  folders = list.dirs(dld_plugin_path(), recursive = FALSE)
  create_folder_and_move = function(js_file, folder) {
    dir = file.path(folder, gsub('[.]js$', '', js_file))
    dir.create(dir)
    file.rename(file.path(folder, js_file), file.path(dir, js_file))
  }
  lapply(folders, function(folder) {
    js_files = list.files(folder, pattern = '[.]js$')
    lapply(js_files, create_folder_and_move, folder = folder)
  })
  invisible()
})

# copy files --------------------------------------------------------------

# update DataTables
# dataTables.uikit.min.css, dataTables.uikit.min.js, dataTables.dataTables.min.js
# may be obsolete
# In addition, there's no license file bundled. Does this matter?
copy_js_css_swf(dld_dt_path('DataTables'), lib_path('datatables'))

# update extensions
local({
  # the only not-extension folders are jszip and pdfmake, which should have
  # been deleted in the above steps
  exts = list.dirs(dld_dt_path(), recursive = FALSE, full.names = FALSE)
  exts = setdiff(exts, 'DataTables')
  invisible(lapply(exts, function(ext) {
    copy_js_css_swf(dld_dt_path(ext), lib_ext_path(ext))
  }))
})

# update plugins whose dependency is only one js file
local({
  plugins = names(DT:::available_plugins())
  lapply(plugins, function(plugin) {
    copy_js_css_swf(
      dld_plugin_path(plugin),
      lib_plugin_path(plugin)
    )
  })
  invisible()
})

# clean up download folder
unlink(dld_folder(), recursive = TRUE)
