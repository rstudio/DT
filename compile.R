set.seed(0102)
options(htmlwidgets.TOJSON_ARGS = list(pretty = TRUE), htmltools.dir.version = FALSE)
Sys.setenv(R_KNITR_OPTIONS = 'knitr.chunk.tidy=FALSE')
library(DT)
o = rmarkdown::render(commandArgs(TRUE), quiet = TRUE)
x = gsub('\\u003c', '<', readLines(o), fixed = TRUE)
x = gsub('<\\\\(/[a-z1-6]+>)', '<\\1', x)
x = blogdown:::clean_widget_html(x)
x = bookdown:::clean_pandoc2_highlight_tags(x)
writeLines(x, o)
unlink(list.files('.', '[.]map$', recursive = TRUE))
unlink(c(
  'libs/bootstrap/js/npm.js', 'libs/bootstrap/js/bootstrap.js',
  'libs/bootstrap/css/bootstrap.css', 'libs/bootstrap/css/bootstrap.min.css',
  'libs/bootstrap/css/bootstrap-theme.css', 'libs/bootstrap/css/bootstrap-theme.min.css',
  sprintf(
    'libs/bootstrap/css/%s.min.css',
    c('cerulean', 'cosmo', 'journal', 'lumen', 'paper', 'readable', 'sandstone', 'simplex', 'spacelab', 'united', 'yeti')
  )
))
