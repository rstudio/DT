for (i in c('css', 'js')) {
  unlink(i, recursive = TRUE); dir.create(i)
}

collection = NULL

for (f in list.files('.', '[.]html$')) {

  fragmentize = function(html, r_match, r_remove, path, tag) {
    m = gregexpr(r_match, html)
    regmatches(html, m) = lapply(regmatches(html, m), function(x) {
      x = gsub(r_remove, '', x)
      z = character(length(x))
      i = match(x, collection)
      for (j in seq_along(x)) {
        if (no <- is.na(i[j])) {
          collection <<- c(collection, x[j])
          i[j] = length(collection)
        }
        p = sprintf(path, i[j])
        z[j] = sprintf(tag, p)
        if (no) xfun::write_utf8(x[j], p)
      }
      z
    })
    html
  }

  html = xfun::file_string(f)
  html = fragmentize(
    html, '<style type\\s*=\\s*"text/css">[^<]+</style>', '^<style[^>]+>|</style>$',
    'css/style-%s.css', '<link rel="stylesheet" href="%s" type="text/css" />'
  )
  html = fragmentize(
    html, '<script>[^<]+</script>', '^<script>|</script>$',
    'js/script-%s.js', '<script src="%s"></script>'
  )

  xfun::write_utf8(html, f)
}
