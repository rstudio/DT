for (f in list.files('.', '[.]R?md$')) {
  xfun::Rscript(c('compile.R', shQuote(f)))
}

sys.source('fragmentize.R', environment())
