## import and export functions from other packages; also automatically generate
## Rd for them to avoid R CMD check NOTE's

importRd = function(names, package) {
  for (name in names) suppressMessages(utils::promptImport(
    NULL, name = gsub('%', '\\\\%', name), importedFrom = package,
    filename = tempfile('import', '.', '.Rd')
  ))
}

#' @importFrom htmlwidgets JS
#' @export JS
#' @importFrom magrittr %>%
#' @export %>%
if (file_test('-d', 'man')) local({
  owd = setwd('man'); on.exit(setwd(owd))
  importRd('JS', 'htmlwidgets')
  importRd('%>%', 'magrittr')
})

rm(importRd)
