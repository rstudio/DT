## import and export functions from other packages; also automatically generate
## Rd for them to avoid R CMD check NOTE's

importRd = function(names, package) {
  library(package, character.only = TRUE)
  for (name in names) suppressMessages(utils::promptImport(
    name, name = name, importedFrom = package
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
