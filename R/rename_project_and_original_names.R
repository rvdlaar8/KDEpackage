#' Hernoem kolomnamen van originele naam naar projectnaam
#'
#' @param vars Character vector van kolomnamen
#' @param naammap Named character vector, waarbij `names(naammap)` originele namen zijn,
#' en `naammap` de corresponderende projectnamen
#'
#' @return Character vector met hernoemde kolommen
#' @examples
#' naammap <- c(LFT = "leeftijd", WT = "gewicht")
#' rename_to_project_names(c("leeftijd", "gewicht"), naammap)
#' 
#' @export
rename_to_project_names <- function(vars, naammap) {
  omgekeerd <- setNames(names(naammap), naammap)
  replace <- intersect(vars, names(omgekeerd))
  vars[match(replace, vars)] <- omgekeerd[replace]
  vars
}

#' Hernoem kolomnamen van projectnaam naar originele naam
#'
#' @param vars Character vector van kolomnamen
#' @param naammap Named character vector, waarbij `names(naammap)` originele namen zijn,
#' en `naammap` de corresponderende projectnamen
#'
#' @return Character vector met hernoemde kolommen
#' @examples
#' naammap <- c(LFT = "leeftijd", WT = "gewicht")
#' rename_to_original_names(c("LFT", "WT"), naammap)
#' 
#' @export
rename_to_original_names <- function(vars, naammap) {
  replace <- intersect(vars, names(naammap))
  vars[match(replace, vars)] <- naammap[replace]
  vars
}
