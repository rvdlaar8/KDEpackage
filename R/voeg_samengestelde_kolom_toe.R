#' Voeg een samengestelde characterkolom toe aan een data.table
#'
#' Voegt een kolom toe aan een data.table op basis van opgegeven kolommen. Als geen kolommen zijn opgegeven (NULL of lengte 0),
#' wordt de kolom gevuld met `"All"`. Als er meerdere kolommen worden opgegeven, worden de waarden samengevoegd met een separator.
#' NA-afhandeling kan worden aangepast via de `na_handling`-parameter.
#'
#' @param dt Een data.table waarin de kolom moet worden toegevoegd.
#' @param v Character vector van kolomnamen om samen te voegen. Mag NULL of leeg zijn.
#' @param kolomnaam Naam van de toe te voegen kolom. Standaard `"nieuwe_kolom"`.
#' @param sep Teken waarmee kolomwaarden worden samengevoegd (alleen bij lengte > 1). Standaard `"."`.
#' @param na_handling Hoe om te gaan met NA's. Mogelijke waarden: `"keep"` (standaard), `"remove"`, `"as_string"`, `"replace"`.
#' @param na_vervanger String waarmee NA's worden vervangen indien `na_handling = "replace"`.
#'
#' @return De data.table wordt aangepast in-place (onzichtbaar geretourneerd).
#'
#'
#' @examples
#' library(data.table)
#' dt <- data.table::data.table(A = c("x", NA, "z"), B = c(1, 2, NA))
#'
#' voeg_samengestelde_kolom_toe(dt, c("A", "B"), "samen")
#' voeg_samengestelde_kolom_toe(dt, NULL, "alle_rijen")
#' voeg_samengestelde_kolom_toe(dt, "A", "alleen_A")
#' voeg_samengestelde_kolom_toe(dt, c("A", "B"), "verwijder_NA", na_handling = "remove")
#' voeg_samengestelde_kolom_toe(dt, c("A", "B"), "vervang_NA", na_handling = "replace", na_vervanger = "leeg")
#' 
#' @export
voeg_samengestelde_kolom_toe <- function(
    dt, 
    v, 
    kolomnaam = "nieuwe_kolom", 
    sep = ".", 
    na_handling = "keep",    
    na_vervanger = "<leeg>" 
) {
  stopifnot(data.table::is.data.table(dt))
  
  # kolom overschrijven indien die al bestaat
  if (kolomnaam %in% names(dt)) {
    dt[, (kolomnaam) := NULL]
  }
  
  # NULL of lege vector => "All"
  if (is.null(v) || length(v) == 0) {
    dt[, (kolomnaam) := "All"]
    
  } else if (length(v) == 1) {
    kol_waarde <- dt[[v[1]]]
    
    if (na_handling == "as_string") {
      kol_waarde[is.na(kol_waarde)] <- "NA"
    } else if (na_handling == "replace") {
      kol_waarde[is.na(kol_waarde)] <- na_vervanger
    }
    
    dt[, (kolomnaam) := as.character(kol_waarde)]
    
  } else {
    # meerdere kolommen → samenvoegen
    .SDcols_input <- v
    
    dt[, (kolomnaam) := {
      tmp <- .SD
      
      if (na_handling == "as_string") {
        tmp <- lapply(tmp, function(x) { x[is.na(x)] <- "NA"; x })
      } else if (na_handling == "replace") {
        tmp <- lapply(tmp, function(x) { x[is.na(x)] <- na_vervanger; x })
      }
      
      rows <- do.call(paste, c(tmp, sep = sep))
      
      if (na_handling == "remove") {
        rows <- apply(tmp, 1, function(x) paste(stats::na.omit(x), collapse = sep))
      }
      
      rows
    }, .SDcols = .SDcols_input]
  }
  
  invisible(dt)
}
