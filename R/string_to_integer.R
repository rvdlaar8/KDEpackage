#' Maak een gemeenschappelijke integermapping van stringwaarden
#'
#' Deze functie genereert een `data.table` die een unieke mapping bevat van stringwaarden uit kolommen in `dt1` en `dt2`
#' naar een geheel getal. Optioneel wordt `NA` ook gemapt.
#'
#' @param dt1 Eerste `data.table` met kolommen `cols1`
#' @param cols1 Vector van kolomnamen in `dt1` (karakter)
#' @param dt2 Tweede `data.table` met kolommen `cols2`
#' @param cols2 Vector van kolomnamen in `dt2` (karakter)
#'
#' @return `data.table` met kolommen `orig` (originele waarden) en `int` (bijbehorende integercodes)
#' @export
maak_gemeenschappelijke_mapping <- function(dt1, cols1, dt2, cols2) {
  stopifnot(is.character(cols1), is.character(cols2))
  stopifnot(all(cols1 %in% names(dt1)), all(cols2 %in% names(dt2)))
  
  v1 <- unique(unlist(dt1[, ..cols1], use.names = FALSE))
  v2 <- unique(unlist(dt2[, ..cols2], use.names = FALSE))
  
  alle_uniek <- unique(c(as.character(v1), as.character(v2)))
  
  groepen_zonder_na <- alle_uniek[!is.na(alle_uniek)]
  bevat_na <- any(is.na(alle_uniek))
  
  map_dt <- data.table::data.table(
    orig = c(groepen_zonder_na, if (bevat_na) NA_character_),
    int = seq_len(length(groepen_zonder_na) + bevat_na)
  )
  
  return(map_dt)
}

#' Vervang meerdere stringkolommen in twee `data.tables` met gemeenschappelijke integercodes
#'
#' Genereert een mapping met `maak_gemeenschappelijke_mapping()` en vervangt stringwaarden in `dt1` en `dt2`
#' door integers.
#'
#' @param dt1 Eerste `data.table`
#' @param cols1 Vector van kolomnamen in `dt1` om te vervangen
#' @param dt2 Tweede `data.table`
#' @param cols2 Vector van kolomnamen in `dt2` om te vervangen
#'
#' @return Een `data.table` met kolommen `orig` en `int` (de gebruikte mapping)
#' @export
vervang_meerdere_stringkolommen <- function(dt1, cols1, dt2, cols2) {
  stopifnot(length(cols1) > 0, length(cols2) > 0)
  
  map_dt <- maak_gemeenschappelijke_mapping(dt1, cols1, dt2, cols2)
  groepen_zonder_na <- map_dt[!is.na(orig), orig]
  na_code <- map_dt[is.na(orig), int]
  
  for (col in cols1) {
    v <- as.character(dt1[[col]])
    m <- match(v, groepen_zonder_na)
    if (length(na_code) > 0) m[is.na(m)] <- na_code
    dt1[, (col) := m]
  }
  
  for (col in cols2) {
    v <- as.character(dt2[[col]])
    m <- match(v, groepen_zonder_na)
    if (length(na_code) > 0) m[is.na(m)] <- na_code
    dt2[, (col) := m]
  }
  
  return(map_dt)
}

#' Herstel integergecodeerde kolommen naar oorspronkelijke stringwaarden
#'
#' Deze functie converteert integerwaarden in de opgegeven kolommen van `dt` terug naar strings,
#' gebruikmakend van `map_dt` zoals gegenereerd door `maak_gemeenschappelijke_mapping()`.
#'
#' @param dt Een `data.table` met integergecodeerde kolommen
#' @param kolommen Vector van kolomnamen in `dt` die hersteld moeten worden
#' @param map_dt Mapping `data.table` met kolommen `orig` en `int`
#'
#' @return Geen returnwaarde; `dt` wordt by reference aangepast
#' @export
herstel_meerdere_integerkolommen <- function(dt, kolommen, map_dt) {
  herstel_map <- setNames(map_dt$orig, as.character(map_dt$int))
  
  for (col in kolommen) {
    v <- as.character(dt[[col]])
    res <- herstel_map[v]
    res[is.na(v)] <- NA_character_
    dt[, (col) := res]
  }
}

#' Vervang stringwaarden in kolommen van een `data.table` op basis van bestaande mapping
#'
#' @param dt1 Een `data.table` waarvan kolommen vervangen worden
#' @param cols1 Vector van kolomnamen in `dt1` die vervangen worden
#' @param map_dt Mapping `data.table` met kolommen `orig` en `int`
#'
#' @return Geen returnwaarde; `dt1` wordt by reference aangepast
#' @export
vervang_stringkolommen_met_bestaande_mapping <- function(dt1, cols1, map_dt) {
  vervang_map <- setNames(map_dt$int, map_dt$orig)
  na_code <- map_dt$int[is.na(map_dt$orig)]
  
  vervang_kolom <- function(dt, kol) {
    v <- as.character(dt[[kol]])
    res <- vervang_map[v]
    if (length(na_code) > 0) res[is.na(v)] <- na_code
    dt[, (kol) := res]
  }
  
  for (kol in cols1) vervang_kolom(dt1, kol)
}
