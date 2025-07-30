#' Schat risico Bx uit dt2
#'
#' Deze functie schat per rij in `dt2` het verwachte aantal `b1`-waarden in dt1 dat overeenkomt
#' met de waarden in `b2` in dt2, op basis van exacte matches binnen aggregaties.
#'
#' @param dt2 Een data.table met targetrecords en kolommen a, b, a_cl en b_cl.
#' @param ag_a Een data.table met kolommen a en kol_N, met per groep het aantal rijen in dt1.
#' @param kol_N Naam van kolom in ag_a met aantallen in dt1.
#' @param kol_a Groepskolom in dt2 (komt overeen met kolom in ag_a).
#' @param kol_b Kolom waarvoor risico wordt geschat.
#' @param kol_a_cl Groepkolom in dt2 voor de dichtheidschatting (vaak aangepaste a).
#' @param kol_b_cl Kolom in dt2 waarvoor matching wordt gedaan binnen kol_a_cl.
#' 
#' @return data.table met kolom "estimated_n_<b>" met risico per rij, geschikt voor cbind met oorspronkelijke dt2.
#' @import data.table
#' @export
#'
#' @examples
#' dt2 <- data.table::data.table(a = c("x", "x", "y"),
#'                   b = c(1, 1, 2),
#'                   a_cl = c("x", "x", "y"),
#'                   b_cl = c(1, 2, 2))
#' ag_a <- data.table::data.table(a = c("x", "y"), N_group = c(100L, 200L))
#' estimate_exact_match_per_rij_b2(dt2, ag_a, kol_N = "N_group",
#'                                 kol_a = "a", kol_b = "b",
#'                                 kol_a_cl = "a_cl", kol_b_cl = "b_cl")

estimate_exact_match_per_rij_b2 <- function(dt2, ag_a, kol_N,
                                            kol_a, kol_b,
                                            kol_a_cl, kol_b_cl) {
  stopifnot(is.data.table(dt2), is.data.table(ag_a))
  stopifnot(all(c(kol_a, kol_a_cl, kol_b, kol_b_cl) %in% names(dt2)))
  stopifnot(all(c(kol_a, kol_N) %in% names(ag_a)))

  n_orig <- nrow(dt2)
  dt2[, rowid := .I]
  
  # Aggregaten op originele kolommen
  ag_dt2_a <- dt2[!is.na(get(kol_b)), .(n_a_bnot_na = .N), by = kol_a] # excl NA
  #ag_dt2_ab <- dt2[!is.na(get(kol_b)), .(n_ab_not_na = .N), by = c(kol_a, kol_b)] # niet nodig
  
  # Aggregaten op '_cl' kolommen
  ag_dt2_a_cl <- dt2[, .(n_a_cl = .N), by = kol_a_cl] # incl NA
  ag_dt2_ab_cl <- dt2[!is.na(get(kol_b_cl)), .(n_ab_cl_bnot_na = .N), by = c(kol_a_cl, kol_b_cl)]
  
  # Koppelen aan dt2
  dt2 <- ag_a[dt2, on = kol_a]
  dt2 <- ag_dt2_a[dt2, on = kol_a]
  #dt2 <- ag_dt2_ab[dt2, on = c(kol_a, kol_b)]
  
  setnames(ag_dt2_a_cl, kol_a_cl, kol_a)
  setnames(ag_dt2_ab_cl, c(kol_a_cl, kol_b_cl), c(kol_a, kol_b))
  
  dt2 <- ag_dt2_a_cl[dt2, on = kol_a]
  dt2 <- ag_dt2_ab_cl[dt2, on = c(kol_a, kol_b)]
  
  # Risico schatting per rij
  dt2[, estimated_n_b1 := get(kol_N) * n_ab_cl_bnot_na/n_a_cl / n_a_bnot_na ]
  # * n_ab_not_na / n_a_not_na 
  dt2[is.na(estimated_n_b1), estimated_n_b1 := 0]
  
  stopifnot(all(dt2$rowid == 1:nrow(dt2)))
  setnames(dt2, "estimated_n_b1", paste0("S_", kol_b))
  
  dt2[, c(kol_N, "n_ab_cl_bnot_na", "n_a_cl", "n_a_bnot_na") := NULL] # , "n_ab_not_na"
  dt2[, c(kol_a, kol_b, kol_a_cl, kol_b_cl, "rowid") := NULL]
  
  if (nrow(dt2) > n_orig) warning("Row explosion: join created extra rows.")
  
  return(dt2[])
}
