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
  
  ka <- as.name(kol_a)
  kb <- as.name(kol_b)
  kb_cl <- as.name(kol_b_cl)
  
  # --- PRE-FILTERS ---
  keys_ab   <- unique(dt2[, .SD, .SDcols = c(kol_a, kol_b)])
  keys_a    <- unique(dt2[, .SD, .SDcols = kol_a])
  keys_abcl <- unique(dt2[!is.na(eval(kb_cl)), .SD,
                          .SDcols = c(kol_a_cl, kol_b_cl)])
  keys_acl  <- unique(dt2[, .SD, .SDcols = kol_a_cl])
  
  dt2_ab   <- dt2[keys_ab,   on = c(kol_a, kol_b),     nomatch = 0L]
  dt2_a    <- dt2[keys_a,    on = kol_a,               nomatch = 0L]
  dt2_abcl <- dt2[keys_abcl, on = c(kol_a_cl, kol_b_cl), nomatch = 0L]
  dt2_acl  <- dt2[keys_acl,  on = kol_a_cl,            nomatch = 0L]
  
  # --- AGGREGATIES ---
  ag_dt2_a     <- dt2_ab[!is.na(eval(kb)), .(n_a_bnot_na = .N), by = kol_a]
  ag_dt2_a_cl  <- dt2_acl[, .(n_a_cl = .N), by = kol_a_cl]
  ag_dt2_ab_cl <- dt2_abcl[!is.na(eval(kb_cl)),
                           .(n_ab_cl_bnot_na = .N),
                           by = c(kol_a_cl, kol_b_cl)]
  
  # Zet kolomnamen gelijk
  setnames(ag_dt2_a_cl,  kol_a_cl, kol_a)
  setnames(ag_dt2_ab_cl, c(kol_a_cl, kol_b_cl), c(kol_a, kol_b))
  
  # --- JOIN TERUG ---
  out <- ag_a[dt2, on = kol_a]
  out <- ag_dt2_a[out, on = kol_a]
  out <- ag_dt2_a_cl[out, on = kol_a]
  out <- ag_dt2_ab_cl[out, on = c(kol_a, kol_b)]
  
  # Risico schatting
  out[, estimated_n_b1 :=
        fifelse(n_a_bnot_na > 0 & n_a_cl > 0,
                out[[kol_N]] * n_ab_cl_bnot_na / n_a_cl / n_a_bnot_na,
                0)]
  
  set(out, which(is.na(out$estimated_n_b1)), "estimated_n_b1", 0)
  stopifnot(all(out$rowid == 1:nrow(out)))
  
  setnames(out, "estimated_n_b1", paste0("S_", kol_b))
  out[, c(kol_N, "n_ab_cl_bnot_na", "n_a_cl", "n_a_bnot_na",
          kol_a, kol_b, kol_a_cl, kol_b_cl, "rowid") := NULL]
  
  if (nrow(out) > n_orig)
    warning("Row explosion: join created extra rows.")
  
  return(out[])
}

