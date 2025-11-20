#' Functie 1 bereken exacte matches Bx var1
#' 
#' @import data.table
#' 
#' @details
#' exacte matches uit dt2 toevoegen aan dt1 (schaalbaar)
#' 
#' @export
tel_exact_match_per_rij_dt1 <- function(dt1, dt2, kol_a, kol_b) {
  stopifnot(!is.null(kol_a), !is.null(kol_b))
  stopifnot(kol_a %in% names(dt1), kol_b %in% names(dt1))
  stopifnot(kol_a %in% names(dt2), kol_b %in% names(dt2))
  
  ka <- as.name(kol_a)
  kb <- as.name(kol_b)
  
  # --- PRE-FILTER ---
  keys_ab <- unique(dt1[, .SD, .SDcols = c(kol_a, kol_b)])
  keys_a  <- unique(dt1[, .SD, .SDcols = kol_a])
  
  dt2_ab <- dt2[keys_ab, on = c(kol_a, kol_b), nomatch = 0L]
  dt2_a  <- dt2[keys_a,  on = kol_a,            nomatch = 0L]
  
  # --- AGGREGATIES ---
  count_matches <- dt2_ab[!is.na(eval(kb)),
                          .(n_match = .N),
                          by = c(kol_a, kol_b)]
  
  groep_totalen <- dt2_a[, .(
    n_a        = .N,
    n_a_not_na = sum(!is.na(eval(kb)))
  ), by = kol_a]
  
  # --- JOIN TERUG ---
  out <- count_matches[dt1, on = c(kol_a, kol_b)]
  out <- groep_totalen[out, on = kol_a]
  
  # NA’s vullen
  for (j in c("n_match","n_a","n_a_not_na")) {
    set(out, which(is.na(out[[j]])), j, 0L)
  }
  
  # Kans berekenen
  out[, paste0("P_", kol_b) :=
        fifelse(n_a_not_na > 0, n_match / n_a_not_na, 0)]
  
  out[, c("n_match","n_a","n_a_not_na", kol_a, kol_b) := NULL]
  return(out[])
}

# tel_exact_match_per_rij_dt1 <- function(dt1, dt2, kol_a, kol_b) { # fast version
#   stopifnot(!is.null(kol_a), !is.null(kol_b))
#   stopifnot(kol_a %in% names(dt1), kol_b %in% names(dt1))
#   stopifnot(kol_a %in% names(dt2), kol_b %in% names(dt2))
#   
#   # Zet kolomnamen vast als symbolen voor snelle evaluatie
#   ka <- as.name(kol_a)
#   kb <- as.name(kol_b)
#   
#   # Tellen per (a,b) → aantal matches
#   count_matches <- dt2[!is.na(eval(kb)), .(n_match = .N), by = c(kol_a, kol_b)]
#   
#   # Tellen per groep a → totalen
#   groep_totalen <- dt2[, .(
#     n_a        = .N,
#     n_a_not_na = sum(!is.na(eval(kb)))
#   ), by = kol_a]
#   
#   # Join in 1 keer terug naar dt1
#   out <- count_matches[dt1, on = c(kol_a, kol_b)]
#   out <- groep_totalen[out, on = kol_a]
#   
#   # NA’s invullen (geen matches)
#   for (j in c("n_match","n_a","n_a_not_na")) {
#     set(out, which(is.na(out[[j]])), j, 0L)
#   }
#   
#   # Kans berekenen
#   out[, paste0("P_", kol_b) := fifelse(n_a_not_na > 0, n_match / n_a_not_na, 0)]
#   
#   # Opruimen van hulpkolommen
#   out[, c("n_match","n_a","n_a_not_na", kol_a, kol_b) := NULL]
#   
#   return(out[])
# }



#' Functie 3 bereken exacte matches Bx var2
#' 
#' @details
#' exacte matches uit dt1 toevoegen aan dt2 (schaalbaar)
#' 
#' @export
tel_exact_match_per_rij_dt2 <- function(dt1, dt2, kol_a, kol_b) {
  # De rijen en rijvolgorde van dt2 moeten behouden blijven
  stopifnot(!is.null(kol_a), !is.null(kol_b))
  stopifnot(kol_a %in% names(dt1), kol_b %in% names(dt1))
  stopifnot(kol_a %in% names(dt2), kol_b %in% names(dt2))
  
  ka <- as.name(kol_a)
  kb <- as.name(kol_b)
  
  # --- PRE-FILTER ---
  keys_ab <- unique(dt2[, .SD, .SDcols = c(kol_a, kol_b)])
  keys_a  <- unique(dt2[, .SD, .SDcols = kol_a])
  
  dt1_ab <- dt1[keys_ab, on = c(kol_a, kol_b), nomatch = 0L]
  dt1_a  <- dt1[keys_a,  on = kol_a,            nomatch = 0L]
  
  # --- AGGREGATIES ---
  count_matches <- dt1_ab[!is.na(eval(kb)),
                          .(N_match = .N),
                          by = c(kol_a, kol_b)]
  
  groep_totalen <- dt2[, .(
    n_a        = .N,
    n_a_not_na = sum(!is.na(eval(kb)))
  ), by = kol_a]
  
  # --- JOIN TERUG ---
  out <- count_matches[dt2, on = c(kol_a, kol_b)]
  out <- groep_totalen[out, on = kol_a]
  
  # NA’s vullen
  for (j in c("N_match","n_a","n_a_not_na")) {
    set(out, which(is.na(out[[j]])), j, 0L)
  }
  
  # Kans berekenen
  out[, paste0("R_", kol_b) :=
        fifelse(n_a_not_na > 0, N_match / n_a_not_na, 0)]
  
  out[, c("N_match","n_a","n_a_not_na", kol_a, kol_b) := NULL]
  return(out[])
}

# tel_exact_match_per_rij_dt2 <- function(dt1, dt2, kol_a, kol_b) { # fast version
#   # dt1 en dt2 zijn kolomsubsets, var2 Bx
#   # de rijen en rijvolgorde van dt2 moeten gelijk blijven
#   stopifnot(!is.null(kol_a), !is.null(kol_b))
#   stopifnot(kol_a %in% names(dt1), kol_b %in% names(dt1))
#   stopifnot(kol_a %in% names(dt2), kol_b %in% names(dt2))
#   
#   ka <- as.name(kol_a)
#   kb <- as.name(kol_b)
#   
#   # Tellen per (a,b) uit dt1
#   count_matches <- dt1[!is.na(eval(kb)), .(N_match = .N), by = c(kol_a, kol_b)]
#   
#   # Eerst join met dt2 (behoudt de rijvolgorde van dt2)
#   out <- count_matches[dt2, on = c(kol_a, kol_b)]
#   
#   # Groep totalen per a op basis van dt2
#   groep_totalen <- dt2[, .(
#     n_a        = .N,
#     n_a_not_na = sum(!is.na(eval(kb)))
#   ), by = kol_a]
#   
#   # Join per groep a
#   out <- groep_totalen[out, on = kol_a]
#   
#   # NA’s vullen
#   for (j in c("N_match","n_a","n_a_not_na")) {
#     set(out, which(is.na(out[[j]])), j, 0L)
#   }
#   
#   # Kans berekenen
#   out[, paste0("R_", kol_b) := fifelse(n_a_not_na > 0, N_match / n_a_not_na, 0)]
#   
#   # Opruimen
#   out[, c("N_match","n_a","n_a_not_na", kol_a, kol_b) := NULL]
#   
#   # Rijvolgorde van dt2 is gegarandeerd behouden
#   return(out[])
# }


