#' Functie 1 bereken exacte matches Bx var1
#' 
#' @import data.table
#' 
#' @details
#' exacte matches uit dt2 toevoegen aan dt1 (schaalbaar)
#' 
#' @export
tel_exact_match_per_rij_dt1 <- function(dt1, dt2, kol_a, kol_b) {
  # dt1 en dt2 zijn kolomsubsets, var1 Bx
  stopifnot(!is.null(kol_a), !is.null(kol_b))
  stopifnot(kol_a %in% names(dt1), kol_b %in% names(dt1))
  stopifnot(kol_a %in% names(dt2), kol_b %in% names(dt2))
  
  kol_groep <- c(kol_a,kol_b)
  count_matches <- dt2[!is.na(get(kol_b)), .(n_match = .N), 
                       by = c(kol_groep)]
  
  setkeyv(count_matches,kol_groep)
  out <- count_matches[dt1, on = kol_groep]
  
  groep_totalen <- dt2[, .(
    n_a = .N,
    n_a_not_na = sum(!is.na(get(kol_b)))
  ), by = kol_a]
  
  setkeyv(groep_totalen,kol_a)
  out <- groep_totalen[out, on = kol_a]

  # NA’s vullen (geen matches)
  for (j in c("n_match","n_a","n_a_not_na")) set(out, which(is.na(out[[j]])), j, 0L)
  
  out[,P_b := n_match/n_a_not_na]
  out[is.na(P_b), P_b := 0]
  setnames(out, "P_b", paste0("P_", kol_b))
  
  out[, c(kol_a, kol_b) := NULL]
  out[, c("n_a", "n_a_not_na", "n_match") := NULL]
  
  return(out[])
}


#' Functie 3 bereken exacte matches Bx var2
#' 
#' @details
#' exacte matches uit dt1 toevoegen aan dt2 (schaalbaar)
#' 
#' @export
tel_exact_match_per_rij_dt2 <- function(dt1, dt2, kol_a, kol_b) {
  # dt1 en dt2 zijn kolom subsets, var2 Bx
  # de rijen en rijvolgorde van dt2 moeten gelijk blijven
  stopifnot(!is.null(kol_a), !is.null(kol_b))
  stopifnot(kol_a %in% names(dt1), kol_b %in% names(dt1))
  stopifnot(kol_a %in% names(dt2), kol_b %in% names(dt2))
  
  kol_groep <- c(kol_a,kol_b)
  count_matches <- dt1[!is.na(get(kol_b)), .(N_match = .N), 
                       by = c(kol_groep)]
  # dt2 <- merge(dt2, count_matches, 
  #              by = c(kol_groep), all.x = TRUE, sort = FALSE)
  setkeyv(count_matches,kol_groep)
  out <- count_matches[dt2, on = kol_groep]
  
  groep_totalen <- dt2[, .(
    n_a = .N,
    n_a_not_na = sum(!is.na(get(kol_b)))
  ), by = kol_a]
  
  setkeyv(groep_totalen,kol_a)
  out <- groep_totalen[out, on = kol_a]
  
  # NA’s vullen (geen matches)
  for (j in c("N_match","n_a","n_a_not_na")) set(out, which(is.na(out[[j]])), j, 0L)
  # trager:
  # out[is.na(n_match), n_match := 0L]
  # out[is.na(n_a), n_a := 0L]
  # out[is.na(n_a_not_na), n_a_not_na := 0L]
  
  out[,R_b := N_match/n_a_not_na]
  out[is.na(R_b), R_b := 0]
  setnames(out, "R_b", paste0("R_", kol_b))
  
  out[, c(kol_a, kol_b) := NULL]
  out[, c("n_a", "n_a_not_na", "N_match") := NULL]
  
  return(out[])
}








