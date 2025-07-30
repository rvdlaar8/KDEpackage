#' Genereer test data.table met *_cl kolommen en kleine afwijking
#'
#' Deze functie maakt een data.table met integer kolommen a1/a2/a3, b1 (en *_cl versies) en numerieke kolommen c1/c1_cl.
#' Afwijkingen worden bepaald door opgegeven deviatieparameters. Kleine deviatie geeft vooral exact gelijke waarden.
#' NA's kunnen willekeurig worden toegevoegd met opgegeven fracties.
#'
#' @param n Aantal rijen (default = 100).
#' @param a_dev Kans op afwijking (0–1) tussen a1/a2/a3 en hun *_cl (default = 0).
#' @param b_dev Kans op afwijking (0–1) tussen b1 en b1_cl (default = 0).
#' @param c_dev Standaarddeviatie voor normale afwijking tussen c1 en c1_cl (default = 0).
#' @param a_na_frac Fractie NA's in a1, a2, a3 (default = 0).
#' @param b_na_frac Fractie NA's in b1 (default = 0).
#' @param c_na_frac Fractie NA's in c1 (default = 0).
#'
#' @return Een data.table met kolommen: id, a1, a2, a3, b1, c1, en *_cl versies.
#'
#' @import data.table
#'
#' @examples
#' dt <- genereer_test_dt_met_cl(n = 1000, a_dev = 0.1, b_dev = 0.3, c_dev = 0.5)
#' 
#' @export
genereer_test_dt_met_cl <- function(n = 100,
                                    a_dev = 0,
                                    b_dev = 0,
                                    c_dev = 0,
                                    a_na_frac = 0,
                                    b_na_frac = 0,
                                    c_na_frac = 0) {
  stopifnot(
    is.numeric(n) && n > 0,
    between(a_dev, 0, 1),
    between(b_dev, 0, 1),
    is.numeric(c_dev) && c_dev >= 0,
    between(a_na_frac, 0, 1),
    between(b_na_frac, 0, 1),
    between(c_na_frac, 0, 1)
  )
  
  set.seed(1)
  
  # Integer domeinen
  a_rng <- list(a1 = c(5, 15), a2 = c(15, 25), a3 = c(25, 35))
  b_rng <- c(80, 120)
  
  dt <- data.table(id = 1:n)
  
  # *_cl integers
  dt[, a1_cl := sample(seq(a_rng$a1[1], a_rng$a1[2]), n, replace = TRUE)]
  dt[, a2_cl := sample(seq(a_rng$a2[1], a_rng$a2[2]), n, replace = TRUE)]
  dt[, a3_cl := sample(seq(a_rng$a3[1], a_rng$a3[2]), n, replace = TRUE)]
  dt[, b1_cl := sample(seq(b_rng[1], b_rng[2]), n, replace = TRUE)]
  dt[, c1_cl := rnorm(n, mean = 10, sd = 1)]
  
  # Afgeleiden met kans op afwijking (binomiaal) → random int in domein ≠ *_cl
  vervang_als_dev <- function(cl_col, rng, p_dev) {
    alt <- function(x, rng) {
      vals <- setdiff(seq(rng[1], rng[2]), x)
      if (length(vals) == 0) return(x)
      sample(vals, 1)
    }
    sapply(cl_col, function(x) {
      if (runif(1) < p_dev) alt(x, rng) else x
    })
  }
  
  dt[, a1 := vervang_als_dev(a1_cl, a_rng$a1, a_dev)]
  dt[, a2 := vervang_als_dev(a2_cl, a_rng$a2, a_dev)]
  dt[, a3 := vervang_als_dev(a3_cl, a_rng$a3, a_dev)]
  
  dt[, b1 := vervang_als_dev(b1_cl, b_rng, b_dev)]
  dt[, c1 := c1_cl + stats::rnorm(n, mean = 0, sd = c_dev)]
  
  # NA's
  maak_na <- function(x, frac) {
    idx <- sample.int(length(x), size = round(frac * length(x)))
    x[idx] <- NA
    x
  }
  
  dt[, a1 := maak_na(a1, a_na_frac)]
  dt[, a2 := maak_na(a2, a_na_frac)]
  dt[, a3 := maak_na(a3, a_na_frac)]
  dt[, b1 := maak_na(b1, b_na_frac)]
  dt[, c1 := maak_na(c1, c_na_frac)]
  
  return(dt[])
}