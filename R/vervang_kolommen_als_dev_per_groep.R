#' Vervang kolomwaarden per groep op basis van verdeling binnen groep
#'
#' @param dt Een data.table
#' @param kol_a Groepskolomnaam (string)
#' @param cols Vector van kolomnamen die aangepast worden
#' @param p_dev Kans om een waarde te vervangen (0–1)
#' @param p_frac Kans om te vervangen door NA in plaats van bestaande waarde (0–1)
#' @param suffix Suffix voor de nieuwe kolommen (standaard "_x")
#' @export
vervang_kolommen_als_dev_per_groep <- function(dt, kol_a, cols, p_dev, p_frac, suffix = "_x") {
  stopifnot(is.data.table(dt))
  stopifnot(is.character(cols), length(cols) > 0)
  stopifnot(is.character(kol_a), length(kol_a) == 1)
  stopifnot(all(cols %in% names(dt)), kol_a %in% names(dt))
  
  for (col in cols) {
    nieuwe_col <- paste0(col, suffix)
    
    # Maak kopie van originele kolom
    dt[, (nieuwe_col) := get(col)]
    
    # Bepaal rijen die mogelijk gewijzigd worden
    wijzig_mask <- runif(nrow(dt)) < p_dev

    if (any(wijzig_mask, na.rm = TRUE)) {
      idxs <- which(wijzig_mask)
      # cat("\nte wijzigen:",length(idxs))
      
      # Splits op groepen
      groep_splits <- split(idxs, dt[[kol_a]][idxs])
      
      # Loop over groepen
      for (groep in names(groep_splits)) {
        rijen <- groep_splits[[groep]]
        
        # Filter rijen in groep
        #groepdata <- dt[get(kol_a) == groep, ..col]
        groepdata <- dt[rijen, ..col]
        geldige_waarden <- na.omit(groepdata[[1]])
        
        if (length(geldige_waarden) == 0L) next
        
        # Genereer vervangende waarden
        nieuwe_waarden <- sample(geldige_waarden, size = length(rijen), replace = TRUE)
        
        # Toepassen vervanging
        dt[rijen, (nieuwe_col) := nieuwe_waarden]
      }
      # if (col == "cl1") cat("\ngewijzigd cl1_x",nrow(dt[cl1 != cl1_x]))
      # if (col == "cl2") cat("\ngewijzigd cl2_x",nrow(dt[cl2 != cl2_x]))
    }
    
    # Fractie vervangen door NA
    na_mask <- runif(nrow(dt)) < p_frac
    idxs_na <-  which(na_mask)
    if (length(idxs_na)) {
      dt[idxs_na, (nieuwe_col) := NA]
    }
  }
  # cat("\ngewijzigd cl1_x",nrow(dt[cl1 != cl1_x]))
  # cat("\ngewijzigd cl2_x",nrow(dt[cl2 != cl2_x]))
  invisible(dt)
}

# library(data.table)
# set.seed(123)
# dt <- data.table(
#   kol_a = sample(letters[1:3], 2000, replace = TRUE),
#   cl1 = sample(1:5, 2000, replace = TRUE),
#   cl2 = sample(6:10, 2000, replace = TRUE)
# )
# 
# 
# vervang_kolommen_als_dev_per_groep(
#   dt,
#   kol_a = "kol_a",
#   cols = c("cl1", "cl2"),
#   p_dev = 0.5,
#   p_frac = 0.01,
#   suffix = "_x"
# )
# 
# dt
# nrow(dt[cl1 != cl1_x,]) # 175 waarom zo laag?
# nrow(dt[cl2 != cl2_x,]) # 165
# 
# nrow(dt[is.na(cl1_x),]) # 1603 ok
# nrow(dt[is.na(cl2_x),]) # 1585 ok
# 
# nrow(dt[is.na(cl1),]) # 0
# nrow(dt[is.na(cl2),]) # 0
# 
# print(dt)