#' get_interval_bounds
#' 
#' @details
#' get_interval_bounds
#' 
#' @export
get_interval_bounds <- function(x, m, mode) {
  if (is.na(x)) return(c(NA_real_, NA_real_))
  if (mode == "divide") {
    if (x >= 0) return(c(x / (1 + m), x / (1 - m)))
    else        return(c(x / (1 - m), x / (1 + m)))
  } else if (mode == "multiply") {
    if (x >= 0) return(c(x * (1 - m), x * (1 + m)))
    else        return(c(x * (1 + m), x * (1 - m)))
  } else {
    stop("interval_mode moet 'divide' of 'multiply' zijn.")
  }
}

#' bereken_matches_en_stats 
#' 
#' @details
#' bereken_matches_en_stats
#' 
#' @export
bereken_matches_en_stats <- function(dt1, dt2, kol_a, kol_x1, kol_x2,
                                     m = 0.01, grid_mode,
                                     k_auto_exact_threshold = 1000,
                                     interval_mode) {
  stopifnot(is.data.table(dt1), is.data.table(dt2))

  gemeenschappelijk <- intersect(unique(dt1[[kol_a]]), unique(dt2[[kol_a]]))
  
  tellingen_dt2 <- dt2[, .(
    n_x2_total  = .N,
    n_x2_not_NA = sum(!is.na(.SD[[1L]]))
  ), by = kol_a, .SDcols = kol_x2]
  
  tellingen_dt1 <- dt1[, .(
    n_x1_total  = .N,
    n_x1_not_NA = sum(!is.na(.SD[[1L]]))
  ), by = kol_a, .SDcols = kol_x1]
  
  # Merge tellingen in dt1
  dt1 <- merge(dt1, tellingen_dt2, by = kol_a, all.x = TRUE)
  dt1 <- merge(dt1, tellingen_dt1, by = kol_a, all.x = TRUE)
  dt1[is.na(n_x2_total), n_x2_total := 0L]
  dt1[is.na(n_x2_not_NA), n_x2_not_NA := 0L]
  dt1[, n_x2_in_range := 0L]
  
  # Interne temp naam
  kol_a_temp <- "kol_a_temp"
  setnames(dt1, kol_a, kol_a_temp)
  setnames(dt2, kol_a, kol_a_temp)
  
  dt1_g <- dt1[kol_a_temp %in% gemeenschappelijk]
  dt2_g <- dt2[kol_a_temp %in% gemeenschappelijk]
  
  # Bereken n_x2_in_range per rij via groeps-ID .I
  dt_result <- dt1_g[, {
    idx_local <- .I
    x2_vals <- dt2_g[kol_a_temp == .BY[[1]], dt2_g[[kol_x2]]]
    #of: x2_vals <- dt2_g[kol_a_temp == .BY[[1]], .SD[[1L]], .SDcols = kol_x2]
    steps <- adaptive_steps_dt(
      x2_vals, m = m, grid_mode = grid_mode,
      k_auto_exact_threshold = k_auto_exact_threshold,
      interval_mode = interval_mode  # toegevoegd
    )
    #x1_vals <- get(kol_x1)
    x1_vals <- dt1_g[[kol_x1]]
    
    if (grid_mode == "exact") {
      n_match <- sapply(x1_vals, function(x1) {
        bounds <- get_interval_bounds(x1, m, mode = interval_mode)
        sum(x2_vals >= bounds[1] & x2_vals <= bounds[2], na.rm = TRUE)
      })
    } else {
      steps <- n_x2_per_step(steps, x2_vals)
      n_match <- sapply(x1_vals, function(x1) {
        bounds <- get_interval_bounds(x1, m, mode = interval_mode)
        idx <- which(steps$lower <= bounds[2] & steps$upper >= bounds[1])
        if (length(idx)) max(steps$n_x2[idx]) else 0L
      })
    }
    
    .(row_idx = idx_local, n_x2_in_range = n_match)
  }, by = kol_a_temp]
  
  # Terugzetten kolomnaam
  setnames(dt1, kol_a_temp, kol_a)
  setnames(dt2, kol_a_temp, kol_a)
  setnames(dt_result, "row_idx", "row_id__")
  
  # Voeg per rij n_x2_in_range toe via .I-index
  dt1[, row_id__ := .I]
  dt1 <- dt1[dt_result, on = "row_id__", n_x2_in_range := i.n_x2_in_range]
  dt1[, row_id__ := NULL]
  
  return(copy(dt1))
}

#' adaptive_steps_dt 
#' 
#' @details
#' adaptive_steps_dt
#' 
#' @export
adaptive_steps_dt <- function(x2_vals, m, grid_mode = "auto", 
                              k_auto_exact_threshold = 1000,
                              interval_mode = "divide") {
  x2_vals <- x2_vals[!is.na(x2_vals)]
  k <- length(x2_vals)
  if (k == 0L) return(data.table(lower = numeric(), upper = numeric()))
  
  make_intervals <- function(vals) {
    bounds <- t(sapply(vals, get_interval_bounds, m = m, mode = interval_mode))
    data.table(lower = bounds[, 1], upper = bounds[, 2])
  }
  
  if (grid_mode == "auto") {
    if (k <= k_auto_exact_threshold) {
      grid_mode <- "exact"
    } else if (m >= 0.025) {
      grid_mode <- "linear"
    } else {
      grid_mode <- "quantile"
    }
  }
  
  grid_len <- 500
  grid <- switch(grid_mode,
                 linear = seq(min(x2_vals), max(x2_vals), length.out = grid_len),
                 quantile = unique(quantile(x2_vals, probs = seq(0, 1, length.out = grid_len), na.rm = TRUE)),
                 exact = return(make_intervals(x2_vals)),
                 stop("Ongeldige grid_mode")
  )
  
  make_intervals(grid)
}

#' n_x2_per_step
#' 
#' @details
#' n_x2_per_step
#' 
#' @export
n_x2_per_step <- function(steps_dt, x2_vals) {
  steps_dt[, n_x2 := sapply(seq_len(.N), function(i) {
    sum(x2_vals >= lower[i] & x2_vals <= upper[i], na.rm = TRUE)
  })]
  steps_dt
}




