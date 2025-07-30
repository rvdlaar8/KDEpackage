log_probleemgroep <- function(groep_info, n1, n2, status, logfile = "foverlaps_fallback_log.csv") {
  if (is.null(groep_info)) groep_info <- NA_character_
  log_dt <- data.table(
    tijd = Sys.time(),
    groep = paste(groep_info, collapse = "|"),
    n_dt1 = n1,
    n_dt2 = n2,
    status = status
  )
  fwrite(log_dt, file = logfile, append = file.exists(logfile))
}

f_overlaps_per_batch_met_fallback <- function(dt1, dt2, by_col, ..., row_limit = 1e6) {
  dt1_split <- split(dt1, by = by_col)
  dt2_split <- split(dt2, by = by_col)
  
  res_list <- vector("list", length(dt1_split))
  
  for (i in seq_along(dt1_split)) {
    d1 <- dt1_split[[i]]
    group_key <- d1[[by_col]][1]
    d2 <- dt2_split[[as.character(group_key)]]
    
    result <- tryCatch({
      # optie: log grote groepen vooraf
      if (nrow(d1) > row_limit || nrow(d2) > row_limit) {
        log_probleemgroep(group_key, nrow(d1), nrow(d2), "groot")
      }
      
      foverlaps(d1, d2, ...)
    }, error = function(e) {
      log_probleemgroep(group_key, nrow(d1), nrow(d2), paste("foverlaps error:", e$message))
      fallback_voor_groep(d1, d2)
    })
    
    # Optioneel: leeg resultaat detecteren
    if (nrow(result) == 0) {
      log_probleemgroep(group_key, nrow(d1), nrow(d2), "lege match")
    }
    
    res_list[[i]] <- result
  }
  
  rbindlist(res_list)
}
