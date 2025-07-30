foverlaps_by_row_limit_with_fallback <- function(
    dt1, dt2,
    kol_a = "a",
    row_limit = 5e6,
    gc_every = 10,
    logfile = NULL,
    nomatch = 0L,
    .verbose = FALSE,
    show_progress = TRUE,
    top_n_groups = 5
) {
  stopifnot(data.table::is.data.table(dt1), data.table::is.data.table(dt2))
  stopifnot(kol_a %in% colnames(dt1), kol_a %in% colnames(dt2))
  
  group_values <- unique(dt1[[kol_a]])
  n_batches <- ceiling(length(group_values) / row_limit)

  if (show_progress) pb <- utils::txtProgressBar(min = 0, max = n_batches, style = 3)
  result_list <- vector("list", n_batches)
  
  for (batch_idx in seq_len(n_batches)) {
    batch_start_time <- Sys.time()
    idx_start <- (batch_idx - 1) * row_limit + 1
    idx_end <- min(batch_idx * row_limit, length(group_values))
    current_group_values <- group_values[idx_start:idx_end]
    
    dt1_batch <- dt1[get(kol_a) %in% current_group_values]
    dt2_batch <- dt2[get(kol_a) %in% current_group_values]
    
    fallback_occurred <- FALSE
    error_message <- NULL
    res <- NULL
    
    tryCatch({
      res <- data.table::foverlaps(dt1_batch, dt2_batch, nomatch = nomatch)
    }, error = function(e) {
      fallback_occurred <<- TRUE
      error_message <<- e$message
      if (.verbose) message("⚠️ Fallback op batch ", batch_idx, ": ", error_message)
      if (.verbose) message("batch bestaat uit:", length(current_group_values), " groepen")
      
      res <<- tryCatch({
        rbindlist(lapply(split(dt1_batch, by = kol_a), function(grp1) {
          grp2 <- dt2_batch[get(kol_a) == grp1[[kol_a]][1]]
          tryCatch(
            data.table::foverlaps(grp1, grp2, nomatch = nomatch),
            error = function(e2) {
              if (.verbose) message("❌ Fout in groep ", grp1[[kol_a]][1], ": ", e2$message)
              return(NULL)
            }
          )
        }), use.names = TRUE, fill = TRUE)
      }, error = function(e2) {
        if (.verbose) message("❌ Volledige fallback batch ", batch_idx, " faalt: ", e2$message)
        NULL
      })
    })
    
    result_list[[batch_idx]] <- res
    
    # logging
    if ((!is.null(logfile)) & fallback_occurred) { # fallback_occurred toegevoegd
      gc_output <- gc()
      mem_now <- round(sum(gc_output[, "used"]), 1)
      mem_peak <- round(sum(gc_output[, "max used"]), 1)
      
      # top-N zwaarste groepen per batch (op basis van dt1_batch)
      zware_groepen <- dt1_batch[, .N, by = kol_a][order(-N)][1:min(top_n_groups, .N)]
      zware_str <- paste(paste0(zware_groepen[[kol_a]], "(", zware_groepen$N, ")"), collapse = ", ")
      
      log_line <- paste0(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | batch ", batch_idx,
        " | groepen: ", length(current_group_values),
        " | nrow dt1: ", nrow(dt1_batch),
        " | nrow dt2: ", nrow(dt2_batch),
        if (fallback_occurred) paste0(" | FOUT: ", error_message) else "",
        " | geheugen: ", mem_now, " MB (peak: ", mem_peak, " MB)",
        " | topgroepen: ", zware_str, "\n"
      )
      cat(log_line, file = logfile, append = TRUE)
    }
    
    if (batch_idx %% gc_every == 0) gc()
    if (show_progress) utils::setTxtProgressBar(pb, batch_idx)
  }
  
  if (show_progress) close(pb)
  
  data.table::rbindlist(result_list, use.names = TRUE, fill = TRUE)
}
