foverlaps_by_row_limit_with_fallback <- function(
    dt1, dt2,
    by_col = "a",
    row_limit = 5e6,
    gc_every = 10,
    log_path = NULL,
    nomatch = 0L,
    .verbose = FALSE,
    show_progress = TRUE,
    top_n_groups = 5
) {
  stopifnot(data.table::is.data.table(dt1), data.table::is.data.table(dt2))
  stopifnot(by_col %in% colnames(dt1), by_col %in% colnames(dt2))
  
  group_values <- unique(dt1[[by_col]])
  n_batches <- ceiling(length(group_values) / row_limit)
  if (show_progress) pb <- utils::txtProgressBar(min = 0, max = n_batches, style = 3)
  
  result_list <- vector("list", n_batches)
  
  for (batch_idx in seq_len(n_batches)) {
    batch_start_time <- Sys.time()
    idx_start <- (batch_idx - 1) * row_limit + 1
    idx_end <- min(batch_idx * row_limit, length(group_values))
    current_group_values <- group_values[idx_start:idx_end]
    
    dt1_batch <- dt1[get(by_col) %in% current_group_values]
    dt2_batch <- dt2[get(by_col) %in% current_group_values]
    
    fallback_occurred <- FALSE
    error_message <- NULL
    res <- NULL
    
    tryCatch({
      res <- data.table::foverlaps(dt1_batch, dt2_batch, nomatch = nomatch)
    }, error = function(e) {
      fallback_occurred <<- TRUE
      error_message <<- e$message
      if (.verbose) message("⚠️ Fallback op batch ", batch_idx, ": ", error_message)
      
      res <<- tryCatch({
        rbindlist(lapply(split(dt1_batch, by = by_col), function(grp1) {
          grp2 <- dt2_batch[get(by_col) == grp1[[by_col]][1]]
          tryCatch(
            data.table::foverlaps(grp1, grp2, nomatch = nomatch),
            error = function(e2) {
              if (.verbose) message("❌ Fout in groep ", grp1[[by_col]][1], ": ", e2$message)
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
    if (!is.null(log_path)) {
      gc_output <- gc()
      mem_now <- round(sum(gc_output[, "used"]), 1)
      mem_peak <- round(sum(gc_output[, "max used"]), 1)
      
      # top-N zwaarste groepen per batch (op basis van dt1_batch)
      zware_groepen <- dt1_batch[, .N, by = by_col][order(-N)][1:min(top_n_groups, .N)]
      zware_str <- paste(paste0(zware_groepen[[by_col]], "(", zware_groepen$N, ")"), collapse = ", ")
      
      log_line <- paste0(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | batch ", batch_idx,
        " | groepen: ", length(current_group_values),
        " | nrow dt1: ", nrow(dt1_batch),
        " | nrow dt2: ", nrow(dt2_batch),
        if (fallback_occurred) paste0(" | FOUT: ", error_message) else "",
        " | geheugen: ", mem_now, " MB (peak: ", mem_peak, " MB)",
        " | topgroepen: ", zware_str, "\n"
      )
      cat(log_line, file = log_path, append = TRUE)
    }
    
    if (batch_idx %% gc_every == 0) gc()
    if (show_progress) utils::setTxtProgressBar(pb, batch_idx)
  }
  
  if (show_progress) close(pb)
  
  data.table::rbindlist(result_list, use.names = TRUE, fill = TRUE)
}

library(data.table)

# Maak testdata
dt1 <- data.table(a = rep(1:3, each = 3), start = 1:9, end = 2:10)
dt2 <- data.table(a = 1:3, start = c(1, 4, 7), end = c(3, 6, 9))

setkey(dt1, a, start, end)
setkey(dt2, a, start, end)

log_path <- "../SEC versie/logs/test_log.txt"
res <- foverlaps_by_row_limit_with_fallback(dt1, dt2, log_path = log_path, .verbose = TRUE)
