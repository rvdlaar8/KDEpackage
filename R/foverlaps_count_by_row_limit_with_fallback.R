foverlaps_count_by_row_limit_with_fallback <- function(
    dt1, dt2,
    kol_a,
    row_limit = 2e7,
    kol_c = NULL,  # Nieuw argument
    show_progress = TRUE,
    gc_every = 10,
    logfile = NULL,
    .verbose = TRUE
) {
  require(data.table)
  stopifnot(is.data.table(dt1), is.data.table(dt2))
  stopifnot(kol_a %in% names(dt1), kol_a %in% names(dt2))
  
  # Voeg rij-id toe om originele volgorde te herstellen
  dt1[, .rowid__ := .I]
  
  gemeenschappelijke_groepen <- intersect(unique(dt1[[kol_a]]), unique(dt2[[kol_a]]))
  dt1_valid <- dt1[get(kol_a) %in% gemeenschappelijke_groepen]
  dt2_valid <- dt2[get(kol_a) %in% gemeenschappelijke_groepen]
  
  # gaf integer overflow:
  # batches <- split(gemeenschappelijke_groepen, ceiling(seq_along(gemeenschappelijke_groepen) * nrow(dt1_valid) / row_limit))
  # n1 <- as.double(nrow(dt1_valid))
  # ind <- ceiling(seq_along(gemeenschappelijke_groepen) * n1 / row_limit)
  # batches <- split(gemeenschappelijke_groepen, ind)

  groepsgroottes <- dt1_valid[, .N, by = kol_a]
  groepsgroottes <- groepsgroottes[get(kol_a) %in% gemeenschappelijke_groepen]
  groepsgroottes[, cumN := cumsum(N)]
  groepsgroottes[, batch := ceiling(cumN / row_limit)]
  batches <- split(groepsgroottes[[kol_a]], groepsgroottes$batch)
  
  message(sprintf("Aantal batches: %d", length(batches)))
  
  # groepen_per_batch <- ceiling(row_limit / nrow(dt1_valid))
  # batches <- split(gemeenschappelijke_groepen, ceiling(seq_along(gemeenschappelijke_groepen) / groepen_per_batch))
  
  result_valid_list <- vector("list", length(batches))
  
  if (show_progress) pb <- txtshow_progressBar(min = 0, max = length(batches), style = 3)
  
  for (i in seq_along(batches)) {
    groepen_i <- batches[[i]]
    dt1_b <- dt1_valid[get(kol_a) %in% groepen_i]
    dt2_b <- dt2_valid[get(kol_a) %in% groepen_i]
    
    tryCatch({
      setnames(dt1_b, c("lower", "upper"), c("start", "end"))
      setnames(dt2_b, c("lower", "upper"), c("start", "end"))
      setkeyv(dt1_b, c(kol_a, "start", "end"))
      setkeyv(dt2_b, c(kol_a, "start", "end"))
      
      overlap <- foverlaps(dt1_b, dt2_b, type = "any", nomatch = 0L)
      
      if (!is.null(kol_c)) {
        overlap <- overlap[!is.na(get(paste0("i.", kol_c)))]
      }
      
      result <- overlap[, .N, by = .rowid__]  # correct per rij in dt1
      result_valid_list[[i]] <- result
    }, error = function(e) {
      if (!is.null(logfile)) {
        logregel <- paste(Sys.time(), "- Batch", i, "- fout:", e$message, "\n")
        cat(logregel, file = logfile, append = TRUE)
      }
      if (.verbose) message("Fout in batch ", i, ": ", e$message)
    })
    
    if (show_progress) setTxtshow_progressBar(pb, i)
    if (i %% gc_every == 0) gc()
  }
  
  if (show_progress) close(pb)
   
  result_valid <- rbindlist(result_valid_list, use.names = TRUE, fill = TRUE)
  
  dt1_out <- copy(dt1)
  dt1_out[, .rowid__ := .I]
  
  result_full <- merge(
    dt1_out,
    result_valid,
    by = ".rowid__",
    all.x = TRUE,
    sort = FALSE
  )[, N := fifelse(is.na(N), 0L, N)]
  
  result_full[, .rowid__ := NULL]  # opschonen
  return(result_full[])
}

# # Functie om testdata te genereren
# gen_dt <- function(n_rows, n_groups, prefix = "a") {
#   setDT(data.table(
#     a = sample(sprintf("%s%03d", prefix, 1:n_groups), n_rows, replace = TRUE),
#     lower = runif(n_rows, 0, 1e6),
#     len = runif(n_rows, 10, 1000),
#     rowid = 1:n_rows
#   ))[, upper := lower + len][]
# }
# 
# #set.seed(123)
# dt1 <- gen_dt(2e7, 1000, prefix = "g")
# dt2 <- gen_dt(2e3, 1000, prefix = "g")
# str(dt1)
# 
# # nieuwe functie:
# nrow(dt1)
# nrow(dt2)
# gc();
# t0 <- Sys.time()
# res1 <- foverlaps_count_by_row_limit_with_fallback(dt1,dt2,kol_a = "a") # heeft veel meer rijen dan dt1
# nrow(res1)
# sum(res1$N)
# t1 <- Sys.time()
# mem_A <- sum(sapply(ls(), function(x) object.size(get(x)))) / 1e6
# cat("Tijdstrategie A:", t1 - t0, "\n")
# cat("Geheugenstrategie A:", round(mem_A, 1), "MB\n")
# 
# # oude functie:
# gc(); 
# t2 <- Sys.time()
# setkeyv(dt1, c("a", "lower", "upper")) # wijzigt rijvolgorde
# setkeyv(dt2, c("a", "lower", "upper")) # wijzigt rijvolgorde
# res2 <- foverlaps_by_row_limit_with_fallback(dt1,dt2,kol_a = "a")
# counts <- res2[, .(n_marge = .N), by = rowid]
# sum(counts$n_marge)
# counts <- res2[, .(n_marge = .N), by = i.rowid]
# sum(counts$n_marge)
# t3 <- Sys.time()
# mem_B <- sum(sapply(ls(), function(x) object.size(get(x)))) / 1e6
# cat("Tijdstrategie B:", t3 - t2, "\n")
# cat("Geheugenstrategie B:", round(mem_B, 1), "MB\n")