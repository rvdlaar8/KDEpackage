#' Functie schatting Cx var3
#' 
#' @details
#' schatting Cx var3
#' 
#' @export
estimate_c1_counts_in_c2_intervals <- function(dt2, 
                                               kol_a, kol_c,
                                               kol_a_cl, kol_c_cl,
                                               ag_a, kol_N, m,
                                               auto_bw = TRUE,
                                               row_limit, nomatch = 0L, ...verbose, 
                                               show_progress = F, logfile, timing = F) {
  stopifnot(is.data.table(dt2), is.data.table(ag_a))
  stopifnot(all(c(kol_a, kol_c, kol_a_cl, kol_c_cl) %in% names(dt2)))
  stopifnot(all(c(kol_a, kol_N) %in% names(ag_a)))

  dt2[, rowid := .I]
  all_cl_groups <- unique(dt2[[kol_a_cl]])
  result_list <- vector("list", length(all_cl_groups))
  
  for (i in seq_along(all_cl_groups)) {
    group_cl_val <- all_cl_groups[i]
   
    #if (...verbose) message(sprintf("[%d/%d] Groep '%s' verwerken...", i, length(all_cl_groups), group_cl_val))
    if (timing) start_time <- proc.time()

    dens_subset <- dt2[dt2[[kol_a_cl]] == group_cl_val]
    dens_vals <- dens_subset[[kol_c_cl]]
    N2 <- length(dens_vals)
    N1_vals <- ag_a[ag_a[[kol_a]] == group_cl_val, ag_a[[kol_N]]]
    if (length(N1_vals) == 0) {
      if (...verbose) message("  - Geen overeenkomstige N1 gevonden, resultaten op NA zetten")
      estimated_n_cx <- rep(NA_real_, nrow(dt2[dt2[[kol_a]] == group_cl_val]))
      method <- NA_character_
      lower <- NA_real_
      upper <- NA_real_
    } else {
      N1 <- N1_vals[1]
      method <- if (N2 >= N1) "exact" else "kde"
      
      target_subset <- dt2[(dt2[[kol_a]] == group_cl_val) & !is.na(dt2[[kol_c]])]
      target_vals <- target_subset[[kol_c]]
      N3 <- length(target_vals) # bij zr_zm vallen target_vals samen met eenheden in dt1
      
      lower <- target_vals / (1 + m)
      upper <- target_vals / (1 - m)
      swap <- lower > upper
      tmp <- lower[swap]; lower[swap] <- upper[swap]; upper[swap] <- tmp
      
      if (method == "exact") {
        # a_cl en c1_cl uit dt2 bij de geselecteerde groep worden "a" en "c1" van dt1
        # hoeveel van deze c1 waarden (zr_zm) van dt1 worden onthuld met de c1 waarden (mr_mm) 
        # in dt2 bij a (mr_mm) = de geselecteerde groep?
        
        dt1_temp <- dt2[dt2[[kol_a_cl]] == group_cl_val]
        
        cols_verw <- c(kol_a,kol_c)
        dt1_temp[,(cols_verw) := NULL]
        setnames(dt1_temp,c(kol_a_cl,kol_c_cl),c(kol_a,kol_c)) # standaard namen in dt1
        
        dt2_temp <- target_subset # de gewone dt2 met a (mr_mm) en c1 (mr_mm), andere records dan dt1_temp
        
        res_temp <- tel_b1_in_interval_rond_b2_foverlaps(dt1 = dt1_temp, dt2 = dt2_temp, 
                                                         kol_a, kol_c, m, 
                                                         row_limit = row_limit, nomatch = nomatch, 
                                                         ...verbose = ...verbose, show_progress = show_progress, 
                                                         logfile = logfile)

        estimated_n_cx <- res_temp$R_c # is al R_c: al gedeeld door n_a_not_na = n_a_cnot_na, maar in 165 nogmaals
        # in cx_var12:
        n_a_not_na_temp <- nrow(dt2_temp[!is.na(dt2_temp[[kol_c]])])
        estimated_n_cx <- estimated_n_cx * n_a_not_na_temp # toegevoegd, nu weer integers
        #cat("\naangepaste noemer\n")
      } else {
        if (length(dens_vals) < 2 || all(is.na(dens_vals))) {
          if (...verbose) message("  - Onvoldoende gegevens voor KDE (lengte < 2 of alles NA)")
          estimated_n_cx <- rep(NA_real_, length(target_vals))
        } else {
          bw <- NULL
          if (auto_bw) {
            dens_tmp <- tryCatch(
              density(dens_vals, n = 512),
              error = function(e) {
                if (...verbose) message("  - Fout bij density(): ", e$message)
                return(NULL)
              }
            )
            if (!is.null(dens_tmp)) {
              dips <- diff(sign(diff(dens_tmp$y)))
              local_minima <- which(dips == 2)
              central_region <- dens_tmp$x >= quantile(dens_tmp$x, 0.3) & dens_tmp$x <= quantile(dens_tmp$x, 0.7)
              deep_dips <- any(local_minima %in% which(central_region) &
                                 dens_tmp$y[local_minima] < 0.5 * max(dens_tmp$y))
              if (deep_dips) {
                if (...verbose) message(sprintf("  - Diep dal gedetecteerd bij %s, gebruik bredere bandbreedte", group_cl_val))
                bw <- 2 * bw.SJ(dens_vals)
              }
            }
          }
          
          dens <- tryCatch(
            if (is.null(bw)) { density(dens_vals, n = 512)
            } else {
              density(dens_vals, n = 512, bw = bw)},
            error = function(e) {
              if (...verbose) message("  - density() faalt alsnog: ", e$message)
              return(NULL)
            }
          )
          
          if (is.null(dens)) {
            estimated_n_cx <- rep(NA_real_, length(target_vals))
          } else {
            cdf_y <- cumsum(dens$y) / sum(dens$y)
            cdf_fun <- approxfun(dens$x, cdf_y, rule = 2)
            probs <- cdf_fun(upper) - cdf_fun(lower)
            #sum(probs) * N1
            # alle schattingen per record, in vector:
            # nieuw:
            estimated_n_cx <- (N1 - N3) * probs + 1 # als iedere c1 samenvalt met een yi in dt1: c1 zr_zm
            # orig: tijdelijk terug:
            #cat("\ntijdelijk terug\n")
            #estimated_n_cx <- N1 * probs
            #cat("\nN1 =",N1,", N2 =",N2,", N3 =",N3)
          }
        }
      }
    }
       
    if (...verbose) message(sprintf(" - Methode: %s, N1 = %s, N2 = %d", method, ifelse(is.null(N1_vals), "NULL", N1_vals[1]), N2))
    if (timing) {
      t_diff <- proc.time() - start_time
      message(sprintf(" - Tijd: %.2fs", t_diff[["elapsed"]]))
    }
   
    target_subset <- dt2[dt2[[kol_a]] == group_cl_val]
    out_dt <- data.table(
      group_cl_val,
      target_vals = target_subset[[kol_c]],
      lower = lower,
      upper = upper,
      estimated_n_cx = estimated_n_cx,
      method = method,
      rowid = target_subset$rowid
    )
    setnames(out_dt, c("group_cl_val", "target_vals"), c(kol_a, kol_c))
    
    setcolorder(out_dt, c(kol_a, kol_c, "lower", "upper", "estimated_n_cx", "method", "rowid"))
    result_list[[i]] <- out_dt
  }

  res <- rbindlist(result_list) # onder elkaar alle records van alle groepen
  setorder(res, rowid)
  setkeyv(res, "rowid")
  res[, c(kol_a, kol_c) := NULL]
  
  out <- res[dt2, on = "rowid"]
  out[is.na(estimated_n_cx), estimated_n_cx := 0]
  
  groep_totalen <- dt2[, .(
    n_a          = .N,
    n_a_cnot_na  = sum(!is.na(.SD[[1L]]))
  ), by = kol_a, .SDcols = kol_c]
  
  setkeyv(groep_totalen, kol_a)
  out <- groep_totalen[out, on = kol_a] # per record, incl groepstotalen
  
  # estimated_n_cx = geschat aantal matches in dt1 per record
  # volgens eq 4 per groep delen door n_c_not_na:
  out[,estimated_n_cx := estimated_n_cx / n_a_cnot_na]
  out[is.na(out[[kol_c]]), estimated_n_cx := NA]
  
  #stopifnot(all(out$rowid == 1:nrow(dt2)))
  if(!all(out$rowid == 1:nrow(dt2))) 
    stop("Error: Rows are in changed order!")
  
  out[, c(kol_a, kol_c, kol_a_cl, kol_c_cl, "n_a", "n_a_cnot_na") := NULL]
  
  setnames(out, c("estimated_n_cx", "method", "lower", "upper"),
           c(paste0(c("S_", "method_", "lower_", "upper_"), kol_c)))
  
  return(out[])
}

