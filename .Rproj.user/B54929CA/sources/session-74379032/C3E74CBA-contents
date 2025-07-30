#' Schat aantal c_cl-waarden rond c per groep
#' 
#' @examples
#' library(data.table)
#' # Voorbeelddata
#' dt2 <- data.table(
#'   a = c("g1", "g1", "g2", "g2", "g3"),
#'   c = c(10, 12, 20, 21, 30),
#'   a_cl = c("g1", "g1", "g2", "g2", "g3"),
#'   c_cl = c(11, 13, 19, 22, 30)
#' )
#' 
#' ag_a <- data.table(a = c("g1", "g2", "g3"), N_group = c(10L, 20L, 5L))
#'
#' Roep functie aan
#' resultaat <- estimate_b1_counts_in_c2_intervals(
#'   dt2 = dt2,
#'   kol_a = "a",
#'   kol_c = "c",
#'   kol_a_cl = "a_cl",
#'   kol_c_cl = "c_cl",
#'   ag_a = ag_a,
#'   kol_N = "N_group",
#'   m = 0.1,
#'   auto_bw = TRUE,
#'   ...verbose = FALSE,
#'   timing = FALSE
#' )
#' 
#' head(resultaat)
#' 
#' @description
#' Voor elke waarde van kol_c in dt2 waarvoor kol_a == kol_a_cl (geclusterde groep),
#' schat deze functie hoeveel c_cl-waarden binnen het multiplicatieve interval rond kol_c vallen.
#' 
#' @details
#' De functie gebruikt per clusterwaarde een kernel density estimate (KDE) van kol_c_cl,
#' tenzij het aantal density-waarnemingen (N2) groter is dan het opgegeven totaal (N1),
#' in welk geval een exacte telling wordt gebruikt. Er wordt automatisch gecontroleerd
#' op scherpe densitydalen (indien auto_bw = TRUE), en dan wordt een bredere bandbreedte toegepast.
#' 
#' @param dt2 Een data.table met kolommen kol_a, kol_c, kol_a_cl, kol_c_cl
#' @param kol_a Naam van kolom met groeps-id
#' @param kol_c Naam van target kolom waarvoor schattingen worden gemaakt
#' @param kol_a_cl Naam van kolom met geclusterde groep (voor densities)
#' @param kol_c_cl Naam van kolom waarop KDE wordt gebaseerd per cluster
#' @param ag_a Een data.table met totalen per groep (kol_a en kol_N)
#' @param kol_N Naam van kolom in ag_a met aantal observaties per groep
#' @param m Margeparameter (bv. 0.2 betekent +/- 20% rond target)
#' @param auto_bw Logisch; TRUE activeert automatische detectie van scherpe densitydalen
#' @param ...verbose Toon voortgangsinformatie
#' @param timing Toon tijdsmetingen
#' 
#' @return Een data.table met toegevoegde kolommen lower, upper, estimated_n_cx, method
#' 
#' @import data.table
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
   
    if (...verbose) message(sprintf("[%d/%d] Groep '%s' verwerken...", i, length(all_cl_groups), group_cl_val))
    if (timing) start_time <- proc.time()
    
    dens_subset <- dt2[get(kol_a_cl) == group_cl_val]
    dens_vals <- dens_subset[[kol_c_cl]]
    N2 <- length(dens_vals)
    N1_vals <- ag_a[get(kol_a) == group_cl_val, get(kol_N)]
    if (length(N1_vals) == 0) {
      if (...verbose) message("  - Geen overeenkomstige N1 gevonden, resultaten op NA zetten")
      estimated_n_cx <- rep(NA_real_, nrow(dt2[get(kol_a) == group_cl_val]))
      method <- NA_character_
    } else {
      N1 <- N1_vals[1]
      method <- if (N2 >= N1) "exact" else "kde"
      
      target_subset <- dt2[get(kol_a) == group_cl_val]
      target_vals <- target_subset[[kol_c]]
      
      lower <- target_vals / (1 + m)
      upper <- target_vals / (1 - m)
      swap <- lower > upper
      tmp <- lower[swap]; lower[swap] <- upper[swap]; upper[swap] <- tmp
      
      if (method == "exact") {
        # a_cl en c1_cl uit dt2 bij de geselecteerde groep worden "a" en "c1" van dt1
        # hoeveel van deze c1 waarden (zr_zm) van dt1 worden onthuld met de c1 waarden (mr_mm) 
        # in dt2 bij a (mr_mm) = de geselecteerde groep?
        
        dt1_temp <- dt2[get(kol_a_cl) == group_cl_val] # de _cl kolommen uit dt2 met cl_namen
        cols_verw <- c(kol_a,kol_c)
        dt1_temp[,(cols_verw) := NULL]
        setnames(dt1_temp,c(kol_a_cl,kol_c_cl),c(kol_a,kol_c)) # standaard namen in dt1
        
        dt2_temp <- target_subset # de gewone dt2 met a (mr_mm) en c1 (mr_mm), andere records dan dt1_temp

        res_temp <- tel_b1_in_interval_rond_b2_foverlaps(dt1 = dt1_temp, dt2 = dt2_temp, 
                                                         kol_a, kol_c, m, 
                                                         row_limit = row_limit, nomatch = nomatch, 
                                                         ...verbose = ...verbose, show_progress = show_progress, 
                                                         logfile = logfile)
        estimated_n_cx <- sum(res_temp$R_c)
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
            estimated_n_cx <- probs * N1
          }
        }
      }
    }
    
    if (...verbose) message(sprintf(" - Methode: %s, N1 = %s, N2 = %d", method, ifelse(is.null(N1_vals), "NULL", N1_vals[1]), N2))
    if (timing) {
      t_diff <- proc.time() - start_time
      message(sprintf(" - Tijd: %.2fs", t_diff[["elapsed"]]))
    }
    
    target_subset <- dt2[get(kol_a) == group_cl_val]
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
  
  res <- rbindlist(result_list)
  setorder(res, rowid)
  setkeyv(res, "rowid")
  res[, c(kol_a, kol_c) := NULL]
  
  out <- res[dt2, on = "rowid"]
  out[is.na(estimated_n_cx), estimated_n_cx := 0]
  
  groep_totalen <- dt2[, .(
    n_a = .N,
    n_a_cnot_na = sum(!is.na(get(kol_c)))
  ), by = kol_a]
  
  setkeyv(groep_totalen, kol_a)
  out <- groep_totalen[out, on = kol_a]
  
  out[,estimated_n_cx := estimated_n_cx / n_a_cnot_na]
  
  #stopifnot(all(out$rowid == 1:nrow(dt2)))
  if(!all(out$rowid == 1:nrow(dt2))) 
    stop("Error: Rows are in changed order!")
  
  out[, c(kol_a, kol_c, kol_a_cl, kol_c_cl, "n_a", "n_a_cnot_na") := NULL]
  
  setnames(out, c("estimated_n_cx", "method", "lower", "upper"),
           c(paste0(c("S_", "method_", "lower_", "upper_"), kol_c)))
  
  return(out[])
}
