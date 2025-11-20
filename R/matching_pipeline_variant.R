#' matching_pipeline_variant
#' 
#' @details
#' matching_pipeline_variant
#' 
#' @export
matching_pipeline_variant <- function(dt1 = NULL, dt2, sep = ".",
                                      kol_a = NULL, kol_a_cl = NULL,
                                      bx = character(), cx = character(),
                                      bx_cl = NULL, cx_cl = NULL,
                                      ag_a = NULL, kol_N = NULL,
                                      variant = c("var1", "var2", "var3"),
                                      marge, na.rm = FALSE,
                                      auto_bw = TRUE, timing = FALSE, 
                                      row_limit = 5e6, nomatch = 0L, 
                                      ..verbose = TRUE, 
                                      show_progress = FALSE, logfile = NULL,
                                      grid_mode) {
  
  voeg_kolommen_toe <- function(dt_doel, dt_nieuw) {
    dt_doel[, (names(dt_nieuw)) := dt_nieuw]
  }
  
  variant <- match.arg(variant)
  
  if (variant == "var3") {
    if (is.null(bx_cl) && is.null(cx_cl)) {
      stop("Voor variant = 'var3' moet minstens één van 'bx_cl' of 'cx_cl' zijn opgegeven.")
    }
    if (!is.null(bx_cl) && length(bx_cl) != length(bx)) {
      stop("Lengte van 'bx_cl' moet overeenkomen met 'bx'")
    }
    if (!is.null(cx_cl) && length(cx_cl) != length(cx)) {
      stop("Lengte van 'cx_cl' moet overeenkomen met 'cx'")
    }
  }
  
  if (variant == "var1" | variant == "var2") { dt1[, rowid := .I] }
  dt2[, rowid := .I]
  
  if (length(bx) > 0){
    for (i in seq_along(bx)) {
      col_group <- c("rowid", kol_a, bx[i])
      if (variant == "var3"){
        if (!is.null(kol_a_cl)) col_group <- c(col_group, kol_a_cl)
        if (!is.null(bx_cl[i])) col_group <- c(col_group, bx_cl[i])
      }
      res <- switch(variant,
                    var1 = tel_exact_match_per_rij_dt1(dt1 = dt1[, ..col_group], dt2 = dt2[, ..col_group],
                                                       kol_a = kol_a, kol_b = bx[i]),
                    var2 = tel_exact_match_per_rij_dt2(dt1 = dt1[, ..col_group], dt2 = dt2[, ..col_group],
                                                       kol_a = kol_a, kol_b = bx[i]),
                    var3 = estimate_exact_match_per_rij_b2(dt2 = dt2[, ..col_group], ag_a = ag_a, kol_N = kol_N,
                                                           kol_a = kol_a, kol_a_cl = kol_a_cl,
                                                           kol_b = bx[i], kol_b_cl = bx_cl[i]))
      if (variant == "var1") {
        voeg_kolommen_toe(dt1, res)
      } else {
        voeg_kolommen_toe(dt2, res)
      }
    }
  }
 
  if (((variant == "var1") | (variant == "var2")) & (length(cx) > 0)){
    for (i in seq_along(cx)) {
      col_group <- c("rowid", kol_a, cx[i])
      dt1_sel <- if (variant == "var2") dt2[, ..col_group] else dt1[, ..col_group]
      dt2_sel <- if (variant == "var2") dt1[, ..col_group] else dt2[, ..col_group]
      
      kol_naam <- cx[i]
      dt1_sel[, (kol_naam) := as.numeric(.SD[[1L]]), .SDcols = kol_naam]
      dt2_sel[, (kol_naam) := as.numeric(.SD[[1L]]), .SDcols = kol_naam]
      
      meta <- dt2_sel[
        , {
          x <- .SD[[1L]]
          .(
            n_x2         = .N,
            n_x2_not_NA  = sum(!is.na(x)),
            unique_x2    = uniqueN(x)
          )
        },
        by = kol_a, 
        .SDcols = cx[i]
      ]
      
      meta_dt1 <- dt1_sel[, .(
        n_x1 = .N,
        n_x1_not_NA = sum(!is.na(.SD[[1L]]))
      ), by = kol_a, .SDcols = cx[i]]
      
      meta <- merge(meta, meta_dt1, by = kol_a, all = TRUE)
      meta[is.na(n_x2), n_x2 := 0L]
      meta[is.na(n_x1), n_x1 := 0L]
      
      meta_modes <- copy(meta)
      
      t1 <- dt1[, .(
        med_center_abs = median(abs(.SD[[1L]]), na.rm = TRUE)
      ), by = kol_a, .SDcols = cx]
      
      t2 <- dt2[, .(
        IQR_x2    = max(IQR(.SD[[1L]], na.rm = TRUE), .Machine$double.eps),
        unique_x2 = uniqueN(.SD[[1L]])
      ), by = kol_a, .SDcols = cx]
      
      meta_modes <- merge(meta, t1[, c(kol_a, "med_center_abs"), with=FALSE], by=kol_a, all.x=TRUE)
      meta_modes <- merge(meta_modes, t2[, c(kol_a, "IQR_x2"), with=FALSE], by=kol_a, all.x=TRUE)
      
      meta_modes[, grenswaarde := (2 * marge * med_center_abs) / IQR_x2]
      aantal_groepen <- nrow(meta_modes)
      
      meta_modes[, grid_mode_gr := fcase(
        unique_x2 <= 1000 & marge < grenswaarde, "exact",
        marge >= grenswaarde,                    "linear",
        default =                                "quantile"
      )]
      
      max_exact_groups <- 10000L
      if (aantal_groepen > max_exact_groups) {
        meta_modes[grid_mode_gr == "exact", grid_mode_gr := "quantile"]
      }
      meta_modes[as.numeric(n_x1) * as.numeric(n_x2) > 1e6 & grid_mode_gr == "exact", grid_mode_gr := "quantile"]
      
      if (grid_mode %chin% c("exact","linear","quantile","foverlaps")) {
        meta_modes[, grid_mode_gr := grid_mode]
      }
      meta_modes[, fallback := (as.numeric(n_x1) * as.numeric(n_x2) > row_limit)]
      meta_modes[fallback == TRUE, grid_mode_gr := "foverlaps"]
      
      if (isTRUE(..verbose)) {
        counts <- meta_modes[, .N, by = grid_mode_gr][order(-N)]
        message(
          "Aantal groepen per grid_mode_gr:\n",
          paste(sprintf("%-10s : %d", counts$grid_mode_gr, counts$N), collapse = "\n")
        )
      }
      
      ## ---------- FIX: named lookup altijd op *naam* (character) ----------
      modes   <- setNames(meta_modes$grid_mode_gr, as.character(meta_modes[[kol_a]]))
      groepen <- as.character(meta[[kol_a]])
      ## --------------------------------------------------------------------

      # splitsen
      groepen_fo <- groepen[modes[groepen] == "foverlaps"]
      groepen_other <- groepen[modes[groepen] != "foverlaps"]
      
      res_lijst <- vector("list", length = length(groepen))
      
      # eerst de niet-foverlaps groepen
      if (length(groepen_other) > 0L){
        for (j in seq_along(groepen_other)) {
          groep  <- groepen_other[j]
          mode_j <- modes[[groep]]     # character-lookup; geen out-of-bounds
          
          #if (is.null(mode_j)) next    # (mini-guard) geen logging, geen fallback
          
          dt1_g <- dt1_sel[dt1_sel[[kol_a]] == if (is.numeric(dt1_sel[[kol_a]])) as.numeric(groep) else groep]
          dt2_g <- dt2_sel[dt2_sel[[kol_a]] == if (is.numeric(dt2_sel[[kol_a]])) as.numeric(groep) else groep]
          
          message("groep ", groep,"/",length(groepen), ", nrow_dt1 =", nrow(dt1_g), ", nrow_dt2 =", nrow(dt2_g))
          
          #if (nrow(dt1_g) == 0) next
          
          res <- bereken_matches_en_stats(
            dt1 = dt1_g, dt2 = dt2_g,
            kol_a = kol_a, kol_x1 = cx[i], kol_x2 = cx[i],
            m = marge, grid_mode = mode_j,
            k_auto_exact_threshold = 1000,
            interval_mode = if (variant == "var1") "multiply" else "divide"
          )
          if (variant == "var1") {
            res[, P := n_x2_in_range / n_x2_not_NA]
            res[n_x2_not_NA == 0, P := 0]
            cols <- c(kol_a, cx[i], "n_x2_not_NA","n_x1_not_NA",
                      "n_x1_total","n_x2_total","n_x2_in_range")
            res[, (cols) := NULL]
            setnames(res, "P", paste0("P_", cx[i]))
          } 
          if (variant == "var2") {
            res[, R := n_x2_in_range / n_x1_not_NA]
            res[n_x1_not_NA == 0, R := 0]
            cols <- c(kol_a, cx[i], "n_x2_not_NA","n_x1_not_NA",
                      "n_x1_total","n_x2_total","n_x2_in_range")
            res[, (cols) := NULL]
            setnames(res, "R", paste0("R_", cx[i]))
          }
        }
        
        res[, mode := mode_j]
        #res_lijst[[j]] <- res
        res_lijst[[which(groepen == groep)]] <- res
      }

      # alle foverlaps-groepen in één keer
      if (length(groepen_fo) > 0L){
        dt1_fo <- dt1_sel[dt1_sel[[kol_a]] %in% groepen_fo]
        dt2_fo <- dt2_sel[dt2_sel[[kol_a]] %in% groepen_fo]
        
        res_fo <- if (variant == "var1") {
          tel_b2_in_interval_rond_b1_foverlaps(
            dt1 = dt1_fo, dt2 = dt2_fo, kol_a = kol_a, kol_c = cx[i],
            marge = marge, row_limit = row_limit,
            nomatch = nomatch, ...verbose = ..verbose,
            show_progress = show_progress, logfile = logfile
          )
        } else {
          tel_b1_in_interval_rond_b2_foverlaps(
            dt1 = dt2_fo, dt2 = dt1_fo, kol_a = kol_a, kol_c = cx[i],
            marge = marge, row_limit = row_limit,
            nomatch = nomatch, ...verbose = ..verbose,
            show_progress = show_progress, logfile = logfile
          )
        }
        res_fo[, mode := "foverlaps"]
      }
      
      # samenvoegen (werkt ook als één van beide leeg is)
      res_all <- rbindlist(
        c(res_lijst, list(res_fo)), 
        use.names = TRUE, 
        fill = TRUE
      )
      
      if (variant == "var1") {
        setnames(res_all, "mode", paste0("P_mode_", cx[i])) 
        dt1 <- merge(dt1, res_all, by = "rowid", all.x = TRUE, sort = FALSE)
        kol_naam <- paste0("P_", cx[i])
        dt1[is.na(dt1[[kol_naam]]), (kol_naam) := 0]
      }
      
      if (variant == "var2") {
        setnames(res_all, "mode", paste0("R_mode_", cx[i]))
        dt2 <- merge(dt2, res_all, by = "rowid", all.x = TRUE, sort = FALSE)
        kol_naam <- paste0("R_", cx[i])
        dt2[is.na(dt2[[kol_naam]]), (kol_naam) := 0]
      }
    } # einde loop over cx
  }
  
  if ((variant == "var3") & (length(cx) > 0)){
    for (i in seq_along(cx)) {
      col_group <- c("rowid", kol_a, cx[i])
      if (!is.null(kol_a_cl)) col_group <- c(col_group, kol_a_cl)
      if (!is.null(cx_cl[i])) col_group <- c(col_group, cx_cl[i])
      
      res <- estimate_c1_counts_in_c2_intervals(dt2 = dt2[, ..col_group], ag_a = ag_a, kol_N = kol_N,
                                                kol_a = kol_a, kol_a_cl = kol_a_cl,
                                                kol_c = cx[i], kol_c_cl = cx_cl[i], 
                                                m = marge, auto_bw = auto_bw,
                                                row_limit = row_limit, nomatch = nomatch, ...verbose = ..verbose, 
                                                show_progress = show_progress, logfile = logfile, timing = timing)
      dt2 <- merge(dt2, res, by = "rowid", all.x = TRUE, sort = FALSE)
      dt2[is.na(dt2[[paste0("S_", cx[i])]]), (paste0("S_", cx[i])) := 0]
    }   
  }
  
  return(list(dt1 = dt1, dt2 = dt2))
}
