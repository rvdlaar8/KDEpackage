#' Wrapper voor matching_pipeline variant 1, 2 of 3
#'
#' Roept de juiste variant aan en loopt over alle kolommen in `bx` en `cx`.
#'
#' @param dt1 Eerste data.table (alleen nodig bij variant 1 of 2)
#' @param dt2 Tweede data.table
#' @param sep Scheidingsteken voor samengestelde kolommen
#' @param kol_a Naam van groepskolom
#' @param kol_a_cl Naam van groepskolom_cl (alleen nodig bij variant 3)
#' @param bx Vector met kolomnamen voor exacte matching
#' @param cx Vector met kolomnamen voor intervalschatting
#' @param bx_cl Kolomnamen van bx_cl (alleen voor variant 3)
#' @param cx_cl Kolomnamen van cx_cl (alleen voor variant 3)
#' @param ag_a Aggregatietabel met groepsgroottes (alleen voor variant 3)
#' @param kol_N Naam van kolom met groepsgrootte in ag_a
#' @param variant Te kiezen variant: "var1" (U_exact), "var2" (s_exact), "var3" (s_geschat)
#' @param marge Margeparameter voor intervalgrenzen
#' @param na.rm Of NA’s moeten worden verwijderd bij samenvoegen
#' @param .verbose Of voortgangsmeldingen getoond moeten worden
#'
#' @return data.table met toegevoegde kolommen, afhankelijk van variant
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
                                      show_progress = FALSE, logfile = NULL) {
  
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
  } else {
    # if (!is.null(bx_cl) || !is.null(cx_cl)) {
    #   warning("Argumenten 'bx_cl' en 'cx_cl' worden genegeerd bij variant != 'var3'")
    # }
  }
  
  # rowid niet toekennen of weghalen binnen tel_b2 of tel_b1
  if (variant != "var3") {if (!"rowid" %in% names(dt1)) dt1[, rowid := .I]}
  if (!"rowid" %in% names(dt2)) {dt2[, rowid := .I]}
  
  if (length(bx) > 0){
    for (i in seq_along(bx)) {
      col_group <- c("rowid",kol_a, bx[i])
      
      if (variant == "var3"){
        if (!is.null(kol_a_cl)) col_group <- c(col_group, kol_a_cl)
        if (!is.null(bx_cl[i])) col_group <- c(col_group, bx_cl[i])
        #col_cl <- if (!is.null(bx_cl)) bx_cl[i] else NULL
        #col_cl <- if (!is.null(cx_cl)) cx_cl[i] else NULL
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
  


  if (length(cx) > 0){
    for (i in seq_along(cx)) {
      col_group <- c("rowid",kol_a, cx[i])
      
      if (variant == "var3"){
        if (!is.null(kol_a_cl)) col_group <- c(col_group, kol_a_cl)
        if (!is.null(cx_cl[i])) col_group <- c(col_group, cx_cl[i])
        #col_cl <- if (!is.null(bx_cl)) bx_cl[i] else NULL
        #col_cl <- if (!is.null(cx_cl)) cx_cl[i] else NULL
      }
  
      res <- switch(variant,
                    var1 = tel_b2_in_interval_rond_b1_foverlaps(dt1 = dt1[, ..col_group], dt2 = dt2[, ..col_group],
                                                                kol_a, kol_c = cx[i], marge, 
                                                                row_limit = row_limit, nomatch = nomatch, ...verbose = ..verbose, 
                                                                show_progress = show_progress, logfile = logfile),
                    #kol_a = kol_a, kol_c = col, marge = marge, verbose = verbose),
                    var2 = tel_b1_in_interval_rond_b2_foverlaps(dt1 = dt1[, ..col_group], dt2 = dt2[, ..col_group],
                                                                kol_a, kol_c = cx[i], marge, 
                                                                row_limit = row_limit, nomatch = nomatch, ...verbose = ..verbose, 
                                                                show_progress = show_progress, logfile = logfile),
                    #kol_a = kol_a, kol_c = col, marge = marge, verbose = verbose),
                    var3 = estimate_c1_counts_in_c2_intervals(dt2 = dt2[, ..col_group], ag_a = ag_a, kol_N = kol_N,
                                                              kol_a = kol_a, kol_a_cl = kol_a_cl,
                                                              kol_c = cx[i], kol_c_cl = cx_cl[i], 
                                                              m = marge, auto_bw = auto_bw,
                                                              row_limit = row_limit, nomatch = nomatch, ...verbose = ..verbose, 
                                                              show_progress = show_progress, logfile = logfile, timing = timing))

      if (variant == "var1") {
        #voeg_kolommen_toe(dt1, res)
        # dt1[, (names(res)) := res]
        dt1 <- merge(dt1, res, by = "rowid", all.x = TRUE, sort = FALSE)
        dt1[is.na(get(paste0("P_", cx))), (paste0("P_", cx)) := 0]
        
      } else {
        #voeg_kolommen_toe(dt2, res)
        # dt2[, (names(res)) := res] # res moet evenveel rijen hebben als dt2
        
        # mogelijk heeft res minder rijen dan dt2:
        dt2 <- merge(dt2, res, by = "rowid", all.x = TRUE, sort = FALSE)

        if (variant == "var2") dt2[is.na(get(paste0("R_", cx[i]))), (paste0("R_", cx[i])) := 0]
        if (variant == "var3") dt2[is.na(get(paste0("S_", cx[i]))), (paste0("S_", cx[i])) := 0]
      }
    }   
  }

  
  #invisible(NULL)  # alles gebeurt by reference
  return(list(dt1 = dt1, dt2 = dt2))
}
