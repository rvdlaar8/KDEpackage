
#' Functie 2 bereken exacte matches Cx var1
#' 
#' @details
#' marge rond b1 in dt1, matchend met b2 in dt2
#' 
#' @export
tel_b2_in_interval_rond_b1_foverlaps <- function(dt1, dt2, kol_a, kol_c, marge, row_limit, 
                                                 nomatch, ...verbose, show_progress, logfile) {

  # dt1 en dt2 zijn kolomsubsets, var1 Cx
  stopifnot(!is.null(kol_a), !is.null(kol_c), !is.null(marge))
  stopifnot(kol_a %in% names(dt1), kol_c %in% names(dt1))
  stopifnot(kol_a %in% names(dt2), kol_c %in% names(dt2))

  dt1[, `:=`(
    #rowid = .I, 
    lower = fifelse(get(kol_c) >= 0, get(kol_c) * (1 - marge), get(kol_c) * (1 + marge)),
    upper = fifelse(get(kol_c) >= 0, get(kol_c) * (1 + marge), get(kol_c) * (1 - marge))
  )]
  dt1_valid <- dt1[!is.na(get(kol_c))]
  
  dt1_valid <- dt1_valid[is.finite(lower) & is.finite(upper) & !is.na(lower) & !is.na(upper)]
  dt1_valid[, c("lower", "upper") := .(pmin(lower, upper), pmax(lower, upper))]
  # lower en upper moeten in dt1 en dt2 hetzelfde type hebben:
  dt1_valid[, `:=` (lower = as.numeric(lower), upper = as.numeric(upper))]
  #dt1_valid[, c("lower", "upper") := lapply(.SD, as.numeric), .SDcols = c("lower", "upper")]
  
  #setkeyv(dt1_valid, c(kol_a, "lower", "upper")) # wijzigt rijvolgorde
  dt2_valid <- dt2[!is.na(get(kol_c))]
  dt2_valid[, `:=`(
    lower = get(kol_c),
    upper = get(kol_c)
  )]
  
  dt2_valid <- dt2_valid[is.finite(lower) & is.finite(upper) & !is.na(lower) & !is.na(upper)]
  dt2_valid[, c("lower", "upper") := .(pmin(lower, upper), pmax(lower, upper))]
  dt2_valid[, `:=` (lower = as.numeric(lower), upper = as.numeric(upper))]
  #dt2_valid[, c("lower", "upper") := lapply(.SD, as.numeric), .SDcols = c("lower", "upper")]
  
  # Toegevoegd: check op lege dt1_valid of dt2_valid vóór foverlaps-aanroep
  if (nrow(dt1_valid) == 0L || nrow(dt2_valid) == 0L) {
    message("⏩ Geen geldige rijen voor foverlaps (tel_b2).")
    
    # output vullen met 0 en structuur behouden
    dummy_out <- dt1[, .SD, .SDcols = c("rowid", kol_a, kol_c)]
    groep_totalen <- dt2[, .(
      n_a = .N,
      n_a_not_na = sum(!is.na(get(kol_c)))
    ), by = kol_a]
    setkeyv(groep_totalen, kol_a)
  
    out <- groep_totalen[dummy_out, on = kol_a]
    out[, `:=`(n_marge = 0L)]
    for (j in c("n_a", "n_a_not_na")) set(out, which(is.na(out[[j]])), j, 0L)
    out <- out[order(rowid)]
    out[, P_c := 0]
    setnames(out, "P_c", paste0("P_", kol_c))
    out[, c(kol_a, kol_c, "n_a", "n_a_not_na", "n_marge") := NULL]
    
    return(out[])
  }

  overlap_result <- tryCatch({
    # foverlaps_by_row_limit_with_fallback(dt2_valid, dt1_valid, kol_a = kol_a, 
    #                                      row_limit = row_limit, nomatch = nomatch, 
    #                                      .verbose = ...verbose, show_progress = show_progress, 
    #                                      logfile = logfile)
    foverlaps_count_by_row_limit_with_fallback(dt1_valid, dt2_valid, kol_a = kol_a, kol_c = kol_c,
                                         row_limit = row_limit, .verbose = ...verbose, 
                                         show_progress = show_progress, logfile = logfile)

  }, error = function(e) {
    message("eerste")
    message("❌ fout bij foverlaps():   ", e$message)
    message("🧪 N dt1_valid: ", nrow(dt1_valid))
    message("🧪 N dt2_valid: ", nrow(dt2_valid))
    message("🧪 kol_a type: ", paste(class(dt1_valid[[kol_a]]), "/", class(dt2_valid[[kol_a]])))
    message("🧪 lower/upper dt1: ", paste(range(dt1_valid$lower, na.rm = TRUE), collapse = " - "))
    message("🧪 lower/upper dt1: ", paste(range(dt1_valid$upper, na.rm = TRUE), collapse = " - "))
    message("🧪 lower/upper dt2: ", paste(range(dt2_valid$lower, na.rm = TRUE), collapse = " - "))
    message("🧪 lower/upper dt2: ", paste(range(dt2_valid$upper, na.rm = TRUE), collapse = " - "))
    return(NULL)
  })

  if (is.null(overlap_result)) {
    print("Unieke types van keykolommen 1:")
    print(length(unique(dt1_valid$key)))
    print(length(unique(dt1_valid$key)))
    
    print(sapply(dt1_valid[, .(kol_a = get(kol_a), lower, upper)], class))
    print(sapply(dt2_valid[, .(kol_a = get(kol_a), lower, upper)], class))

    print("Aantal rijen met lower > upper:")
    print(sum(dt1_valid$lower > dt1_valid$upper, na.rm = TRUE))
    print(sum(dt2_valid$lower > dt2_valid$upper, na.rm = TRUE))
    stop("foverlaps() is mislukt in tel_b2 – zie foutdetails hierboven.")
  }
  
  groep_totalen <- dt2[, .( # niet dt2_valid
    n_a = .N,
    n_a_not_na = sum(!is.na(get(kol_c)))
  ), by = kol_a]

  setkeyv(groep_totalen,kol_a)

  out <- groep_totalen[overlap_result, on = kol_a]
  
  #out <- out[order(rowid)]
  out[,P_c := N/n_a_not_na]
  out[n_a_not_na == 0, P_c := 0]
  out[is.na(P_c), P_c := 0]
  setnames(out, c("P_c","lower","upper"), paste0(c("P_","lower_","upper_"), kol_c))

  out[, c(kol_a,kol_c) := NULL]
  out[, c("n_a", "n_a_not_na", "N") := NULL]

  return(out[])
}


#' Functie 4 bereken exacte matches Cx var2
#' 
#' marge-interval rond b2 in dt2, matchend met b1 in dt1
#' 
#' @export
tel_b1_in_interval_rond_b2_foverlaps <- function(dt1, dt2, kol_a, kol_c, marge, row_limit, 
                                                 nomatch, ...verbose, show_progress, logfile) {
  #dt1 en dt2 zijn kolommen subsets, var2 Cx
 
  stopifnot(!is.null(kol_a), !is.null(kol_c), !is.null(marge))
  stopifnot(kol_a %in% names(dt1), kol_c %in% names(dt1))
  stopifnot(kol_a %in% names(dt2), kol_c %in% names(dt2))
  
  dt2[, `:=`(
    #rowid = .I, 
    lower = fifelse(get(kol_c) >= 0, get(kol_c) / (1 + marge), get(kol_c) / (1 - marge)),
    upper = fifelse(get(kol_c) >= 0, get(kol_c) / (1 - marge), get(kol_c) / (1 + marge))
  )]
  dt2_valid <- dt2[!is.na(get(kol_c))]
  
  dt2_valid <- dt2_valid[is.finite(lower) & is.finite(upper) & !is.na(lower) & !is.na(upper)]
  dt2_valid[, c("lower", "upper") := .(pmin(lower, upper), pmax(lower, upper))]
  dt2_valid[, `:=` (lower = as.numeric(lower), upper = as.numeric(upper))]
  #dt2_valid[, c("lower", "upper") := lapply(.SD, as.numeric), .SDcols = c("lower", "upper")]
  
  dt1_valid <- dt1[!is.na(get(kol_c))]
  dt1_valid[, `:=`(
    lower = get(kol_c),
    upper = get(kol_c)
  )]
  
  dt1_valid <- dt1_valid[is.finite(lower) & is.finite(upper) & !is.na(lower) & !is.na(upper)]
  dt1_valid[, c("lower", "upper") := .(pmin(lower, upper), pmax(lower, upper))]
  dt1_valid[, `:=` (lower = as.numeric(lower), upper = as.numeric(upper))]
  #dt1_valid[, c("lower", "upper") := lapply(.SD, as.numeric), .SDcols = c("lower", "upper")]
  
  # Toegevoegd: check op lege dt1_valid of dt2_valid vóór foverlaps-aanroep
  if (nrow(dt1_valid) == 0L || nrow(dt2_valid) == 0L) {
    message("⏩ Geen geldige rijen voor foverlaps (tel_b1).")
    
    dummy_out <- dt2[, .SD, .SDcols = c("rowid", kol_a, kol_c)]
    groep_totalen <- dt2[, .(
      n_a = .N,
      n_a_not_na = sum(!is.na(get(kol_c)))
    ), by = kol_a]
    setkeyv(groep_totalen, kol_a)
    
    out <- groep_totalen[dummy_out, on = kol_a]
    out[, `:=`(N_marge = 0L)]
    for (j in c("n_a", "n_a_not_na")) set(out, which(is.na(out[[j]])), j, 0L)
    out <- out[order(rowid)] # originele rijvolgorde dt2, ook door volgende merge
    out[, R_c := 0]
    setnames(out, "R_c", paste0("R_", kol_c))
    out[, c(kol_a, kol_c, "n_a", "n_a_not_na", "N_marge") := NULL]
    
    return(out[])
  }
  
  overlap_result <- tryCatch({
    # foverlaps_by_row_limit_with_fallback(dt1_valid, dt2_valid, kol_a = kol_a,
    #                                      row_limit = row_limit, nomatch = nomatch, 
    #                                      .verbose = ...verbose, show_progress = show_progress, 
    #                                      logfile = logfile)
    foverlaps_count_by_row_limit_with_fallback(dt2_valid, dt1_valid, kol_a = kol_a, kol_c = kol_c,
                                               row_limit = row_limit, .verbose = ...verbose, 
                                               show_progress = show_progress, logfile = logfile)
  }, error = function(e) {
    message("tweede")
    message("❌ fout bij foverlaps(): ", e$message)
    print(dt1_valid)
    print(dt2_valid)
    message("🧪 N dt1_valid: ", nrow(dt1_valid))
    message("🧪 N dt2_valid: ", nrow(dt2_valid))
    message("🧪 kol_a type: ", paste(class(dt1_valid[[kol_a]]), "/", class(dt2_valid[[kol_a]])))
    message("🧪 lower/upper dt1: ", paste(range(dt1_valid$lower, na.rm = TRUE), collapse = " - "))
    message("🧪 lower/upper dt1: ", paste(range(dt1_valid$upper, na.rm = TRUE), collapse = " - "))
    message("🧪 lower/upper dt2: ", paste(range(dt2_valid$lower, na.rm = TRUE), collapse = " - "))
    message("🧪 lower/upper dt2: ", paste(range(dt2_valid$upper, na.rm = TRUE), collapse = " - "))
    return(NULL)
  })
  
  if (is.null(overlap_result)) {
    print("Unieke types van keykolommen 2:")
    
    print(length(unique(dt1_valid$key)))
    print(length(unique(dt1_valid$key)))
    
    print(sapply(dt1_valid[, .(kol_a = get(kol_a), lower, upper)], class))
    print(sapply(dt2_valid[, .(kol_a = get(kol_a), lower, upper)], class))

    print("Aantal rijen met lower > upper:")
    print(sum(dt1_valid$lower > dt1_valid$upper, na.rm = TRUE))
    print(sum(dt2_valid$lower > dt2_valid$upper, na.rm = TRUE))
    stop("foverlaps() is mislukt in tel_b1 – zie foutdetails hierboven.")
  }
 
  groep_totalen <- dt2[, .( # niet dt2_valid
    n_a = .N,
    n_a_not_na = sum(!is.na(get(kol_c)))
  ), by = kol_a]

  setkeyv(groep_totalen,kol_a)
  out <- groep_totalen[overlap_result, on = kol_a]

  #out <- out[order(rowid)]
  out[,R_c := N/n_a_not_na]
  out[n_a_not_na == 0, R_c := 0]
  out[is.na(R_c), R_c := 0]

  setnames(out, c("R_c","lower","upper"), paste0(c("R_","lower_","upper_"), kol_c))
  out[, c(kol_a,kol_c) := NULL]
  out[, c("n_a", "n_a_not_na", "N") := NULL]
  
  return(out[])
  
}
