library(data.table)

count_group_constraints <- function(dt, k_v, c_v = character(), n_v = character(), p) {
  # p is fractie: 0.01 = 1%
  if (!is.data.table(dt)) stop("dt must be a data.table")
  if (!is.character(k_v) || length(k_v) != 1L) {
    stop("k_v must be a single column name (character scalar).")
  }
  
  # Sta expliciet NULL toe voor c_v en n_v
  if (is.null(c_v)) c_v <- character(0)
  if (is.null(n_v)) n_v <- character(0)
  
  all_cols <- c(k_v, c_v, n_v)
  missing_cols <- setdiff(all_cols, names(dt))
  if (length(missing_cols)) {
    stop("Missing columns in dt: ", paste(missing_cols, collapse = ", "))
  }
  if (!is.numeric(p) || length(p) != 1L || p < 0 || p >= 1) {
    stop("p must be a single numeric in [0,1). Use 0.01 for 1%.")
  }
  
  ## -------------------------
  ## 1. c_v: constant + geen NA
  ## -------------------------
  res_c <- integer(0)
  
  if (length(c_v)) {
    tmp_c <- dt[
      ,
      {
        lapply(.SD, function(x) {
          # Groep telt alleen mee als:
          # - geen NA's in deze kolom
          # - alle waarden gelijk
          if (anyNA(x)) return(0L)
          if (uniqueN(x) == 1L) .N else 0L
        })
      },
      by = k_v,
      .SDcols = c_v
    ]
    
    sums_c <- tmp_c[, lapply(.SD, sum), .SDcols = c_v]
    res_c  <- as.integer(unlist(sums_c, use.names = FALSE))
    names(res_c) <- c_v
  }
  
  ## -----------------------------------------------
  ## 2. n_v: > 0 check + binnen ±p% + geen NA in groep
  ## -----------------------------------------------
  res_n <- integer(0)
  
  if (length(n_v)) {
    # 2a. Eerst: alle n_v numeriek?
    is_num <- vapply(dt[, ..n_v], is.numeric, logical(1L))
    if (!all(is_num)) {
      stop("All n_v columns must be numeric.")
    }
    
    # 2b. Per n_v-kolom checken of er niet-NA waarden <= 0 zijn.
    #     Als dat zo is, geven we uiteindelijk NA terug voor die kolom.
    valid_positive <- vapply(
      n_v,
      function(col) {
        x <- dt[[col]]
        # TRUE als er GEEN niet-NA waarden <= 0 zijn
        !any(!is.na(x) & x <= 0)
      },
      logical(1L)
    )
    
    good_n <- n_v[valid_positive]      # kolommen met alleen > 0 (of NA)
    bad_n  <- n_v[!valid_positive]     # kolommen waar n_v > 0 aanname wordt geschonden
    
    # Outputvector voor n_v alvast vullen met NA
    res_n <- rep.int(NA_integer_, length(n_v))
    names(res_n) <- n_v
    
    if (length(good_n)) {
      tmp_n <- dt[
        ,
        {
          lapply(.SD, function(x) {
            # Groep met NA in deze kolom telt helemaal niet mee
            if (anyNA(x)) return(0L)
            if (!length(x)) return(0L)
            
            r  <- range(x)
            mn <- r[1L]
            mx <- r[2L]
            
            # Door valid_positive weten we dat alle niet-NA waarden > 0,
            # dus mn > 0. Extra check is niet strikt nodig, maar kan blijven:
            if (mn <= 0) return(0L)
            
            # Criterium: alle waarden binnen elkaars ±p%
            # ⇔ max(x) <= (1 + p) * min(x)
            if (mx <= (1 + p) * mn) length(x) else 0L
          })
        },
        by = k_v,
        .SDcols = good_n
      ]
      
      sums_n_good <- tmp_n[, lapply(.SD, sum), .SDcols = good_n]
      
      res_n_good <- as.integer(unlist(sums_n_good, use.names = FALSE))
      names(res_n_good) <- good_n
      
      # Vul alleen de "goede" kolommen in; de "slechte" blijven NA
      res_n[match(good_n, n_v)] <- res_n_good
    }
    
    # bad_n blijven expliciet NA in res_n
  }
  
  # Gecombineerde outputvector
  c(res_c, res_n)
}

