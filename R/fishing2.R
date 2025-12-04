#' Count records under group-wise constancy and tolerance constraints
#'
#' Given a [data.table][data.table::data.table] \code{dt} and a grouping variable
#' \code{k_v}, this function counts, for each column in \code{c_v}, how many
#' records belong to groups where that column is constant and contains no
#' missing values; and for each column in \code{n_v}, how many records belong
#' to groups where that column is strictly positive, contains no missing
#' values, and all values within the group fall within a relative band of
#' \code{±p} around each other.
#'
#' More precisely:
#' \itemize{
#'   \item For each \code{c} in \code{c_v}, a group (defined by \code{k_v})
#'   contributes \code{.N} rows to the total for that column if:
#'   \itemize{
#'     \item \code{anyNA(c) == FALSE} within the group, and
#'     \item \code{uniqueN(c) == 1} within the group (i.e. constant).
#'   }
#'
#'   \item For each \code{n} in \code{n_v}, the column is first checked
#'   globally: if there exists any non-missing value \code{<= 0}, the output
#'   for that column is \code{NA_integer_} and no group-wise calculation is
#'   performed.
#'
#'   Otherwise (all non-missing values > 0), a group contributes \code{.N}
#'   rows to the total for that column if:
#'   \itemize{
#'     \item the column has no \code{NA} within that group, and
#'     \item letting \code{mn = min(x)}, \code{mx = max(x)} in the group,
#'       the relative spread satisfies
#'       \code{mx <= (1 + p) * mn}.
#'   }
#'   If either condition fails, that group contributes 0 for that column.
#' }
#'
#' Columns listed in \code{c_v} and \code{n_v} may overlap; they are treated
#' independently under their respective rules.
#'
#' @param dt A \code{data.table}. Will be coerced to \code{data.table} by
#'   reference if it is a plain \code{data.frame}.
#' @param k_v Character scalar; name of the grouping column in \code{dt}.
#' @param c_v Character vector of column names in \code{dt} to be checked
#'   under the “constant + no NA per group” rule. May be \code{NULL} or
#'   length 0, in which case no counts are produced for \code{c_v}.
#' @param n_v Character vector of column names in \code{dt} to be checked
#'   under the “strictly positive + within ±p% + no NA per group” rule.
#'   May be \code{NULL} or length 0, in which case no counts are produced
#'   for \code{n_v}.
#' @param p Numeric scalar in \code{[0, 1)}; the relative band as a fraction.
#'   For example, \code{p = 0.01} corresponds to a ±1\% band.
#'
#' @return
#' An integer named vector of length \code{length(c_v) + length(n_v)}.
#'
#' \itemize{
#'   \item The first \code{length(c_v)} elements correspond to \code{c_v},
#'     in the same order, and contain the total number of records whose groups
#'     satisfy the constancy + no-NA condition for that column.
#'   \item The last \code{length(n_v)} elements correspond to \code{n_v}, in
#'     the same order. For each column that globally violates the
#'     \code{> 0} assumption (i.e. has at least one non-missing value
#'     \code{<= 0}), the result is \code{NA_integer_}. For the remaining
#'     columns, the value is the total number of records whose groups satisfy
#'     the positivity + no-NA + relative-band condition.
#' }
#'
#' If both \code{c_v} and \code{n_v} are \code{NULL} or length 0, the function
#' returns an empty integer vector.
#'
#' @examples
#' library(data.table)
#'
#' dt <- data.table(
#'   g  = c("A","A","A","B","B","B","C","C"),
#'   c1 = c(1, 1, 1,   2, 2, NA,   3, 3),
#'   n1 = c(100, 100.5, 99.8,  50, 60, 55,  10, NA),
#'   n2 = c(10, 10.05, 9.95,  20, 20.1, 19.9,  5, 5.3)
#' )
#'
#' # Count constraints per group g:
#' count_group_constraints_fast(
#'   dt   = dt,
#'   k_v  = "g",
#'   c_v  = "c1",
#'   n_v  = c("n1", "n2"),
#'   p    = 0.01
#' )
#'
#' @export
#' @import data.table
count_group_constraints_fast <- function(dt,
                                         k_v,
                                         c_v = NULL,
                                         n_v = NULL,
                                         p) {
  # Coerce to data.table if needed
  if (!data.table::is.data.table(dt)) {
    data.table::setDT(dt)
  }
  
  # Basic argument checks
  if (!is.character(k_v) || length(k_v) != 1L) {
    stop("`k_v` must be a single column name (character scalar).", call. = FALSE)
  }
  
  # Allow NULL for c_v and n_v
  if (is.null(c_v)) c_v <- character(0L)
  if (is.null(n_v)) n_v <- character(0L)
  
  all_cols <- c(k_v, c_v, n_v)
  missing_cols <- setdiff(all_cols, names(dt))
  if (length(missing_cols)) {
    stop(
      "Missing columns in `dt`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!is.numeric(p) || length(p) != 1L || p < 0 || p >= 1) {
    stop("`p` must be a single numeric in [0, 1). Use 0.01 for 1%.", call. = FALSE)
  }
  
  ## -------------------------
  ## 1. c_v: constant + geen NA
  ## -------------------------
  res_c <- integer(0L)
  
  if (length(c_v)) {
    tmp_c <- dt[
      ,
      {
        lapply(.SD, function(x) {
          # Groep telt alleen mee als:
          # - geen NA's in deze kolom
          # - alle waarden gelijk
          if (anyNA(x)) return(0L)
          if (data.table::uniqueN(x) == 1L) .N else 0L
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
  res_n <- integer(0L)
  
  if (length(n_v)) {
    # 2a. Alle n_v moeten numeriek zijn
    is_num <- vapply(dt[, ..n_v], is.numeric, logical(1L))
    if (!all(is_num)) {
      stop("All `n_v` columns must be numeric.", call. = FALSE)
    }
    
    # 2b. Per n_v-kolom checken of er niet-NA waarden <= 0 zijn.
    #     Als dat zo is, geven we NA terug voor die kolom.
    valid_positive <- vapply(
      n_v,
      function(col) {
        x <- dt[[col]]
        # TRUE als er GEEN niet-NA waarden <= 0 zijn
        !any(!is.na(x) & x <= 0)
      },
      logical(1L)
    )
    
    good_n <- n_v[valid_positive]   # kolommen die aan > 0-voorwaarde voldoen
    bad_n  <- n_v[!valid_positive]  # kolommen die de aanname schenden
    
    # Maak alvast outputvector voor n_v gevuld met NA
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
            
            # Door valid_positive weten we dat alle niet-NA waarden > 0
            # (mn > 0), extra check is vooral defensief.
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
      res_n_good  <- as.integer(unlist(sums_n_good, use.names = FALSE))
      names(res_n_good) <- good_n
      
      # Vul alleen de "goede" kolommen in; de "slechte" blijven NA
      res_n[match(good_n, n_v)] <- res_n_good
    }
    
    # bad_n blijven NA in res_n
  }
  
  # Gecombineerde outputvector (kan ook leeg zijn)
  c(res_c, res_n)
}
