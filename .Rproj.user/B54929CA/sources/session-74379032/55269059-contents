# Vergelijk of twee data.tables exact gelijk zijn, ongeacht rijvolgorde
compare_data_tables_diff_long <- function(dt1, dt2, key_col = NULL) {
  stopifnot(data.table::is.data.table(dt1), data.table::is.data.table(dt2))
  stopifnot(identical(names(dt1), names(dt2)))
  
  common_cols <- names(dt1)
  if (!is.null(key_col)) {
    stopifnot(key_col %in% common_cols)
    setkeyv(dt1, key_col)
    setkeyv(dt2, key_col)
    dt1 <- dt1[dt2, on = key_col, nomatch = 0, allow.cartesian = TRUE]
    dt2 <- dt2[dt1, on = key_col, nomatch = 0, allow.cartesian = TRUE]
  }
  
  # Detect verschillen per kolom en rij
  diff_list <- lapply(common_cols, function(col) {
    v1 <- dt1[[col]]
    v2 <- dt2[[col]]
    neq <- which(!(is.na(v1) & is.na(v2)) & (is.na(v1) | is.na(v2) | v1 != v2))
    if (length(neq) > 0) {
      data.table::data.table(
        row = neq,
        kolom = col,
        waarde_dt1 = v1[neq],
        waarde_dt2 = v2[neq]
      )
    } else {
      NULL
    }
  })
  
  # Combineer tot 1 lange data.table
  res <- data.table::rbindlist(diff_list, use.names = TRUE, fill = TRUE)
  
  # Als key_col is opgegeven, voeg die toe voor interpretatie
  if (!is.null(key_col) && nrow(res) > 0) {
    res[, (key_col) := dt1[[key_col]][res$row]]
    data.table::setcolorder(res, c(key_col, "row", "kolom", "waarde_dt1", "waarde_dt2"))
  }
  
  res[]
}
library(data.table)

dt1 <- data.table(id = 1:3, x = c(1, 2, 3), y = c("a", "b", "c"))
dt2 <- data.table(id = 1:3, x = c(1, 99, 3), y = c("a", "b", "z"))

compare_data_tables_diff_long(dt1, dt2, key_col = "id")
