# Testdata voorbereiden
example_dt2 <- data.table(
  a       = c("A", "A", "A", "B", "B", "C"),
  b       = c(1, 1, 2, 2, NA, 3),
  a_cl    = c("A", "A", "A", "B", "B", "X"),
  b_cl    = c(1, 1, 2, 2, NA, 9)
)

example_ag_a <- data.table(
  a = c("A", "B"),
  N_group = c(100, 50)
)

# Verwachte output-kolom
expected_col <- "S_b"

# De test

test_that("estimate_exact_match_per_rij_b2 werkt correct", {
  library(data.table)
  
  result <- estimate_exact_match_per_rij_b2(
    dt2         = copy(example_dt2),
    ag_a        = copy(example_ag_a),
    kol_N       = "N_group",
    kol_a       = "a",
    kol_b       = "b",
    kol_a_cl    = "a_cl",
    kol_b_cl    = "b_cl"
  )
  
  # 1. Zelfde aantal rijen als input
  expect_equal(nrow(result), nrow(example_dt2))
  
  # 2. Kolom bestaat
  expect_true(expected_col %in% names(result))
  
  # 3. Waarden zijn numeriek en geen NA's
  expect_type(result[[expected_col]], "double")
  expect_false(any(is.na(result[[expected_col]])))
  
  # 4. Specifieke waarde controleren (manueel berekend)
  # Bijv. eerste rij heeft a = "A", b = 1 → verwacht kleine > 0 waarde
  expect_gt(result[1][[expected_col]], 0)
  
  # 5. Rijen met NA in b (zoals rij 5) moeten risico 0 krijgen
  expect_equal(result[5][[expected_col]], 0)
})
