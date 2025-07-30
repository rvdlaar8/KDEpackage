test_that("matching_pipeline_variant werkt voor var1", {
  dt1 <- data.table(a = c("x", "x", "y"), b = c(1, 2, 3), d = c(4, 5, 6))
  dt2 <- data.table(a = c("x", "y", "x"), b = c(1, 3, 2), d = c(4, 6, 5))
  
  res <- matching_pipeline_variant(
    dt1 = dt1,
    dt2 = dt2,
    kol_a = "a",
    bx = c("b", "d"),
    variant = "var1"
  )
  dt1 <- res$dt1
  dt2 <- res$dt2
  
  expect_true(all(c("P_b", "P_d") %in% names(dt1)))
  expect_equal(nrow(dt1), 3)
})

test_that("matching_pipeline_variant werkt voor var2", {
  dt1 <- data.table(a = c("x", "x", "y"), b = c(1, 2, 3), d = c(4, 5, 6))
  dt2 <- data.table(a = c("x", "y", "x"), b = c(1, 3, 2), d = c(4, 6, 5))
  
  res <- matching_pipeline_variant(
    dt1 = dt1,
    dt2 = dt2,
    kol_a = "a",
    bx = c("b", "d"),
    variant = "var2"
  )
  dt1 <- res$dt1
  dt2 <- res$dt2
  
  expect_true(all(c("R_b", "R_d") %in% names(dt2)))
  expect_equal(nrow(dt2), 3)
})

test_that("matching_pipeline_variant werkt voor var3 met schatting", {
  dt2 <- data.table(
    a = c("x", "x", "y", "y"),
    c = c(10, 20, 30, 40),
    d = c(100, 200, 300, 400),
    a_cl = c("x", "x", "y", "y"),
    c_cl = c(10, 20, 30, 40),
    d_cl = c(100, 200, 300, 400)
  )
  ag_a <- data.table(a = c("x", "y"), N_group = c(100, 80))
  
  res <- matching_pipeline_variant(
    dt2 = dt2,
    kol_a = "a",
    kol_a_cl = "a_cl",
    cx = c("c", "d"),
    cx_cl = c("c_cl", "d_cl"),
    ag_a = ag_a,
    kol_N = "N_group",
    variant = "var3",
    marge = 0.1,
    ..verbose = FALSE
  )
  dt1 <- res$dt1
  dt2 <- res$dt2
  
  expect_true(all(c("S_c", "S_d") %in% names(dt2)))
  expect_equal(nrow(dt2), 4)
})