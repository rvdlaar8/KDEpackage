test_that("matching_pipeline_variant value tests var1", {
  dt1 <- data.table(a = c("x", "x", "y"), b = c(1, 2, 3)) # a is al de key
  dt2 <- data.table(a = c("x", "y", "x"), b = c(1, 3, 2))
  
  res <- matching_pipeline_variant(
    dt1 = dt1,
    dt2 = dt2,
    kol_a = "a",
    bx = "b",
    variant = "var1"
  )
  dt1 <- res$dt1
  dt2 <- res$dt2
  
  expect_true("P_b" %in% names(dt1))
  expect_equal(nrow(dt1), 3)
})

test_that("matching_pipeline_variant value tests var2", {
  dt1 <- data.table(a = c("x", "x", "y"), b = c(1, 2, 3))
  dt2 <- data.table(a = c("x", "y", "x"), b = c(1, 3, 2))
  
  res <- matching_pipeline_variant(
    dt1 = dt1,
    dt2 = dt2,
    kol_a = "a",
    bx = "b",
    variant = "var2"
  )
  dt1 <- res$dt1
  dt2 <- res$dt2
  
  expect_true("R_b" %in% names(dt2))
  expect_equal(nrow(dt2), 3)
})

test_that("matching_pipeline_variant dimension tests var3", {
  dt2 <- data.table(
    a = c("x", "x", "y", "y"),
    c = c(10, 20, 30, 40),
    a_cl = c("x", "x", "y", "y"),
    c_cl = c(10, 20, 30, 40)
  )
  ag_a <- data.table(a = c("x", "y"), N_group = c(100, 80))
  
  res <- matching_pipeline_variant(
    dt2 = dt2,
    kol_a = "a",
    kol_a_cl = "a_cl",
    cx = "c",
    cx_cl = "c_cl",
    ag_a = ag_a,
    kol_N = "N_group",
    variant = "var3",
    marge = 0.1,
    auto_bw = TRUE,
    ..verbose = FALSE
  )
  dt1 <- res$dt1
  dt2 <- res$dt2
  
  expect_true(all(c("S_c","lower_c","upper_c","method_c") %in% names(dt2)))
  expect_equal(nrow(dt2), 4)
})