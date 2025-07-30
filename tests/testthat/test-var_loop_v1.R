test_that("matching_pipeline var 1 expected columns", {
  dt1 <- data.table(a = c("x", "x", "y"), b = c(1, 2, 3), c = c(7.5,8.5,11))
  dt2 <- data.table(a = c("x", "x", "y"), b = c(1, 2, 3), c = c(6,2,7))

  nrow_dt10 <- nrow(dt1)
  res <- matching_pipeline_variant(dt1, dt2, variant = "var1", 
                                   kol_a = "a", bx = c("b"), cx = c("c"),
                                   marge = 0.2)
  dt1 <- res$dt1
  dt2 <- res$dt2
  expect_equal(nrow_dt10, nrow(dt1))
  expect_true(all(c("a","b","c","P_b","P_c") %in% names(dt1)))
})

test_that("matching_pipeline wrapper var 2 expected columns", {
  dt1 <- data.table(a = c("x", "x", "y"), b = c(1, 2, 3), c = c(7.5,8.5,11))
  dt2 <- data.table(a = c("x", "x", "y"), b = c(1, 2, 3), c = c(6,2,7))

  nrow_dt20 <- nrow(dt2)
  res <- matching_pipeline_variant(dt1, dt2, variant = "var2", 
                                   kol_a = "a", bx = c("b"), cx = c("c"),
                                   marge = 0.2)
  dt1 <- res$dt1
  dt2 <- res$dt2
  
  expect_equal(nrow_dt20, nrow(dt2))
  expect_true(all(c("a","b","c","R_b","R_c") %in% names(dt2)))
})

test_that("matching_pipeline wrapper var 3 expected columns", {
  dt2 <- data.table(a1 = c("x", "x", "y"), b1 = c(1, 2, 3), c1 = c(6,2,7),
                    a1_cl = c("x", "x", "y"), b1_cl = c(1, 2, 3), c1_cl = c(6,2,7))
  N1_a <- data.table(a1 = c("x", "y"), N1_a = c(12, 21))
  
  nrow_dt20 <- nrow(dt2)
  res <- matching_pipeline_variant(dt2 = dt2, variant = "var3", 
                                   kol_a = "a1", bx = c("b1"), cx = c("c1"),
                                   kol_a_cl = "a1_cl", bx_cl = c("b1_cl"), cx_cl = c("c1_cl"),
                                   ag_a = N1_a, kol_N = "N1_a",
                                   marge = 0.05,  # auto_bw = FALSE, 
                                   ..verbose = TRUE)
  dt2 <- res$dt2
  
  expect_equal(nrow_dt20, nrow(dt2))
  expect_true(all(c("a1","b1","c1", "a1_cl","b1_cl","c1_cl", "a1","a1_cl",
                    "S_b1","S_c1","method_c1") %in% names(dt2)))
})

