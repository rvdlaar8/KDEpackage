test_that("matching_pipeline variant 1 adds expected columns", {
  dt1 <- data.table(a1 = c("x", "x", "y"), a2 = c("p", "q", "r"), 
                    b = c(1, 2, 3), c = c(7.1,7.2,8.8))
  dt2 <- data.table(a1 = c("x", "x", "y", "y"), a2 = c("p", "q", "q", "r"), 
                    b = c(1, 2, 3, 4), c = c(7.1,7.2,8.8,11))
  #N1_a <- data.table(a = c("x", "y"), N1_a = c(12, 15))
  
  concat_columns(dt1, c("a1.a2"), "a", sep = ".")
  concat_columns(dt2, c("a1.a2"), "a", sep = ".")
  
  nrow_dt_0 <- nrow(dt1)
  res <- matching_pipeline_variant(dt1 = dt1, dt2 = dt2, 
                            kol_a = "a",
                            bx = c("b"), 
                            variant = "var1",
                            marge = 0.05)
  dt1 <- res$dt1
  dt2 <- res$dt2
  
  expect_equal(nrow_dt_0, nrow(dt1))
  expect_true("P_b" %in% names(dt1))
})

