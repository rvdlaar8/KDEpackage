# voorbeeld 1 schatting b:
test_that("estimate_b1_counts_at_b2_values returns correct values for Rdak_b vb1", {
  dt2 <- data.table(a    = c("1.1", "1.1", "2.2"),  
                    b    = c(1, 2, 4),
                    a_cl = c("1.1", "1.1", "2.2"), # a_cl gelijk
                    b_cl = c(1, 2, 4)) # b_cl gelijk
  N1_a <- data.table(a = c("1.1", "2.2"), N_group = c(3, 1))
  
  res <- estimate_exact_match_per_rij_b2(dt2, 
                                         ag_a = N1_a, kol_N = "N_group",
                                         kol_a = "a",
                                         kol_a_cl = "a_cl",
                                         kol_b = "b",
                                         kol_b_cl = "b_cl")
  expect_true(is.data.table(res))
  expect_equal(nrow(res), nrow(dt2))
  expect_true(all(c("S_b") %in% names(res)))
  expect_true(all(!is.na(res$estimated_n_b)))
  
  correct <- c(3/4, 3/4, 1) # N_A * n_ab / n_a^2
  expect_true(all(res$estimated_n_b == correct))
})

# voorbeeld 2 schatting b:
test_that("estimate_b1_counts_at_b2_values returns correct values for Rdak_b vb2", {
  dt2 <- data.table(a          = c("1.1", "1.1", "2.2"),  
                    b          = c(1, 2, 4),
                    a_cl = c("1.1", "2.2", "2.2"), # a_cl verschilt
                    b_cl = c(1, 2, 3)) # b_cl verschilt
  N1_a <- data.table(a = c("1.1", "2.2"), N_group = c(3, 1))  
  
  res <- estimate_exact_match_per_rij_b2(dt2, 
                                         ag_a = N1_a, kol_N = "N_group",
                                         kol_a = "a",
                                         kol_a_cl = "a_cl",
                                         kol_b = "b",
                                         kol_b_cl = "b_cl")
  expect_true(is.data.table(res))
  expect_equal(nrow(res), nrow(dt2))
  expect_true(all(c("S_b") %in% names(res)))
  expect_true(all(!is.na(res$estimated_n_b)))
  
  correct <- c(0.75, 0, 0) # N_A * n_ab_clean / n_a_clean * n_ab/n_a_not_na / n_a_not_na
  expect_true(all(res$Rdak_b == correct))
})

# nog:
# voorbeeld 1 schatting c:
# test_that("estimate_c1_counts_in_c2_intervals KDE returns correct values for Rdak_c vb1", {

test_that("estimate_b1_counts_in_b2_intervals KDE returns correct dimensions", {
  dt2 <- data.table(a    = c("1.1", "1.1", "2.2"),  
                    c    = c(1.1, 2.1, 4.2),
                    a_cl = c("1.1", "1.1", "2.2"), 
                    c_cl = c(1.1, 2.1, 4.2)) # cl gelijk
  N1_a <- data.table(a = c("1.1", "2.2"), N_group = c(3, 1))
  
  res <- estimate_c1_counts_in_c2_intervals(dt2, 
                                            ag_a = N1_a, 
                                            kol_a = "a", 
                                            kol_a_cl = "a_cl",
                                            kol_N = "N_group", 
                                            kol_c = "c", 
                                            kol_c_cl = "c_cl",
                                            m = 0.2, logfile = NULL,
                                            row_limit = 5e6,
                                            ...verbose = F, timing = F)
  
  expect_true(is.data.table(res))
  expect_equal(nrow(res), nrow(dt2))
  expect_true(all(c("S_c","method_c") %in% names(res)))
  expect_true(all(!is.na(res$S_c)))
})