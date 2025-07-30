test_that("tel_exact_match_per_rij_dt1 Function 1 returns correct dimensions for B1", {
  dt1 <- data.table(a = c("1.1", "1.1", "2.2"), 
                    b = c(1, 2, 3))
  dt2 <- data.table(a = c("1.1", "1.1", "1.1", "2.2", "2.2"), 
                    b = c(1, 1, 2, 3, 4))
  
  res <- tel_exact_match_per_rij_dt1(dt1, dt2, "a", "b") # Function 1
  
  expect_equal(nrow(res), nrow(dt1))
  expect_true(all(c("P_b") %in% names(res)))
  expect_true(all(!is.na(res$P_b)))
  expect_true(all(res$P_b <= 1))
})

test_that("tel_exact_match_per_rij_dt1 Function 1 returns correct dimensions for Bx", {
  dt1 <- data.table(a          = c("1.1", "1.1", "1.1", "2.2"), b = c(1, 2, 3, 4))
  dt2 <- data.table(a          = c("1.1", "1.1", "2.2"),  
                    a_from_dt1 = c("1.1", "1.1", "2.2"), 
                    b          = c(1, 2, 4),
                    b_from_dt1 = c(1, 2, 4))
  N1_a <- data.table(a = c("1.1", "2.2"), N_group = c(3, 1))
  
  res <- tel_exact_match_per_rij_dt1(dt1, dt2, kol_a = "a", kol_b = "b")
  
  expect_true(is.data.table(res))
  expect_equal(nrow(res), nrow(dt1))
  expect_true(all(c("P_b") %in% names(res)))
  expect_true(all(!is.na(res$P_b)))
})

test_that("tel_exact_match_per_rij_dt1 Function 1 returns correct counts for B1", {
  # dt1 en dt2 moeten gelijk kolomnamen gebruiken
  dt1 <- data.table(a = c(1, 2,2, 3,3,3, 4,4,4,4, 5,5,5,5, 10),
                    b = c(5, 1,2, 1,2,4, 1,1,2,3, 1,2,3,4, 3))
  dt2 <- data.table(a          = c(1,1,2,2,3,3,3,4,4,4,4,5,5,5,5, 12,12,12),
                    a_from_dt1 = c(1,1,2,2,3,3,3,4,4,4,4,5,5,5,5, 12,12,12),
                    b          = c(NA,NA, 1,NA, 1,2,NA, 1,1,2,NA, 1,2,2,3, 1,2,3),
                    b_from_dt1 = c(NA,NA, 1,NA, 1,2,NA, 1,1,2,NA, 1,2,2,3, 1,2,3))
  
  res <- tel_exact_match_per_rij_dt1(dt1, dt2, kol_a = "a", kol_b = "b")
  
  expect_true(is.data.table(res))
  expect_equal(nrow(res), nrow(dt1))
  expect_true(all(c("P_b") %in% names(res)))
  expect_true(all(!is.na(res$P_b)))
  expect_true(all(res$P_b <= 1))
  
  correct <- c(0, 1,0, 1/2,1/2,0, 2/3,2/3,1/3,0, 1/4,1/2,1/4,0, 0)
  expect_true(all(res$P == correct))
})

test_that("tel_b2_in_interval_rond_b1_foverlaps Function 2 returns correct counts for C1", {
  # dt1 en dt2 moeten gelijk kolomnamen gebruiken
  dt1 <- data.table(a = c(6,6,6,6,    7, 8,8, 9,9,9,9, 11),
                    c = c(1,2,3.03,4, 5, 1,2, 1,2,3,4, 1))
  dt2 <- data.table(a          = c(6,6,6,6,6,6, 7,7, 8,8, 9,9,9,9,9, 13,13),
                    a_from_dt1 = c(6,6,6,6,6,6, 7,7, 8,8, 9,9,9,9,9, 13,13),
                    c          = c(1,1.98,2,2.02,3,NA, NA,NA, 1,NA, 1,1.04,2,4,NA, 1,2),
                    c_from_dt1 = c(1,1.98,2,2.02,3,NA, NA,NA, 1,NA, 1,1.04,2,4,NA, 1,2))
  # var 1 Cx
  res <- tel_b2_in_interval_rond_b1_foverlaps(dt1, dt2, 
                                              kol_a = "a", kol_c = "c", 
                                              marge = 0.05, row_limit = 5e5, 
                                              nomatch = 0L, ...verbose = F, 
                                              show_progress = F, logfile = NULL)
 
  expect_true(is.data.table(res))
  expect_equal(nrow(res), nrow(dt1))
  expect_true(all(c("P_c") %in% names(res)))
  expect_true(all(!is.na(res$P_c)))
  expect_true(all(res$P_c <= 1))
  
  correct <- c(0.2,0.6,0.2,0, 0, 1,0, 0.5,0.25,0,0.25, 0)
  expect_true(all(res$P_c == correct))
})

test_that("For C1 var1 equals var2", {
  set.seed(12)
  
  # dt1 en dt2 moeten gelijk kolomnamen gebruiken
  dt1 <- data.table(a = sample(letters,10000,replace = TRUE),
                    c = rnorm(10000, 5, 2))
  s <- sample(x = 1:10000, size = 2000, replace = FALSE)
  dt2 <- dt1[s]

  # var 1 Cx
  d_ex_totaal <- tel_b2_in_interval_rond_b1_foverlaps(dt1, dt2, 
                                              kol_a = "a", kol_c = "c", 
                                              marge = 0.05, row_limit = 5e5, 
                                              nomatch = 0L, ...verbose = F, 
                                              show_progress = F, logfile = NULL)
  # var 2 Cx
  s_ex_totaal <- tel_b1_in_interval_rond_b2_foverlaps(dt1, dt2, 
                                                      kol_a = "a", kol_c = "c", 
                                                      marge = 0.05, row_limit = 5e5, 
                                                      nomatch = 0L, ...verbose = F, 
                                                      show_progress = F, logfile = NULL)
  expect_true(is.data.table(d_ex_totaal))
  expect_true(is.data.table(s_ex_totaal))
  expect_equal(nrow(d_ex_totaal), nrow(dt1))
  expect_equal(nrow(s_ex_totaal), nrow(dt2))
  expect_true(all(c("P_c") %in% names(d_ex_totaal)))
  expect_true(all(c("R_c") %in% names(s_ex_totaal)))
  expect_true(all(d_ex_totaal$P_c <= 1))
  expect_true(sum(d_ex_totaal$P_c, na.rm = TRUE) == sum(s_ex_totaal$R_c, na.rm = TRUE))
})



