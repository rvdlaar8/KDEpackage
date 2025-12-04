test_that("fishing 1 counts c_v and n_v", {
  # test dataset 1:
  dt <- data.table(
    g  = c("A","A","A",   # groep A
           "B","B","B",   # groep B
           "C","C"),      # groep C
    c1 = c(1, 1, 1,       # A: constant, geen NA
           2, 2, NA,      # B: bevat NA → hele groep B telt niet mee voor c1
           3, 3),         # C: constant, geen NA
    c2 = c(5, 6, 5,       # A: niet constant
           7, 7, 7,       # B: constant, geen NA
           NA, 9),        # C: bevat NA → hele groep C telt niet mee voor c2
    n1 = c(100, 100.5, 99.8,  # A: alle waarden binnen ±1%
           50, 60, 55,        # B: verder uit elkaar
           10, NA),           # C: bevat NA → hele groep C telt niet mee voor n1
    n2 = c(10, 10.05, 9.95,   # A: binnen ±1%
           20, 20.1, 19.9,    # B: ook binnen ±1%
           5, 5.3)            # C: net iets te ver uit elkaar voor 1%
  )
  
  dt[]
  
  k_v <- "g"
  c_v <- c("c1", "c2")
  n_v <- c("n1", "n2")
  p   <- 0.01  # 1%
  
  result1 <- count_group_constraints_fast(dt, k_v, c_v, n_v, p)

  # test dataset 2:
  dt_test <- data.table(
    g  = c("A","A","B","B","C","C"),
    c1 = c(1, 1, 2, 2, 3, NA),   # c1: alleen groep A volledig goed (constant + geen NA)
    n_pos = c(10, 10.05,  20, 20.1,  30, 30.2),  # allemaal > 0
    n_mix = c(5, 5.05,     0, 0.01,  -1, -1.01)  # bevat 0 en negatieve waarden
  )
  
  dt_test[]
  
  k_v <- "g"
  c_v <- "c1"
  n_v <- c("n_pos", "n_mix")
  p   <- 0.01   # 1% marge
  
  result2 <- count_group_constraints_fast(dt_test, k_v, c_v, n_v, p)
  
  expect_equal(result1, c(c1 = 5L, c2 = 3L, n1 = 3L, n2 = 0L))
  expect_equal(result2, c(c1 = 4L, n_pos = 6L, n_mix = NA))
})



