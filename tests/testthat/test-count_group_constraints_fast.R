# tests/testthat/test-count_group_constraints_fast.R

test_that("count_group_constraints_fast: basic c_v behaviour (constant + no NA)", {
  library(data.table)
  
  dt <- data.table(
    g  = c("A","A","A",
           "B","B","B",
           "C","C"),
    c1 = c(1, 1, 1,    # A: constant, geen NA
           2, 2, NA,   # B: heeft NA -> telt niet mee
           3, 3),      # C: constant, geen NA
    c2 = c(5, 6, 5,    # A: niet constant
           7, 7, 7,    # B: constant, geen NA
           NA, 9)      # C: heeft NA -> telt niet mee
  )
  
  res <- count_group_constraints_fast(
    dt   = dt,
    k_v  = "g",
    c_v  = c("c1", "c2"),
    n_v  = NULL,
    p    = 0.01
  )
  
  # Verwachting:
  # c1: A=3, B=0, C=2 -> 5
  # c2: A=0, B=3, C=0 -> 3
  expect_named(res, c("c1", "c2"))
  expect_identical(res[["c1"]], 5L)
  expect_identical(res[["c2"]], 3L)
})

test_that("count_group_constraints_fast: n_v ±p% band en NA binnen groepen", {
  library(data.table)
  
  dt <- data.table(
    g  = c("A","A","A",
           "B","B","B",
           "C","C"),
    n1 = c(100, 100.5, 99.8,  # A: binnen ±1% -> telt 3
           50, 60, 55,        # B: te brede spreiding -> telt 0
           10, NA),           # C: NA in groep -> hele groep telt niet mee
    n2 = c(10, 10.05, 9.95,   # A: net buiten ±1% (10.05 > 1.01*9.95)
           20, 20.1, 19.9,    # B: idem, net buiten
           5, 5.3)            # C: te brede spreiding
  )
  
  res <- count_group_constraints_fast(
    dt   = dt,
    k_v  = "g",
    c_v  = NULL,
    n_v  = c("n1", "n2"),
    p    = 0.01
  )
  
  # Verwachting:
  # n1: alleen groep A (3 rijen) -> 3
  # n2: geen enkele groep voldoet -> 0
  expect_named(res, c("n1", "n2"))
  expect_identical(res[["n1"]], 3L)
  expect_identical(res[["n2"]], 0L)
})

test_that("count_group_constraints_fast: n_v kolommen met waarden <= 0 geven NA", {
  library(data.table)
  
  dt <- data.table(
    g      = c("A","A","B","B","C","C"),
    c1     = c(1, 1, 2, 2, 3, 3),
    n_pos  = c(10, 10.05, 20, 20.1, 30, 30.2),  # allemaal > 0
    n_mix  = c(5, 5.05, 0, 0.01, -1, -1.01)     # bevat 0 en < 0
  )
  
  res <- count_group_constraints_fast(
    dt   = dt,
    k_v  = "g",
    c_v  = "c1",
    n_v  = c("n_pos", "n_mix"),
    p    = 0.01
  )
  
  # c1: alle groepen constant en geen NA -> 6
  expect_identical(res[["c1"]], 6L)
  
  # n_pos: alle groepen binnen ±1% -> 6
  expect_identical(res[["n_pos"]], 6L)
  
  # n_mix: bevat niet-NA waarden <= 0 -> NA
  expect_true(is.na(res[["n_mix"]]))
})

test_that("count_group_constraints_fast: c_v en n_v mogen NULL of lengte 0 zijn", {
  library(data.table)
  
  dt <- data.table(
    g  = c("A","A","A"),
    x  = 1:3
  )
  
  # Beide NULL -> lege vector
  res1 <- count_group_constraints_fast(
    dt   = dt,
    k_v  = "g",
    c_v  = NULL,
    n_v  = NULL,
    p    = 0.01
  )
  expect_identical(res1, integer(0))
  
  # Alleen c_v
  res2 <- count_group_constraints_fast(
    dt   = dt,
    k_v  = "g",
    c_v  = "x",
    n_v  = character(0),
    p    = 0.01
  )
  expect_named(res2, "x")
  
  # Alleen n_v
  res3 <- count_group_constraints_fast(
    dt   = dt,
    k_v  = "g",
    c_v  = character(0),
    n_v  = "x",
    p    = 0.01
  )
  expect_named(res3, "x")
})

test_that("count_group_constraints_fast: errors on missing columns, bad types or p out of range", {
  library(data.table)
  
  dt <- data.table(
    g  = c("A","A","B","B"),
    x  = 1:4
  )
  
  # k_v must exist
  expect_error(
    count_group_constraints_fast(dt, "h", "x", NULL, p = 0.01),
    "Missing columns"
  )
  
  # n_v must be numeric
  dt_bad <- copy(dt)
  dt_bad[, x := as.character(x)]
  expect_error(
    count_group_constraints_fast(dt_bad, "g", NULL, "x", p = 0.01),
    "must be numeric"
  )
  
  # p out of range
  expect_error(
    count_group_constraints_fast(dt, "g", "x", NULL, p = -0.1),
    "must be a single numeric in \\[0, 1\\)"
  )
  expect_error(
    count_group_constraints_fast(dt, "g", "x", NULL, p = 1.0),
    "must be a single numeric in \\[0, 1\\)"
  )
})

test_that("count_group_constraints_fast: column order and names are preserved", {
  library(data.table)
  
  dt <- data.table(
    g   = c("A","A","B","B"),
    c1  = c(1, 1, 2, 2),
    c2  = c(3, 3, 3, 3),
    n1  = c(10, 10.1, 20, 20.1),
    n2  = c(5, 5.02, 5, 5.02)
  )
  
  res <- count_group_constraints_fast(
    dt   = dt,
    k_v  = "g",
    c_v  = c("c2", "c1"),
    n_v  = c("n2", "n1"),
    p    = 0.05
  )
  
  expect_identical(names(res), c("c2", "c1", "n2", "n1"))
})