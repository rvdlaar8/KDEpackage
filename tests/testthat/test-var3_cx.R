test_that("estimate_c1_counts_in_c2_intervals werkt correct op kleine testdata", {
  library(data.table)
  
  dt2 <- data.table( # a is al de key
    a = c("g1", "g1", "g2", "g2", "g3"), 
    c = c(10, 12, 20, 21, 30),
    a_cl = c("g1", "g1", "g2", "g2", "g3"),
    c_cl = c(11, 13, 19, 22, 30)
  )
  
  ag_a <- data.table(a = c("g1", "g2", "g3"), N_group = c(10L, 20L, 5L))
  
  res <- estimate_c1_counts_in_c2_intervals(
    dt2 = dt2,
    kol_a = "a",
    kol_c = "c",
    kol_a_cl = "a_cl",
    kol_c_cl = "c_cl",
    ag_a = ag_a,
    kol_N = "N_group",
    m = 0.1,
    auto_bw = TRUE,
    ...verbose = FALSE,
    timing = FALSE
  )

  expect_true(is.data.table(res))
  expect_true("S_c" %in% names(res))
  expect_equal(nrow(res), nrow(dt2))
  expect_false(any(is.na(res$S_c)))
})

test_that("estimate_c1_counts_in_c2_intervals werkt correct op kleine dataset", {
  library(data.table)
  
  # Simuleer voorbeelddata
  dt2 <- data.table(
    a = rep(c("groep1", "groep2"), each = 5),
    c = c(1:5, 11:15),
    a_cl = rep(c("groep1", "groep2"), each = 5),
    c_cl = c(1.1, 2.2, 3.0, 4.1, 5.0, 11.2, 12.2, 13.1, 14.0, 15.0)
  )
  
  # Aggregatietabel met N per groep
  ag_a <- dt2[, .(N = .N), by = a]
  
  # Roep functie aan
  out <- estimate_c1_counts_in_c2_intervals(
    dt2 = dt2,
    kol_a = "a",
    kol_c = "c",
    kol_a_cl = "a_cl",
    kol_c_cl = "c_cl",
    ag_a = ag_a,
    kol_N = "N",
    m = 0.1,
    auto_bw = FALSE,
    row_limit = 5e5,
    nomatch = 0L,
    ...verbose = FALSE,
    show_progress = FALSE,
    logfile = NULL,
    timing = FALSE
  )
  
  # Check: aantal rijen gelijk
  expect_equal(nrow(out), nrow(dt2))
  
  # Check: output bevat juiste kolommen
  verwachte_kolommen <- c(
    paste0("S_", "c"),
    paste0("method_", "c"),
    paste0("lower_", "c"),
    paste0("upper_", "c")
  )
  expect_true(all(verwachte_kolommen %in% names(out)))
  
  # Check: estimated_n_c is numeriek
  expect_type(out[[paste0("S_", "c")]], "double")
})

test_that("estimate_c1_counts_in_c2_intervals verwerkt randgevallen correct", {
  library(data.table)
  
  # Bouw testdata op met expliciete groepen
  dt2 <- data.table(
    a = c("g1", "g2", "g3", "g4", "g5", "g5", "g6", "g6", "g7", "g8"),
    c =  c(10,   20,   30,   40,   50,   51,   60,   61,   70,   80),
    a_cl = c("g1", "g2", "g2", "g3", "g4", "g4", "gX", "g6", "g6", "g9"),
    c_cl = c(10.1, 20.5, 20.7, NA, 50.5, 51.1, NA, 61.0, 70.2, 80.3)
  )
  
  # Overzicht:
  # - g1 heeft exact 1 rij in a_cl = g1 en a = g1 → ok
  # - g2 heeft 2 rijen in a_cl maar slechts 1 in a
  # - g3 heeft 1 rij in a en 1 rij in a_cl
  # - g4 heeft 2 in a_cl, 1 in a
  # - g5 heeft 2 in a, geen in a_cl
  # - g6 heeft 2 rijen in zowel a als a_cl
  # - g7 heeft 1 rij in a, géén a_cl
  # - g8 heeft a = g8, a_cl = g9 → a_cl onbekend in a
  
  ag_a <- dt2[, .(N = .N), by = a_cl]
  setnames(ag_a,"a_cl","a")
  
  out <- estimate_c1_counts_in_c2_intervals(
    dt2 = copy(dt2),
    kol_a = "a",
    kol_c = "c",
    kol_a_cl = "a_cl",
    kol_c_cl = "c_cl",
    ag_a = ag_a,
    kol_N = "N",
    m = 0.1,
    auto_bw = FALSE,
    row_limit = 5e5,
    nomatch = 0L,
    ...verbose = FALSE,
    show_progress = FALSE,
    logfile = NULL,
    timing = FALSE
  )
  
  res <- cbind(dt2,out)
  
  # Controleer aantal rijen komt overeen met dt2
  expect_equal(nrow(out), nrow(dt2))
  
  # Check op aanwezigheid van outputkolommen
  expect_true(all(c("S_c", "method_c", "lower_c", "upper_c") %in% names(out)))
  
  # Elke rij moet numerieke uitkomst hebben of NA (alle NAs zijn omgezet naar 0)
  expect_type(out$S_c, "double")
  
  # Check dat NA-invulling correct is: geen NA's meer
  expect_false(any(is.na(out$S_c)))
  
  # Check dat method_c alleen 'exact' of 'kde' bevat
  expect_true(all(out$method_c %in% c("exact", "kde", NA)))
  
  # Debug-info voor handmatige check (optioneel)
  cat("\nTestrijen met methodes:\n")
  print(out[, .(a = dt2$a, a_cl = dt2$a_cl, method_c, S_c)])
  
  # Speciale checks:
  expect_true(res[a == "g3", all(S_c == 0)])   # in dt2 wordt bij a_cl=g3 c_cl=NA niet door de c=30 bij a=g3 benaderd
  expect_true(res[a == "g5", all(S_c == 0)])   # geen a_cl = g5
  expect_true(res[a_cl == "gX", all(is.na(c_cl))]) # alleen NA in c_cl
})

test_that("estimate_c1_counts_in_c2_intervals handles edge cases correctly", {
  set.seed(42)
  
  dt2 <- data.table(
    a     = rep(paste0("g", 1:5), each = 4),
    c     = c(1:4, 5:8, 9:12, NA, NA, NA, NA, 13:16),
    a_cl  = rep(c("g1", "g2", "g3", "gX", "g1"), each = 4),
    c_cl  = c(rnorm(n = 4, mean = 2), rnorm(n = 4, mean = 6), rnorm(n = 4, mean = 10), 
              NA, NA, NA, NA, rnorm(n = 4, mean = 14))
  )
  
  ag_a <- dt2[, .(N = sum(!is.na(c))), by = a]
  
  out <- estimate_c1_counts_in_c2_intervals(
    dt2 = dt2,
    kol_a = "a",
    kol_c = "c",
    kol_a_cl = "a_cl",
    kol_c_cl = "c_cl",
    ag_a = ag_a,
    kol_N = "N",
    m = 0.05,
    auto_bw = FALSE,
    row_limit = 5e5,
    nomatch = 0L,
    ...verbose = FALSE,
    show_progress = FALSE,
    logfile = NULL,
    timing = FALSE
  )
  
  res <- cbind(dt2, out)
  # groep_totalen <- dt2[, .(
  #   n_a = .N,
  #   n_a_bnot_na = sum(!is.na(get("c")))
  # ), by = "a"]
  groep_totalen <- dt2[, .(
    n_a = .N,
    n_a_bnot_na = sum(!is.na(c))
  ), by = a]
  res <- groep_totalen[res, on = "a"]
  res[a=="g3",]
  
  # Basiscontroles
  expect_true("S_c" %in% names(res))
  expect_equal(nrow(res), nrow(dt2))
  
  # Speciale checks:
  #expect_true(res[a == "g3", all(S_c == 1/n_a)])   # kleine overlap: exacte telling
  expect_true(all(res[a == "g3", ]$S_c == c(0,1,1,2)/4)) # exacte telling, maar dt2 met ruis
  expect_true(res[a == "g5", all(S_c == 0)])   # geen a_cl = g5
  expect_true(res[a_cl == "gX", all(is.na(c_cl))])       # alleen NA in c_cl
  # dt2[a == "g3",]
  # ag_a[a == "g3",]
  # res[a=="g3",]$S_c
  # er staat: 0 0.25 0.25 0.5, met N_A = 4 waren de geschatte aantallen: 0 1 1 2
  # omdat in dt2 hier 4 rijen, en N_A = 4, dan exact tellen: 
  # in 8.55-9.45, in 9.5-10.5, in 10.45-11.55, in 11.4-12.6: dat zijn er 1 1 1 1
  # maar ruis op a en c in dt2: daar intervallen omheen tov a_cl en c_cl in dt2 werd dt1
})

