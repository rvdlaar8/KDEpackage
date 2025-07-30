test_that("maak_acc_cols_gev_ongev werkt correct met gev en ongev", {
  dt <- data.table(
    b1 = c(1, 2, 5, 6),
    b2 = c(4, 6, 7, 9),
    b3 = c(10, 11, 12, 13)
  )
  
  gv <- list(
    b1 = c(2, 5),
    b2 = c(4, 6)  # b3 ontbreekt opzettelijk
  )
  
  modes <- c(b1 = "gev", b2 = "ongev", b3 = "gev")
  
  maak_acc_cols_gev_ongev(dt, b_col = c("b1", "b2", "b3"), gv_list = gv, default_mode = modes)
  
  expect_equal(dt$b1acc, c(0, 2, 5, 0)) # b1: alleen 2 en 5 behouden
  expect_equal(dt$b2acc, c(0, 0, 7, 9)) # b2: 4 en 6 worden 0, rest behouden
  expect_equal(dt$b3acc, dt$b3)         # b3: ontbreekt in gv_list, mode = "gev" → behouden
})

