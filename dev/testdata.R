library(data.table)

#' Genereer een test-data.table met lichte afwijkingen voor *_cl kolommen
#'
#' @param n Aantal rijen (standaard 50000)
#' @return data.table met kolommen a1, a2, a3, b1, c1, a1_cl, b1_cl, c1_cl
#' @import data.table
#' @export
genereer_test_dt_met_cl <- function(n = 50000) {
  stopifnot(requireNamespace("data.table"))
  set.seed(123)
  
  dt <- data.table(
    a1 = sample(1:10, n, replace = TRUE),
    a2 = sample(1:2, n, replace = TRUE),
    a3 = sample(1:5, n, replace = TRUE)
  )
  
  # b1: licht gecorreleerd met a1
  dt[, b1 := pmin(pmax(round(a1 + rnorm(n, mean = 0, sd = 1)), 1), 10)]
  
  # c1: licht gecorreleerd met a2; met een scheve verdeling
  base_c1 <- rgamma(n, shape = 2, rate = 1)
  dt[, c1 := base_c1 + (a2 - mean(a2)) * 0.5 + rnorm(n, 0, 0.3)]
  
  # a1_cl: licht afwijkend van a1
  dt[, a1_cl := pmin(pmax(a1 + sample(c(-1L, 0L, 1L), .N, replace = TRUE, prob = c(0.1, 0.8, 0.1)), 1L), 10L)]
  
  # b1_cl: vergelijkbare variatie als b1
  dt[, b1_cl := pmin(pmax(round(b1 + rnorm(n, 0, 1)), 1), 10)]
  
  # c1_cl: licht afwijkend van c1, met extra ruis
  dt[, c1_cl := c1 + rnorm(n, mean = 0, sd = 0.5)]
  
  return(dt[])
}
dt_test <- genereer_test_dt_met_cl()
str(dt_test)
dim(dt_test)

#' Bereken correlaties van b1 en c1 met a1, a2, a3
#'
#' @param dt Een data.table met kolommen a1, a2, a3, b1, c1
#' @return data.table met kolommen: afhankelijke variabele, onafhankelijke variabele, correlatie
#' @import data.table
#' @export
bereken_correlaties_bc_a123 <- function(dt) {
  stopifnot(is.data.table(dt))
  stopifnot(all(c("a1", "a2", "a3", "b1", "c1") %in% names(dt)))
  
  resultaat <- rbindlist(lapply(c("a1", "a2", "a3"), function(akol) {
    data.table(
      afhankelijke = c("b1", "c1"),
      onafhankelijke = akol,
      correlatie = c(cor(dt[[akol]], dt[["b1"]], use = "complete.obs"),
                     cor(dt[[akol]], dt[["c1"]], use = "complete.obs"))
    )
  }))
  
  return(resultaat[])
}

dt_test <- genereer_test_dt_met_cl()
bereken_correlaties_bc_a123(dt_test)

library(data.table)
library(ggplot2)

dt_test <- genereer_test_dt_met_cl()

plots <- plot_correlaties_bc_a123(dt_test)

print(plots$b1_plot)
print(plots$c1_plot)

#' Plot correlaties van b1 en c1 t.o.v. a1, a2, a3
#'
#' @param dt Een data.table met kolommen a1, a2, a3, b1, c1
#' @return Twee ggplot-objecten: één voor b1 en één voor c1
#' @import data.table
#' @import ggplot2
#' @export
plot_correlaties_bc_a123 <- function(dt) {
  stopifnot(is.data.table(dt))
  stopifnot(all(c("a1", "a2", "a3", "b1", "c1") %in% names(dt)))
  
  dt_long <- rbindlist(list(
    dt[, .(a = a1, b1, c1, a_type = "a1")],
    dt[, .(a = a2, b1, c1, a_type = "a2")],
    dt[, .(a = a3, b1, c1, a_type = "a3")]
  ))
  
  p1 <- ggplot(dt_long, aes(x = factor(a), y = b1)) +
    geom_boxplot(outlier.alpha = 0.1, fill = "skyblue") +
    facet_wrap(~a_type, scales = "free_x") +
    labs(title = "b1 t.o.v. a1, a2, a3", x = "a-waarde", y = "b1") +
    theme_minimal()
  
  p2 <- ggplot(dt_long, aes(x = factor(a), y = c1)) +
    geom_boxplot(outlier.alpha = 0.1, fill = "lightgreen") +
    facet_wrap(~a_type, scales = "free_x") +
    labs(title = "c1 t.o.v. a1, a2, a3", x = "a-waarde", y = "c1") +
    theme_minimal()
  
  return(list(b1_plot = p1, c1_plot = p2))
}

#' Plot scatterplots met regressielijnen van b1 en c1 t.o.v. a1, a2, a3
#'
#' @param dt Een data.table met kolommen a1, a2, a3, b1, c1
#' @return Twee ggplot-objecten: één voor b1, één voor c1
#' @import data.table
#' @import ggplot2
#' @export
plot_correlaties_scatter_bc_a123 <- function(dt) {
  stopifnot(is.data.table(dt))
  stopifnot(all(c("a1", "a2", "a3", "b1", "c1") %in% names(dt)))
  
  dt_long <- rbindlist(list(
    dt[, .(a = a1, b1, c1, a_type = "a1")],
    dt[, .(a = a2, b1, c1, a_type = "a2")],
    dt[, .(a = a3, b1, c1, a_type = "a3")]
  ))
  
  # Plot 1: b1 vs a1, a2, a3
  p1 <- ggplot(dt_long, aes(x = a, y = b1)) +
    geom_point(alpha = 0.2, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    facet_wrap(~a_type, scales = "free_x") +
    labs(title = "b1 versus a1, a2, a3", x = "a-waarde", y = "b1") +
    theme_minimal()
  
  # Plot 2: c1 vs a1, a2, a3
  p2 <- ggplot(dt_long, aes(x = a, y = c1)) +
    geom_point(alpha = 0.2, color = "darkgreen") +
    geom_smooth(method = "lm", color = "black", se = FALSE) +
    facet_wrap(~a_type, scales = "free_x") +
    labs(title = "c1 versus a1, a2, a3", x = "a-waarde", y = "c1") +
    theme_minimal()
  
  return(list(b1_scatter = p1, c1_scatter = p2))
}

# ✨ Toelichting
# geom_point() met alpha = 0.2 voorkomt overplotting bij veel rijen.
# geom_smooth(method = "lm") tekent per facet een lineaire regressielijn.
# facet_wrap(~a_type) zorgt voor duidelijke aparte panelen per a-kolom.

plots <- plot_correlaties_scatter_bc_a123(dt_test)

print(plots$b1_scatter)
print(plots$c1_scatter)
