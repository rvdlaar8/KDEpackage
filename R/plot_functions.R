#' Plot dichtheid van een c-kolom met intervallen en verticale lijntjes
#'
#' Deze functie maakt een ggplot met een dichtheidslijn per groep `a`, horizontale rode intervallen
#' en verticale rode lijntjes vanaf het midden (target) die evenredig zijn aan `geschatte_n`.
#' De hoogte van het hoogste verticale lijntje is instelbaar via `max_line_height_frac`.
#'
#' @param dt Een data.table met kolommen: een c-kolom, bijbehorende intervalkolommen, en `geschatte_n`.
#' @param c_col Naam van de c-kolom (karakter, default `"c1"`).
#' @param estimate_col Naam van de kolom met schattingen (karakter, default `"geschatte_n"`).
#' @param max_line_height_frac Fractie van maximale hoogte die het hoogste lijntje mag bereiken (default = 0.25).
#'
#' @return Een ggplot-object.
#'
#' @import data.table
#' @import ggplot2
#' @importFrom rlang sym !!
#' @export
plot_c2_density_with_intervals <- function(
    dt,
    group_col = "key",
    group_col_cl = "key_cl",
    c_col = "c1",
    c_col_cl = "c1_cl",
    estimate_col = "estimated_n_c",
    max_line_height_frac = 0.25
) {
  
  stopifnot(is.data.table(dt))
  stopifnot(all(c(c_col, estimate_col, group_col_cl) %in% names(dt)))
  stopifnot(between(max_line_height_frac, 0, 1))
  
  lower_col  <- paste0("lower_", c_col)
  upper_col  <- paste0("upper_", c_col)
  target_col <- c_col
  
  if (!all(c(lower_col, upper_col, target_col, estimate_col) %in% names(dt))) {
    stop("Vereiste kolommen ontbreken in dt.")
  }
  
  group_cl_levels <- unique(dt[[group_col_cl]])
  plots <- vector("list", length(group_cl_levels))
  
  dt <- copy(dt)

  # Bereken dichtheden per groep
  N_groep <- dt[!is.na(.SD[[1L]]), .N, by = group_col_cl]
  
  dens_dt <- dt[!is.na(.SD[[1L]]),
                if (.N > 1)
                  .(x = density(.SD[[1L]])$x,
                    y = density(.SD[[1L]])$y),
                by = group_col_cl,
                .SDcols = c_col_cl]
  
  # Maak plot per groep
  for (i in seq_along(group_cl_levels)) {
    a_cl_val <- group_cl_levels[i]
    target_subset <- dt[dt[[group_col]] == a_cl_val]
    target_vals   <- target_subset[[c_col]]
    
    dens_subset <- dens_dt[dens_dt[[group_col_cl]] == a_cl_val]
    dens_vals   <- dens_subset[["y"]]

    if (length(dens_vals) < 2 || nrow(dens_subset) == 0) {
      plots[[i]] <- ggplot() + 
        ggtitle(paste("Groep", a_cl_val, "- onvoldoende data")) + 
        theme_minimal()
      next
    }

    max_estimate <- max(target_subset[[estimate_col]], na.rm = TRUE)
    max_density_y <- max(dens_vals, na.rm = TRUE)
    
    scale_factor <- max_line_height_frac * max_density_y / max_estimate

    target_subset[, y_start := 0]
    target_subset[, y_end := target_subset[[estimate_col]] * scale_factor]
    #of: target_subset[, y_end := .SD[[1L]] * scale_factor, .SDcols = estimate_col]
   
    p <- ggplot() +
      geom_line(data = dens_subset, aes(x = x, y = y), linewidth = 1) + # na y=y: , color = a
      geom_segment(data = target_subset,
                   aes(x = !!sym(lower_col), xend = !!sym(upper_col),
                       y = 0, yend = 0),
                   color = "red", linewidth = 1.5) +
      geom_segment(data = target_subset,
                   aes(x = !!sym(target_col), xend = !!sym(target_col),
                       y = y_start, yend = y_end),
                   color = "red", linewidth = 0.7) +
      labs(x = c_col, y = "Density", title = paste("Density with intervals for", c_col)) +
      theme_minimal()
    
    plots[[i]] <- p
    
  }
  return(plots)
}
  
  
  

  


