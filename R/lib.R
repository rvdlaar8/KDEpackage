#' Controleer object is referenced
#' 
#' @import data.table
#' 
#' @export
is_referenced <- function(original, candidate, col = NULL) {
  if (!is.data.table(original) || !is.data.table(candidate)) {
    stop("Beide objecten moeten data.tables zijn")
  }
  
  if (is.null(col)) {
    # Kies eerste gemeenschappelijke kolom
    gemeen <- intersect(names(original), names(candidate))
    if (length(gemeen) == 0) stop("Geen gemeenschappelijke kolommen")
    col <- gemeen[1]
  }
  
  adr_orig <- address(original[[col]])
  adr_cand <- address(candidate[[col]])
  
  identical(adr_orig, adr_cand)
}

#' Bewaar plots als pdf
#' 
#' @export
bewaar_plots_als_pdf <- function(plotlijst, indices, bestandsnaam = "plots.pdf", width = 8, height = 6) {
  folder <- dirname(bestandsnaam)
  if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
  
  pdf(file = bestandsnaam, width = width, height = height)
  on.exit(dev.off())  # Zorg dat PDF correct wordt afgesloten, ook bij fout

  for (i in indices) {
    print(plotlijst[[i]])
  }
  cat("PDF opgeslagen als:", bestandsnaam, "\n")
}
# Elk plot komt op een aparte pagina. Bestandsnaam mag een pad bevatten.
# Je kunt eventueel een subset maken met which(names(plots) %in% ...) als je 
# op naam wilt selecteren.

#' Toon 1 seconde per plot
#' 
#' @export
toon_plots_1_seconde <- function(plotlijst, indices = NULL){
  if (is.null(indices)) indices <- seq(from = 1, to = length(plotlijst))
  for (j in indices) {
    print(plotlijst[[j]])
    cat("Toon plot", j, "\n")
    Sys.sleep(1)  # wacht 1 seconde
  }
}

#' Toon plots interactief
#' 
#' @export
toon_plots_interactief <- function(plotlijst, indices = NULL){
  if (is.null(indices)) indices <- seq(from = 1, to = length(plotlijst))
  for (j in indices) {
    print(plotlijst[[j]])
    readline(prompt = "Druk op Enter voor de volgende plot...")
  }
}

# 
# voorbeeld data
# dt <- data.table(a = 1:5, b1 = rnorm(5), b2 = rnorm(5), b3 = rnorm(5))
# vector met kolomnamen
# v <- c("b1", "b2", "b3")
# jouw bewerkingsfunctie, bv. vermenigvuldig a met b*
# bereken_y <- function(a, b) {
#   a + b^2  # voorbeeld: a + b kwadraat
# }
# loop over kolommen en voeg y1, y2, ... toe
# for (i in seq_along(v)) {
#   b_col <- v[i]
#   y_col <- paste0("y", i)
#   dt[, (y_col) := bereken_y(a, get(b_col))]
# }
# dt
# 📌 Wat maakt dit snel?
# get() haalt de kolom op zonder eval(parse(...)).
# := voegt kolommen toe zonder kopiëren.
# De for-loop is zuinig zolang de bereken_y() vectorized is.
# ⚠️ Let op
# Zorg dat bereken_y() vectorized is (geen for-lus binnenin nodig).
# Als v lang is en performance belangrijk, is dit al dicht bij optimaal.
# Indien dt groot is (miljoenen rijen), vermijd kopiëren (dus geen := buiten 
# de data.table context).

# Wat zie je hier?
# Boven de functie algemene informele toelichting met #.
# Direct eronder volledige roxygen2 documentatie met #'.
# Binnen de functie duidelijke commentaar per blok of stap, zonder lege regels tussen commentaar en code.
# De functie is zo ook heel goed te onderhouden en te documenteren voor gebruikers.


# Deze functie maakt per kolom een 'acc'-kolom aan
# die op basis van een lijst gewenste/ongewenste waarden
# de originele waarde doorgeeft of vervangt door 0.

#' Maak acc-kolommen op basis van gewenste of ongewenste waarden
#'
#' @param dt Een data.table waarin kolommen worden toegevoegd.
#' @param b_col Character vector van kolomnamen waarvoor acc-kolommen gemaakt worden.
#' @param gv_list Named list met per kolom de gewenste of ongewenste waarden.
#' @param default_mode Named character vector met per kolom "gev" of "ongev".
#'                     Indien mode = "gev": waarden in gv worden behouden, rest = 0.
#'                     Indien mode = "ongev": waarden in gv worden 0, rest wordt behouden.
#'                     Indien kolom ontbreekt in gv_list, dan:
#'                        - bij mode = "gev": hele kolom behouden
#'                        - bij mode = "ongev": hele kolom 0
#' @param suffix Suffix voor de naam van de nieuwe kolommen (default = "acc")
#'
#' @return De originele data.table, met extra kolommen toegevoegd (invisibly).
#' 
#' @export
maak_acc_cols_gev_ongev <- function(dt, b_col, gv_list, default_mode, suffix = "acc") {
  # Controleer argumenten
  stopifnot(is.data.table(dt), is.character(b_col), is.list(gv_list))
  stopifnot(is.character(default_mode),
            length(default_mode) == length(b_col),
            setequal(names(default_mode), b_col))
  
  # Loop over alle kolommen die moeten worden verwerkt
  for (col in b_col) {
    new_col <- paste0(col, suffix)  # dynamische naam voor acc-kolom
    mode <- default_mode[[col]]     # modus "gev" of "ongev"
    vals <- dt[[col]]               # originele waarden
    gv_cat <- gv_list[[col]]        # toegestane/ongewenste waarden (kan NULL zijn)
    
    # Afhankelijk van de modus en aanwezigheid van gv_cat bepalen we output
    result <- switch(mode,
                     gev = {
                       if (!is.null(gv_cat)) {
                         # Geef waarde door als die in gv_cat zit, anders 0
                         fifelse(vals %in% gv_cat, vals, 0L)
                       } else {
                         # Geen gv_cat, hele kolom behouden
                         vals
                       }
                     },
                     ongev = {
                       if (!is.null(gv_cat)) {
                         # Geef 0 als waarde in gv_cat zit, anders origineel
                         fifelse(vals %in% gv_cat, 0L, vals)
                       } else {
                         # Geen gv_cat, hele kolom 0
                         0L
                       }
                     },
                     stop(sprintf("Ongeldige mode '%s' voor kolom '%s'. Gebruik 'gev' of 'ongev'.", mode, col))
    )
    
    # Schrijf nieuwe kolom weg in dt
    dt[, (new_col) := result]
  }
  
  invisible(dt)
}

#' Genereer functielijst per script
#' 
#' @import data.table
#'
#' @export
genereer_functielijst_per_script_mooi <- function(mapnaam = "R", 
                                                  output_bestand = "functielijst_per_script.txt", 
                                                  sorteer = TRUE, 
                                                  naar_dev = FALSE) {
  # Pad naar de inputmap
  pad <- normalizePath(mapnaam, mustWork = TRUE)
  
  # Vind alle .R bestanden
  r_bestanden <- list.files(pad, pattern = "\\.R$", full.names = TRUE)
  if (length(r_bestanden) == 0) {
    stop("Geen .R-bestanden gevonden in de map: ", mapnaam)
  }
  
  # Hulpfunctie om functienamen uit een scriptbestand te halen
  haal_functies_op <- function(bestand) {
    regels <- readLines(bestand, warn = FALSE)
    matches <- grep("^[[:space:]]*([a-zA-Z0-9._]+)[[:space:]]*(<-|=)[[:space:]]*function\\s*\\(", regels, value = TRUE)
    functies <- sub("^[[:space:]]*([a-zA-Z0-9._]+)[[:space:]]*(<-|=)[[:space:]]*function\\s*\\(.*", "\\1", matches)
    if (sorteer) sort(functies) else functies
  }
  
  # Verzamelen van tekstoutput
  output <- character()
  for (bestand in r_bestanden) {
    functies <- haal_functies_op(bestand)
    script_naam <- basename(bestand)
    output <- c(output, script_naam)
    if (length(functies) == 0) {
      output <- c(output, "  (geen functies gevonden)")
    } else {
      output <- c(output, paste0("  - ", functies))
    }
    output <- c(output, "")  # lege regel na elk script
  }
  
  # Outputlocatie bepalen
  if (naar_dev) {
    uitvoermap <- file.path(getwd(), "dev")
    if (!dir.exists(uitvoermap)) {
      dir.create(uitvoermap)
    }
  } else {
    uitvoermap <- pad
  }
  uitvoer_pad <- file.path(uitvoermap, output_bestand)
  
  # Wegschrijven
  writeLines(output, con = uitvoer_pad)
  message("Functielijst weggeschreven naar: ", uitvoer_pad)
  
  return(invisible(output))
}



#' Hernoemt kolommen met een prefix
#'
#' @import data.table
#'
#' @param dt Een data.table met kolommen om te hernoemen.
#' @param prefix Een prefix toe te voegen aan alle kolomnamen.
#'
#' @return De aangepaste data.table.
#' 
#' @examples
#' dt <- data.table::data.table(x = 1:3)
#' rename_with_prefix(dt, "test")
#' 
#' @export
rename_with_prefix <- function(dt, prefix) {
  setnames(dt, old = names(dt), new = paste0(prefix, "_", names(dt)))
  dt
}

#' Voeg samengestelde kolom toe door samenvoegen van bestaande kolommen
#' 
#' Deze functie voegt aan een data.table een nieuwe kolom toe door opgegeven kolommen 
#' te concatenaten met een scheidingsteken, alles by-reference.
#' 
#' @param dt data.table waarin de nieuwe kolom toegevoegd wordt (wordt by-reference aangepast).
#' @param keystring Character string met kolomnamen gescheiden door punt (bijv. "a.b.c").
#' @param new_col Naam van de toe te voegen kolom als string.
#' @param sep Scheidingsteken voor samenvoegen van kolommen (default is ".").
#' @param verbose Logical, als TRUE wordt een bericht weergegeven na toevoegen.
#' 
#' @return NULL (invisible), aanpassing gebeurt by-reference in \code{dt}.
#' 
#' @import data.table
#' @export
concat_columns <- function(dt, keystring, new_col, sep = ".", verbose = FALSE) {
  stopifnot(is.data.table(dt))
  
  # Selfref check — voorkomt warning over 'invalid internal selfref'
  if (!truelength(dt)) dt <- copy(dt)
  
  setDT(dt) # forceert geldige selfref en data.table structuur
  
  keycols <- strsplit(keystring, "\\.")[[1]]
  
  dt[, (new_col) := do.call(paste, c(.SD, sep = sep)), .SDcols = keycols]
  
  # if (verbose) message("✅ Kolom toegevoegd: ", new_col)
  # if (verbose) message("Eerste waarden in ", new_col, ": ", paste(head(dt[[new_col]]), collapse = ", "))
  
  #invisible(NULL)  # stille by-reference functie
  return(dt)
}

#' @title Genereer vector met geleidelijk toenemende stapgroottes
#'
#' @description
#' Genereert een numerieke vector tussen `from` en `to`, waarbij de stapgroottes
#' geleidelijk toenemen. De stapgroottes zijn vooraf gespecificeerd, en het totaal
#' aantal elementen benadert `approx_total_length`. De overgang tussen stapgroottes
#' verloopt geleidelijk om abrupte sprongen te vermijden.
#'
#' @param from Numerieke ondergrens van de vector (default = 1)
#' @param to Numerieke bovengrens van de vector (default = 1e6)
#' @param steps Vector met mogelijke stapgroottes (standaard: \code{c(1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000)})
#' @param approx_total_length Gewenste (ongeveer) totale lengte van de outputvector (default = 500)
#' @param min_per_step Minimum aantal punten per stapgrootte (default = 1)
#'
#' @return Een oplopende vector van unieke waarden tussen `from` en `to`
#' @export
#'
#' @examples
#' gen_vector_met_geleidelijk_grotere_stap(300, 100000, approx_total_length = 20)
#' gen_vector_met_geleidelijk_grotere_stap(1, 1e6, approx_total_length = 100)
gen_vector_met_geleidelijk_grotere_stap <- function(from = 1, to = 1e6,
                                                           steps = c(1, 5, 10, 50, 100, 500,
                                                                     1000, 5000, 10000, 50000,
                                                                     100000, 500000),
                                                           approx_total_length = 500,
                                                           min_per_step = 1) {
  stopifnot(is.numeric(from), is.numeric(to), from < to, approx_total_length > 0)
  
  v <- numeric()
  step_start_index <- which.max(steps >= (from / 100))
  if (step_start_index == 0) step_start_index <- 1
  steps_use <- steps[step_start_index:length(steps)]
  
  # Bepaal geschatte grenzen per stapgrootte
  bounds <- lapply(seq_along(steps_use), function(i) {
    s <- steps_use[i]
    start <- ceiling(from / s) * s
    end <- min(to, start + s * 1e4)
    c(start, end)
  })
  
  # Gebruik log van het bereik als gewicht
  weights <- sapply(bounds, function(b) log1p(max(1, b[2] - b[1])))
  weights <- weights / sum(weights)
  
  # Bepaal aantal punten per stap
  n_per_step_vec <- pmax(round(weights * approx_total_length), min_per_step)
  
  current_start <- from
  
  for (i in seq_along(steps_use)) {
    step <- steps_use[i]
    n_points <- n_per_step_vec[i]
    if (n_points <= 0) next
    
    aligned_start <- ceiling(current_start / step) * step
    current_end <- min(to, aligned_start + (n_points - 1) * step)
    
    if (aligned_start > to) break
    
    v <- c(v, seq(from = aligned_start, to = current_end, by = step))
    current_start <- current_end + step
  }
  
  # Afronden tot 'to' met passende grotere stap
  if (max(v) < to) {
    remaining <- to - max(v)
    next_step_index <- which(steps > remaining / 10)
    final_step <- if (length(next_step_index)) steps[next_step_index[1]] else tail(steps, 1)
    aligned_start <- ceiling((max(v) + 1) / final_step) * final_step
    v <- c(v, seq(from = aligned_start, to = to, by = final_step))
  }
  
  v <- sort(unique(v[v >= from & v <= to]))
  return(v)
}