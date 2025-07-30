# om package te maken na restart R-Studio:
getwd()
#setwd(paste0(getwd(),"/KDEpackage"))
library(devtools)
library(testthat)
document()
check()
build(binary = TRUE)
packageVersion("KDEpackage")

detach(package:KDEpackage, unload=TRUE)

# let op: pas versienummer aan in zip-naam:
getwd() # zip staat 1 map hoger:
install.packages("../KDEpackage_0.1.5.zip", repos = NULL)
library(KDEpackage)

#laptop indien zip staat in de projectmap: 
#install.packages("KDEpackage_0.1.3.zip", repos = NULL, type = "win.binary")
#install.packages("pad/naar/mijnpakket_0.2.0.zip", repos = NULL, type = "win.binary")


##########################
# om lokaal te ontwikkelen:
library(data.table)
library(testthat)
library(ggplot2)

source("R/var3_cx.R")
source("R/var3_bx.R") # nw_func 5 voor s_gesch Bx
source("R/matching_pipeline_variant.R")
source("R/var1en2.R")
source("R/lib.R")
source("R/voeg_samengestelde_kolom_toe.R")
#source("R/switch_column_suffixes.R")
source("R/rename_project_and_original_names.R")

#genereer_functielijst_per_script_mooi("tests/testthat") # leeg

# .Rbuildignore aanpassen: voorkomt opname bij bouwen van de package:
# usethis::use_build_ignore("R/functielijst_per_script.txt")

# Automatisch verwijderen: als het alleen een tijdelijk hulpmiddel is
# file.remove("R/functielijst_per_script.txt")
# Stap + doel:
# of zet het in een aparte dev/ map of /tmp/ = Bestand buiten R/ bewaren

# Schrijf naar R-map (zoals eerst)
genereer_functielijst_per_script_mooi("R")
# Schrijf naar dev/
genereer_functielijst_per_script_mooi("R", naar_dev = TRUE)
# Schrijf naar dev/ zonder sorteren
genereer_functielijst_per_script_mooi("R", sorteer = FALSE, naar_dev = TRUE)

###################
# merging met on = :
dt1 <- data.table(id = c(1, 1, 2), waarde = c("a", "b", "c"))
dt2 <- data.table(id = c(1, 2))

dt2[dt1, on = "id"] # Gaat alle matches koppelen (dus beide rijen met id = 1)
dt1[dt2, on = "id"] # idem

dt1 <- data.table(id = c(1, 1, 2), waarde = c("a", "b", "c"))
dt2 <- data.table(id = c(1, 1, 2))

dt2[dt1, on = "id"] # alle matches koppelen (dus beide rijen met id = 1)
dt1[dt2, on = "id"] # idem (ipv slechts eerste match per id)
dt1[dt2, on = "id", mult = "first"]  # slechts eerste match per id
dt1[dt2, on = "id", mult = "last"]  # slechts laatste match per id
dt1[dt2, on = "id", mult = "all"]  # slechts eerste match per id


# blijven alle dt2-rijen staan?
dt2 <- data.table(id = c(1, 1, 2), waarde = c("a", "b", "c"))
dt1 <- data.table(id = c(3, 4), extra = c("d","e"))
dt1[dt2, on = "id"] # goed
dt1[dt2, on = "id"] # goed
dt1[dt2, on = "id", mult = "first"]  # slechts eerste match per id
dt1[dt2, on = "id", mult = "last"]  # slechts laatste match per id
dt1[dt2, on = "id", mult = "all"]  # slechts eerste match per id

# package:
library(devtools)
# zet eerst de wd op KDEpackage
devtools::document()
# in .Rbuildignore de regel met ^dev$ toegevoegd
# let op: overal in examples data.table::data.table ipv data.table
# idem voor andere aangeroepen packages, zoals ggplot2

# op eigen pc of andere pc:
# indien 'in use' eerst detach("KDEpackage") of uitvinken in lijst Packages
detach(package:KDEpackage, unload = TRUE)
check()

build(binary = TRUE) # dit maakt een Windows package (.zip) die kan je delen en laten installeren
#als de zip-file in de wd staat: install.packages("KDEpackage_0.1.0.zip", repos = NULL)
#als de zip-file 1 hoger dan de wd staat: install.packages("../KDEpackage_0.1.0.zip", repos = NULL)

# ggplot:
dt2 <- data.table(a    = c("1.1", "1.1", "2.2"),  
                  c    = c(1.1, 2.1, 4.2),
                  a_cl = c("1.1", "1.1", "2.2"), 
                  c_cl = c(1.1, 2.1, 4.2)) # cl gelijk
N1_a <- data.table(a = c("1.1", "2.2"), N_group = c(3, 1))

res <- estimate_b1_counts_in_c2_intervals(dt2, ag_a = N1_a, 
                                          kol_a = "a", kol_a_cl = "a_cl",
                                          kol_N = "N_group", 
                                          kol_c = "c", kol_c_cl = "c_cl",
                                          m = 0.2)
dt2 <- cbind(dt2,res)

plotlijst <- plot_c2_density_with_intervals(dt2, 
                               group_col = "a", c_col = "c", 
                               group_cl_col = "a_cl", c_cl = "c_cl")
plotlijst
toon_plots_interactief(plotlijst)
  
  
# v2:
# Voorbeelddata
dt2 <- data.table(
  a = c("g1", "g1", "g2", "g2", "g3"),
  c = c(10, 12, 20, 21, 30),
  a_cl = c("g1", "g1", "g2", "g2", "g3"),
  c_cl = c(11, 13, 19, 22, 30)
)

ag_a <- data.table(a = c("g1", "g2", "g3"), N_group = c(10L, 20L, 5L))

# Roep functie aan
resultaat <- estimate_b1_counts_in_c2_intervals(
  dt2 = dt2,
  kol_a = "a",
  kol_c = "c",
  kol_a_cl = "a_cl",
  kol_c_cl = "c_cl",
  ag_a = ag_a,
  kol_N = "N_group",
  m = 0.1,
  auto_bw = TRUE,
  verbose = FALSE,
  timing = FALSE
)

head(resultaat)
dt2 <- cbind(dt2,resultaat)

plotlijst <- plot_c2_density_with_intervals(dt2, 
                                            group_col = "a", c_col = "c", 
                                            group_cl_col = "a_cl", c_cl = "c_cl")
plotlijst
toon_plots_interactief(plotlijst)
