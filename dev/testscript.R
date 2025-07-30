library(data.table)
library(ggplot2)

source("R/exact_b1_counts.R")
source("R/plot dt2.R")
source("R/plot dt3.R")
#source("R/plot_density_with_b2_intervals.R")
#source("R/plot_density_with_b3_intervals.R")
source("R/estimate_b1_counts_in_b2_intervals.R")
source("R/estimate_b1_counts_in_b3_intervals.R")
source("R/div.R")

#library(KDEpackage)

# Simuleer testdata
set.seed(1)
dt2 <- data.table(a = rep(1:3, c(5, 5, 6)),
                  b2 = c(rnorm(5, 10, 3), rnorm(5, -10, 2), runif(6, 0, 50)))
N1_a <- data.table(a = 1:3, N1_a = c(5, 50, 6))  # groep 1 & 3 exact, groep 2 kde

# Run met logging
res <- estimate_b1_counts_in_b2_intervals(dt2, N1_a, m = 0.2, verbose = TRUE)

# Gebruik eerdere data
res <- estimate_b1_counts_in_b2_intervals(dt2, N1_a, m = 0.2, verbose = FALSE)
res <- cbind(dt2,res)
names(res)

# Plot met intervallen en schattingen
#plot_b2_density_with_intervals(dt2, res, m = 0.2)
#plot_b2_density_with_intervals(dt2, res, varx = "") # nog afvangen
setnames(res, old = c("lower","upper","n2_a_not_na","estimated_n_b1"), 
         new = c("lower_C1","upper_C1","n2_a_not_na_C1","est_n_b1_C1"))
names(res)
plots <- plot_b2_density_with_intervals(dt2, res, varx = "C1")
#plot_b2_density_with_intervals(dt2, m = 0.2) # bedoeling?
plots[[1]]
plots[[2]]
plots[[3]]

bewaar_plots_als_pdf(plots, indices = 1:3, bestandsnaam = "plots_output/mijn_plots.pdf")

# 1 seconde per plot:
toon_plots_1_seconde(plots,1:3)
toon_plots_1_seconde(plots) # toont alle plots

# interactief:
toon_plots_interactief(plots,1:3)
toon_plots_interactief(plots) # toont alle plots

# een derde dataset:
dt3 <- data.table(a = rep(1:3, c(5, 5, 6)),
                  b3 = c(rnorm(5, 10, 3), rnorm(5, -10, 2), runif(6, 0, 50)))

dt3_with_estimates <- estimate_b1_counts_in_b3_intervals(dt2, dt3, N1_a, m = 0.2,
                                                                     b1_col = "b1", b2_col = "b2", b3_col = "b3",
                                                                     group_col = "a", verbose = TRUE, timing = TRUE)
dt3_with_estimates <- cbind(dt3, dt3_with_estimates)
names(dt3_with_estimates)

table(dt3_with_estimates$method)

plots <- plot_b3_density_with_intervals(dt2, dt3_with_estimates,
                               b2_col = "b2", b3_col = "b3", group_col = "a")
plots[[1]]
plots[[2]]
plots[[3]]

##############

# functie tests:
# Functie 1
#1️⃣ Subsets maken met hernoeming
DT <- data.table(key0 = c(1, 5, 10, 15, 20, 3,3,3,3,3, 3), Cx = c(4, 9, 14, 19, 24, 1,2,3,4,1,NA))
Q <- data.table(keyacc = c(1, 5, 10, 3,3, 3,3,3, 3,3,3,3,3, 3), Cxacc = c(4, 9, 14, 1,1, 2,2,2, 3,3,3,3,3, NA))
# Hernoem naar vaste functionele namen
dt1_subset <- DT[, .(a1 = key0, b1 = Cx)]
dt2_subset  <- Q[, .(a1 = keyacc, b1 = Cxacc)]

#3️⃣ Aanroep en cbind()
nieuwe_kolommen <- tel_b2_in_interval_rond_b1_foverlaps(dt1_subset,dt2_subset,"a1","b1",0.05)

# Terugzetten op originele DT
DT <- cbind(DT, nieuwe_kolommen)
DT
sum(DT$P) # 4.2
sum(DT[key0==3,P]) # 1.2

# Je kunt zonder problemen dit doen:
# nieuwe_kolommen <- bereken_kolommen_cbind(
#   DT[, .(id = evt, start = van, end = tot)],
#   Q[, .(qid = code, start = begin, end = eind)]
# )

# benchmark:
x <- data.table(a = sample(1e6, 1e6, TRUE), b = sample(1e6, 1e6, TRUE))
x0 <- x
y <- data.table(a = sample(1e6, 1e5, TRUE), b = sample(1e6, 1e5, TRUE), val = rnorm(1e5))

# Zonder keys
system.time(tel_marge_rond_b1_dt1_foverlaps(x[],y[],0.05))
identical(x0,x) # TRUE
system.time(tel_marge_rond_b1_dt1_foverlaps(x,y,0.05))
identical(x0,x) # TRUE

# # Met keys
# setkey(x, id)
# setkey(y, id)
# system.time(merge(x, y, all.x = TRUE, sort = FALSE))
# 
# # Snellere variant
# system.time(x[y, on = "id"])

# Functie 2
#1️⃣ Subsets maken met hernoeming
DT <- data.table(key0 = c(1, 5, 10, 15, 20, 3,3,3,3,3, 3), Cx = c(4, 9, 14, 19, 24, 1,2,3,4,1,NA))
Q <- data.table(keyacc = c(1, 5, 10, 3,3, 3,3,3, 3,3,3,3,3, 3), Cxacc = c(4, 9, 14, 1,1, 2,2,2, 3,3,3,3,3, NA))
# Hernoem naar vaste functionele namen
dt1_subset <- DT[, .(a1 = key0, b1 = Cx)]
dt2_subset  <- Q[, .(a1 = keyacc, b1 = Cxacc)]

dim(Q)
Q
nieuwe_kolommen <- tel_b1_in_interval_rond_b2_foverlaps(dt1_subset,dt2_subset,"a1","b1",0.05)

# Terugzetten op originele DT
Q <- cbind(Q, nieuwe_kolommen)
sum(Q$R) # 4.2
sum(Q[keyacc==3,R]) # 1.2

# Functie 3
#1️⃣ Subsets maken met hernoeming
DT <- data.table(key0 = c(1, 5, 10, 15, 20, 3,3,3,3,3, 3), Cx = c(4, 9, 14, 19, 24, 1,2,3,4,1,NA))
Q <- data.table(keyacc = c(1, 5, 10, 3,3, 3,3,3, 3,3,3,3,3, 3), Cxacc = c(4, 9, 14, 1,1, 2,2,2, 3,3,3,3,3, NA))
# Hernoem naar vaste functionele namen
dt1_subset <- DT[, .(a1 = key0, b1 = Cx)]
dt2_subset  <- Q[, .(a1 = keyacc, b1 = Cxacc)]

#3️⃣ Aanroep en cbind()
nieuwe_kolommen <- tel_exact_match_per_rij_dt1(dt1_subset,dt2_subset,"a1","b1")

# Terugzetten op originele DT
DT <- cbind(DT, nieuwe_kolommen)
DT
sum(DT$P) # 4.2
sum(DT[key0==3,P]) # 1.2

# Functie 4
#1️⃣ Subsets maken met hernoeming
DT <- data.table(key0 = c(1, 5, 10, 15, 20, 3,3,3,3,3, 3), Cx = c(4, 9, 14, 19, 24, 1,2,3,4,1,NA))
Q <- data.table(keyacc = c(1, 5, 10, 3,3, 3,3,3, 3,3,3,3,3, 3), Cxacc = c(4, 9, 14, 1,1, 2,2,2, 3,3,3,3,3, NA))
# Hernoem naar vaste functionele namen
dt1_subset <- DT[, .(a1 = key0, b1 = Cx)]
dt2_subset  <- Q[, .(a1 = keyacc, b1 = Cxacc)]

dim(Q)
Q
nieuwe_kolommen <- tel_exact_match_per_rij_dt2(dt1_subset,dt2_subset,"a1","b1")

# Terugzetten op originele DT
Q <- cbind(Q, nieuwe_kolommen)
sum(Q$R) # 4.2
sum(Q[keyacc==3,R]) # 1.2


# test gebruik is_referenced:
#📦 Voorbeeldgebruik
DT <- data.table(a = 1:5, b = 6:10)

# 1. Directe verwijzing
ref <- DT
is_referenced(DT, ref)  # TRUE

# 2. Shallow copy met één kolom
sub1 <- DT[, .(a)]
is_referenced(DT, sub1)  # TRUE  → gedeelde kolom >> ik krijg hier FALSE

# 3. Twee kolommen (container verandert)
sub2 <- DT[, .(a, b)]
is_referenced(DT, sub2)  # FALSE → container ontkoppeld

# 4. copy() expliciet
kopie <- copy(DT)
is_referenced(DT, kopie)  # FALSE

dt <- data.table(
  A1 = c("x", "y", NA),
  A2 = c("1", NA, "3"),
  A3 = c("a", "b", "c")
)

# concat_columns test
# Separator als streepje
concat_columns(dt, s = "A1.A2.A3", new_col = "met_streepje", sep = "-")
dt
# Separator als underscore
concat_columns(dt, s = "A1.A2", new_col = "met_underscore", sep = "_")
dt
concat_columns(dt, s = "A1.A2", new_col = "default")
dt
# Geen kolommen opgegeven
concat_columns(dt, s = NULL, new_col = "groep")
dt

dt2 <- data.table(
  A1 = c("x", "y", NA),
  C1 = c(1, 2, 3),
  C1_cl = c(1, 2, 3)
)
plot_c2_density_with_intervals(dt2, group_col = "A1", c_col = "C1", 
                               c_col_cl = "C1_cl")
dt2 <- data.table(
  A1 = c("x", "y", NA),
  C1 = c(1, 2, 3),
  C1_cl = c(1, 2, 3),
  lower_C1 = c(0,1,1),
  upper_C1 = c(2,4,5)
)
plot_c2_density_with_intervals(dt2, group_col = "A1", c_col = "C1", 
                               c_col_cl = "C1_cl")

dt2 <- data.table(
  A1 = c("x", "y", NA),
  A1_cl = c("x", "y", NA),
  C1 = c(1, 2, 3),
  C1_cl = c(1, 2, 3),
  lower_C1 = c(0,1,1),
  upper_C1 = c(2,4,5)
)
plot_c2_density_with_intervals(dt2, group_col = "A1", group_col_cl = "A1_cl", 
                               c_col = "C1", c_col_cl = "C1_cl")
