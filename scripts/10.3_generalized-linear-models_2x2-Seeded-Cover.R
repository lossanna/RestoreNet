# Created: 2024-09-09
# Last updated: 2024-09-20

# Purpose: Run generalized linear models for 2x2 m plot data, with Seeded_Cover as response variable. 

# Use Perc_dev_cum_abs to measure precip variability, and Since_last_precip_sqrt to indicate
#   recent wet or dry conditions.

# Poisson used to help with overdispersion.
# Including (1 | Site / Plot) accounts for the non-independence of repeat sampling.

# *** indicates model is included in PPT of draft figures and model results.

# Models are the same as 07.4.R script, results are condensed here (results are identical).
# Removed sq-root and log transformation from later models because GLMs can handle non-normal
#   explanatory variables, and this makes interpretation easier.
# Added "Days_elapsed", which is the number of days between seeding and monitoring.

library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)

# Load data ---------------------------------------------------------------

p2x2.rich.cover.raw <- read_csv("data/cleaned/04.2_2x2-richness-cover_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")

# Data wrangling ----------------------------------------------------------

## Combine ----------------------------------------------------------------

# Reorganize columns for left_join()
cum.pd.2x2 <- cum.pd |>
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Perc_deviation, Deviation_mm) |>
  rename(Perc_dev_cum = Perc_deviation,
         Dev_mm_cum = Deviation_mm)

# Combine all variables
p2x2.rich.cover <- p2x2.rich.cover.raw |>
  left_join(prism.data) |>
  left_join(ai) |>
  left_join(cum.pd.2x2)

# Check for NAs
apply(p2x2.rich.cover, 2, anyNA)


## Remove Inf from Perc_dev_cum and add Perc_dev_cum_abs ------------------

richcover <- p2x2.rich.cover |> 
  filter(Perc_dev_cum != Inf) |> 
  mutate(Perc_dev_cum_abs = abs(Perc_dev_cum))


## Add PlotMix_Climate col ------------------------------------------------

richcover <- richcover |>
  mutate(PlotMix_Climate = case_when(
    str_detect(richcover$Site, "Creosote|Mesquite|Patagonia|SRER") &
      richcover$PlotMix == "Medium" ~ "Current",
    str_detect(richcover$Site, "Creosote|Mesquite|Patagonia|SRER") &
      richcover$PlotMix == "Warm" ~ "Projected",
    str_detect(richcover$Site, "AguaFria|MOWE|PEFO|Spiderweb") &
      richcover$PlotMix == "Med-Warm" ~ "Current",
    str_detect(richcover$Site, "AguaFria|MOWE|PEFO|Spiderweb") &
      richcover$PlotMix == "Warm" ~ "Projected",
    str_detect(richcover$Site, "BarTBar|FlyingM|CRC|Salt_Desert") &
      richcover$PlotMix == "Cool-Med" ~ "Current",
    str_detect(richcover$Site, "BarTBar|FlyingM|CRC|Salt_Desert") &
      richcover$PlotMix == "Med-Warm" ~ "Projected",
    str_detect(richcover$Site, "BabbittPJ|UtahPJ") &
      richcover$PlotMix == "Cool" ~ "Current",
    str_detect(richcover$Site, "BabbittPJ|UtahPJ") &
      richcover$PlotMix == "Cool-Med" ~ "Projected",
    str_detect(richcover$Site, "29_Palms|AVRCD|Preserve|SCC|Roosevelt|Pleasant|TLE") &
      richcover$PlotMix == "Cool" ~ "Current",
    str_detect(richcover$Site, "29_Palms|AVRCD|Preserve|SCC|Roosevelt|Pleasant|TLE") &
      richcover$PlotMix == "Warm" ~ "Projected",
    TRUE ~ richcover$PlotMix))
richcover$PlotMix_Climate <- factor(richcover$PlotMix_Climate)


## Re-level categorical variables to set reference ------------------------

# Treatment
unique(richcover$Treatment)
richcover$Treatment <- as.factor(richcover$Treatment)
richcover$Treatment <- relevel(richcover$Treatment, ref = "Control")

# PlotMix_Climate
unique(richcover$PlotMix_Climate)
#   Allow Current to remain reference; because of uneven sample sizes, if
#     None is reference then Projected will be dropped from models. Better
#     to drop None and be able to compare Current & Projected.


## Transform AridityIndex and Cum_precip for normalization ----------------

# 05.3.R showed these transformations help improve normality
richcover <- richcover |> 
  mutate(Cum_precip_sqrt = sqrt(Cum_precip)) |> 
  mutate(AridityIndex_log = log(AridityIndex)) |> 
  mutate(Since_last_precip_sqrt = sqrt(Since_last_precip))

## Add Days_elapsed col ----------------------------------------------------

richcover <- richcover |> 
  mutate(Days_elapsed = difftime(Date_Monitored, Date_Seeded)) |> 
  mutate(Days_elapsed = as.numeric(Days_elapsed))


## Round cover values -----------------------------------------------------

# Sometimes Seeded_Cover was missing (not recorded)
richcover <- richcover |> 
  filter(!is.na(Seeded_Cover))

# Check for non-integers in Seeded_Cover
non_integers <- richcover$Seeded_Cover != floor(richcover$Seeded_Cover)

# Round non-integers
richcover$Seeded_Cover <- ifelse(richcover$Seeded_Cover != floor(richcover$Seeded_Cover), round(richcover$Seeded_Cover), richcover$Seeded_Cover)



## Separate out Sonoran sites (6) -----------------------------------------

sonoran <- richcover |> 
  filter(Region %in% c("Sonoran SE", "Sonoran Central"))

# No control plots, adjust reference levels to match Count & Height
sonoran.seed <- sonoran |> 
  filter(Treatment != "Control")
sonoran.seed$Treatment <- relevel(sonoran.seed$Treatment, ref = "Seed")


## Separate out Northern AZ sites (8) -------------------------------------

naz <- richcover |> 
  filter(Region == "Colorado Plateau")

# No control plots, adjust reference levels to match Count & Height
naz.seed <- naz |> 
  filter(Treatment != "Control")
naz.seed$Treatment <- relevel(naz.seed$Treatment, ref = "Seed")


# Sonoran Desert ----------------------------------------------------------

## All plots --------------------------------------------------------------

# All variables, with nested random effect
pos.sonoran <- glmmTMB(Seeded_Cover ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + 
                         PlotMix_Climate + MAT + MAP + Sand_content + Since_last_precip_sqrt + 
                         (1 | Site / Plot),
                       data = sonoran, family = genpois)
summary(pos.sonoran)
r2(pos.sonoran)
res.pos.sonoran <- simulateResiduals(pos.sonoran)
plotQQunif(pos.sonoran)
plotResiduals(pos.sonoran)
check_overdispersion(pos.sonoran) # no overdispersion detected
check_zeroinflation(pos.sonoran) # no zero-inflation detected
check_collinearity(pos.sonoran) # should drop AI or MAP

# 1: Drop MAP (collinearity)
pos.sonoran1 <- glmmTMB(Seeded_Cover ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + 
                          PlotMix_Climate + MAT + Sand_content + Since_last_precip_sqrt + 
                          (1 | Site / Plot),
                        data = sonoran, family = genpois)
summary(pos.sonoran1)
r2(pos.sonoran1)
res.pos.sonoran1 <- simulateResiduals(pos.sonoran1)
plotQQunif(pos.sonoran1)
plotResiduals(pos.sonoran1) # first time I have a residuals plot that actually looks okay lol
check_overdispersion(pos.sonoran1) # no overdispersion detected
check_zeroinflation(pos.sonoran1) # model is overfitting zeros
check_collinearity(pos.sonoran1)


## Control plots excluded -------------------------------------------------

# All variables, with nested random effect
pos.sonoran.seed <- glmmTMB(Seeded_Cover ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + 
                              PlotMix_Climate + MAT + Sand_content + Since_last_precip_sqrt + 
                              (1 | Site / Plot),
                            data = sonoran.seed, family = genpois)
summary(pos.sonoran.seed)
r2(pos.sonoran.seed)
res.pos.sonoran.seed <- simulateResiduals(pos.sonoran.seed)
plotQQunif(pos.sonoran.seed)
plotResiduals(pos.sonoran.seed) # residuals are actually okay lol
check_overdispersion(pos.sonoran.seed) # no overdispersion detected
check_zeroinflation(pos.sonoran.seed) # no zero-inflation detected
check_collinearity(pos.sonoran.seed)


# *** 1: Drop Sand_content, add Days_elapsed, no transformations: Seeded ***
pos.sonoran1.seed <- glmmTMB(Seeded_Cover ~ Perc_dev_cum_abs + AridityIndex + Treatment + 
                              PlotMix_Climate + MAT + Since_last_precip + 
                              Days_elapsed + (1 | Site / Plot),
                            data = sonoran.seed, family = genpois)
summary(pos.sonoran1.seed)
r2(pos.sonoran1.seed)
res.pos.sonoran1.seed <- simulateResiduals(pos.sonoran1.seed)
plotQQunif(pos.sonoran1.seed)
plotResiduals(pos.sonoran1.seed) # residuals are actually okay lol
check_overdispersion(pos.sonoran1.seed) # no overdispersion detected
check_zeroinflation(pos.sonoran1.seed) # no zero-inflation detected
check_collinearity(pos.sonoran1.seed)


# Northern Arizona Plateau ------------------------------------------------

## All plots --------------------------------------------------------------

# All variables, with nested random effect
pos.naz <- glmmTMB(Seeded_Cover ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + 
                     PlotMix_Climate + MAT + MAP + Sand_content + Since_last_precip_sqrt + 
                     (1 | Site / Plot),
                   data = naz, family = genpois)
summary(pos.naz)
r2(pos.naz)
res.pos.naz <- simulateResiduals(pos.naz)
plotQQunif(pos.naz)
plotResiduals(pos.naz)
check_overdispersion(pos.naz) # no overdispersion detected
check_zeroinflation(pos.naz) # no zero-inflation
check_collinearity(pos.naz)


## Control plots excluded -------------------------------------------------

# ***All variables, with nested random effect***
pos.naz.seed <- glmmTMB(Seeded_Cover ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + 
                          PlotMix_Climate + MAT + Sand_content + Since_last_precip_sqrt + 
                          (1 | Site / Plot),
                        data = naz.seed, family = genpois)
summary(pos.naz.seed)
r2(pos.naz.seed)
res.pos.naz.seed <- simulateResiduals(pos.naz.seed)
plotQQunif(pos.naz.seed)
plotResiduals(pos.naz.seed) 
check_overdispersion(pos.naz.seed) # no overdispersion detected
check_zeroinflation(pos.naz.seed) # no zero-inflation
check_collinearity(pos.naz.seed)

# *** 1: Add Days_elapsed, no transformations: Seeded ***
pos.naz1.seed <- glmmTMB(Seeded_Cover ~ Perc_dev_cum_abs + AridityIndex + Treatment + 
                          PlotMix_Climate + MAT + Sand_content + Since_last_precip + 
                          Days_elapsed + (1 | Site / Plot),
                        data = naz.seed, family = genpois)
summary(pos.naz1.seed)
r2(pos.naz1.seed)
res.pos.naz1.seed <- simulateResiduals(pos.naz1.seed)
plotQQunif(pos.naz1.seed)
plotResiduals(pos.naz1.seed) 
check_overdispersion(pos.naz1.seed) # no overdispersion detected
check_zeroinflation(pos.naz1.seed) # no zero-inflation
check_collinearity(pos.naz1.seed)


save.image("RData/10.3_generalized-linear-models_2x2-Seeded-Cover.RData")
