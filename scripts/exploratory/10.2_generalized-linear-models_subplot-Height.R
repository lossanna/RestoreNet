# Created: 024-09-09
# Last updated: 2024-09-09

# Purpose: Run generalized linear models for subplot data, with Height as response variable. 

# Use Perc_dev_cum_abs to measure precip variability, and Since_last_precip_sqrt to indicate
#   recent wet or dry conditions.

# Negative binomial used to help with overdispersion.
# Including (1 | Site / Plot) accounts for the non-independence of repeat sampling.

# Transformations do not really improve model fit or residuals plots.

# Run for desirable, weedy, and seeded species (with no control plots), for better
#   comparison with GLMs with Seeded_Cover as response variable.

# *** indicates model is included in PPT of draft figures and model results.

# Models are the same as those run in 08.2.R script (including same name); this script
#   is just to condense results.

library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)

# Load data ---------------------------------------------------------------

subplot.raw <- read_csv("data/cleaned/04.1_subplot-data_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")

# Data wrangling ----------------------------------------------------------

## Combine ----------------------------------------------------------------

# Reorganize columns for left_join()
cum.pd.subplot <- cum.pd |> 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Perc_deviation, Deviation_mm) |> 
  rename(Perc_dev_cum = Perc_deviation,
         Dev_mm_cum = Deviation_mm)

# Combine all variables
subplot <- subplot.raw |> 
  left_join(prism.data) |> 
  left_join(ai) |> 
  left_join(cum.pd.subplot)


## Remove Inf from Perc_dev_cum and NA height values ----------------------

# Infinity created when there was no rain during the monitoring period. This only happens
#   twice and these instances can be dropped.
subplot <- subplot |> 
  filter(Perc_dev_cum != Inf)

# Sometimes Height was missing (not recorded)
subplot <- subplot |> 
  filter(!is.na(Height))

# Check for non-integers in Height
non_integers <- subplot$Height != floor(subplot$Height)
subplot$Height[non_integers]

# Round non-integers
subplot$Height <- ifelse(subplot$Height != floor(subplot$Height), round(subplot$Height), subplot$Height)


## Re-level categorical variables to set reference ------------------------

# Treatment
unique(subplot$Treatment)
subplot$Treatment <- as.factor(subplot$Treatment)
subplot$Treatment <- relevel(subplot$Treatment, ref = "Control")

# PlotMix_Climate
unique(subplot$PlotMix_Climate)
#   Allow Current to remain reference; because of uneven sample sizes, if
#     None is reference then Projected will be dropped from models. Better
#     to drop None and be able to compare Current & Projected.

# Change ref from 0 for better comparisons
#   Lifeform
unique(subplot$Lifeform)
subplot$Lifeform <- as.factor(subplot$Lifeform)
subplot$Lifeform <- relevel(subplot$Lifeform, ref = "Forb")
#   Relevel to see if Forb is different than Grass (vs. different than 0)

# Duration
unique(subplot$Duration)
subplot$Duration <- as.factor(subplot$Duration)
subplot$Duration <- relevel(subplot$Duration, ref = "Annual")
#   Relevel to see if Annual is different than Perennial (vs. different than 0)

# PlantSource2
unique(subplot$PlantSource2)
subplot$PlantSource2 <- as.factor(subplot$PlantSource2)
subplot$PlantSource2 <- relevel(subplot$PlantSource2, ref = "Recruit")
#   Assign different reference for Desirable data set later


## Add transformation for normalization -----------------------------------

subplot <- subplot |> 
  mutate(Cum_precip_sqrt = sqrt(Cum_precip)) |> 
  mutate(AridityIndex_log = log(AridityIndex),
         Since_last_precip_sqrt = sqrt(Since_last_precip))


## Add Perc_dev_cum_abs col -----------------------------------------------

subplot <- subplot |> 
  mutate(Perc_dev_cum_abs = abs(Perc_dev_cum))


## Separate out Sonoran sites (6) -----------------------------------------

# Desirable
sonoran.des <- subplot |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran SE", "Sonoran Central"))
sonoran.des$PlantSource2 <- relevel(sonoran.des$PlantSource2, ref = "Native recruit")

# Weedy
sonoran.weed <- subplot |> 
  filter(Weedy != "Desirable")|> 
  filter(Region %in% c("Sonoran SE", "Sonoran Central"))

# Seeded
sonoran.seed <- sonoran.des |> 
  filter(SpeciesSeeded == "Yes") # removes Control plots
sonoran.seed$Treatment <- relevel(sonoran.seed$Treatment, ref = "Seed")


## Separate out Northern AZ sites (8) -------------------------------------

# Desirable
naz.des <- subplot |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau")
naz.des$PlantSource2 <- relevel(naz.des$PlantSource2, ref = "Native recruit")

# Weedy
naz.weed <- subplot |> 
  filter(Weedy != "Desirable")|> 
  filter(Region == "Colorado Plateau")

# Seeded
naz.seed <- naz.des |> 
  filter(SpeciesSeeded == "Yes") # removes Control plots
naz.seed$Treatment <- relevel(naz.seed$Treatment, ref = "Seed")



# Sonoran Desert ----------------------------------------------------------

## Desirable --------------------------------------------------------------

# All variables, nested random effect of Site/Plot: Desirable
nb.sonoran.des.abs2 <- glmmTMB(Height ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                                 PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                                 Since_last_precip_sqrt + (1 | Site / Plot),
                               data = sonoran.des,
                               family = nbinom2)
summary(nb.sonoran.des.abs2)
r2(nb.sonoran.des.abs2)
res.nb.sonoran.des.abs2 <- simulateResiduals(nb.sonoran.des.abs2)
plotQQunif(res.nb.sonoran.des.abs2)
plotResiduals(res.nb.sonoran.des.abs2)
check_overdispersion(nb.sonoran.des.abs2) # no overdispersion detected
check_zeroinflation(nb.sonoran.des.abs2) # no zero-inflation detected
check_collinearity(nb.sonoran.des.abs2) # should drop MAP or AridityIndex_log

# *** 1: Drop MAP (for collinearity): Desirable ***
nb.sonoran1.des.abs2 <- glmmTMB(Height ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                                  PlotMix_Climate + Duration + Lifeform + MAT + Sand_content + 
                                  Since_last_precip_sqrt + (1 | Site / Plot),
                                data = sonoran.des,
                                family = nbinom2)
summary(nb.sonoran1.des.abs2)
r2(nb.sonoran1.des.abs2)
res.nb.sonoran1.des.abs2 <- simulateResiduals(nb.sonoran1.des.abs2)
plotQQunif(res.nb.sonoran1.des.abs2)
plotResiduals(res.nb.sonoran1.des.abs2)
check_overdispersion(nb.sonoran1.des.abs2) # no overdispersion detected
check_zeroinflation(nb.sonoran1.des.abs2) # no zero-inflation detected
check_collinearity(nb.sonoran1.des.abs2)


## Weedy ------------------------------------------------------------------

# All variables, nested random effect of Site/Plot: Weedy: does not converge
nb.sonoran.weed.abs2 <- glmmTMB(Height ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                                  PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                                  Since_last_precip_sqrt + (1 | Site / Plot),
                                data = sonoran.weed,
                                family = nbinom2) # did not converge

# *** 1: Drop MAP & Duration (for convergence): Weedy ***
nb.sonoran1.weed.abs2 <- glmmTMB(Height ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 +
                                   PlotMix_Climate + Lifeform + MAT + Sand_content + 
                                   Since_last_precip_sqrt + (1 | Site / Plot),
                                 data = sonoran.weed,
                                 family = nbinom2)
summary(nb.sonoran1.weed.abs2)
r2(nb.sonoran1.weed.abs2)
res.nb.sonoran1.weed.abs2 <- simulateResiduals(nb.sonoran1.weed.abs2)
plotQQunif(res.nb.sonoran1.weed.abs2)
plotResiduals(res.nb.sonoran1.weed.abs2)
check_overdispersion(nb.sonoran1.weed.abs2) # no overdispersion detected
check_zeroinflation(nb.sonoran1.weed.abs2) # no zero-inflation detected
check_collinearity(nb.sonoran1.weed.abs2)


## Seeded -----------------------------------------------------------------

# All variables, nested random effect of Site/Plot: Seeded
nb.sonoran.seed.abs2 <- glmmTMB(Height ~ Perc_dev_cum_abs + AridityIndex_log + Treatment +  
                                  PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                                  Since_last_precip_sqrt + (1 | Site / Plot),
                                data = sonoran.seed,
                                family = nbinom2)
summary(nb.sonoran.seed.abs2)
r2(nb.sonoran.seed.abs2) # can't compute
res.nb.sonoran.seed.abs2 <- simulateResiduals(nb.sonoran.seed.abs2)
plotQQunif(res.nb.sonoran.seed.abs2)
plotResiduals(res.nb.sonoran.seed.abs2)
check_overdispersion(nb.sonoran.seed.abs2) # no overdispersion detected
check_zeroinflation(nb.sonoran.seed.abs2) # model is overfitting zeros
check_collinearity(nb.sonoran.seed.abs2)

#   1: Drop MAP (collinearity): Seeded
nb.sonoran1.seed.abs2 <- glmmTMB(Height ~ Perc_dev_cum_abs + AridityIndex_log + Treatment +  
                                   PlotMix_Climate + Duration + Lifeform + MAT + Sand_content + 
                                   Since_last_precip_sqrt + (1 | Site / Plot),
                                 data = sonoran.seed,
                                 family = nbinom2)
summary(nb.sonoran1.seed.abs2)
r2(nb.sonoran1.seed.abs2) # can't compute
res.nb.sonoran1.seed.abs2 <- simulateResiduals(nb.sonoran1.seed.abs2)
plotQQunif(res.nb.sonoran1.seed.abs2)
plotResiduals(res.nb.sonoran1.seed.abs2)
check_overdispersion(nb.sonoran1.seed.abs2) # no overdispersion detected
check_zeroinflation(nb.sonoran1.seed.abs2) # model is overfitting zeros
check_collinearity(nb.sonoran1.seed.abs2)
check_singularity(nb.sonoran1.seed.abs2)


# *** 2: Drop MAP (collinearity), AridityIndex_log, Sand_content (singularity): Seeded ***
nb.sonoran2.seed.abs2 <- glmmTMB(Height ~ Perc_dev_cum_abs + Treatment +  
                                   PlotMix_Climate + Duration + Lifeform + MAT +  
                                   Since_last_precip_sqrt + (1 | Site / Plot),
                                 data = sonoran.seed,
                                 family = nbinom2)
summary(nb.sonoran2.seed.abs2)
r2(nb.sonoran2.seed.abs2)
res.nb.sonoran2.seed.abs2 <- simulateResiduals(nb.sonoran2.seed.abs2)
plotQQunif(res.nb.sonoran2.seed.abs2)
plotResiduals(res.nb.sonoran2.seed.abs2)
check_overdispersion(nb.sonoran2.seed.abs2) # no overdispersion detected
check_zeroinflation(nb.sonoran2.seed.abs2) # model is overfitting zeros
check_collinearity(nb.sonoran2.seed.abs2)

#   Investigate Height by Duration 
summary(filter(sonoran.seed, Duration == "Annual")$Height)
summary(filter(sonoran.seed, Duration == "Perennial")$Height)
count(sonoran.seed, Duration)
count(filter(sonoran.seed, PlotMix_Climate == "Current"), Duration)
count(filter(sonoran.seed, PlotMix_Climate == "Projected"), Duration)
#     I don't understand how DurationPerennial can be negative when obviously Perennials are taller?



# Northern Arizona Plateau ------------------------------------------------

## Desirable --------------------------------------------------------------

# *** All variables, nested random effect of Site/Plot: Desirable ***
nb.naz.des.abs2 <- glmmTMB(Height ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                             Since_last_precip_sqrt + (1 | Site / Plot),
                           data = naz.des,
                           family = nbinom2)
summary(nb.naz.des.abs2)
r2(nb.naz.des.abs2)
res.nb.naz.des.abs2 <- simulateResiduals(nb.naz.des.abs2)
plotQQunif(res.nb.naz.des.abs2)
plotResiduals(res.nb.naz.des.abs2)
check_overdispersion(nb.naz.des.abs2) # no overdispersion detected
check_zeroinflation(nb.naz.des.abs2) # no zero-inflation detected
check_collinearity(nb.naz.des.abs2)


## Weedy ------------------------------------------------------------------

#   All variables, nested random effect of Site/Plot: Weedy
nb.naz.weed.abs2 <- glmmTMB(Height ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                              PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                              Since_last_precip_sqrt + (1 | Site / Plot),
                            data = naz.weed,
                            family = nbinom2)
summary(nb.naz.weed.abs2)
r2(nb.naz.weed.abs2)
res.nb.naz.weed.abs2 <- simulateResiduals(nb.naz.weed.abs2)
plotQQunif(res.nb.naz.weed.abs2)
plotResiduals(res.nb.naz.weed.abs2)
check_overdispersion(nb.naz.weed.abs2) # no overdispersion detected
check_zeroinflation(nb.naz.weed.abs2) # no zero-inflation detected
check_collinearity(nb.naz.weed.abs2) # should drop PlantSource2 or Duration

# *** 1: Drop Duration (for collinearity): Weedy ***
#   Basically all of them are annuals
nb.naz1.weed.abs2 <- glmmTMB(Height ~ Perc_dev_cum_abs + AridityIndex_log + Treatment +  PlantSource2 +
                               PlotMix_Climate + Lifeform + MAT + MAP + Sand_content + 
                               Since_last_precip_sqrt + (1 | Site / Plot),
                             data = naz.weed,
                             family = nbinom2)
summary(nb.naz1.weed.abs2)
r2(nb.naz1.weed.abs2)
res.nb.naz1.weed.abs2 <- simulateResiduals(nb.naz1.weed.abs2)
plotQQunif(res.nb.naz1.weed.abs2)
plotResiduals(res.nb.naz1.weed.abs2)
check_overdispersion(nb.naz1.weed.abs2) # no overdispersion detected
check_zeroinflation(nb.naz1.weed.abs2) # no zero-inflation detected
check_collinearity(nb.naz1.weed.abs2)


## Seeded -----------------------------------------------------------------

# *** All variables, nested random effect of Site/Plot: Seeded ***
nb.naz.seed.abs2 <- glmmTMB(Height ~ Perc_dev_cum_abs + AridityIndex_log + Treatment +  
                              PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                              Since_last_precip_sqrt + (1 | Site / Plot),
                            data = naz.seed,
                            family = nbinom2)
summary(nb.naz.seed.abs2)
r2(nb.naz.seed.abs2)
res.nb.naz.seed.abs2 <- simulateResiduals(nb.naz.seed.abs2)
plotQQunif(res.nb.naz.seed.abs2)
plotResiduals(res.nb.naz.seed.abs2)
check_overdispersion(nb.naz.seed.abs2) # no overdispersion detected
check_collinearity(nb.naz.seed.abs2)


save.image("RData/10.2_generalized-linear-models_subplot-Height.RData")
