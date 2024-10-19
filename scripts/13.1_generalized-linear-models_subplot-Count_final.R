# Created: 024-10-17
# Last updated: 2024-10-18

# Purpose: Run *finalized* generalized linear models for subplot data, with Count as response variable.
#   Models already have some variables dropped to improve convergence. 

# Difference from 10.1: "Seed" used as reference for Treatment instead of "Control" because
#   this better captures the effect of just the soil surface treatments alone. This changes
#   the weedy and desirable models, but not the seeded one (ref was already "Seed" for those).

# For previous exploration, see 10.1_generalized-linear-models_subplot-Count.R. 

# 6 models:
#   Sonoran Desert, Desirable
#   Sonoran Desert, Weedy
#   Sonoran Desert, Seeded
#   Northern Arizona, Desirable
#   Northern Arizona, Weedy
#   Northern Arizona, Seeded


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


## Remove Inf from Perc_dev_cum -------------------------------------------

# Infinity created when there was no rain during the monitoring period. This only happens
#   twice and these instances can be dropped.
subplot <- subplot |> 
  filter(Perc_dev_cum != Inf)


## Re-level categorical variables to set reference ------------------------

# Treatment
unique(subplot$Treatment)
subplot$Treatment <- as.factor(subplot$Treatment)
subplot$Treatment <- relevel(subplot$Treatment, ref = "Seed")

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
#   Assign different reference for Desirable data set


## Add Perc_dev_cum_abs and Days_elapsed cols -----------------------------

subplot <- subplot |> 
  mutate(Perc_dev_cum_abs = abs(Perc_dev_cum)) |> 
  mutate(Days_elapsed = difftime(Date_Monitored, Date_Seeded)) |> 
  mutate(Days_elapsed = as.numeric(Days_elapsed))


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



# Sonoran Desert ----------------------------------------------------------

## Desirable --------------------------------------------------------------

# MAP dropped for collinearity
nb.sonoran.des <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex + Treatment + PlantSource2 + 
                                  PlotMix_Climate + Duration + Lifeform + MAT + 
                                  Since_last_precip + Days_elapsed + (1 | Site / Plot),
                                data = sonoran.des,
                                family = nbinom2)
summary(nb.sonoran.des)
r2(nb.sonoran.des)
res.nb.sonoran.des <- simulateResiduals(nb.sonoran.des)
plotQQunif(res.nb.sonoran.des)
plotResiduals(res.nb.sonoran.des)
check_overdispersion(nb.sonoran.des) # overdispersion detected
check_zeroinflation(nb.sonoran.des) # model is overfitting zeros
check_collinearity(nb.sonoran.des)


## Weedy ------------------------------------------------------------------

# MAP & Duration dropped for collinearity; most weeds were annuals
nb.sonoran.weed <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex + Treatment + PlantSource2 + 
                                   PlotMix_Climate + Duration + Lifeform + MAT + 
                                   Since_last_precip + Days_elapsed + (1 | Site / Plot),
                                 data = sonoran.weed,
                                 family = nbinom2)
summary(nb.sonoran.weed)
r2(nb.sonoran.weed)
res.nb.sonoran.weed <- simulateResiduals(nb.sonoran.weed)
plotQQunif(res.nb.sonoran.weed)
plotResiduals(res.nb.sonoran.weed)
check_overdispersion(nb.sonoran.weed) # no overdispersion detected
check_zeroinflation(nb.sonoran.weed) # model is overfitting zeros
check_collinearity(nb.sonoran.weed)


## Seeded -----------------------------------------------------------------

# Same as nb.sonoran3.seed.abs2 from 10.1.R
nb.sonoran.seed <- glmmTMB(Count ~ Perc_dev_cum_abs + Treatment +  
                                   PlotMix_Climate + Duration + Lifeform + MAT +  
                                   Since_last_precip + Days_elapsed + (1 | Site / Plot),
                                 data = sonoran.seed,
                                 family = nbinom2)
summary(nb.sonoran.seed)
r2(nb.sonoran.seed)
res.nb.sonoran.seed <- simulateResiduals(nb.sonoran.seed)
plotQQunif(res.nb.sonoran.seed)
plotResiduals(res.nb.sonoran.seed)
check_overdispersion(nb.sonoran.seed) # overdispersion detected
check_collinearity(nb.sonoran.seed)


# Northern Arizona --------------------------------------------------------

## Desirable --------------------------------------------------------------

# All variables
nb.naz.des <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex + Treatment + PlantSource2 + 
                              PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content +  
                              Since_last_precip + Days_elapsed + (1 | Site / Plot),
                            data = naz.des,
                            family = nbinom2)
summary(nb.naz.des)
r2(nb.naz.des)
res.nb.naz.des <- simulateResiduals(nb.naz.des)
plotQQunif(res.nb.naz.des)
plotResiduals(res.nb.naz.des)
check_overdispersion(nb.naz.des) # overdispersion detected
check_zeroinflation(nb.naz.des) # model is overfitting zeros
check_collinearity(nb.naz.des)


## Weedy ------------------------------------------------------------------

# Duration dropped for collinearity
nb.naz.weed <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex + Treatment + PlantSource2 + 
                               PlotMix_Climate + Lifeform + MAT + MAP + Sand_content + 
                               Since_last_precip + Days_elapsed + (1 | Site / Plot),
                             data = naz.weed,
                             family = nbinom2)
summary(nb.naz.weed)
r2(nb.naz.weed)
res.nb.naz.weed <- simulateResiduals(nb.naz.weed)
plotQQunif(res.nb.naz.weed)
plotResiduals(res.nb.naz.weed)
check_overdispersion(nb.naz.weed) # overdispersion detected
check_zeroinflation(nb.naz.weed) # model is overfitting zeros
check_collinearity(nb.naz.weed)


## Seeded -----------------------------------------------------------------

# From 10.1.R: nb.naz1.seed.abs2
nb.naz.seed <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex + Treatment +  
                               PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                               Since_last_precip + Days_elapsed + (1 | Site / Plot),
                             data = naz.seed,
                             family = nbinom2)
summary(nb.naz.seed)
r2(nb.naz.seed)
res.nb.naz.seed <- simulateResiduals(nb.naz.seed)
plotQQunif(res.nb.naz.seed)
plotResiduals(res.nb.naz.seed)
check_overdispersion(nb.naz.seed) # overdispersion detected
check_collinearity(nb.naz.seed)

save.image("RData/13.1_generalized-linear-models_subplot-Count_final.RData")
