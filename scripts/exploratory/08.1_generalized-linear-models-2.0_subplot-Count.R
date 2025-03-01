# Created: 2024-08-29
# Last updated: 2024-09-04

# Purpose: Run generalized linear models for subplot data, with Count as response variable. 
#   Check for overdispersion and zero-inflation.

# Differences between this and v 1.0 script (07.1.R):
#   Only negative binomial distribution used.
#   Data always split by Weedy/Desirable.
#   AridityIndex and Cum_precip have been transformed to improve normality.
#   3 datasets used: (1) all data, minus Perc_dev_cum outliers; (2) Sonoran Desert; (3) CO Plateau
#   Also investigated Perc_dev_cum in absolute value, and with Since_last_precip instead of Cum_precip.
#   And changed reference of PlantSource2 for weedy species to "Recruit".

# Negative binomial used to help with overdispersion.
# Including (1 | Site / Plot) accounts for the non-independence of repeat sampling.

# Transformations do not really improve model fit or residuals plots.

# For Perc_dev_abs_cum + Since_last_precip_sqrt, also ran model on just seeded species, for better
#   comparison with GLMs with Seeded_Cover as response variable.

# *** indicates model is included in PPT of draft figures and model results.

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
#   Assign different reference for Desirable data set


## Add transformation for normalization -----------------------------------

subplot <- subplot |> 
  mutate(Cum_precip_sqrt = sqrt(Cum_precip)) |> 
  mutate(AridityIndex_log = log(AridityIndex),
         Since_last_precip_sqrt = sqrt(Since_last_precip))


# AI: higher negative AI values = drier sites
ai |> 
  mutate(AI_log = log(AridityIndex)) |> 
  ggplot(aes(x = AridityIndex, y = AI_log)) +
  geom_point()

prism.data |> 
  mutate(Since_last_precip_sqrt = sqrt(Since_last_precip)) |> 
  ggplot(aes(x = Since_last_precip, y = Since_last_precip_sqrt)) +
  geom_point()

## Separate Weedy and Desirable & add Perc_dev_cum_abs --------------------

# With 800% precip dev from Mojave removed
#   Desirable
subplot.des <- subplot |> 
  filter(Weedy != "Weedy") |> 
  filter(Perc_dev_cum < 8)|> 
  mutate(Perc_dev_cum_abs = abs(Perc_dev_cum))
subplot.des$PlantSource2 <- relevel(subplot.des$PlantSource2, ref = "Native recruit")

#   Weedy
subplot.weed <- subplot |> 
  filter(Perc_dev_cum < 8) |> 
  filter(Weedy != "Desirable")|> 
  mutate(Perc_dev_cum_abs = abs(Perc_dev_cum))


## Separate out Sonoran sites (6) -----------------------------------------

# Desirable
sonoran.des <- subplot.des |> 
  filter(Region %in% c("Sonoran SE", "Sonoran Central"))

# Weedy
sonoran.weed <- subplot.weed |> 
  filter(Region %in% c("Sonoran SE", "Sonoran Central"))

# Seeded
sonoran.seed <- sonoran.des |> 
  filter(SpeciesSeeded == "Yes") # removes Control plots
sonoran.seed$Treatment <- relevel(sonoran.seed$Treatment, ref = "Seed")


## Separate out Northern AZ sites (8) -------------------------------------

# Desirable
naz.des <- subplot.des |> 
  filter(Region == "Colorado Plateau")

# Weedy
naz.weed <- subplot.weed |> 
  filter(Region == "Colorado Plateau")

# Seeded
naz.seed <- naz.des |> 
  filter(SpeciesSeeded == "Yes") # removes Control plots
naz.seed$Treatment <- relevel(naz.seed$Treatment, ref = "Seed")


# All sites ---------------------------------------------------------------

## Perc_dev_cum (wetter & drier) ------------------------------------------

# Desirable
#   All variables, nested random effect of Site/Plot: Desirable
nb.all.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex_log + Treatment + PlantSource2 + 
                           PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                           Cum_precip_sqrt + (1 | Site / Plot),
                         data = subplot.des,
                         family = nbinom2)
summary(nb.all.des)
r2(nb.all.des)
res.nb.all.des <- simulateResiduals(nb.all.des)
plotResiduals(nb.all.des)
plotQQunif(res.nb.all.des)
check_overdispersion(nb.all.des) # overdispersion detected
check_zeroinflation(nb.all.des) # model is overfitting zeros
check_collinearity(nb.all.des)

# Weedy
#   All variables, nested random effect of Site/Plot: Weedy
nb.all.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex_log + Treatment + PlantSource2 + 
                            PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                            Cum_precip_sqrt + (1 | Site / Plot),
                          data = subplot.weed,
                          family = nbinom2)
summary(nb.all.weed)
r2(nb.all.weed)
res.nb.all.weed <- simulateResiduals(nb.all.weed)
plotResiduals(res.nb.all.weed)
plotQQunif(res.nb.all.weed)
check_overdispersion(nb.all.weed) # overdispersion detected
check_zeroinflation(nb.all.weed) # model is overfitting zeros
check_collinearity(nb.all.weed) # Duration & PlantSource2 correlated

#   1: Drop PlantSource2 (collinearity): Weedy
nb.all1.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex_log + Treatment +  
                             PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                             Cum_precip_sqrt + (1 | Site / Plot),
                           data = subplot.weed,
                           family = nbinom2)
summary(nb.all1.weed)
r2(nb.all1.weed)
res.nb.all1.weed <- simulateResiduals(nb.all1.weed)
plotResiduals(res.nb.all1.weed)
plotQQunif(res.nb.all1.weed)
check_overdispersion(nb.all1.weed) # overdispersion detected
check_zeroinflation(nb.all1.weed) # model is overfitting zeros
check_collinearity(nb.all1.weed)


## By Perc_dev_cum_abs & Cum_precip_sqrt -----------------------------------

# Desirable
#   All variables, nested random effect of Site/Plot: Desirable
nb.all.des.abs <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                            PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                            Cum_precip_sqrt + (1 | Site / Plot),
                          data = subplot.des,
                          family = nbinom2)
summary(nb.all.des.abs)
r2(nb.all.des.abs)
res.nb.all.des.abs <- simulateResiduals(nb.all.des.abs)
plotResiduals(nb.all.des.abs)
plotQQunif(res.nb.all.des.abs)
check_overdispersion(nb.all.des.abs) # overdispersion detected
check_zeroinflation(nb.all.des.abs) # model is overfitting zeros
check_collinearity(nb.all.des.abs)

# Weedy
#   All variables, nested random effect of Site/Plot: Weedy
nb.all.weed.abs <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                             Cum_precip_sqrt + (1 | Site / Plot),
                           data = subplot.weed,
                           family = nbinom2)
summary(nb.all.weed.abs)
r2(nb.all.weed.abs)
res.nb.all.weed.abs <- simulateResiduals(nb.all.weed.abs)
plotResiduals(res.nb.all.weed.abs)
plotQQunif(res.nb.all.weed.abs)
check_overdispersion(nb.all.weed.abs) # overdispersion detected
check_zeroinflation(nb.all.weed.abs) # model is overfitting zeros
check_collinearity(nb.all.weed.abs) # Duration & PlantSource2 correlated

#   1: Drop PlantSource2 (collinearity): Weedy
nb.all1.weed.abs <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment +  
                              PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                              Cum_precip_sqrt + (1 | Site / Plot),
                            data = subplot.weed,
                            family = nbinom2)
summary(nb.all1.weed.abs)
r2(nb.all1.weed.abs)
res.nb.all1.weed.abs <- simulateResiduals(nb.all1.weed.abs)
plotResiduals(res.nb.all1.weed.abs)
plotQQunif(res.nb.all1.weed.abs)
check_overdispersion(nb.all1.weed.abs) # overdispersion detected
check_zeroinflation(nb.all1.weed.abs) # model is overfitting zeros
check_collinearity(nb.all1.weed.abs)


## By Perc_dev_cum_abs & Since_last_precip_sqrt ---------------------------

# Desirable
#   *** All variables, nested random effect of Site/Plot: Desirable ***
nb.all.des.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                             Since_last_precip_sqrt + (1 | Site / Plot),
                           data = subplot.des,
                           family = nbinom2)
summary(nb.all.des.abs2)
r2(nb.all.des.abs2)
res.nb.all.des.abs2 <- simulateResiduals(nb.all.des.abs2)
plotResiduals(nb.all.des.abs2)
plotQQunif(res.nb.all.des.abs2)
check_overdispersion(nb.all.des.abs2) # overdispersion detected
check_zeroinflation(nb.all.des.abs2) # model is overfitting zeros
check_collinearity(nb.all.des.abs2)

# Weedy
#   All variables, nested random effect of Site/Plot: Weedy
nb.all.weed.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                              PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                              Since_last_precip_sqrt + (1 | Site / Plot),
                            data = subplot.weed,
                            family = nbinom2)
summary(nb.all.weed.abs2)
r2(nb.all.weed.abs2)
res.nb.all.weed.abs2 <- simulateResiduals(nb.all.weed.abs2)
plotResiduals(res.nb.all.weed.abs2)
plotQQunif(res.nb.all.weed.abs2)
check_overdispersion(nb.all.weed.abs2) # overdispersion detected
check_zeroinflation(nb.all.weed.abs2) # model is overfitting zeros
check_collinearity(nb.all.weed.abs2) # Duration & PlantSource2 correlated

#   *** 1: Drop PlantSource2 (collinearity): Weedy ***
nb.all1.weed.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment +  
                              PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                               Since_last_precip_sqrt + (1 | Site / Plot),
                            data = subplot.weed,
                            family = nbinom2)
summary(nb.all1.weed.abs2)
r2(nb.all1.weed.abs2)
res.nb.all1.weed.abs2 <- simulateResiduals(nb.all1.weed.abs2)
plotResiduals(res.nb.all1.weed.abs2)
plotQQunif(res.nb.all1.weed.abs2)
check_overdispersion(nb.all1.weed.abs2) # overdispersion detected
check_zeroinflation(nb.all1.weed.abs2) # model is overfitting zeros
check_collinearity(nb.all1.weed.abs2)



# Sonoran Desert ----------------------------------------------------------

## Perc_dev_cum (wetter & drier) ------------------------------------------

# Desirable
#   All variables, nested random effect of Site/Plot: Desirable
nb.sonoran.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex_log + Treatment + PlantSource2 + 
                            PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                            Cum_precip_sqrt + (1 | Site / Plot),
                          data = sonoran.des,
                          family = nbinom2)
summary(nb.sonoran.des)
r2(nb.sonoran.des)
res.nb.sonoran.des <- simulateResiduals(nb.sonoran.des)
plotQQunif(res.nb.sonoran.des)
plotResiduals(res.nb.sonoran.des)
check_overdispersion(nb.sonoran.des) # overdispersion detected
check_zeroinflation(nb.sonoran.des) # model is overfitting zeros
check_collinearity(nb.sonoran.des) # should drop MAP

#   1: Drop MAP (for collinearity): Desirable
nb.sonoran1.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex_log + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT + Sand_content + 
                             Cum_precip_sqrt + (1 | Site / Plot),
                           data = sonoran.des,
                           family = nbinom2)
summary(nb.sonoran1.des)
r2(nb.sonoran1.des)
res.nb.sonoran1.des <- simulateResiduals(nb.sonoran1.des)
plotQQunif(res.nb.sonoran1.des)
plotResiduals(res.nb.sonoran1.des)
check_overdispersion(nb.sonoran1.des) # overdispersion detected
check_zeroinflation(nb.sonoran1.des) # model is overfitting zeros
check_collinearity(nb.sonoran1.des)


# Weedy
#   All variables, nested random effect of Site/Plot: Weedy
nb.sonoran.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex_log + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                             Cum_precip_sqrt + (1 | Site / Plot),
                           data = sonoran.weed,
                           family = nbinom2)
summary(nb.sonoran.weed)
r2(nb.sonoran.weed)
res.nb.sonoran.weed <- simulateResiduals(nb.sonoran.weed)
plotQQunif(res.nb.sonoran.weed)
plotResiduals(res.nb.sonoran.weed)
check_overdispersion(nb.sonoran.weed) # overdispersion detected
check_zeroinflation(nb.sonoran.weed) # model is overfitting zeros
check_collinearity(nb.sonoran.weed) # should drop MAP & Duration

#   1: Drop MAP & Duration (for collinearity): Weedy
nb.sonoran1.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex_log + Treatment + PlantSource2 + 
                              PlotMix_Climate + Lifeform + MAT + Sand_content + 
                              Cum_precip_sqrt + (1 | Site / Plot),
                            data = sonoran.weed,
                            family = nbinom2)
summary(nb.sonoran1.weed)
r2(nb.sonoran1.weed)
res.nb.sonoran1.weed <- simulateResiduals(nb.sonoran1.weed)
plotQQunif(res.nb.sonoran1.weed)
plotResiduals(res.nb.sonoran1.weed)
check_overdispersion(nb.sonoran1.weed) # overdispersion detected
check_zeroinflation(nb.sonoran1.weed) # model is overfitting zeros
check_collinearity(nb.sonoran1.weed)



## By Perc_dev_cum_abs & Cum_precip_sqrt -----------------------------------

# Desirable
#   All variables, nested random effect of Site/Plot: Desirable
nb.sonoran.des.abs <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                            PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                            Cum_precip_sqrt + (1 | Site / Plot),
                          data = sonoran.des,
                          family = nbinom2)
summary(nb.sonoran.des.abs)
r2(nb.sonoran.des.abs)
res.nb.sonoran.des.abs <- simulateResiduals(nb.sonoran.des.abs)
plotQQunif(res.nb.sonoran.des.abs)
plotResiduals(res.nb.sonoran.des.abs)
check_overdispersion(nb.sonoran.des.abs) # overdispersion detected
check_zeroinflation(nb.sonoran.des.abs) # model is overfitting zeros
check_collinearity(nb.sonoran.des.abs) # should drop MAP

#   1: Drop MAP (for collinearity): Desirable
nb.sonoran1.des.abs <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT + Sand_content + 
                             Cum_precip_sqrt + (1 | Site / Plot),
                           data = sonoran.des,
                           family = nbinom2)
summary(nb.sonoran1.des.abs)
r2(nb.sonoran1.des.abs)
res.nb.sonoran1.des.abs <- simulateResiduals(nb.sonoran1.des.abs)
plotQQunif(res.nb.sonoran1.des.abs)
plotResiduals(res.nb.sonoran1.des.abs)
check_overdispersion(nb.sonoran1.des.abs) # overdispersion detected
check_zeroinflation(nb.sonoran1.des.abs) # model is overfitting zeros
check_collinearity(nb.sonoran1.des.abs)


# Weedy
#   All variables, nested random effect of Site/Plot: Weedy
nb.sonoran.weed.abs <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                             Cum_precip_sqrt + (1 | Site / Plot),
                           data = sonoran.weed,
                           family = nbinom2)
summary(nb.sonoran.weed.abs)
r2(nb.sonoran.weed.abs) # something is wrong
res.nb.sonoran.weed.abs <- simulateResiduals(nb.sonoran.weed.abs)
plotQQunif(res.nb.sonoran.weed.abs)
plotResiduals(res.nb.sonoran.weed.abs)
check_overdispersion(nb.sonoran.weed.abs) # overdispersion detected
check_zeroinflation(nb.sonoran.weed.abs) # model is overfitting zeros
check_collinearity(nb.sonoran.weed.abs) # should drop MAP & Duration

#   1: Drop MAP & Duration (for collinearity): Weedy
nb.sonoran1.weed.abs <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                              PlotMix_Climate + Lifeform + MAT + Sand_content + 
                              Cum_precip_sqrt + (1 | Site / Plot),
                            data = sonoran.weed,
                            family = nbinom2)
summary(nb.sonoran1.weed.abs)
r2(nb.sonoran1.weed.abs)
res.nb.sonoran1.weed.abs <- simulateResiduals(nb.sonoran1.weed.abs)
plotQQunif(res.nb.sonoran1.weed.abs)
plotResiduals(res.nb.sonoran1.weed.abs)
check_overdispersion(nb.sonoran1.weed.abs) # overdispersion detected
check_zeroinflation(nb.sonoran1.weed.abs) # model is overfitting zeros
check_collinearity(nb.sonoran1.weed.abs)


## By Perc_dev_cum_abs & Since_last_precip_sqrt ---------------------------

# Desirable
#   All variables, nested random effect of Site/Plot: Desirable
nb.sonoran.des.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                                 PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                                 Since_last_precip_sqrt + (1 | Site / Plot),
                               data = sonoran.des,
                               family = nbinom2)
summary(nb.sonoran.des.abs2)
r2(nb.sonoran.des.abs2)
res.nb.sonoran.des.abs2 <- simulateResiduals(nb.sonoran.des.abs2)
plotQQunif(res.nb.sonoran.des.abs2)
plotResiduals(res.nb.sonoran.des.abs2)
check_overdispersion(nb.sonoran.des.abs2) # overdispersion detected
check_zeroinflation(nb.sonoran.des.abs2) # model is overfitting zeros
check_collinearity(nb.sonoran.des.abs2) # should drop MAP

#   *** 1: Drop MAP (for collinearity): Desirable ***
nb.sonoran1.des.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                                  PlotMix_Climate + Duration + Lifeform + MAT + Sand_content + 
                                  Since_last_precip_sqrt + (1 | Site / Plot),
                                data = sonoran.des,
                                family = nbinom2)
summary(nb.sonoran1.des.abs2)
r2(nb.sonoran1.des.abs2)
res.nb.sonoran1.des.abs2 <- simulateResiduals(nb.sonoran1.des.abs2)
plotQQunif(res.nb.sonoran1.des.abs2)
plotResiduals(res.nb.sonoran1.des.abs2)
check_overdispersion(nb.sonoran1.des.abs2) # overdispersion detected
check_zeroinflation(nb.sonoran1.des.abs2) # model is overfitting zeros
check_collinearity(nb.sonoran1.des.abs2)


# Weedy
#   All variables, nested random effect of Site/Plot: Weedy: does not converge
nb.sonoran.weed.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                                  PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                                  Since_last_precip_sqrt + (1 | Site / Plot),
                                data = sonoran.weed,
                                family = nbinom2) # did not converge

#   *** 1: Drop MAP & Duration (for collinearity): Weedy ***
nb.sonoran1.weed.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                                   PlotMix_Climate + Lifeform + MAT + Sand_content + 
                                   Since_last_precip_sqrt + (1 | Site / Plot),
                                 data = sonoran.weed,
                                 family = nbinom2)
summary(nb.sonoran1.weed.abs2)
r2(nb.sonoran1.weed.abs2)
res.nb.sonoran1.weed.abs2 <- simulateResiduals(nb.sonoran1.weed.abs2)
plotQQunif(res.nb.sonoran1.weed.abs2)
plotResiduals(res.nb.sonoran1.weed.abs2)
check_overdispersion(nb.sonoran1.weed.abs2) # overdispersion detected
check_zeroinflation(nb.sonoran1.weed.abs2) # model is overfitting zeros
check_collinearity(nb.sonoran1.weed.abs2)


# Seeded
nb.sonoran.seed.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment +  
                                  PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                                  Since_last_precip_sqrt + (1 | Site / Plot),
                                data = sonoran.seed,
                                family = nbinom2)
summary(nb.sonoran.seed.abs2)
r2(nb.sonoran.seed.abs2) # can't compute
res.nb.sonoran.seed.abs2 <- simulateResiduals(nb.sonoran.seed.abs2)
plotQQunif(res.nb.sonoran.seed.abs2)
plotResiduals(res.nb.sonoran.seed.abs2)
check_overdispersion(nb.sonoran.seed.abs2) # overdispersion detected
check_collinearity(nb.sonoran.seed.abs2)

#   1: Drop MAP (collinearity): Seeded
nb.sonoran1.seed.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment +  
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


#   *** 2: Drop MAP (collinearity), AridityIndex_log, Sand_content (singularity): Seeded ***
nb.sonoran2.seed.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + Treatment +  
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
check_collinearity(nb.sonoran2.seed.abs2)


# Northern Arizona Plateau ------------------------------------------------

## Perc_dev_cum (wetter & drier) ------------------------------------------

# Desirable
#   All variables, nested random effect of Site/Plot: Desirable
nb.naz.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex_log + Treatment + PlantSource2 + 
                        PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                        Cum_precip_sqrt + (1 | Site / Plot),
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


# Weedy
#   All variables, nested random effect of Site/Plot: Weedy
nb.naz.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex_log + Treatment + PlantSource2 + 
                         PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                         Cum_precip_sqrt + (1 | Site / Plot),
                       data = naz.weed,
                       family = nbinom2)
summary(nb.naz.weed)
r2(nb.naz.weed)
res.nb.naz.weed <- simulateResiduals(nb.naz.weed)
plotQQunif(res.nb.naz.weed)
plotResiduals(res.nb.naz.weed)
check_overdispersion(nb.naz.weed) # overdispersion detected
check_zeroinflation(nb.naz.weed) # model is overfitting zeros
check_collinearity(nb.naz.weed) # drop PlantSource2

#   1: Drop Duration (for collinearity): Weedy
#     Basically all of them are annuals
nb.naz1.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex_log + Treatment + PlantSource2 + 
                          PlotMix_Climate + Lifeform + MAT + MAP + Sand_content + 
                          Cum_precip_sqrt + (1 | Site / Plot),
                        data = naz.weed,
                        family = nbinom2)
summary(nb.naz1.weed)
r2(nb.naz1.weed)
res.nb.naz1.weed <- simulateResiduals(nb.naz1.weed)
plotQQunif(res.nb.naz1.weed)
plotResiduals(res.nb.naz1.weed)
check_overdispersion(nb.naz1.weed) # overdispersion detected
check_zeroinflation(nb.naz1.weed) # model is overfitting zeros
check_collinearity(nb.naz1.weed)


## By Perc_dev_cum_abs & Cum_precip_sqrt -----------------------------------

# Desirable
#   All variables, nested random effect of Site/Plot: Desirable
nb.naz.des.abs <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                            PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                            Cum_precip_sqrt + (1 | Site / Plot),
                          data = naz.des,
                          family = nbinom2)
summary(nb.naz.des.abs)
r2(nb.naz.des.abs)
res.nb.naz.des.abs <- simulateResiduals(nb.naz.des.abs)
plotQQunif(res.nb.naz.des.abs)
plotResiduals(res.nb.naz.des.abs)
check_overdispersion(nb.naz.des.abs) # overdispersion detected
check_zeroinflation(nb.naz.des.abs) # model is overfitting zeros
check_collinearity(nb.naz.des.abs)


# Weedy
#   All variables, nested random effect of Site/Plot: Weedy
nb.naz.weed.abs <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                             Cum_precip_sqrt + (1 | Site / Plot),
                           data = naz.weed,
                           family = nbinom2)
summary(nb.naz.weed.abs)
r2(nb.naz.weed.abs)
res.nb.naz.weed.abs <- simulateResiduals(nb.naz.weed.abs)
plotQQunif(res.nb.naz.weed.abs)
plotResiduals(res.nb.naz.weed.abs)
check_overdispersion(nb.naz.weed.abs) # overdispersion detected
check_zeroinflation(nb.naz.weed.abs) # model is overfitting zeros
check_collinearity(nb.naz.weed.abs) # drop PlantSource2 or Duration

#   1: Drop Duration (for collinearity): Weedy
#     Basically all of them are annuals
nb.naz1.weed.abs <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                              PlotMix_Climate + Lifeform + MAT + MAP + Sand_content + 
                              Cum_precip_sqrt + (1 | Site / Plot),
                            data = naz.weed,
                            family = nbinom2)
summary(nb.naz1.weed.abs)
r2(nb.naz1.weed.abs)
res.nb.naz1.weed.abs <- simulateResiduals(nb.naz1.weed.abs)
plotQQunif(res.nb.naz1.weed.abs)
plotResiduals(res.nb.naz1.weed.abs)
check_overdispersion(nb.naz1.weed.abs) # overdispersion detected
check_zeroinflation(nb.naz1.weed.abs) # model is overfitting zeros
check_collinearity(nb.naz1.weed.abs)


## By Perc_dev_cum_abs & Since_last_precip_sqrt ---------------------------

# Desirable
#   *** All variables, nested random effect of Site/Plot: Desirable ***
nb.naz.des.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                             Since_last_precip_sqrt + (1 | Site / Plot),
                           data = naz.des,
                           family = nbinom2)
summary(nb.naz.des.abs2)
r2(nb.naz.des.abs2)
res.nb.naz.des.abs2 <- simulateResiduals(nb.naz.des.abs2)
plotQQunif(res.nb.naz.des.abs2)
plotResiduals(res.nb.naz.des.abs2)
check_overdispersion(nb.naz.des.abs2) # overdispersion detected
check_zeroinflation(nb.naz.des.abs2) # model is overfitting zeros
check_collinearity(nb.naz.des.abs2)


# Weedy
#   All variables, nested random effect of Site/Plot: Weedy
nb.naz.weed.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                              PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                              Since_last_precip_sqrt + (1 | Site / Plot),
                            data = naz.weed,
                            family = nbinom2)
summary(nb.naz.weed.abs2)
r2(nb.naz.weed.abs2)
res.nb.naz.weed.abs2 <- simulateResiduals(nb.naz.weed.abs2)
plotQQunif(res.nb.naz.weed.abs2)
plotResiduals(res.nb.naz.weed.abs2)
check_overdispersion(nb.naz.weed.abs2) # overdispersion detected
check_zeroinflation(nb.naz.weed.abs2) # model is overfitting zeros
check_collinearity(nb.naz.weed.abs2) # drop PlantSource2 or Duration

#   *** 1: Drop Duration (for collinearity): Weedy ***
#     Basically all of them are annuals
nb.naz1.weed.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                               PlotMix_Climate + Lifeform + MAT + MAP + Sand_content + 
                               Since_last_precip_sqrt + (1 | Site / Plot),
                             data = naz.weed,
                             family = nbinom2)
summary(nb.naz1.weed.abs2)
r2(nb.naz1.weed.abs2)
res.nb.naz1.weed.abs2 <- simulateResiduals(nb.naz1.weed.abs2)
plotQQunif(res.nb.naz1.weed.abs2)
plotResiduals(res.nb.naz1.weed.abs2)
check_overdispersion(nb.naz1.weed.abs2) # overdispersion detected
check_zeroinflation(nb.naz1.weed.abs2) # model is overfitting zeros
check_collinearity(nb.naz1.weed.abs2)


# *** Seeded ***
nb.naz.seed.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment +  
                              PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                              Since_last_precip_sqrt + (1 | Site / Plot),
                            data = naz.seed,
                            family = nbinom2)
summary(nb.naz.seed.abs2)
r2(nb.naz.seed.abs2)
res.nb.naz.seed.abs2 <- simulateResiduals(nb.naz.seed.abs2)
plotQQunif(res.nb.naz.seed.abs2)
plotResiduals(res.nb.naz.seed.abs2)
check_overdispersion(nb.naz.seed.abs2) # overdispersion detected
check_collinearity(nb.naz.seed.abs2)


save.image("RData/08.1_generalized-linear-models-2.0_subplot-Count.RData")
