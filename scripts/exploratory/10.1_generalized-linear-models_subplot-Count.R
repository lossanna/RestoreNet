# Created: 024-09-09
# Last updated: 2024-10-01

# Purpose: Run generalized linear models for subplot data, with Count as response variable. 

# Use Perc_dev_cum_abs to measure precip variability, and Since_last_precip_sqrt to indicate
#   recent wet or dry conditions.

# Negative binomial used to help with overdispersion.
# Including (1 | Site / Plot) accounts for the non-independence of repeat sampling.

# Transformations do not really improve model fit or residuals plots.

# Run for desirable, weedy, and seeded species (with no control plots), for better
#   comparison with GLMs with Seeded_Cover as response variable.

# *** indicates model is included in PPT of draft figures and model results.

# Most of the models are the same as those run in 08.1.R script (including same name); this script
#   is just to condense results. (Technically nb.naz.des.abs2 wasn't identical to the one
#   saved in .RData for 08.1.R, and I'm not sure why because everything else is identical,
#   but the changes are so minute it doesn't affect interpretation or significance.)
# Decided to drop Sand_content from Sonoran Desert desirable & weedy models because only one
#   site had low sand content - seeded model already had Sand_content dropped. This improved
#   model fit of weedy model slightly. (Northern AZ models are fine because they are more balanced.)
# Also tried to model just the observations from "extremes" (+24% or wetter, and -23% or drier),
#   but that didn't really work.
# Removed sq-root and log transformation from later models because GLMs can handle non-normal
#   explanatory variables, and this makes interpretation easier.
# Added "Days_elapsed", which is the number of days between seeding and monitoring.

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
#   AI: higher negative AI values = drier sites


## Add Perc_dev_cum_abs col -----------------------------------------------

subplot <- subplot |> 
  mutate(Perc_dev_cum_abs = abs(Perc_dev_cum))


## Add Days_elapsed col ----------------------------------------------------

subplot <- subplot |> 
  mutate(Days_elapsed = difftime(Date_Monitored, Date_Seeded)) |> 
  mutate(Days_elapsed = as.numeric(Days_elapsed))


# Scale numeric variables -------------------------------------------------

numeric.vars <- c("Perc_dev_cum_abs", "AridityIndex", "MAT", "MAP", "Since_last_precip", "Days_elapsed")
subplot <- subplot |> 
  mutate(across(all_of(numeric.vars), ~ scale(.)[, 1], .names = "{.col}_scaled"))
  

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

# Desirable, extremes
sonoran.des.extreme.wet <- sonoran.des |> 
  filter(Perc_dev_cum > 0.24)
sonoran.des.extreme.dry <- sonoran.des |> 
  filter(Perc_dev_cum < -0.23)
sonoran.des.extreme.codes <- intersect(sonoran.des.extreme.wet$Code, sonoran.des.extreme.dry$Code)
sonoran.des.extreme <- sonoran.des |> 
  filter(Code %in% sonoran.des.extreme.codes) |> 
  filter(Perc_dev_cum > 0.24 | Perc_dev_cum < -0.23) |> 
  filter(Code != "0") 

#   Normalize by sum of all desirable individuals per plot
sonoran.des.extreme2 <- sonoran.des.extreme|> 
  group_by(Site, Date_Seeded, Date_Monitored, Plot, Duration, Lifeform, AridityIndex_log,
           Perc_dev_cum_abs, Since_last_precip_sqrt) |> 
  summarise(SumCount = sum(Count), .groups = "keep")


# Weedy, extremes
sonoran.weed.extreme.wet <- sonoran.weed |> 
  filter(Perc_dev_cum > 0.24)
sonoran.weed.extreme.dry <- sonoran.weed |> 
  filter(Perc_dev_cum < -0.23)
sonoran.weed.extreme.codes <- intersect(sonoran.weed.extreme.wet$Code, sonoran.weed.extreme.dry$Code)
sonoran.weed.extreme <- sonoran.weed |> 
  filter(Code %in% sonoran.weed.extreme.codes) |> 
  filter(Perc_dev_cum > 0.24 | Perc_dev_cum < -0.23) |> 
  filter(Code != "0")


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

# 1: Drop MAP (for collinearity): Desirable
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

# 2: Drop Sand_content (only 1 site had low sand content): Desirable
nb.sonoran2.des.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                                  PlotMix_Climate + Duration + Lifeform + MAT + 
                                  Since_last_precip_sqrt + (1 | Site / Plot),
                                data = sonoran.des,
                                family = nbinom2)
summary(nb.sonoran2.des.abs2)
r2(nb.sonoran2.des.abs2)
res.nb.sonoran2.des.abs2 <- simulateResiduals(nb.sonoran2.des.abs2)
plotQQunif(res.nb.sonoran2.des.abs2)
plotResiduals(res.nb.sonoran2.des.abs2)
check_overdispersion(nb.sonoran2.des.abs2) # overdispersion detected
check_zeroinflation(nb.sonoran2.des.abs2) # model is overfitting zeros
check_collinearity(nb.sonoran2.des.abs2)

# 3: *** Add Days_elapsed, no transformations: Desirable ***
nb.sonoran3.des.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex + Treatment + PlantSource2 + 
                                  PlotMix_Climate + Duration + Lifeform + MAT + 
                                  Since_last_precip + Days_elapsed + (1 | Site / Plot),
                                data = sonoran.des,
                                family = nbinom2)
summary(nb.sonoran3.des.abs2)
r2(nb.sonoran3.des.abs2)
res.nb.sonoran3.des.abs2 <- simulateResiduals(nb.sonoran3.des.abs2)
plotQQunif(res.nb.sonoran3.des.abs2)
plotResiduals(res.nb.sonoran3.des.abs2)
check_overdispersion(nb.sonoran3.des.abs2) # overdispersion detected
check_zeroinflation(nb.sonoran3.des.abs2) # model is overfitting zeros
check_collinearity(nb.sonoran3.des.abs2)

# 4: Center and scale numeric variables
nb.sonoran4.des.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs_scaled + AridityIndex_scaled + 
                                  Treatment + PlantSource2 + 
                                  PlotMix_Climate + Duration + Lifeform + MAT_scaled + 
                                  Since_last_precip_scaled + Days_elapsed_scaled + (1 | Site / Plot),
                                data = sonoran.des,
                                family = nbinom2)
summary(nb.sonoran4.des.abs2)
r2(nb.sonoran4.des.abs2)
res.nb.sonoran4.des.abs2 <- simulateResiduals(nb.sonoran4.des.abs2)
plotQQunif(res.nb.sonoran4.des.abs2)
plotResiduals(res.nb.sonoran4.des.abs2)
check_overdispersion(nb.sonoran4.des.abs2) # overdispersion detected
check_zeroinflation(nb.sonoran4.des.abs2) # model is overfitting zeros
check_collinearity(nb.sonoran4.des.abs2)



## Weedy ------------------------------------------------------------------

# All variables, nested random effect of Site/Plot: Weedy: does not converge
nb.sonoran.weed.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                                  PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                                  Since_last_precip_sqrt + (1 | Site / Plot),
                                data = sonoran.weed,
                                family = nbinom2) # did not converge

# 1: Drop MAP & Duration (for collinearity): Weedy
#   Most weeds are annuals
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

# 2: Drop Sand_content (only 1 site had low sand content): Weedy
nb.sonoran2.weed.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + PlantSource2 + 
                                   PlotMix_Climate + Lifeform + MAT +  
                                   Since_last_precip_sqrt + (1 | Site / Plot),
                                 data = sonoran.weed,
                                 family = nbinom2)
summary(nb.sonoran2.weed.abs2)
r2(nb.sonoran2.weed.abs2)
res.nb.sonoran2.weed.abs2 <- simulateResiduals(nb.sonoran2.weed.abs2)
plotQQunif(res.nb.sonoran2.weed.abs2)
plotResiduals(res.nb.sonoran2.weed.abs2)
check_overdispersion(nb.sonoran2.weed.abs2) # no overdispersion detected
check_zeroinflation(nb.sonoran2.weed.abs2) # model is overfitting zeros
check_collinearity(nb.sonoran2.weed.abs2)

# 3: *** Add Days_elapsed, no transformations: Weedy ***
nb.sonoran3.weed.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex + Treatment + PlantSource2 + 
                                  PlotMix_Climate + Duration + Lifeform + MAT + 
                                  Since_last_precip + Days_elapsed + (1 | Site / Plot),
                                data = sonoran.weed,
                                family = nbinom2)
summary(nb.sonoran3.weed.abs2)
r2(nb.sonoran3.weed.abs2)
res.nb.sonoran3.weed.abs2 <- simulateResiduals(nb.sonoran3.weed.abs2)
plotQQunif(res.nb.sonoran3.weed.abs2)
plotResiduals(res.nb.sonoran3.weed.abs2)
check_overdispersion(nb.sonoran3.weed.abs2) # no overdispersion detected
check_zeroinflation(nb.sonoran3.weed.abs2) # model is overfitting zeros
check_collinearity(nb.sonoran3.weed.abs2)


## Seeded -----------------------------------------------------------------

# All variables, nested random effect of Site/Plot: Seeded
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


# 2: Drop MAP (collinearity), AridityIndex_log, Sand_content (singularity): Seeded 
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

# 3: *** Add Days_elapsed, no transformations: Seeded ***
nb.sonoran3.seed.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + Treatment +  
                                   PlotMix_Climate + Duration + Lifeform + MAT +  
                                   Since_last_precip + Days_elapsed + (1 | Site / Plot),
                                 data = sonoran.seed,
                                 family = nbinom2)
summary(nb.sonoran3.seed.abs2)
r2(nb.sonoran3.seed.abs2)
res.nb.sonoran3.seed.abs2 <- simulateResiduals(nb.sonoran3.seed.abs2)
plotQQunif(res.nb.sonoran3.seed.abs2)
plotResiduals(res.nb.sonoran3.seed.abs2)
check_overdispersion(nb.sonoran3.seed.abs2) # overdispersion detected
check_collinearity(nb.sonoran3.seed.abs2)




# Northern Arizona Plateau ------------------------------------------------

## Desirable --------------------------------------------------------------

# All variables, nested random effect of Site/Plot: Desirable 
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


# *** 1. Add Days_elapsed, no transformations: Desirable ***
nb.naz1.des.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content +  
                             Since_last_precip + Days_elapsed + (1 | Site / Plot),
                           data = naz.des,
                           family = nbinom2)
summary(nb.naz1.des.abs2)
r2(nb.naz1.des.abs2)
res.nb.naz1.des.abs2 <- simulateResiduals(nb.naz1.des.abs2)
plotQQunif(res.nb.naz1.des.abs2)
plotResiduals(res.nb.naz1.des.abs2)
check_overdispersion(nb.naz1.des.abs2) # overdispersion detected
check_zeroinflation(nb.naz1.des.abs2) # model is overfitting zeros
check_collinearity(nb.naz1.des.abs2)


## Weedy ------------------------------------------------------------------

# All variables, nested random effect of Site/Plot: Weedy
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

# 1: Drop Duration (for collinearity): Weedy 
#   Basically all of them are annuals
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

# *** 2: Add Days_elapsed, no transformations: Weedy ***
nb.naz2.weed.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex + Treatment + PlantSource2 + 
                               PlotMix_Climate + Lifeform + MAT + MAP + Sand_content + 
                               Since_last_precip + Days_elapsed + (1 | Site / Plot),
                             data = naz.weed,
                             family = nbinom2)
summary(nb.naz2.weed.abs2)
r2(nb.naz2.weed.abs2)
res.nb.naz2.weed.abs2 <- simulateResiduals(nb.naz2.weed.abs2)
plotQQunif(res.nb.naz2.weed.abs2)
plotResiduals(res.nb.naz2.weed.abs2)
check_overdispersion(nb.naz2.weed.abs2) # overdispersion detected
check_zeroinflation(nb.naz2.weed.abs2) # model is overfitting zeros
check_collinearity(nb.naz2.weed.abs2)


## Seeded -----------------------------------------------------------------

# All variables, nested random effect of Site/Plot: Seeded 
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

# *** 1. Add Days_elapsed, no transformations: Seeded ***
nb.naz1.seed.abs2 <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex + Treatment +  
                              PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                              Since_last_precip + Days_elapsed + (1 | Site / Plot),
                            data = naz.seed,
                            family = nbinom2)
summary(nb.naz1.seed.abs2)
r2(nb.naz1.seed.abs2)
res.nb.naz1.seed.abs2 <- simulateResiduals(nb.naz1.seed.abs2)
plotQQunif(res.nb.naz1.seed.abs2)
plotResiduals(res.nb.naz1.seed.abs2)
check_overdispersion(nb.naz1.seed.abs2) # overdispersion detected
check_collinearity(nb.naz1.seed.abs2)




# Sonoran Desert, precip extremes -----------------------------------------

## Desirable --------------------------------------------------------------

nb.sonoran.des.ex <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log +
                               PlotMix_Climate + Duration + Lifeform + 
                                Since_last_precip_sqrt + (1 | Site / Plot),
                              data = sonoran.des.extreme,
                              family = nbinom2)
summary(nb.sonoran.des.ex)
r2(nb.sonoran.des.ex) # can't compute
res.nb.sonoran.des.ex <- simulateResiduals(nb.sonoran.des.ex)
plotQQunif(res.nb.sonoran.des.ex)
plotResiduals(res.nb.sonoran.des.ex)
check_overdispersion(nb.sonoran.des.ex) # no overdispersion detected
check_collinearity(nb.sonoran.des.ex)


nb.sonoran.des.ex2 <- glmmTMB(SumCount ~ Perc_dev_cum_abs + AridityIndex_log +
                                Duration + Lifeform + 
                                 Since_last_precip_sqrt + (1 | Site / Plot),
                               data = sonoran.des.extreme2,
                               family = nbinom2)
summary(nb.sonoran.des.ex2)
r2(nb.sonoran.des.ex2) # can't compute
res.nb.sonoran.des.ex2 <- simulateResiduals(nb.sonoran.des.ex2)
plotQQunif(res.nb.sonoran.des.ex2)
plotResiduals(res.nb.sonoran.des.ex2)
check_overdispersion(nb.sonoran.des.ex2) # no overdispersion detected
check_collinearity(nb.sonoran.des.ex2)


## Weedy ------------------------------------------------------------------

nb.sonoran.weed.ex <- glmmTMB(Count ~ Perc_dev_cum_abs + AridityIndex_log +  
                               Duration + Lifeform + 
                               Since_last_precip_sqrt + (1 | Site / Plot),
                             data = sonoran.weed.extreme,
                             family = nbinom2)
summary(nb.sonoran.weed.ex)
r2(nb.sonoran.weed.ex) # can't compute
res.nb.sonoran.weed.ex <- simulateResiduals(nb.sonoran.weed.ex)
plotQQunif(res.nb.sonoran.weed.ex)
plotResiduals(res.nb.sonoran.weed.ex)
check_overdispersion(nb.sonoran.weed.ex) # no overdispersion detected
check_collinearity(nb.sonoran.weed.ex)

save.image("RData/10.1_generalized-linear-models_subplot-Count.RData")
