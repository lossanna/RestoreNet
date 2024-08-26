# Created: 2024-05-26
# Last updated: 2024-08-26

# Purpose: Run generalized linear models for subplot data, with Count as response variable. 
#   Check for overdispersion and zero-inflation.

# Presence of overdispersion and for Count indicates a negative binomial should be used.
# Including (1 | Site / Plot) accounts for the non-independence of repeat sampling.

# "All variables" includes: Perc_dev_cum, AridityIndex, Treatment, PlantSource2,
#     PlotMix_Climate, Duration, Lifeform, MAT, MAP, Sand_content, Cum_precip
# However, AridityIndex and MAP are usually too correlated to include both, so MAP often dropped.


library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)

# Load data ---------------------------------------------------------------

subplot.raw <- read_csv("data/cleaned/04.1_subplot-data_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")
siteplot.id <- read_csv("data/cleaned/02_SitePlotID_clean.csv")


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

# Check for NAs
apply(subplot, 2, anyNA)


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

# Weedy
unique(subplot$Weedy)
subplot$Weedy <- as.factor(subplot$Weedy)
subplot$Weedy <- relevel(subplot$Weedy, ref = "Weedy")

# PlantSource2
unique(subplot$PlantSource2)
subplot$PlantSource2 <- as.factor(subplot$PlantSource2)
subplot$PlantSource2 <- relevel(subplot$PlantSource2, ref = "Introduced/Invasive")



## Remove Pellets ---------------------------------------------------------

# Pellets were only used at Mojave
subplot |> 
  filter(Treatment == "Pellets") |> 
  select(Region, Site) |> 
  distinct(.keep_all = TRUE)

# All
nopellet <- subplot |> 
  filter(Treatment != "Pellets")

# Desirable
nopellet.des <- nopellet |> 
  filter(Weedy != "Weedy")
unique(nopellet.des$PlantSource2)
nopellet.des$PlantSource2 <- relevel(nopellet.des$PlantSource2, ref = "Native recruit")

# Weedy
nopellet.weed <- nopellet |> 
  filter(Weedy != "Desirable")


## Separate out Pits, Seed and Control only -------------------------------

# All
pitseed <- subplot |> 
  filter(Treatment %in% c("Control", "Seed", "Pits"))

# Desirable
pitseed.des <- pitseed |> 
  filter(Weedy != "Weedy")
pitseed.des$PlantSource2 <- relevel(pitseed.des$PlantSource2, ref = "Native recruit")

# Weedy
pitseed.weed <- pitseed |> 
  filter(Weedy != "Desirable")


## Separate out Sonoran sites (6) -----------------------------------------

# All
sonoran <- subplot |> 
  filter(Region %in% c("Sonoran SE", "Sonoran Central"))

# Desirable
sonoran.des <- sonoran |> 
  filter(Weedy != "Weedy")
sonoran.des$PlantSource2 <- relevel(sonoran.des$PlantSource2, ref = "Native recruit")

# Weedy
sonoran.weed <- sonoran |> 
  filter(Weedy != "Desirable")


## Separate out Northern AZ sites (8) -------------------------------------

# All
naz <- subplot |> 
  filter(Region == "Colorado Plateau")

# Desirable
naz.des <- naz |> 
  filter(Weedy != "Weedy")
naz.des$PlantSource2 <- relevel(naz.des$PlantSource2, ref = "Native recruit")

# Weedy
naz.weed <- naz |> 
  filter(Weedy != "Desirable")


## Separate out Utah sites (3) --------------------------------------------

# All
utah <- subplot |> 
  filter(Region == "Utah")

# Desirable
utah.des <- utah |> 
  filter(Weedy != "Weedy")
utah.des$PlantSource2 <- relevel(utah.des$PlantSource2, ref = "Native recruit")

# Weedy
utah.weed <- utah |> 
  filter(Weedy != "Desirable")



# All data ----------------------------------------------------------------

## Poisson ----------------------------------------------------------------

# All variables, no random effects
pos.all.0 <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                   PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                 data = subplot, family = "poisson")
summary(pos.all.0)
check_overdispersion(pos.all.0) # overdispersion detected
check_zeroinflation(pos.all.0) # model is overfitting zeros

# All variables, with random effects: does not converge
pos.all <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                     PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                     Cum_precip + (1 | Site / Plot),
                   data = subplot,
                   family = genpois) # did not converge


## Negative binomial ------------------------------------------------------

# All variables, no random effects
#   Perc_cum_dev significant
nb.all.0 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                      PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                      Cum_precip,
                    data = subplot,
                    family = nbinom2)
summary(nb.all.0)
check_overdispersion(nb.all.0) # overdispersion detected
check_zeroinflation(nb.all.0) # model is overfitting zeros


# All variables, nested random effect of Site/Plot
#   Perc_cum_dev not significant
nb.all <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                    PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                    Cum_precip + (1 | Site / Plot),
                  data = subplot,
                  family = nbinom2)
summary(nb.all)
r2(nb.all)
res.nb.all <- simulateResiduals(nb.all)
plotResiduals(nb.all)
plotQQunif(nb.all)
check_overdispersion(nb.all) # overdispersion detected
check_zeroinflation(nb.all) # model is overfitting zeros
check_collinearity(nb.all)




# nopellet dataset --------------------------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, no random effect
nb.nopellet0 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                          PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                          Cum_precip,
                        data = nopellet,
                        family = nbinom2)
summary(nb.nopellet0)

# All variables, nested Site/Plot as random effect
nb.nopellet <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                         PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                         Cum_precip + (1 | Site / Plot),
                       data = nopellet,
                       family = nbinom2)
summary(nb.nopellet)
r2(nb.nopellet)
res.nb.nopellet <- simulateResiduals(nb.nopellet)
plotResiduals(res.nb.nopellet)
plotQQunif(res.nb.nopellet)
check_overdispersion(nb.nopellet) # overdispersion detected
check_zeroinflation(nb.nopellet) # model is overfitting zeros
check_collinearity(nb.nopellet)
testOutliers(res.nb.nopellet)
outliers(res.nb.nopellet)



# pitseed dataset (Control, Seed, Pits) -----------------------------------

# Control, Seed, and Pits plots included (Pellets, Mulch & ConMod excluded)

## Poisson ----------------------------------------------------------------

# All variables, ref adjusted, no random effects: does not converge
pos.pitseed.0 <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                       PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                     data = pitseed, family = "poisson")
summary(pos.pitseed.0)
r2(pos.pitseed.0)
res.pos.pitseed.0 <- simulateResiduals(pos.pitseed.0)
check_overdispersion(pos.pitseed.0) # overdispersion detected
check_zeroinflation(pos.pitseed.0) # model is overfitting zeros
plotResiduals(pos.pitseed.0)


## Negative binomial ------------------------------------------------------

# All variables, no random effects
nb.pitseed.0 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                        PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                        Cum_precip,
                      data = pitseed,
                      family = nbinom2)
summary(nb.pitseed.0)
res.nb.pitseed.0 <- simulateResiduals(nb.pitseed.0)
check_model(nb.pitseed.0)
check_zeroinflation(nb.pitseed.0) # model is overfitting zeros
testZeroInflation(res.nb.pitseed.0)
check_overdispersion(nb.pitseed.0) # overdispersion detected
plotResiduals(res.nb.pitseed.0)
plotQQunif(res.nb.pitseed.0)
outliers(res.nb.pitseed.0)


# All variables, nested random effect of Site/Plot
nb.pitseed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                        PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                        Cum_precip + (1 | Site / Plot),
                      data = pitseed,
                      family = nbinom2)
summary(nb.pitseed)
r2(nb.pitseed)
res.nb.pitseed <- simulateResiduals(nb.pitseed)
plotResiduals(nb.pitseed)
plotQQunif(nb.pitseed)
check_model(nb.pitseed)
check_overdispersion(nb.pitseed) # overdispersion detected
testDispersion(res.nb.pitseed, alternative = "greater") # indicates overdispersion still
check_zeroinflation(nb.pitseed) # model is overfitting zeros
check_collinearity(nb.pitseed) # MAP should be dropped
step(nb.pitseed)


# 1: Drop MAP (collinearity)
nb.pitseed1 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                        PlotMix_Climate + Duration + Lifeform + MAT + Sand_content + 
                        Cum_precip + (1 | Site / Plot),
                      data = pitseed,
                      family = nbinom2)
summary(nb.pitseed1)
r2(nb.pitseed1)
res.nb.pitseed1 <- simulateResiduals(nb.pitseed1)
check_model(nb.pitseed1)
plotResiduals(nb.pitseed1)
plotQQunif(nb.pitseed1)




# pitseed subset by Weedy/Desirable ---------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, Site/Plot random effect: Desirable
nb.pitseed.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                          PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                          Cum_precip + (1 | Site / Plot),
                        data = pitseed.des,
                        family = nbinom2)
summary(nb.pitseed.des)
r2(nb.pitseed.des)
res.nb.pitseed.des <- simulateResiduals(nb.pitseed.des)
plotResiduals(res.nb.pitseed.des)
plotQQunif(res.nb.pitseed.des)
check_overdispersion(nb.pitseed.des) # overdispersion detected
check_zeroinflation(nb.pitseed.des) # model is overfitting zeros
check_collinearity(nb.pitseed.des)
testOutliers(res.nb.pitseed.des)

# All variables, no random effects: Weedy
nb.pitseed.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                              PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                              Cum_precip + (1 | Site / Plot),
                            data = pitseed.weed,
                            family = nbinom2)
summary(nb.pitseed.weed)
r2(nb.pitseed.weed)
res.nb.pitseed.weed <- simulateResiduals(nb.pitseed.weed)
plotResiduals(res.nb.pitseed.weed)
plotQQunif(res.nb.pitseed.weed)
check_overdispersion(nb.pitseed.weed) # overdispersion detected
check_zeroinflation(nb.pitseed.weed) # model is overfitting zeros
check_collinearity(nb.pitseed.weed)


# 1: Drop MAP (collinearity): Desirable
nb.pitseed1.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                            PlotMix_Climate + Duration + Lifeform + MAT + Sand_content + 
                            Cum_precip + (1 | Site / Plot),
                          data = pitseed.des,
                          family = nbinom2)
summary(nb.pitseed1.des)
r2(nb.pitseed1.des)
res.nb.pitseed1.des <- simulateResiduals(nb.pitseed1.des)
plotResiduals(res.nb.pitseed1.des)
plotQQunif(res.nb.pitseed1.des)
check_overdispersion(nb.pitseed1.des) # overdispersion detected
check_zeroinflation(nb.pitseed1.des) # model is overfitting zeros
check_collinearity(nb.pitseed1.des)

# 1: Drop MAP & Duration (collinearity): Weedy
nb.pitseed1.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                             PlotMix_Climate + Lifeform + MAT + Sand_content + 
                             Cum_precip + (1 | Site / Plot),
                           data = pitseed.weed,
                           family = nbinom2)
summary(nb.pitseed1.weed)
r2(nb.pitseed1.weed)
res.nb.pitseed1.weed <- simulateResiduals(nb.pitseed1.weed)
plotResiduals(res.nb.pitseed1.weed)
plotQQunif(res.nb.pitseed1.weed)
check_overdispersion(nb.pitseed1.weed) # overdispersion detected
check_zeroinflation(nb.pitseed1.weed) # model is overfitting zeros
check_collinearity(nb.pitseed1.weed)




# nopellet subset by Weedy/Desirable --------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot: Desirable
nb.nopellet.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                             Cum_precip + (1 | Site / Plot),
                           data = nopellet.des,
                           family = nbinom2)
summary(nb.nopellet.des)
r2(nb.nopellet.des)
res.nb.nopellet.des <- simulateResiduals(nb.nopellet.des)
plotResiduals(nb.nopellet.des)
plotQQunif(res.nb.nopellet.des)
check_overdispersion(nb.nopellet.des) # overdispersion detected
check_collinearity(nb.nopellet.des)

# All variables, nested random effect of Site/Plot: Weedy
nb.nopellet.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                             Cum_precip + (1 | Site / Plot),
                           data = nopellet.weed,
                           family = nbinom2)
summary(nb.nopellet.weed)
r2(nb.nopellet.weed)
res.nb.nopellet.weed <- simulateResiduals(nb.nopellet.weed)
plotResiduals(res.nb.nopellet.weed)
plotQQunif(res.nb.nopellet.weed)
check_overdispersion(nb.nopellet.weed) # overdispersion detected
check_collinearity(nb.nopellet.weed)


# 1: Drop MAP (collinearity): Desirable
nb.nopellet1.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                              PlotMix_Climate + Duration + Lifeform + MAT + Sand_content + 
                              Cum_precip + (1 | Site / Plot),
                            data = nopellet.des,
                            family = nbinom2)
summary(nb.nopellet1.des)
r2(nb.nopellet1.des)
res.nb.nopellet1.des <- simulateResiduals(nb.nopellet1.des)
plotResiduals(res.nb.nopellet1.des)
plotQQunif(res.nb.nopellet1.des)
check_overdispersion(nb.nopellet1.des) # overdispersion detected
check_zeroinflation(nb.nopellet1.des) # model is overfitting zeros

# 1: Drop MAP & Duration (collinearity): Weedy
nb.nopellet1.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                               PlotMix_Climate + Lifeform + MAT + Sand_content + 
                               Cum_precip + (1 | Site / Plot),
                             data = nopellet.weed,
                             family = nbinom2)
summary(nb.nopellet1.weed)
r2(nb.nopellet1.weed)
res.nb.nopellet1.weed <- simulateResiduals(nb.nopellet1.weed)
plotResiduals(res.nb.nopellet1.weed)
plotQQunif(res.nb.nopellet1.weed)
check_overdispersion(nb.nopellet1.weed) # overdispersion detected
check_zeroinflation(nb.nopellet1.weed) # model is overfitting zeros




# Sonoran sites -----------------------------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot
nb.sonoran <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                            PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                            Cum_precip + (1 | Site / Plot),
                          data = sonoran,
                          family = nbinom2) 
summary(nb.sonoran)
r2(nb.sonoran) # something is wrong?
res.nb.sonoran <- simulateResiduals(nb.sonoran)
plotResiduals(res.nb.sonoran)
plotQQunif(res.nb.sonoran)
check_overdispersion(nb.sonoran) # overdispersion detected
check_zeroinflation(nb.sonoran) # model is overfitting zeros
check_collinearity(nb.sonoran)

# 1: Drop MAP (for collinearity)
nb.sonoran1 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                        PlotMix_Climate + Duration + Lifeform + MAT + Sand_content + 
                        Cum_precip + (1 | Site / Plot),
                      data = sonoran,
                      family = nbinom2) 
summary(nb.sonoran1)
r2(nb.sonoran1) # something is wrong?
res.nb.sonoran1 <- simulateResiduals(nb.sonoran1)
plotResiduals(res.nb.sonoran1)
plotQQunif(res.nb.sonoran1)
check_overdispersion(nb.sonoran1) # overdispersion detected
check_collinearity(nb.sonoran1)



# Sonoran sites by Weedy/Desirable ----------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot: Desirable
nb.sonoran.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                             Cum_precip + (1 | Site / Plot),
                           data = sonoran.des,
                           family = nbinom2)
summary(nb.sonoran.des)
r2(nb.sonoran.des)
res.nb.sonoran.des <- simulateResiduals(nb.sonoran.des)
plotQQunif(res.nb.sonoran.des)
plotResiduals(res.nb.sonoran.des)
check_model(nb.sonoran.des)
check_overdispersion(nb.sonoran.des) # overdispersion detected
check_zeroinflation(nb.sonoran.des) # model is overfitting zeros
check_collinearity(nb.sonoran.des) # should drop MAP

# All variables, nested random effect of Site/Plot: Weedy
nb.sonoran.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                            PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                            Cum_precip + (1 | Site / Plot),
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


# 1: Drop MAP (for collinearity): Desirable
nb.sonoran1.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                            PlotMix_Climate + Duration + Lifeform + MAT + Sand_content + 
                            Cum_precip + (1 | Site / Plot),
                          data = sonoran.des,
                          family = nbinom2)
summary(nb.sonoran1.des)
r2(nb.sonoran1.des)
res.nb.sonoran1.des <- simulateResiduals(nb.sonoran1.des)
plotQQunif(res.nb.sonoran1.des)
plotResiduals(res.nb.sonoran1.des)
check_model(nb.sonoran1.des)
check_overdispersion(nb.sonoran1.des) # overdispersion detected
check_zeroinflation(nb.sonoran1.des) # model is overfitting zeros

# 1: Drop MAP & Duration (for collinearity): Weedy
nb.sonoran1.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                              PlotMix_Climate + Lifeform + MAT + Sand_content + 
                              Cum_precip + (1 | Site / Plot),
                            data = sonoran.weed,
                            family = nbinom2)
summary(nb.sonoran1.weed)
r2(nb.sonoran1.weed)
res.nb.sonoran1.weed <- simulateResiduals(nb.sonoran1.weed)
plotQQunif(res.nb.sonoran1.weed)
plotResiduals(res.nb.sonoran1.weed)
check_model(nb.sonoran1.weed)
check_overdispersion(nb.sonoran1.weed) # overdispersion detected
check_zeroinflation(nb.sonoran1.weed) # model is overfitting zeros


# 2: Drop MAP (for collinearity) & Sand_content (for singularity): Desirable
nb.sonoran2.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT +  
                             Cum_precip + (1 | Site / Plot),
                           data = sonoran.des,
                           family = nbinom2)
summary(nb.sonoran2.des)
r2(nb.sonoran2.des)
res.nb.sonoran2.des <- simulateResiduals(nb.sonoran2.des)
plotQQunif(res.nb.sonoran2.des)
plotResiduals(res.nb.sonoran2.des)
check_model(nb.sonoran2.des)
check_overdispersion(nb.sonoran2.des) # overdispersion detected
check_zeroinflation(nb.sonoran2.des) # model is overfitting zeros




# Northern AZ sites -------------------------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot
nb.naz <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                        PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                        Cum_precip + (1 | Site / Plot),
                      data = naz,
                      family = nbinom2) 
summary(nb.naz)
r2(nb.naz)
res.nb.naz <- simulateResiduals(nb.naz)
plotQQunif(res.nb.naz)
plotResiduals(res.nb.naz)
check_model(nb.naz)
check_overdispersion(nb.naz) # overdispersion detected
check_zeroinflation(nb.naz) # model is overfitting zeros
check_collinearity(nb.naz)



# Northern AZ sites by Weedy/Desirable ------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot: Desirable
nb.naz.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                        PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                        Cum_precip + (1 | Site / Plot),
                      data = naz.des,
                      family = nbinom2)
summary(nb.naz.des)
r2(nb.naz.des)
res.nb.naz.des <- simulateResiduals(nb.naz.des)
plotQQunif(res.nb.naz.des)
plotResiduals(res.nb.naz.des)
check_model(nb.naz.des)
check_overdispersion(nb.naz.des) # overdispersion detected
check_zeroinflation(nb.naz.des) # model is overfitting zeros
check_collinearity(nb.naz.des)

# All variables, nested random effect of Site/Plot: Weedy
nb.naz.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                         PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                         Cum_precip + (1 | Site / Plot),
                       data = naz.weed,
                       family = nbinom2)
summary(nb.naz.weed)
r2(nb.naz.weed)
res.nb.naz.weed <- simulateResiduals(nb.naz.weed)
plotQQunif(res.nb.naz.weed)
plotResiduals(res.nb.naz.weed)
check_model(nb.naz.weed)
check_overdispersion(nb.naz.weed) # overdispersion detected
check_zeroinflation(nb.naz.weed) # model is overfitting zeros
check_collinearity(nb.naz.weed)

# 1: Drop PlantSource2 (for collinearity): Weedy
nb.naz1.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment +  
                         PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                         Cum_precip + (1 | Site / Plot),
                       data = naz.weed,
                       family = nbinom2)
summary(nb.naz1.weed)
r2(nb.naz1.weed)
res.nb.naz1.weed <- simulateResiduals(nb.naz1.weed)
plotQQunif(res.nb.naz1.weed)
plotResiduals(res.nb.naz1.weed)
check_model(nb.naz1.weed)
check_overdispersion(nb.naz1.weed) # overdispersion detected
check_zeroinflation(nb.naz1.weed) # model is overfitting zeros




# Utah sites --------------------------------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot: produces NaNs
nb.utah <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                    PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                    Cum_precip + (1 | Site / Plot),
                  data = utah,
                  family = nbinom2) # produces NaNs
summary(nb.utah)


# Utah sites by Weedy/Desirable -------------------------------------------


## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot: Desirable
nb.utah.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                        PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                        Cum_precip + (1 | Site / Plot),
                      data = utah.des,
                      family = nbinom2) # produces NaNs
summary(nb.utah.des)

# All variables, nested random effect of Site/Plot: Weedy
nb.utah.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                         PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                         Cum_precip + (1 | Site / Plot),
                       data = utah.weed,
                       family = nbinom2) # does not converge



save.image("RData/07.1_generalized-linear-models_subplot-Count.RData")
