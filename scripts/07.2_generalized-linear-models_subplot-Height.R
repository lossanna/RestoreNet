# Created: 2024-07-30
# Last updated: 2024-08-29

# Purpose: Run generalized linear models for subplot data, with Height as response variable. 
#   Check for overdispersion and zero-inflation.

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

# Check for NAs
apply(subplot, 2, anyNA)



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

# Weedy
unique(subplot$Weedy)
subplot$Weedy <- as.factor(subplot$Weedy)
subplot$Weedy <- relevel(subplot$Weedy, ref = "Weedy")

# PlantSource2
unique(subplot$PlantSource2)
subplot$PlantSource2 <- as.factor(subplot$PlantSource2)
subplot$PlantSource2 <- relevel(subplot$PlantSource2, ref = "Introduced/Invasive")


## Separate Weedy and Desirable -------------------------------------------

# Desirable
subplot.des <- subplot |> 
  filter(Weedy != "Weedy")

# Weedy
subplot.weed <- subplot |> 
  filter(Weedy != "Desirable")

# With 8000% precip dev from Mojave removed
#   Desirable
subplot.des8rm <- subplot.des |> 
  filter(Perc_dev_cum < 8)

#   Weedy
subplot.weed8rm <- subplot.weed |> 
  filter(Perc_dev_cum < 8)



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

# All variables, no random effect
pos.all.0 <- glm(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                   PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                 data = subplot, family = "poisson")
summary(pos.all.0)
check_overdispersion(pos.all.0) # overdispersion detected
check_zeroinflation(pos.all.0) # no zero-inflation detected

# All variables, with random effect: creates NaNs
pos.all <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                     PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                     Cum_precip + (1 | Site / Plot),
                   data = subplot,
                   family = genpois)


## Negative binomial ------------------------------------------------------

# All variables, ref adjusted, nested random effect of Site/Plot
nb.all <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                    PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                    Cum_precip + (1 | Site / Plot),
                  data = subplot,
                  family = nbinom2)
summary(nb.all)
r2(nb.all)
res.nb.all <- simulateResiduals(nb.all)
plotResiduals(nb.all)
plotQQunif(nb.all)
check_overdispersion(nb.all) # no overdispersion detected
check_zeroinflation(nb.all) # model is overfitting zeros
check_collinearity(nb.all)

# 1: Drop MAP (collinearity)
nb.all1 <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                    PlotMix_Climate + Duration + Lifeform + MAT + Sand_content + 
                    Cum_precip + (1 | Site / Plot),
                  data = subplot,
                  family = nbinom2)
summary(nb.all1)
r2(nb.all1)
res.nb.all1 <- simulateResiduals(nb.all1)
plotResiduals(nb.all1)
plotQQunif(nb.all1)
check_overdispersion(nb.all1) # no overdispersion detected
check_zeroinflation(nb.all1) # model is overfitting zeros
check_collinearity(nb.all1)


# All data, subset by Weedy/Desirable -------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot: Desirable
nb.all.des <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                        PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                        Cum_precip + (1 | Site / Plot),
                      data = subplot.des,
                      family = nbinom2)
summary(nb.all.des)
r2(nb.all.des)
res.nb.all.des <- simulateResiduals(nb.all.des)
plotResiduals(nb.all.des)
plotQQunif(res.nb.all.des)
check_overdispersion(nb.all.des) # no overdispersion detected
check_zeroinflation(nb.all.des) # no zero-inflation
check_collinearity(nb.all.des)

# All variables, nested random effect of Site/Plot: Weedy
nb.all.weed <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                         PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                         Cum_precip + (1 | Site / Plot),
                       data = subplot.weed,
                       family = nbinom2)
summary(nb.all.weed)
r2(nb.all.weed)
res.nb.all.weed <- simulateResiduals(nb.all.weed)
plotResiduals(res.nb.all.weed)
plotQQunif(res.nb.all.weed)
check_overdispersion(nb.all.weed) # no overdispersion detected
check_collinearity(nb.all.weed)


# 1: Drop MAP (collinearity): Desirable
nb.all1.des <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                         PlotMix_Climate + Duration + Lifeform + MAT + Sand_content + 
                         Cum_precip + (1 | Site / Plot),
                       data = subplot.des,
                       family = nbinom2)
summary(nb.all1.des)
r2(nb.all1.des)
res.nb.all1.des <- simulateResiduals(nb.all1.des)
plotResiduals(res.nb.all1.des)
plotQQunif(res.nb.all1.des)
check_overdispersion(nb.all1.des) # no overdispersion detected
check_zeroinflation(nb.all1.des) # no zero-inflation

# 1: Drop MAP (collinearity): Weedy
nb.all1.weed <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                          PlotMix_Climate + Duration + Lifeform + MAT + Sand_content + 
                          Cum_precip + (1 | Site / Plot),
                        data = subplot.weed,
                        family = nbinom2)
summary(nb.all1.weed)
r2(nb.all1.weed)
res.nb.all1.weed <- simulateResiduals(nb.all1.weed)
plotResiduals(res.nb.all1.weed)
plotQQunif(res.nb.all1.weed)
check_overdispersion(nb.all1.weed) # underdispersion detected
check_zeroinflation(nb.all1.weed) # model is overfitting zeros



# All data (minus 8000% outlier), subset by Weedy/Desirable ---------------

# In general, very little looks different when outliers are included

## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot: Desirable
nb.all.des8rm <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                           PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                           Cum_precip + (1 | Site / Plot),
                         data = subplot.des8rm,
                         family = nbinom2)
summary(nb.all.des8rm)
r2(nb.all.des8rm)
res.nb.all.des8rm <- simulateResiduals(nb.all.des8rm)
plotResiduals(nb.all.des8rm)
plotQQunif(res.nb.all.des8rm)
check_overdispersion(nb.all.des8rm) # no overdispersion detected
check_zeroinflation(nb.all.des8rm) # no zero-inflation
check_collinearity(nb.all.des8rm)

# All variables, nested random effect of Site/Plot: Weedy
nb.all.weed8rm <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                            PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                            Cum_precip + (1 | Site / Plot),
                          data = subplot.weed8rm,
                          family = nbinom2)
summary(nb.all.weed8rm)
r2(nb.all.weed8rm)
res.nb.all.weed8rm <- simulateResiduals(nb.all.weed8rm)
plotResiduals(res.nb.all.weed8rm)
plotQQunif(res.nb.all.weed8rm)
check_overdispersion(nb.all.weed8rm) # no overdispersion detected
check_collinearity(nb.all.weed8rm)



# Sonoran sites -----------------------------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot
nb.sonoran <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                        PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                        Cum_precip + (1 | Site / Plot),
                      data = sonoran,
                      family = nbinom2) 
summary(nb.sonoran)
r2(nb.sonoran)
res.nb.sonoran <- simulateResiduals(nb.sonoran)
plotResiduals(res.nb.sonoran)
plotQQunif(res.nb.sonoran)
check_overdispersion(nb.sonoran) # overdispersion detected
check_zeroinflation(nb.sonoran) # model is overfitting zeros
check_collinearity(nb.sonoran) # should drop MAP

# 1: Drop MAP (for collinearity)
nb.sonoran1 <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                         PlotMix_Climate + Duration + Lifeform + MAT + Sand_content + 
                         Cum_precip + (1 | Site / Plot),
                       data = sonoran,
                       family = nbinom2) 
summary(nb.sonoran1)
r2(nb.sonoran1)
res.nb.sonoran1 <- simulateResiduals(nb.sonoran1)
plotResiduals(res.nb.sonoran1)
plotQQunif(res.nb.sonoran1)
check_overdispersion(nb.sonoran1) # no overdispersion detected
check_zeroinflation(nb.sonoran1) # model is overfitting zeros
check_collinearity(nb.sonoran1)



# Sonoran sites by Weedy/Desirable ----------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot: Desirable
nb.sonoran.des <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                            PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                            Cum_precip + (1 | Site / Plot),
                          data = sonoran.des,
                          family = nbinom2)
summary(nb.sonoran.des)
r2(nb.sonoran.des)
res.nb.sonoran.des <- simulateResiduals(nb.sonoran.des)
plotQQunif(res.nb.sonoran.des)
plotResiduals(res.nb.sonoran.des)
check_overdispersion(nb.sonoran.des) # no overdispersion detected
check_zeroinflation(nb.sonoran.des) # model is overfitting zeros
check_collinearity(nb.sonoran.des) # should drop MAP

# All variables, nested random effect of Site/Plot: Weedy
nb.sonoran.weed <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                             Cum_precip + (1 | Site / Plot),
                           data = sonoran.weed,
                           family = nbinom2)
summary(nb.sonoran.weed)
r2(nb.sonoran.weed) # something is wrong?
res.nb.sonoran.weed <- simulateResiduals(nb.sonoran.weed)
plotQQunif(res.nb.sonoran.weed)
plotResiduals(res.nb.sonoran.weed)
check_overdispersion(nb.sonoran.weed) # no overdispersion detected
check_zeroinflation(nb.sonoran.weed) # no zero-inflation detected
check_collinearity(nb.sonoran.weed) # should drop MAP or AridityIndex


# 1: Drop AridityIndex (for collinearity): Desirable
#     Dropping MAP instead creates model convergence problems
nb.sonoran1.des <- glmmTMB(Height ~ Perc_dev_cum + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAP + MAT + Sand_content + 
                             Cum_precip + (1 | Site / Plot),
                           data = sonoran.des,
                           family = nbinom2)
summary(nb.sonoran1.des)
r2(nb.sonoran1.des)
res.nb.sonoran1.des <- simulateResiduals(nb.sonoran1.des)
plotQQunif(res.nb.sonoran1.des)
plotResiduals(res.nb.sonoran1.des)
check_overdispersion(nb.sonoran1.des) # no overdispersion detected
check_zeroinflation(nb.sonoran1.des) # model is overfitting zeros (but it is minor)

# 1: Drop MAP (for collinearity), and Lifeform (for convergence): Weedy
nb.sonoran1.weed <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                              PlotMix_Climate +  Duration + MAT + Sand_content + 
                              Cum_precip + (1 | Site / Plot),
                            data = sonoran.weed,
                            family = nbinom2)
summary(nb.sonoran1.weed)
r2(nb.sonoran1.weed)
res.nb.sonoran1.weed <- simulateResiduals(nb.sonoran1.weed)
plotQQunif(res.nb.sonoran1.weed)
plotResiduals(res.nb.sonoran1.weed)
check_overdispersion(nb.sonoran1.weed) # no overdispersion detected
check_zeroinflation(nb.sonoran1.weed) # no zero-inflation detected




# Northern AZ sites -------------------------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot
nb.naz <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                    PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                    Cum_precip + (1 | Site / Plot),
                  data = naz,
                  family = nbinom2) 
summary(nb.naz)
r2(nb.naz)
res.nb.naz <- simulateResiduals(nb.naz)
plotQQunif(res.nb.naz)
plotResiduals(res.nb.naz)
check_overdispersion(nb.naz) # no overdispersion detected
check_zeroinflation(nb.naz) # model is overfitting zeros
check_collinearity(nb.naz)



# Northern AZ sites by Weedy/Desirable ------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot: Desirable
nb.naz.des <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                        PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                        Cum_precip + (1 | Site / Plot),
                      data = naz.des,
                      family = nbinom2)
summary(nb.naz.des)
r2(nb.naz.des)
res.nb.naz.des <- simulateResiduals(nb.naz.des)
plotQQunif(res.nb.naz.des)
plotResiduals(res.nb.naz.des)
check_overdispersion(nb.naz.des) # no overdispersion detected
check_zeroinflation(nb.naz.des) # no zero-inflation detected
check_collinearity(nb.naz.des)

# All variables, nested random effect of Site/Plot: Weedy
nb.naz.weed <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                         PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                         Cum_precip + (1 | Site / Plot),
                       data = naz.weed,
                       family = nbinom2)
summary(nb.naz.weed)
r2(nb.naz.weed)
res.nb.naz.weed <- simulateResiduals(nb.naz.weed)
plotQQunif(res.nb.naz.weed)
plotResiduals(res.nb.naz.weed)
check_overdispersion(nb.naz.weed) # no overdispersion detected
check_zeroinflation(nb.naz.weed) # no zero-inflation detected
check_collinearity(nb.naz.weed)

# 1: Drop PlantSource2 (for collinearity): Weedy
nb.naz1.weed <- glmmTMB(Height ~ Perc_dev_cum + AridityIndex + Treatment +  
                          PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                          Cum_precip + (1 | Site / Plot),
                        data = naz.weed,
                        family = nbinom2)
summary(nb.naz1.weed)
r2(nb.naz1.weed)
res.nb.naz1.weed <- simulateResiduals(nb.naz1.weed)
plotQQunif(res.nb.naz1.weed)
plotResiduals(res.nb.naz1.weed)
check_overdispersion(nb.naz1.weed) # no overdispersion detected
check_zeroinflation(nb.naz1.weed) # no zero-inflation detected


save.image("RData/07.2_generalized-linear-models_subplot-Height.RData")
