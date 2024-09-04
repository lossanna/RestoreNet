# This analysis is deprecated.

# Created: 2024-08-22
# Last updated: 2024-08-22

# Purpose: Run generalized linear models for subplot data (with 0s added for seeded species), 
#   with Count as response variable. Check for overdispersion and zero-inflation.

# Presence of overdispersion and for Count indicates a negative binomial should be used.
# Including (1 | Site / Plot) accounts for the non-independence of repeat sampling.

# "All variables" includes: Perc_dev_cum, AridityIndex, Treatment, PlantSource2,
#     PlotMix_Climate, Duration, Lifeform, MAT, MAP, Sand_content, Cum_precip
# However, AridityIndex and MAP are usually too correlated to include both, so MAP often dropped.

library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)
library(lme4)

# Load data ---------------------------------------------------------------

subplot.raw <- read_csv("data/cleaned/04.15_subplot-data_clean-0-added.csv")
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
nopellet <- subplot |> 
  filter(Treatment != "Pellets")




# All data ----------------------------------------------------------------

## Poisson ----------------------------------------------------------------

# All variables, ref adjusted, no random effects
pos.all.0 <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                   PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                 data = subplot, family = "poisson")
summary(pos.all.0)
check_overdispersion(pos.all.0) # overdispersion detected
check_zeroinflation(pos.all.0) # model is underfitting zeros



# Negative binomial -------------------------------------------------------

# All variables, no random effect
nb.all.0 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                    PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                    Cum_precip,
                  data = subplot,
                  family = nbinom2)
summary(nb.all.0)
res.nb.all.0 <- simulateResiduals(nb.all.0)
plotResiduals(nb.all.0)
plotQQunif(nb.all.0)
check_overdispersion(nb.all.0) # overdispersion detected
check_zeroinflation(nb.all.0) # no zero-inflation detected

# All variables, nested random effect of Site/Plot
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
check_overdispersion(nb.all) # underdispersion detected
check_zeroinflation(nb.all) # no zero-inflation detected
check_collinearity(nb.all)

# 1: Drop MAP (collinearity)
nb.all1 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                    PlotMix_Climate + Duration + Lifeform + MAT + Sand_content + 
                    Cum_precip + (1 | Site / Plot),
                  data = subplot,
                  family = nbinom2)
summary(nb.all1)
r2(nb.all1)
res.nb.all1 <- simulateResiduals(nb.all1)
plotResiduals(nb.all1)
plotQQunif(nb.all1)
check_overdispersion(nb.all1) # underdispersion detected
check_zeroinflation(nb.all1) # no zero-inflation detected
check_collinearity(nb.all1)





# nopellet dataset --------------------------------------------------------

## Poisson ----------------------------------------------------------------

# All variables, no random effects
pos.nopellet.0 <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                   PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                 data = nopellet, family = "poisson")
summary(pos.nopellet.0)
check_overdispersion(pos.nopellet.0) # overdispersion detected
check_zeroinflation(pos.nopellet.0) # model is underfitting zeros


# All variables, nested Site/Plot as random effect
pos.nopellet <- glmer(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                         PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                         Cum_precip + (1 | Site / Plot),
                       data = nopellet,
                       family = poisson) # unsure why this does not work

pos.nopellet <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                         PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                         Cum_precip + (1 | Site / Plot),
                       data = nopellet,
                       family = genpois)


## Negative binomial ------------------------------------------------------

# All variables, no random effect
nb.nopellet.0 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                          PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                          Cum_precip,
                        data = nopellet,
                        family = nbinom2)

summary(nb.nopellet.0)
res.nb.nopellet.0 <- simulateResiduals(nb.nopellet.0)
plotResiduals(nb.nopellet.0)
plotQQunif(nb.nopellet.0)
check_overdispersion(nb.nopellet.0) # overdispersion detected
check_zeroinflation(nb.nopellet.0) # no zero-inflation


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
check_overdispersion(nb.nopellet) # underdispersion detected
check_zeroinflation(nb.nopellet) # model is overfitting zeros
check_collinearity(nb.nopellet)
testOutliers(res.nb.nopellet)
outliers(res.nb.nopellet)




save.image("RData/07.15_generalized-linear-models_subplot-0-added-Count.RData")
