# Created: 2024-05-26
# Last updated: 2024-08-01

# Purpose: Run generalized linear models for subplot data, with Count as response variable. 
#   Check for overdispersion and zero-inflation.

# Presence of overdispersion and for Count indicates a negative binomial should be used.
# Including (1 | Site / Plot) accounts for the non-independence of repeat sampling.

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


## Re-level categorical variables to set reference ------------------------

# PlantSource, PlantSource2, Weedy, Duration, Lifeform, Native, SpeciesSeeded: no re-level needed (0 is ref)

unique(subplot$Native)
# Treatment
unique(subplot$Treatment)
subplot$Treatment <- as.factor(subplot$Treatment)
subplot$Treatment <- relevel(subplot$Treatment, ref = "Control")

# PlotMix_Climate
unique(subplot$PlotMix_Climate)
#   Allow Current to remain reference; because of uneven sample sizes, if
#     None is reference then Projected will be dropped from models. Better
#     to drop None and be able to compare Current & Projected.


## Remove Inf from Perc_dev_cum -------------------------------------------

# Infinity created when there was no rain during the monitoring period. This only happens
#   twice and these instances can be dropped.
subplot <- subplot |> 
  filter(Perc_dev_cum != Inf)


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

# Weedy
pitseed.weed <- pitseed |> 
  filter(Weedy != "Desirable")



## Separate out Sonoran Desert sites --------------------------------------

# All
sonoran <- subplot |> 
  filter(Region %in% c("Sonoran SE", "Sonoran Central"))

# Desirable
sonoran.des <- sonoran |> 
  filter(Weedy != "Weedy")

# Weedy
sonoran.weed <- sonoran |> 
  filter(Weedy != "Desirable")



## Separate out northern AZ sites -----------------------------------------

# All
naz <- subplot |> 
  filter(Region == "Colorado Plateau")

# Desirable
naz.des <- naz |> 
  filter(Weedy != "Weedy")

# Weedy
naz.weed <- naz |> 
  filter(Weedy != "Desirable")



# pitseed dataset (Control, Seed, Pits) -----------------------------------

# Control, Seed, and Pits plots included (Pellets, Mulch & ConMod excluded)

## Poisson ----------------------------------------------------------------

# All variables, no random effects
pos.pitseed00 <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                     PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                   data = pitseed, family = "poisson")
summary(pos.pitseed00)
check_overdispersion(pos.pitseed00) # overdispersion detected
check_zeroinflation(pos.pitseed00) # no zero-inflation detected
check_model(pos.pitseed00)

# All variables, with random effects: does not converge
pos.pitseed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                        PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                        Cum_precip + (1 | Site / Plot),
                      data = pitseed,
                      family = genpois) # did not converge


## Quasi-Poisson ----------------------------------------------------------

# All variables, no random effects
qpos.pitseed00 <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                       PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                     data = pitseed, family = "quasipoisson")
summary(qpos.pitseed00)
check_model(qpos.pitseed00)
check_overdispersion(qpos.pitseed00) # overdispersion detected
check_zeroinflation(qpos.pitseed00) # no zero-inflation detected


## Negative binomial ------------------------------------------------------

# All variables, no random effects
nb.pitseed00 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                        PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                        Cum_precip,
                      data = pitseed,
                      family = nbinom2)
summary(nb.pitseed00)
res.nb.pitseed00 <- simulateResiduals(nb.pitseed00)
check_model(nb.pitseed00)
check_zeroinflation(nb.pitseed00) # no zero-inflation
testZeroInflation(res.nb.pitseed00) # no zero-inflation
check_overdispersion(nb.pitseed00) # overdispersion detected
plotResiduals(res.nb.pitseed00)
plotQQunif(res.nb.pitseed00)
outliers(res.nb.pitseed00)


# All variables, nested random effect of Site/Plot
nb.pitseed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                        PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                        Cum_precip + (1 | Site / Plot),
                      data = pitseed,
                      family = nbinom2)
summary(nb.pitseed)
res.nb.pitseed <- simulateResiduals(nb.pitseed)
check_model(nb.pitseed)
check_overdispersion(nb.pitseed) # indicates overdispersion still
step(nb.pitseed)
testDispersion(res.nb.pitseed, alternative = "greater") # indicates overdispersion still


AIC(pos.pitseed00)
AIC(qpos.pitseed00)
AIC(nb.pitseed00)
AIC(nb.pitseed)


## Zero-inflated negative binomial ----------------------------------------

# All explanatory variables (no random effect): does not converge
zinb.pitseed00 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                          PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                        data = pitseed,
                        family = nbinom2,
                        ziformula = ~.) # did not converge



# All data ----------------------------------------------------------------

## Poisson ----------------------------------------------------------------

# All variables, no random effects
pos.all00 <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                     PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                   data = subplot, family = "poisson")
summary(pos.all00)
check_overdispersion(pos.all00) # overdispersion detected
check_zeroinflation(pos.all00) # no zero-inflation detected

# All variables, with random effects: does not converge
pos.all <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                         PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                         Cum_precip + (1 | Site / Plot),
                       data = subplot,
                       family = genpois) # did not converge


## Negative binomial ------------------------------------------------------

# All variables, no random effects
nb.all00 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                          PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                          Cum_precip,
                        data = subplot,
                        family = nbinom2)
summary(nb.all00)
check_overdispersion(nb.all00) # overdispersion detected
check_zeroinflation(nb.all00) # no zero-inflation detected

# All variables, nested random effect of Site/Plot
nb.all <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                      PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                      Cum_precip + (1 | Site / Plot),
                    data = subplot,
                    family = nbinom2)
summary(nb.all)
res.nb.all <- simulateResiduals(nb.all)
check_overdispersion(nb.all) # overdispersion detected
check_zeroinflation(nb.all) # no zero-inflation detected




# nopellet dataset --------------------------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, nested Site/Plot as random effect
nb.nopellet <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                              PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                              Cum_precip + (1 | Site / Plot),
                            data = nopellet,
                            family = nbinom2)
summary(nb.nopellet)
res.nb.nopellet <- simulateResiduals(nb.nopellet)
check_overdispersion(nb.nopellet) # overdispersion detected
check_zeroinflation(nb.nopellet) # no zero-inflation detected
plotQQunif(res.nb.nopellet)
testOutliers(res.nb.nopellet)
outliers(res.nb.nopellet)
plotResiduals(res.nb.nopellet)



# pitseed subset by Weedy/Desirable ---------------------------------------

## Poisson ----------------------------------------------------------------

# All variables, no random effects: Desirable
pos.pitseed00.des <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                           PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                         data = pitseed.des, family = "poisson")
summary(pos.pitseed00.des)
res.pos.pitseed00.des <- simulateResiduals(pos.pitseed00.des)
check_overdispersion(pos.pitseed00.des) # overdispersion detected
check_zeroinflation(pos.pitseed00.des) # no zero-inflation detected
plotQQunif(res.pos.pitseed00.des)
plotResiduals(res.pos.pitseed00.des)

# All variables, no random effects: Weedy
pos.pitseed00.weed <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                           PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                         data = pitseed.weed, family = "poisson")
summary(pos.pitseed00.weed)
res.pos.pitseed00.weed <- simulateResiduals(pos.pitseed00.weed)
check_overdispersion(pos.pitseed00.weed) # overdispersion detected
check_zeroinflation(pos.pitseed00.weed) # no zero-inflation detected


## Negative binomial ------------------------------------------------------

# All variables, no random effects: Desirable
nb.pitseed00.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                          PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                          Cum_precip,
                        data = pitseed.des,
                        family = nbinom2)
summary(nb.pitseed00.des)
res.nb.pitseed00.des <- simulateResiduals(nb.pitseed00.des)
check_overdispersion(nb.pitseed00.des) # overdispersion detected
check_zeroinflation(nb.pitseed00.des) # no zero-inflation detected
plotQQunif(res.nb.pitseed00.des)
testOutliers(res.nb.pitseed00.des)
outliers(res.nb.pitseed00.des)
plotResiduals(res.nb.pitseed00.des)

# All variables, no random effects: Weedy
nb.pitseed00.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                              PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                              Cum_precip,
                            data = pitseed.weed,
                            family = nbinom2)
summary(nb.pitseed00.weed)
res.nb.pitseed00.weed <- simulateResiduals(nb.pitseed00.weed)
check_overdispersion(nb.pitseed00.weed) # overdispersion detected
check_zeroinflation(nb.pitseed00.weed) # no zero-inflation detected
plotQQunif(res.nb.pitseed00.weed)
testOutliers(res.nb.pitseed00.weed)
outliers(res.nb.pitseed00.weed)
plotResiduals(res.nb.pitseed00.weed)



# nopellet subset by Weedy/Desirable --------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot: Desirable
nb.nopellet.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                             Cum_precip + (1 | Site / Plot),
                           data = nopellet.des,
                           family = nbinom2)
summary(nb.nopellet.des)
res.nb.nopellet.des <- simulateResiduals(nb.nopellet.des)
plotQQunif(res.nb.nopellet.des)
check_overdispersion(nb.nopellet.des) # overdispersion detected
step(nb.nopellet.des)

# All variables, nested random effect of Site/Plot: Weedy
nb.nopellet.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                             Cum_precip + (1 | Site / Plot),
                           data = nopellet.weed,
                           family = nbinom2)
summary(nb.nopellet.weed)
res.nb.nopellet.weed <- simulateResiduals(nb.nopellet.weed)
plotQQunif(res.nb.nopellet.weed)
plotResiduals(res.nb.nopellet.weed)
check_overdispersion(nb.nopellet.weed) # overdispersion detected


# 1: drop PlantSource2 and PlotMix_Climate: Desirable
nb.nopellet01.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment +  
                             Duration + Lifeform + MAT + MAP + Sand_content + 
                             Cum_precip + (1 | Site / Plot),
                           data = nopellet.des,
                           family = nbinom2)
summary(nb.nopellet01.des)
res.nb.nopellet01.des <- simulateResiduals(nb.nopellet01.des)
plotQQunif(res.nb.nopellet01.des)
plotResiduals(res.nb.nopellet01.des)
check_model(nb.nopellet01.des)
step(nb.nopellet01.des)

# 1: drop PlantSource2 and PlotMix_Climate: Weedy
nb.nopellet01.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment +  
                               Duration + Lifeform + MAT + MAP + Sand_content + 
                               Cum_precip + (1 | Site / Plot),
                             data = nopellet.weed,
                             family = nbinom2)
summary(nb.nopellet01.weed)
res.nb.nopellet01.weed <- simulateResiduals(nb.nopellet01.weed)
plotQQunif(res.nb.nopellet01.weed)
plotResiduals(res.nb.nopellet01.weed)
check_model(nb.nopellet01.weed)
step(nb.nopellet01.weed)


# 2: drop PlotMix_Climate, replace PlantSource2 with SpeciesSeeded: Desirable
nb.nopellet02.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + SpeciesSeeded +  
                               Duration + Lifeform + MAT + MAP + Sand_content + 
                               Cum_precip + (1 | Site / Plot),
                             data = nopellet.des,
                             family = nbinom2)
summary(nb.nopellet02.des)
res.nb.nopellet02.des <- simulateResiduals(nb.nopellet02.des)
plotQQunif(res.nb.nopellet02.des)
plotResiduals(res.nb.nopellet02.des)
check_model(nb.nopellet02.des)
step(nb.nopellet02.des)

# 2: drop PlotMix_Climate: Weedy
nb.nopellet02.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                                Duration + Lifeform + MAT + MAP + Sand_content + 
                                Cum_precip + (1 | Site / Plot),
                              data = nopellet.weed,
                              family = nbinom2)
summary(nb.nopellet02.weed)
res.nb.nopellet02.weed <- simulateResiduals(nb.nopellet02.weed)
plotQQunif(res.nb.nopellet02.weed)
plotResiduals(res.nb.nopellet02.weed)
check_model(nb.nopellet02.weed)
step(nb.nopellet02.weed)



# Sonoran sites -----------------------------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot: produces NaNs
nb.sonoran <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                            PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                            Cum_precip + (1 | Site / Plot),
                          data = sonoran,
                          family = nbinom2) # produces NaNs

# 1: Drop MAT & MAP (for collinearity): Desirable
nb.sonoran01 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                              PlotMix_Climate + Duration + Lifeform + Sand_content + 
                              Cum_precip + (1 | Site / Plot),
                            data = sonoran,
                            family = nbinom2) 
summary(nb.sonoran01)
res.nb.sonoran01 <- simulateResiduals(nb.sonoran01)
plotQQunif(res.nb.sonoran01)
plotResiduals(res.nb.sonoran01)
check_overdispersion(nb.sonoran01) # overdispersion detected
check_zeroinflation(nb.sonoran01) # no zero-inflation detected
check_model(nb.sonoran01)
step(nb.sonoran01)



# Sonoran sites by Weedy/Desirable ----------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot: Desirable
nb.sonoran.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                             PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                             Cum_precip + (1 | Site / Plot),
                           data = sonoran.des,
                           family = nbinom2)
summary(nb.sonoran.des)
res.nb.sonoran.des <- simulateResiduals(nb.sonoran.des)
plotQQunif(res.nb.sonoran.des)
plotResiduals(res.nb.sonoran.des)
check_overdispersion(nb.sonoran.des) # overdispersion detected
check_zeroinflation(nb.sonoran.des) # no overdispersion detected
check_model(nb.sonoran.des)
step(nb.sonoran.des)

# All variables, nested random effect of Site/Plot: Weedy
nb.sonoran.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                            PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                            Cum_precip + (1 | Site / Plot),
                          data = sonoran.weed,
                          family = nbinom2)
summary(nb.sonoran.weed)
res.nb.sonoran.weed <- simulateResiduals(nb.sonoran.weed)
plotQQunif(res.nb.sonoran.weed)
plotResiduals(res.nb.sonoran.weed)
check_overdispersion(nb.sonoran.weed) # overdispersion detected
check_model(nb.sonoran.weed)
step(nb.sonoran.weed)


# 1: Drop MAT & MAP (for collinearity): Desirable
nb.sonoran01.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                            PlotMix_Climate + Duration + Lifeform + Sand_content + 
                            Cum_precip + (1 | Site / Plot),
                          data = sonoran.des,
                          family = nbinom2)
summary(nb.sonoran01.des)
res.nb.sonoran01.des <- simulateResiduals(nb.sonoran01.des)
plotQQunif(res.nb.sonoran01.des)
plotResiduals(res.nb.sonoran01.des)
check_overdispersion(nb.sonoran01.des) # overdispersion detected
check_zeroinflation(nb.sonoran01.des) # no zero-inflation detected
check_model(nb.sonoran01.des)
step(nb.sonoran01.des)

# 1: Drop MAT & MAP (for collinearity): Weedy
nb.sonoran01.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                              PlotMix_Climate + Duration + Lifeform + Sand_content + 
                              Cum_precip + (1 | Site / Plot),
                            data = sonoran.weed,
                            family = nbinom2)
summary(nb.sonoran01.weed)
res.nb.sonoran01.weed <- simulateResiduals(nb.sonoran01.weed)
plotQQunif(res.nb.sonoran01.weed)
plotResiduals(res.nb.sonoran01.weed)
check_overdispersion(nb.sonoran01.weed) # overdispersion detected
check_zeroinflation(nb.sonoran01.weed) # no zero-inflation detected
check_model(nb.sonoran01.weed)
step(nb.sonoran01.weed)



# Northern AZ sites -------------------------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot
nb.naz <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                        PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                        Cum_precip + (1 | Site / Plot),
                      data = naz,
                      family = nbinom2) 
summary(nb.naz)
res.nb.naz <- simulateResiduals(nb.naz)
plotQQunif(res.nb.naz)
plotResiduals(res.nb.naz)
check_overdispersion(res.nb.naz)
check_model(nb.naz)


# Northern AZ sites by Weedy/Desirable ------------------------------------

## Negative binomial ------------------------------------------------------

# All variables, nested random effect of Site/Plot: Desirable
nb.naz.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                        PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                        Cum_precip + (1 | Site / Plot),
                      data = naz.des,
                      family = nbinom2)
summary(nb.naz.des)
res.nb.naz.des <- simulateResiduals(nb.naz.des)
plotQQunif(res.nb.naz.des)
plotResiduals(res.nb.naz.des)
check_overdispersion(nb.naz.des) # overdispersion detected
check_zeroinflation(nb.naz.des) # no overdispersion detected
check_model(nb.naz.des)
step(nb.naz.des)

# All variables, nested random effect of Site/Plot: Weedy
nb.naz.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                         PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                         Cum_precip + (1 | Site / Plot),
                       data = naz.weed,
                       family = nbinom2)
summary(nb.naz.weed)
res.nb.naz.weed <- simulateResiduals(nb.naz.weed)
plotQQunif(res.nb.naz.weed)
plotResiduals(res.nb.naz.weed)
check_overdispersion(nb.naz.weed) # overdispersion detected
check_model(nb.naz.weed)
step(nb.naz.weed)


# 1: Perc_dev_cum as cubic: Desirable
nb.naz01.des <- glmmTMB(Count ~ I(Perc_dev_cum ^ 3) + AridityIndex + Treatment + PlantSource2 + 
                          PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                          Cum_precip + (1 | Site / Plot),
                        data = naz.des,
                        family = nbinom2)
summary(nb.naz01.des)
res.nb.naz01.des <- simulateResiduals(nb.naz01.des)
plotQQunif(res.nb.naz01.des)
plotResiduals(res.nb.naz01.des)
check_overdispersion(nb.naz01.des) # overdispersion detected
check_zeroinflation(nb.naz01.des) # no zero-inflation detected
check_model(nb.naz01.des)
step(nb.naz01.des)

# 1: Perc_dev_cum as cubic, drop PlantSource (for collinearity): Weedy
nb.naz01.weed <- glmmTMB(Count ~ I(Perc_dev_cum ^ 3) + AridityIndex + Treatment +  
                           PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                           Cum_precip + (1 | Site / Plot),
                         data = naz.weed,
                         family = nbinom2)
summary(nb.naz01.weed)
res.nb.naz01.weed <- simulateResiduals(nb.naz01.weed)
plotQQunif(res.nb.naz01.weed)
plotResiduals(res.nb.naz01.weed)
check_overdispersion(nb.naz01.weed) # overdispersion detected
check_zeroinflation(nb.naz01.weed) # no zero-inflation detected
check_model(nb.naz01.weed)
step(nb.naz01.weed)


save.image("RData/07.1_linear-models_subplot-Count.RData")
