# All the ZINBs


# Created: 2024-05-26
# Last updated: 2024-07-30

# Purpose: Run generalized linear models for subplot data, with Count as response variable. 
#   Check for overdispersion and zero-inflation.

# Presence of overdispersion and zero-inflation for Count indicates a zero-inflated negative binomial 
#   should be used.
# Including SitePlotID as a random variable accounts for the non-independence of repeat sampling, but
#   the variable cannot be included in the zero-inflated model, because it will not converge.

library(tidyverse)
library(MASS)
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
  dplyr::select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Perc_deviation, Deviation_mm) |> 
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
subplot$PlotMix_Climate <- as.factor(subplot$PlotMix_Climate)
subplot$PlotMix_Climate <- relevel(subplot$PlotMix_Climate, ref = "None")


## Remove Inf from Perc_dev_cum -------------------------------------------

# Infinity created when there was no rain during the monitoring period. This only happens
#   twice and these instances can be dropped.
subplot <- subplot |> 
  filter(Perc_dev_cum != Inf)


# Remove Pellets ----------------------------------------------------------

# Pellets were only used at a couple of sites

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



# pitseed dataset (Control, Seed, Pits) -----------------------------------

# Control, Seed, and Pits plots included (Pellets, Mulch & ConMod excluded)

## Poisson ----------------------------------------------------------------

glm.pitseed.pos <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                         PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                       data = pitseed, family = "poisson")
summary(glm.pitseed.pos)
#   overdispersion according to residual deviance/degrees freedom
check_overdispersion(glm.pitseed.pos) # overdispersion detected
check_zeroinflation(glm.pitseed.pos) # no zero-inflation detected


## Quasi-Poisson ----------------------------------------------------------

glm.pitseed.quasi <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                           PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                         data = pitseed, family = "quasipoisson")
summary(glm.pitseed.quasi)
check_overdispersion(glm.pitseed.quasi) # overdispersion detected
check_zeroinflation(glm.pitseed.quasi) # no zero-inflation detected
check_model(glm.pitseed.quasi) 


## Negative binomial ------------------------------------------------------

glm.pitseed.nb <- glm.nb(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                           PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                         data = pitseed)
summary(glm.pitseed.nb)
step(glm.pitseed.nb) # suggests to drop PlotMix_Climate, Cum_precip
check_zeroinflation(glm.pitseed.nb) # no zero-inflation detected
plot(glm.pitseed.nb, which = 1:3)


## Zero-inflated negative binomial ----------------------------------------

# All explanatory variables (no random effect): does not converge
zinb.pitseed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                          PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                        data = pitseed,
                        family = nbinom2,
                        ziformula = ~.) # did not converge


# 1: dropped PlotMix_Climate and Cum_precip: does not converge
zinb.pitseed01 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                            Duration + Lifeform + MAT + MAP + Sand_content,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.) # did not converge


# 2: dropped PlotMix_Climate, Cum_precip, Lifeform: does not converge
zinb.pitseed02 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                            Duration + MAT + MAP + Sand_content,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.)


# 3: Perc_dev_cum, AridityIndex, Treatment, PlantSource2, Duration: does not converge
zinb.pitseed03 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                            Duration,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.) # did not converge


# 4: Perc_dev_cum, AridityIndex, Treatment, PlantSource2 does not converge
zinb.pitseed04 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.) # did not converge


# 5: Perc_dev_cum, AridityIndex, Treatment
zinb.pitseed05 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.)
summary(zinb.pitseed05) # AIC: 48216.1


# 6: Perc_dev_cum, Treatment, MAP, MAT: produced NaNs
zinb.pitseed06 <- glmmTMB(Count ~ Perc_dev_cum + Treatment + MAP + MAT,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.) # failed to invert Hessian from numDeriv::jacobian(), falling back to internal vcov estimate
#                                   idk something went wrong


# 7: Perc_dev_cum, AridityIndex, Treatment, MAP, MAT: does not converge 
zinb.pitseed07 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + MAP + MAT,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.) # did not converge


# 8: Perc_dev_cum, Treatment
zinb.pitseed08 <- glmmTMB(Count ~ Perc_dev_cum + Treatment,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.) 
summary(zinb.pitseed08) # AIC: 48434.3


# 9: Perc_dev_cum, AridityIndex, Treatment, MAP, MAT, Sand_content, Weedy: does not converge
zinb.pitseed09 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + MAP + MAT + Sand_content + Weedy,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.) # did not converge


# 10: Perc_dev_cum, Lifeform, MAP: does not converge
zinb.pitseed10 <- glmmTMB(Count ~ Perc_dev_cum + Lifeform + MAP,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.) # did not converge


# 11: Perc_dev_cum, MAP
zinb.pitseed11 <- glmmTMB(Count ~ Perc_dev_cum + MAP,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.)
summary(zinb.pitseed11) # AIC: 48546.9


# 12: Perc_dev_cum, Treatment, MAP
zinb.pitseed12 <- glmmTMB(Count ~ Perc_dev_cum + Treatment + MAP,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.)
summary(zinb.pitseed12) # AIC: 48335.6


# 13: Perc_dev_cum, Treatment, MAP, Weedy: does not converge
zinb.pitseed13 <- glmmTMB(Count ~ Perc_dev_cum + Treatment + MAP + Weedy,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.) # did not converge

# 14: Perc_dev_cum, AridityIndex
zinb.pitseed14 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.)
summary(zinb.pitseed14) # AIC: 48356.8 


# 15: AridityIndex, Treatment
zinb.pitseed15 <- glmmTMB(Count ~ AridityIndex + Treatment,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.)
summary(zinb.pitseed15) # AIC: 48540.4


# 16: AridityIndex, Treatment, Weedy: does not converge
zinb.pitseed15 <- glmmTMB(Count ~ AridityIndex + Treatment + Weedy,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.) # did not converge

# 17: Perc_dev_cum, Weedy: does not converge
zinb.pitseed17 <- glmmTMB(Count ~ Perc_dev_cum + Weedy,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.) # did not converge

# 18: Perc_dev_cum, PlantSource2: does not converge
zinb.pitseed18 <- glmmTMB(Count ~ Perc_dev_cum + PlantSource2,
                          data = pitseed,
                          family = nbinom2,
                          ziformula = ~.) # did not converge


# Compare AIC
AIC(zinb.pitseed05) # AridityIndex does better than MAP
AIC(zinb.pitseed12)

AIC(zinb.pitseed05) # Treatment should be included
AIC(zinb.pitseed14)


## Hurdle model -----------------------------------------------------------


# Hurdle model
#   All explanatory variables: 




# All data ----------------------------------------------------------------

## Poisson ----------------------------------------------------------------

glm.all.pos <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                     PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                   data = subplot, family = "poisson")
check_overdispersion(glm.all.pos) # overdispersion detected
check_zeroinflation(glm.all.pos) # zero-inflation detected


## Negative binomial ------------------------------------------------------

glm.all.nb <- glm.nb(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                       PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                     data = subplot)
summary(glm.all.nb)
step(glm.all.nb) # suggests to drop Cum_precip and PlotMix_Climate
check_zeroinflation(glm.all.nb) # zero-inflation detected


## Zero-inflated negative binomial ----------------------------------------

# The models that do not converge for the pitseed dataset also do not converge for
#   the all-data dataset, so code is omitted.


# 5: Perc_dev_cum, AridityIndex, Treatment
zinb.all05 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment,
                      data = subplot,
                      family = nbinom2,
                      ziformula = ~.)
summary(zinb.all05)

#   5.1: with random effects: does not converge
zinb.all05.10 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + (1 | SitePlotID),
                         data = subplot,
                         family = nbinom2,
                         ziformula = ~.) # did not converge

#   5.11: with random effects, but not in zero-inflated
zinb.all05.11 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + (1 | SitePlotID),
                         data = subplot,
                         family = nbinom2,
                         ziformula = ~ Perc_dev_cum + AridityIndex + Treatment) 
summary(zinb.all05.11) # AIC: 86645.5


# 8: Perc_dev_cum, Treatment
zinb.all08 <- glmmTMB(Count ~ Perc_dev_cum + Treatment,
                      data = subplot,
                      family = nbinom2,
                      ziformula = ~.)
summary(zinb.all08) # AIC: 90500.2

#   8.1: with random effects: does not converge
zinb.all08.10 <- glmmTMB(Count ~ Perc_dev_cum + Treatment + (1 | SitePlotID),
                         data = subplot,
                         family = nbinom2,
                         ziformula = ~.) # did not converge

#   8.11: with random effects, but not in zero-inflated
zinb.all08.11 <- glmmTMB(Count ~ Perc_dev_cum + Treatment + (1 | SitePlotID),
                         data = subplot,
                         family = nbinom2,
                         ziformula = ~ Perc_dev_cum + AridityIndex + Treatment) 
summary(zinb.all08.11) # AIC: 86826.3


# 12: Perc_dev_cum, Treatment, MAP
zinb.all12 <- glmmTMB(Count ~ Perc_dev_cum + Treatment + MAP ,
                      data = subplot,
                      family = nbinom2,
                      ziformula = ~.)
summary(zinb.all12) # AIC: 90198.3



# nopellet dataset --------------------------------------------------------

## Zero-inflated negative binomial ----------------------------------------

# 5: Perc_dev_cum, AridityIndex, Treatment
zinb.nopellet05 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment,
                           data = nopellet,
                           family = nbinom2,
                           ziformula = ~.)
summary(zinb.nopellet05) # AIC: 89562.4

#   5.1: with random effects: does not converge
zinb.nopellet05.10 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + (1 | SitePlotID),
                              data = nopellet,
                              family = nbinom2,
                              ziformula = ~.) # did not converge

#   5.11: with random effects, but not in zero-inflated
zinb.nopellet05.11 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + (1 | SitePlotID),
                              data = nopellet,
                              family = nbinom2,
                              ziformula = ~ Perc_dev_cum + AridityIndex + Treatment) 
summary(zinb.nopellet05.11) # AIC: 86062.4





# Data subset by Weedy/Desirable ------------------------------------------


# Poisson -----------------------------------------------------------------

pos.pitseed05.11.des <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment,
                            data = pitseed.des, family = "poisson")
summary(pos.pitseed05.11.des)
#   overdispersion according to residual deviance/degrees freedom
check_overdispersion(pos.pitseed05.11.des) # overdispersion detected
check_zeroinflation(pos.pitseed05.11.des) # zero-inflation detected


## Negative binomial ------------------------------------------------------

# 5.11, nopellet, Desirable
nb.nopellet05.11.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + (1 | SitePlotID),
                                data = nopellet.des,
                                family = nbinom2) 
resid.nb.nopellet05.11.des <- simulateResiduals(fittedModel = nb.nopellet05.11.des)
testZeroInflation(resid.nb.nopellet05.11.des)
check_zeroinflation(nb.nopellet05.11.des)


## Zero-inflated negative binomial ----------------------------------------

# 5.11, pitseed, Desirable 
zinb.pitseed05.11.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + (1 | SitePlotID),
                                 data = pitseed.des,
                                 family = nbinom2,
                                 ziformula = ~ Perc_dev_cum + AridityIndex + Treatment)
summary(zinb.pitseed05.11.des)
check_model(zinb.pitseed05.11.des)

# 5.11, pitseed, Weedy
zinb.pitseed05.11.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + (1 | SitePlotID),
                                  data = pitseed.weed,
                                  family = nbinom2,
                                  ziformula = ~ Perc_dev_cum + AridityIndex + Treatment)
summary(zinb.pitseed05.11.weed)



# 5.11, nopellet, Desirable
zinb.nopellet05.11.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + (1 | SitePlotID),
                                  data = nopellet.des,
                                  family = nbinom2,
                                  ziformula = ~ Perc_dev_cum + AridityIndex + Treatment) 
summary(zinb.nopellet05.11.des)
resid.zinb.nopellet05.11.des <- simulateResiduals(fittedModel = zinb.nopellet05.11.des)
plot(resid.zinb.nopellet05.11.des)
testDispersion(resid.zinb.nopellet05.11.des, alternative = "greater") # no overdispersion detected
testZeroInflation(resid.zinb.nopellet05.11.des)



# 5.11, nopellet, Weedy
zinb.nopellet05.11.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + (1 | SitePlotID),
                                   data = nopellet.weed,
                                   family = nbinom2,
                                   ziformula = ~ Perc_dev_cum + AridityIndex + Treatment) 
summary(zinb.nopellet05.11.weed)


save.image("RData/07.1_linear-models_subplot-Count.RData")
