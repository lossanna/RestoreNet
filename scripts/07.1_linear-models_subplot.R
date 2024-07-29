# Created: 2024-05-26
# Last updated: 2024-07-29

# Purpose: Run generalized linear models for subplot data. Check for overdispersion and zero-inflation.

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

# Remove Inf from Perc_dev_cum
#   Infinity created when there was no rain during the monitoring period. This only happens
#     twice and these instances can be dropped.
subplot <- subplot |> 
  filter(Perc_dev_cum != Inf)

# Separate out Pits, Seed, and Control only
pitseed <- subplot |> 
  filter(Treatment %in% c("Control", "Seed", "Pits"))
apply(pitseed, 2, anyNA)

pitseed.des <- pitseed |> 
  filter(Weedy != "Weedy")

pitseed.weed <- pitseed |> 
  filter(Weedy != "Desirable")

# Create separate data with NA Height values dropped
subplot.height <- subplot |> 
  filter(!is.na(Height))
pitseed.height <- pitseed |> 
  filter(!is.na(Height))



# Count -------------------------------------------------------------------

# All data ----------------------------------------------------------------

# Poisson
glm.all.pos <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                     PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                   data = subplot, family = "poisson")
check_overdispersion(glm.all.pos) # overdispersion detected
check_zeroinflation(glm.all.pos) # zero-inflation detected

# Negative binomial
glm.all.nb <- glm.nb(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                       PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                     data = subplot)
summary(glm.all.nb)
step(glm.all.nb) # suggests to drop Cum_precip and PlotMix_Climate
check_zeroinflation(glm.all.nb) # zero-inflation detected


# Zero-inflated negative binomial
#   All (no random effects, all variables included in zero model): does not converge
zinb.all <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                  PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                data = subplot,
                family = nbinom2,
                ziformula = ~.)

#   1: dropped PlotMix_Climate and Cum_precip (no random): does not converge
zinb.all1 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                      Duration + Lifeform + MAT + MAP + Sand_content,
                    data = subplot,
                    family = nbinom2,
                    ziformula = ~.)

#   4: Perc_dev_cum, AridityIndex, Treatment, PlantSource2 did not converge
zinb.all4 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2,
                         data = subplot,
                         family = nbinom2,
                         ziformula = ~.) # did not converge

#   5: Perc_dev_cum, AridityIndex, Treatment
zinb.all5 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment,
                         data = subplot,
                         family = nbinom2,
                         ziformula = ~.)
summary(zinb.all5)

#     5.1: with random effects
zinb.all5.10 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + (1 | SitePlotID),
                     data = subplot,
                     family = nbinom2,
                     ziformula = ~.) # did not converge


#   8: Perc_dev_cum, Treatment
zinb.all8 <- glmmTMB(Count ~ Perc_dev_cum + Treatment,
                         data = subplot,
                         family = nbinom2,
                         ziformula = ~.)
summary(zinb.all8)

#     8.1: with random effects
zinb.all8.10 <- glmmTMB(Count ~ Perc_dev_cum + Treatment + (1 | SitePlotID),
                     data = subplot,
                     family = nbinom2,
                     ziformula = ~.)



## pitseed dataset (Control, Seed, Pits) -----------------------------------

# Poisson
glm.pitseed.pos <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                 PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
               data = pitseed, family = "poisson")
summary(glm.pitseed.pos)
#   overdispersion according to residual deviance/degrees freedom
check_overdispersion(glm.pitseed.pos) # overdispersion detected
check_zeroinflation(glm.pitseed.pos) # zero-inflation detected

# Quasi-Poisson
glm.pitseed.quasi <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                   PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                 data = pitseed, family = "quasipoisson")
summary(glm.pitseed.quasi)
check_overdispersion(glm.pitseed.quasi) # overdispersion detected
check_zeroinflation(glm.pitseed.quasi) # zero-inflation detected
check_model(glm.pitseed.quasi) 

# Negative binomial
glm.pitseed.nb <- glm.nb(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
              data = pitseed)
summary(glm.pitseed.nb)
step(glm.pitseed.nb) # suggests to drop PlotMix_Climate, Cum_precip
check_zeroinflation(glm.pitseed.nb) # zero-inflation detected
plot(glm.pitseed.nb, which = 1:3)


# Zero-inflated negative binomial
#   All explanatory variables (no random effect): does not converge
zinb.pitseed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                 PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
               data = pitseed,
               family = nbinom2,
               ziformula = ~.) # did not converge

#   1: dropped PlotMix_Climate and Cum_precip: does not converge
zinb.pitseed1 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                 Duration + Lifeform + MAT + MAP + Sand_content,
               data = pitseed,
               family = nbinom2,
               ziformula = ~.) # did not converge

#   2: dropped PlotMix_Climate, Cum_precip, Lifeform: does not converge
zinb.pitseed2 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                  Duration + MAT + MAP + Sand_content,
                data = pitseed,
                family = nbinom2,
                ziformula = ~.)

#   3: Perc_dev_cum, AridityIndex, Treatment, PlantSource2, Duration: did not converge
zinb.pitseed3 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 + 
                  Duration,
                data = pitseed,
                family = nbinom2,
                ziformula = ~.) # did not converge

#   4: Perc_dev_cum, AridityIndex, Treatment, PlantSource2 did not converge
zinb.pitseed4 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2,
                data = pitseed,
                family = nbinom2,
                ziformula = ~.) # did not converge

#   5: Perc_dev_cum, AridityIndex, Treatment
zinb.pitseed5 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment,
                data = pitseed,
                family = nbinom2,
                ziformula = ~.)
summary(zinb.pitseed5)

#   6: Perc_dev_cum, Treatment, MAP, MAT: produced NaNs
zinb.pitseed6 <- glmmTMB(Count ~ Perc_dev_cum + Treatment + MAP + MAT,
                 data = pitseed,
                 family = nbinom2,
                 ziformula = ~.) # failed to invert Hessian from numDeriv::jacobian(), falling back to internal vcov estimate
#                                   idk something went wrong

#   7: Perc_dev_cum, AridityIndex, Treatment, MAP, MAT 
zinb.pitseed7 <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment + MAP + MAT,
                 data = pitseed,
                 family = nbinom2,
                 ziformula = ~.) # did not converge

#   8: Perc_dev_cum, Treatment
zinb.pitseed8 <- glmmTMB(Count ~ Perc_dev_cum + Treatment,
                 data = pitseed,
                 family = nbinom2,
                 ziformula = ~.) 
summary(zinb.pitseed8)

#   9: Perc_dev_cum, Treatment, MAP, MAT, AridityIndex, Sand_content, Weedy: did not converge
zinb.pitseed8 <- glmmTMB(Count ~ Perc_dev_cum + Treatment + MAP + MAT + AridityIndex + Sand_content + Weedy,
                         data = pitseed,
                         family = nbinom2,
                         ziformula = ~.) # did not converge



# Hurdle model
#   All explanatory variables: does not converge




## Data subset by weedy/desirable -----------------------------------------

# Zero-inflated negative binomial
#   Desirable
zinb5.des <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment,
                     data = pitseed.des,
                     family = nbinom2,
                     ziformula = ~.)
summary(zinb5.des)
plot(residuals(zinb5.des) ~ fitted(zinb5.des))


#   Weedy
zinb5.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex + Treatment,
                      data = pitseed.weed,
                      family = nbinom2,
                      ziformula = ~.)
summary(zinb5.weed)




save.image("RData/07.1_linear-models_subplot.RData")
