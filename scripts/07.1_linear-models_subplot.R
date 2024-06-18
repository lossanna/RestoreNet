# Created: 2024-05-26
# Last updated: 2024-05-29

# Purpose: Run generalized linear models for subplot data.

library(tidyverse)
library(mvabund)
library(ecostats)
library(MASS)
library(pscl)
library(performance)
library(lmtest)
library(assessor)
library(qqplotr)

# Load data ---------------------------------------------------------------

subplot.raw <- read_csv("data/cleaned/04.1_subplot-data_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")


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

## All variables ----------------------------------------------------------

# Poisson
glm.pos <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                 PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
               data = pitseed, family = "poisson")
summary(glm.pos)
#   overdispersion according to residual deviance/degrees freedom
check_overdispersion(glm.pos) # overdispersion detected
check_zeroinflation(glm.pos) # zero-inflation detected

# Quasi-Poisson
glm.quasi <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                   PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                 data = pitseed, family = "quasipoisson")
summary(glm.quasi)
check_overdispersion(glm.quasi)
check_model(glm.quasi)

# Negative binomial
glm.nb <- glm.nb(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
              data = pitseed)
summary(glm.nb)
step(glm.nb) # suggests to drop PlotMix_Climate, Cum_precip
plot(glm.nb, which = 1:3)


# Zero-inflated Poisson
#   All explanatory variables: does not converge
zip <- zeroinfl(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                       PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip |
                       Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                       PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                     dist = "poisson",
                     link = "logit",
                     data = pitseed)

#   1: dropped PlotMix_Climate and Cum_precip: does not converge
zip1 <- zeroinfl(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                        Duration + Lifeform + MAT + MAP + Sand_content |
                        Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                        Duration + Lifeform + MAT + MAP + Sand_content,
                      dist = "poisson",
                      link = "logit",
                      data = pitseed)

#   2: dropped PlotMix_Climate, Cum_precip, Lifeform: does not converge
zip2 <- zeroinfl(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                        Duration + MAT + MAP + Sand_content |
                        Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                        Duration + MAT + MAP + Sand_content,
                      dist = "poisson",
                      link = "logit",
                      data = pitseed)

#   3: did not converge
zip3 <- zeroinfl(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                        Duration |
                        Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                        Duration,
                      dist = "poisson",
                      link = "logit",
                      data = pitseed)

#   4: did not converge
zip4 <- zeroinfl(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 |
                        Perc_dev_cum + AridityIndex + Treatment + PlantSource2,
                      dist = "poisson",
                      link = "logit",
                      data = pitseed)

#   5: Perc_dev_cum, AridityIndex, Treatment
zip5 <- zeroinfl(Count ~ Perc_dev_cum + AridityIndex + Treatment  |
                        Perc_dev_cum + AridityIndex + Treatment,
                      dist = "poisson",
                      link = "logit",
                      data = pitseed)
summary(zip5)


# Zero-inflated negative binomial
#   All explanatory variables: does not converge
zinb <- zeroinfl(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                       PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip |
                       Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                       PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
                     dist = "negbin",
                     link = "logit",
                     data = pitseed)

#   1: dropped PlotMix_Climate and Cum_precip: does not converge
zinb1 <- zeroinfl(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                       Duration + Lifeform + MAT + MAP + Sand_content |
                       Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                       Duration + Lifeform + MAT + MAP + Sand_content,
                     dist = "negbin",
                     link = "logit",
                     data = pitseed)

#   2: dropped PlotMix_Climate, Cum_precip, Lifeform: does not converge
zinb2 <- zeroinfl(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                        Duration + MAT + MAP + Sand_content |
                        Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                        Duration + MAT + MAP + Sand_content,
                      dist = "negbin",
                      link = "logit",
                      data = pitseed)

#   3: did not converge
zinb3 <- zeroinfl(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                        Duration |
                        Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                        Duration,
                      dist = "negbin",
                      link = "logit",
                      data = pitseed)

#   4: did not converge
zinb4 <- zeroinfl(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 |
                        Perc_dev_cum + AridityIndex + Treatment + PlantSource2,
                      dist = "negbin",
                      link = "logit",
                      data = pitseed)

#   5: Perc_dev_cum, AridityIndex, Treatment
zinb5 <- zeroinfl(Count ~ Perc_dev_cum + AridityIndex + Treatment  |
                        Perc_dev_cum + AridityIndex + Treatment,
                      dist = "negbin",
                      link = "logit",
                      data = pitseed)
summary(zinb5)

#   6: Perc_dev_cum, Treatment, MAP, MAT
zinb6 <- zeroinfl(Count ~ Perc_dev_cum + Treatment + MAP + MAT |
                    Perc_dev_cum + Treatment + MAP + MAT,
                  dist = "negbin",
                  link = "logit",
                  data = pitseed)
summary(zinb6)

#   7: Perc_dev_cum, Treatment, MAP, MAT, AridityIndex
zinb7 <- zeroinfl(Count ~ Perc_dev_cum + Treatment + MAP + MAT + AridityIndex |
                    Perc_dev_cum + Treatment + MAP + MAT + AridityIndex,
                  dist = "negbin",
                  link = "logit",
                  data = pitseed)
summary(zinb7)

#   8: Perc_dev_cum, Treatment, MAP, MAT, AridityIndex, Sand_content
zinb8 <- zeroinfl(Count ~ Perc_dev_cum + Treatment + MAP + MAT + AridityIndex + Sand_content |
                    Perc_dev_cum + Treatment + MAP + MAT + AridityIndex + Sand_content,
                  dist = "negbin",
                  link = "logit",
                  data = pitseed)
summary(zinb8)

#   9: Perc_dev_cum, Treatment, MAP, MAT, AridityIndex, Sand_content, Weedy: did not converge
zinb9 <- zeroinfl(Count ~ Perc_dev_cum + Treatment + MAP + MAT + AridityIndex + Sand_content + Weedy |
                    Perc_dev_cum + Treatment + MAP + MAT + AridityIndex + Sand_content + Weedy,
                  dist = "negbin",
                  link = "logit",
                  data = pitseed)
summary(zinb9)


# Compare zero-inflated Poisson and zero-inflated negative binomial
lrtest(zip5, zinb5) # go with negative binomial (there is still overdispersion with zero-inflation correction)



## Data subset by weedy/desirable -----------------------------------------

# Zero-inflated negative binomial
#   Desirable
zinb5.des <- zeroinfl(Count ~ Perc_dev_cum + AridityIndex + Treatment  |
                        Perc_dev_cum + AridityIndex + Treatment,
                      dist = "negbin",
                      link = "logit",
                      data = pitseed.des)
summary(zinb5.des)
resid_zeroinfl(zinb5.des, plot = TRUE, scale = "uniform")
plot(residuals(zinb5.des) ~ fitted(zinb5.des))
qqrplot(zinb5.des)

#   Weedy
zinb5.weed <- zeroinfl(Count ~ Perc_dev_cum + AridityIndex + Treatment  |
                            Perc_dev_cum + AridityIndex + Treatment,
                          dist = "negbin",
                          link = "logit",
                          data = pitseed.weed)
summary(zinb5.weed)
















m2 <- manyglm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2 +
                PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + Cum_precip,
              data = pitseed, family = "negative.binomial")
plot(m2, which = 1:3)




glm.all2 <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2,
                data = pitseed, family = "poisson")
summary(glm.all2)


glm.nb.all <- manyglm(Count ~ Perc_dev_cum + AridityIndex + Treatment + Weedy,
                      data = pitseed, family = "negative.binomial")
coefficients(glm.nb.all)