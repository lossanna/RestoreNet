# Created: 2024-08-29
# Last updated: 2024-08-29

# Purpose: Run generalized linear models for subplot data, with Count as response variable. 
#   Check for overdispersion and zero-inflation.

# Differences between this and v 1.0 script (07.1.R):
#   Only negative binomial distribution used.
#   Data always split by Weedy/Desirable.
#   AridityIndex and Cum_precip have been transformed to improve normality.
#   3 datasets used: (1) all data, minus Perc_dev_cum outliers; (2) Sonoran Desert; (3) CO Plateau

# Negative binomial used to help with overdispersion.
# Including (1 | Site / Plot) accounts for the non-independence of repeat sampling.

# "All variables" includes: Perc_dev_cum, AridityIndex_log, Treatment, PlantSource2,
#     PlotMix_Climate, Duration, Lifeform, MAT, MAP, Sand_content, Cum_precip_sqrt.


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

# Weedy
unique(subplot$Weedy)
subplot$Weedy <- as.factor(subplot$Weedy)
subplot$Weedy <- relevel(subplot$Weedy, ref = "Weedy")

# PlantSource2
unique(subplot$PlantSource2)
subplot$PlantSource2 <- as.factor(subplot$PlantSource2)
subplot$PlantSource2 <- relevel(subplot$PlantSource2, ref = "Introduced/Invasive")



## Transform AridityIndex and Cum_precip for normalization ----------------

subplot <- subplot |> 
  mutate(Cum_precip_sqrt = sqrt(Cum_precip))

subplot <- subplot |> 
  mutate(AridityIndex_log = log(AridityIndex))


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





# All data (minus 8000% outlier), subset by Weedy/Desirable ---------------

# All variables, nested random effect of Site/Plot: Desirable
nb.all.des8rm <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex_log + Treatment + PlantSource2 + 
                           PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                           Cum_precip_sqrt + (1 | Site / Plot),
                         data = subplot.des8rm,
                         family = nbinom2)
summary(nb.all.des8rm)
r2(nb.all.des8rm)
res.nb.all.des8rm <- simulateResiduals(nb.all.des8rm)
plotResiduals(nb.all.des8rm)
plotQQunif(res.nb.all.des8rm)
check_overdispersion(nb.all.des8rm) # overdispersion detected
check_zeroinflation(nb.all.des8rm) # model is overfitting zeros
check_collinearity(nb.all.des8rm)

# All variables, nested random effect of Site/Plot: Weedy
nb.all.weed8rm <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex_log + Treatment + PlantSource2 + 
                            PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                            Cum_precip_sqrt + (1 | Site / Plot),
                          data = subplot.weed8rm,
                          family = nbinom2)
summary(nb.all.weed8rm)
r2(nb.all.weed8rm)
res.nb.all.weed8rm <- simulateResiduals(nb.all.weed8rm)
plotResiduals(res.nb.all.weed8rm)
plotQQunif(res.nb.all.weed8rm)
check_overdispersion(nb.all.weed8rm) # overdispersion detected
check_collinearity(nb.all.weed8rm) # Duration & PlantSource2 correlated

# 1: Drop PlantSource2 (collinearity): Weedy
nb.all1.weed8rm <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex_log + Treatment +  
                             PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
                             Cum_precip_sqrt + (1 | Site / Plot),
                           data = subplot.weed8rm,
                           family = nbinom2)
summary(nb.all1.weed8rm)
r2(nb.all1.weed8rm)
res.nb.all1.weed8rm <- simulateResiduals(nb.all1.weed8rm)
plotResiduals(res.nb.all1.weed8rm)
plotQQunif(res.nb.all1.weed8rm)
check_overdispersion(nb.all1.weed8rm) # overdispersion detected
check_zeroinflation(nb.all1.weed8rm) # model is overfitting zeros
check_collinearity(nb.all1.weed8rm)



# Sonoran sites by Weedy/Desirable ----------------------------------------

# All variables, nested random effect of Site/Plot: Desirable
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

# 1: Drop MAP (for collinearity): Desirable
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

# All variables, nested random effect of Site/Plot: Weedy
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

# 1: Drop MAP & Duration (for collinearity): Weedy
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



# Northern AZ sites by Weedy/Desirable ------------------------------------

# All variables, nested random effect of Site/Plot: Desirable
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

# All variables, nested random effect of Site/Plot: Weedy
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

# 1: Drop PlantSource2 (for collinearity): Weedy
nb.naz1.weed <- glmmTMB(Count ~ Perc_dev_cum + AridityIndex_log + Treatment +  
                          PlotMix_Climate + Duration + Lifeform + MAT + MAP + Sand_content + 
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


save.image("RData/08.1_generalized-linear-models-2.0_subplot-Count.RData")
