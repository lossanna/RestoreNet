# Created: 2024-08-30
# Last updated: 2024-08-30

# Purpose: Run GLMs for Sonoran Desert by weedy vs. desirable, but also split Perc_dev_cum
#   into separate datasets to see if there is potentially a differing response to wetter vs. drier
#   deviations - just looking at Sonoran Desert, and modeling Perc_dev_cum as quadratic based on
#   plots of Perc_dev_cum vs. Count.

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

# With 800% precip dev from Mojave removed
#   Desirable
subplot.des <- subplot |>
  filter(Weedy != "Weedy") |> 
  filter(Perc_dev_cum < 8)

#   Weedy
subplot.weed <- subplot |> 
  filter(Perc_dev_cum < 8) |> 
  filter(Weedy != "Desirable")


## Separate negative and positive Per_cum_dev -----------------------------

des.pos <- subplot.des |> 
  filter(Perc_dev_cum > 0) |> 
  mutate(Perc_wetter = Perc_dev_cum)
des.neg <- subplot.des |> 
  filter(Perc_dev_cum < 0) |> 
  mutate(Perc_drier = abs(Perc_dev_cum))

weed.pos <- subplot.weed |> 
  filter(Perc_dev_cum > 0) |> 
  mutate(Perc_wetter = Perc_dev_cum)
weed.neg <- subplot.weed |> 
  filter(Perc_dev_cum < 0) |> 
  mutate(Perc_drier = abs(Perc_dev_cum))


## Separate out Sonoran sites (6) -----------------------------------------

# Desirable
sonoran.des.pos <- des.pos |> 
  filter(Region %in% c("Sonoran SE", "Sonoran Central")) 
sonoran.des.pos$PlantSource2 <- relevel(sonoran.des.pos$PlantSource2, ref = "Native recruit")
sonoran.des.neg <- des.neg |> 
  filter(Region %in% c("Sonoran SE", "Sonoran Central")) 
sonoran.des.neg$PlantSource2 <- relevel(sonoran.des.neg$PlantSource2, ref = "Native recruit")

# Weedy
sonoran.weed.pos <- weed.pos |>  
  filter(Region %in% c("Sonoran SE", "Sonoran Central")) 
sonoran.weed.neg <- weed.neg |> 
  filter(Region %in% c("Sonoran SE", "Sonoran Central")) 



# Sonoran Desert by Weedy/Desirable and pos/neg ---------------------------

# Desirable + positive (wetter)
# Select variables, nested random effect of Site/Plot: Desirable + positive
nb.sonoran.des.pos <- glmmTMB(Count ~ I(Perc_wetter ^ 2) + AridityIndex_log + Treatment + PlantSource2 + 
                            PlotMix_Climate + Duration + Lifeform + MAT + (1 | Site / Plot),
                          data = sonoran.des.pos,
                          family = nbinom2)
summary(nb.sonoran.des.pos)
r2(nb.sonoran.des.pos)
res.nb.sonoran.des.pos <- simulateResiduals(nb.sonoran.des.pos)
plotQQunif(res.nb.sonoran.des.pos)
plotResiduals(res.nb.sonoran.des.pos)
check_overdispersion(nb.sonoran.des.pos) # overdispersion detected
check_zeroinflation(nb.sonoran.des.pos) # model is overfitting zeros
check_collinearity(nb.sonoran.des.pos) 

# Desirable + negative (drier)
# Select variables, nested random effect of Site/Plot: Desirable + negative
#   Perc_drier is not significant when modeled as linear
nb.sonoran.des.neg <- glmmTMB(Count ~ I(Perc_drier ^ 2) + AridityIndex_log + Treatment + PlantSource2 + 
                                PlotMix_Climate + Duration + Lifeform + MAT + (1 | Site / Plot),
                              data = sonoran.des.neg,
                              family = nbinom2)
summary(nb.sonoran.des.neg)
r2(nb.sonoran.des.neg)
res.nb.sonoran.des.neg <- simulateResiduals(nb.sonoran.des.neg)
plotQQunif(res.nb.sonoran.des.neg)
plotResiduals(res.nb.sonoran.des.neg)
check_overdispersion(nb.sonoran.des.neg) # overdispersion detected
check_zeroinflation(nb.sonoran.des.neg) # model is overfitting zeros
check_collinearity(nb.sonoran.des.neg) 


# Weedy + positive (wetter)
# Select variables, nested random effect of Site/Plot: Weedy + positive
nb.sonoran.weed.pos <- glmmTMB(Count ~ I(Perc_wetter ^ 2) + AridityIndex_log + Treatment + PlantSource2 + 
                                PlotMix_Climate + Duration + Lifeform + MAT + (1 | Site / Plot),
                              data = sonoran.weed.pos,
                              family = nbinom2)
summary(nb.sonoran.weed.pos)
r2(nb.sonoran.weed.pos)
res.nb.sonoran.weed.pos <- simulateResiduals(nb.sonoran.weed.pos)
plotQQunif(res.nb.sonoran.weed.pos)
plotResiduals(res.nb.sonoran.weed.pos)
check_overdispersion(nb.sonoran.weed.pos) # overdispersion detected
check_zeroinflation(nb.sonoran.weed.pos) # model is overfitting zeros
check_collinearity(nb.sonoran.weed.pos) 

# Weedy + negative (drier)
# Select variables, nested random effect of Site/Plot: Weedy + negative
nb.sonoran.weed.neg <- glmmTMB(Count ~ I(Perc_drier ^ 2) + AridityIndex_log + Treatment + PlantSource2 + 
                                PlotMix_Climate + Duration + Lifeform + MAT + (1 | Site / Plot),
                              data = sonoran.weed.neg,
                              family = nbinom2)
summary(nb.sonoran.weed.neg)
r2(nb.sonoran.weed.neg)
res.nb.sonoran.weed.neg <- simulateResiduals(nb.sonoran.weed.neg)
plotQQunif(res.nb.sonoran.weed.neg)
plotResiduals(res.nb.sonoran.weed.neg)
check_overdispersion(nb.sonoran.weed.neg) # overdispersion detected
check_zeroinflation(nb.sonoran.weed.neg) # model is overfitting zeros
check_collinearity(nb.sonoran.weed.neg) 


save.image("RData/08.15_generalized-linear-models-3.0_subplot-Count.RData")
