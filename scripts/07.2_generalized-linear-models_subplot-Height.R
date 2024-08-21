# Created: 2024-07-30
# Last updated: 2024-08-21

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
#   Perc_cum_dev not significant
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
check_overdispersion(nb.all)
check_zeroinflation(nb.all) # model is overfitting zeros
check_collinearity(nb.all)
check_model(nb.all)


save.image("RData/07.2_generalized-linear-models_subplot-Height.RData")
