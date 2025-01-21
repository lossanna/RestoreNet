# Created: 2024-12-13
# Last updated: 2024-12-13

# Purpose: First iteration of results for cool-spring model selection.

setwd("/groups/egornish/lossanna/RestoreNet/")

library(readr)
library(dplyr)
library(glmmTMB)
library(MuMIn)

# Load data ---------------------------------------------------------------

sonoran.subplot.raw <- read_csv("data/14.1_Sonoran-Desert_subplot_with-seasonality_clean.csv")
prism.data <- read_csv("data/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
since.pd <- read_csv("data/03.3_since-last-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/03.4_aridity-index-values_clean.csv")
sonoran.monitor <- read_csv("data/14.2_Sonoran-Desert_monitoring-events.csv")

# Data wrangling ----------------------------------------------------------

## Combine ----------------------------------------------------------------

# Reorganize columns for left_join()
since.pd.join <- since.pd |> 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Perc_deviation, Deviation_mm) |> 
  rename(Perc_dev_since = Perc_deviation,
         Dev_mm_since = Deviation_mm)

# Combine all variables
subplot <- sonoran.subplot.raw |> 
  left_join(sonoran.monitor) |> 
  left_join(prism.data) |> 
  left_join(ai) |> 
  left_join(since.pd.join)



## Re-level categorical variables to set reference ------------------------

# Treatment
unique(subplot$Treatment)
subplot$Treatment <- as.factor(subplot$Treatment)
subplot$Treatment <- relevel(subplot$Treatment, ref = "Seed")

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

# PlantSource2
unique(subplot$PlantSource2)
subplot$PlantSource2 <- as.factor(subplot$PlantSource2)
subplot$PlantSource2 <- relevel(subplot$PlantSource2, ref = "Native recruit")
#   Assign different reference for Desirable data set

# Seasonality
unique(subplot$Seasonality)
subplot$Seasonality <- as.factor(subplot$Seasonality)
subplot$Seasonality <- relevel(subplot$Seasonality, ref = "Cool")



## Separate datasets by seasonality ---------------------------------------

# Cool-Spring
cool.spring <- subplot |> 
  filter(Seasonality == "Cool") |> 
  filter(Monitor_season == "Spring")

# Warm-Fall
warm.fall <- subplot |> 
  filter(Seasonality == "Warm") |> 
  filter(Monitor_season == "Fall")

# Year-all
year.all <- subplot |> 
  filter(Seasonality %in% c("Year round", "Unknown"))


# Cool-Spring -------------------------------------------------------------

# All variables
cool.spring_full <- glmmTMB(Count ~ Perc_dev_since + AridityIndex +
                              Treatment + PlantSource2 + Duration + Lifeform +  
                              Perc_dev_since * Treatment +
                              Perc_dev_since * PlantSource2 + 
                              Perc_dev_since * Duration + 
                              Perc_dev_since * Lifeform +
                              Perc_dev_since * AridityIndex +
                              AridityIndex * Treatment +
                              AridityIndex * Duration +
                              AridityIndex * PlantSource2 +
                              (1 | Site / Plot),
                            data = cool.spring,
                            family = nbinom2)

options(na.action = "na.fail")

cool.spring_set <- dredge(cool.spring_full)

head(cool.spring_set)

cool.spring_best_model <- get.models(cool.spring_set, 1)[[1]]

summary(cool.spring_best_model)


save.image("RData/cool-spring01.RData")
sessionInfo()