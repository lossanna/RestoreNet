# Created: 2024-12-12
# Last updated: 2024-12-13

# Purpose: Run GLMMs for plants separated by seasonality.
#   Run model selection on UA HPC (`dredge()`), and load the results into the script here.

# With 1 node and 94 cores at 5 GB/core:
#   Cool-Spring took about 2 hours to run
#   Warm-Fall took about an hour
#   Year-All took less than an hour

library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)
library(MuMIn)

# Load data ---------------------------------------------------------------

sonoran.subplot.raw <- read_csv("data/cleaned/14.1_Sonoran-Desert_subplot_with-seasonality_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
since.pd <- read_csv("data/cleaned/03.3_since-last-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")
sonoran.monitor <- read_csv("data/cleaned/14.2_Sonoran-Desert_monitoring-events.csv")

# Load RData from UA HPC
load("Sonoran-UA-HPC/RData/cool-spring01.RData")
load("Sonoran-UA-HPC/RData/warm-fall01.RData")
load("Sonoran-UA-HPC/RData/year.all01.RData")


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

## Global model -----------------------------------------------------------

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
summary(cool.spring_full)
r2(cool.spring_full)
res.cool.spring_full <- simulateResiduals(cool.spring_full)
plotQQunif(res.cool.spring_full)
plotResiduals(res.cool.spring_full)


## Cool-Spring top models -------------------------------------------------

# Model selection on UA HPC (do not run)
#   options(na.action = "na.fail")
#   cool.spring_set <- dredge(cool.spring_full)

# Examine top model
cool.spring_best_model <- get.models(cool.spring_set, 1)[[1]]
summary(cool.spring_best_model)

# Examine models within top 2 AICc units
cool.spring_top_models <- subset(cool.spring_set, delta < 2) |> 
  filter(!is.na(delta)) 

# Assign each model to a separate object (takes a few minutes to run all 12)
for (i in 1:nrow(cool.spring_top_models)) {
  assign(paste0("cool.spring_model_", i), get.models(cool.spring_top_models, subset = i)[[1]])
}

# 1: AridityIndex + Duration + Lifeform + Perc_dev_since + PlantSource2 + Treatment +  
#     (1 | Site/Plot) + AridityIndex:Duration +  
#     AridityIndex:PlantSource2 + Duration:Perc_dev_since + Perc_dev_since:PlantSource2
summary(cool.spring_model_1)
r2(cool.spring_model_1)
res.cool.spring_model_1 <- simulateResiduals(cool.spring_model_1)
plotQQunif(res.cool.spring_model_1)
plotResiduals(res.cool.spring_model_1)

# 2: AridityIndex + Duration + Lifeform + Perc_dev_since + PlantSource2 + Treatment + 
#   (1 | Site/Plot) + AridityIndex:Duration + AridityIndex:Perc_dev_since + AridityIndex:PlantSource2 +  
#   Duration:Perc_dev_since + Perc_dev_since:PlantSource2
summary(cool.spring_model_2)
r2(cool.spring_model_2)
res.cool.spring_model_2 <- simulateResiduals(cool.spring_model_2)
plotQQunif(res.cool.spring_model_2)
plotResiduals(res.cool.spring_model_2)

# 3: AridityIndex + Duration + Lifeform + Perc_dev_since + PlantSource2 + Treatment + 
#   (1 | Site/Plot) + AridityIndex:Duration + AridityIndex:Perc_dev_since + AridityIndex:PlantSource2 +  
#   Duration:Perc_dev_since + Perc_dev_since:PlantSource2 + Perc_dev_since:Treatment
summary(cool.spring_model_3)
r2(cool.spring_model_3)
res.cool.spring_model_3 <- simulateResiduals(cool.spring_model_3)
plotQQunif(res.cool.spring_model_3)
plotResiduals(res.cool.spring_model_3)

# 4: Count ~ AridityIndex + Duration + Lifeform + Perc_dev_since + PlantSource2 + Treatment + 
#   (1 | Site/Plot) + AridityIndex:Duration + AridityIndex:PlantSource2 + 
#   Duration:Perc_dev_since + Perc_dev_since:PlantSource2 + Perc_dev_since:Treatment
summary(cool.spring_model_4)
r2(cool.spring_model_4)
res.cool.spring_model_4 <- simulateResiduals(cool.spring_model_4)
plotQQunif(res.cool.spring_model_4)
plotResiduals(res.cool.spring_model_4)

# 5: AridityIndex + Duration + Lifeform + Perc_dev_since + PlantSource2 + Treatment + 
#   (1 | Site/Plot) + AridityIndex:Duration +  AridityIndex:Perc_dev_since + 
#   AridityIndex:PlantSource2 +  AridityIndex:Treatment + Duration:Perc_dev_since + 
#   Perc_dev_since:PlantSource2 + Perc_dev_since:Treatment
summary(cool.spring_model_5)
r2(cool.spring_model_5)
res.cool.spring_model_5 <- simulateResiduals(cool.spring_model_5)
plotQQunif(res.cool.spring_model_5)
plotResiduals(res.cool.spring_model_5)

# 6: AridityIndex + Duration + Lifeform + Perc_dev_since + PlantSource2 + Treatment + 
#   (1 | Site/Plot) + AridityIndex:Duration + AridityIndex:PlantSource2 + 
#   Duration:Perc_dev_since + Lifeform:Perc_dev_since + Perc_dev_since:PlantSource2
summary(cool.spring_model_6)
r2(cool.spring_model_6)
res.cool.spring_model_6 <- simulateResiduals(cool.spring_model_6)
plotQQunif(res.cool.spring_model_6)
plotResiduals(res.cool.spring_model_6)

# 7: AridityIndex + Duration + Lifeform + Perc_dev_since + PlantSource2 + Treatment + 
#   (1 | Site/Plot) + AridityIndex:Duration + AridityIndex:PlantSource2 + 
#   Duration:Perc_dev_since + Lifeform:Perc_dev_since + Perc_dev_since:PlantSource2 + 
#   Perc_dev_since:Treatment
summary(cool.spring_model_7)
r2(cool.spring_model_7)
res.cool.spring_model_7 <- simulateResiduals(cool.spring_model_7)
plotQQunif(res.cool.spring_model_7)
plotResiduals(res.cool.spring_model_7)

# 8: AridityIndex + Duration + Lifeform + Perc_dev_since + PlantSource2 + Treatment + 
#   (1 | Site/Plot) + AridityIndex:Duration + AridityIndex:Perc_dev_since + 
#   AridityIndex:PlantSource2 + AridityIndex:Treatment + Duration:Perc_dev_since + 
#   Perc_dev_since:PlantSource2
summary(cool.spring_model_8)
r2(cool.spring_model_8)
res.cool.spring_model_8 <- simulateResiduals(cool.spring_model_8)
plotQQunif(res.cool.spring_model_8)
plotResiduals(res.cool.spring_model_8)

# 9: AridityIndex + Duration + Lifeform + Perc_dev_since + PlantSource2 + Treatment + 
#   (1 | Site/Plot) + AridityIndex:Duration + AridityIndex:PlantSource2 + 
#   AridityIndex:Treatment + Duration:Perc_dev_since + Perc_dev_since:PlantSource2 + 
#   Perc_dev_since:Treatment
summary(cool.spring_model_9)
r2(cool.spring_model_9)
res.cool.spring_model_9 <- simulateResiduals(cool.spring_model_9)
plotQQunif(res.cool.spring_model_9)
plotResiduals(res.cool.spring_model_9)

# 10: AridityIndex + Duration + Lifeform + Perc_dev_since + PlantSource2 + Treatment + 
#   (1 | Site/Plot) + AridityIndex:Duration + AridityIndex:PlantSource2 + 
#   AridityIndex:Treatment + Duration:Perc_dev_since + Perc_dev_since:PlantSource2
summary(cool.spring_model_10)
r2(cool.spring_model_10)
res.cool.spring_model_10 <- simulateResiduals(cool.spring_model_10)
plotQQunif(res.cool.spring_model_10)
plotResiduals(res.cool.spring_model_10)

# 11: AridityIndex + Duration + Lifeform + Perc_dev_since + PlantSource2 + Treatment + 
#   (1 | Site/Plot) + AridityIndex:Duration + AridityIndex:Perc_dev_since + 
#   AridityIndex:PlantSource2 + Duration:Perc_dev_since + Lifeform:Perc_dev_since + 
#   Perc_dev_since:PlantSource2 + Perc_dev_since:Treatment
summary(cool.spring_model_11)
r2(cool.spring_model_11)
res.cool.spring_model_11 <- simulateResiduals(cool.spring_model_11)
plotQQunif(res.cool.spring_model_11)
plotResiduals(res.cool.spring_model_11)

# 12: AridityIndex + Duration + Lifeform + Perc_dev_since + PlantSource2 + Treatment + 
#   (1 | Site/Plot) + AridityIndex:Duration + AridityIndex:PlantSource2 + 
#   AridityIndex:Treatment + Duration:Perc_dev_since + Lifeform:Perc_dev_since + 
#   Perc_dev_since:PlantSource2 + Perc_dev_since:Treatment
summary(cool.spring_model_12)
r2(cool.spring_model_12)
res.cool.spring_model_12 <- simulateResiduals(cool.spring_model_12)
plotQQunif(res.cool.spring_model_12)
plotResiduals(res.cool.spring_model_12)




## Cool-Spring model averaging --------------------------------------------

# Average top models
cool.spring_averaged <- model.avg(cool.spring_model_1, cool.spring_model_2,
                                cool.spring_model_3, cool.spring_model_4,
                                cool.spring_model_5, cool.spring_model_6,
                                cool.spring_model_7, cool.spring_model_8,
                                cool.spring_model_9, cool.spring_model_10,
                                cool.spring_model_11, cool.spring_model_12) 

summary(cool.spring_averaged)





# Warm-Fall ---------------------------------------------------------------

## Global model -----------------------------------------------------------

# All variables 
warm.fall_full <- glmmTMB(Count ~ Perc_dev_since + Treatment + PlantSource2 +
                              Duration + Lifeform + AridityIndex + MAT +
                              Perc_dev_since * Treatment +
                              Perc_dev_since * PlantSource2 + 
                              Perc_dev_since * Duration + 
                              Perc_dev_since * Lifeform +
                              Perc_dev_since * AridityIndex +
                              Perc_dev_since * MAT +
                              (1 | Site / Plot),
                            data = warm.fall,
                            family = nbinom2)
summary(warm.fall_full)
r2(warm.fall_full) # can't compute
res.warm.fall_full <- simulateResiduals(warm.fall_full)
plotQQunif(res.warm.fall_full)
plotResiduals(res.warm.fall_full)



## Warm-Fall top models ---------------------------------------------------

# Model selection on UA HPC (do not run)
#   options(na.action = "na.fail")
#   warm.fall_set <- dredge(warm.fall_full)

# Examine top model
warm.fall_best_model <- get.models(warm.fall_set, 1)[[1]]
summary(warm.fall_best_model)

# Examine models within top 2 AICc units
warm.fall_top_models <- subset(warm.fall_set, delta < 2) |> 
  filter(!is.na(delta)) 

# Assign each model to a separate object
for (i in 1:nrow(warm.fall_top_models)) {
  assign(paste0("warm.fall_model_", i), get.models(warm.fall_top_models, subset = i)[[1]])
}

# 1: Lifeform + Perc_dev_since + PlantSource2 + 
#   (1 | Site/Plot) + Lifeform:Perc_dev_since
summary(warm.fall_model_1)
r2(warm.fall_model_1) # can't compute
res.warm.fall_model_1 <- simulateResiduals(warm.fall_model_1)
plotQQunif(res.warm.fall_model_1)
plotResiduals(res.warm.fall_model_1)


# 2: AridityIndex + Lifeform + Perc_dev_since + PlantSource2 +  
#   (1 | Site/Plot) + AridityIndex:Perc_dev_since + Lifeform:Perc_dev_since + Perc_dev_since:PlantSource2
summary(warm.fall_model_2)
r2(warm.fall_model_2) # can't compute
res.warm.fall_model_2 <- simulateResiduals(warm.fall_model_2)
plotQQunif(res.warm.fall_model_2)
plotResiduals(res.warm.fall_model_2)


# 3: Duration + Lifeform + Perc_dev_since + PlantSource2 +  
#     (1 | Site/Plot) + Lifeform:Perc_dev_since + Perc_dev_since:PlantSource2
summary(warm.fall_model_3)
r2(warm.fall_model_3)
res.warm.fall_model_3 <- simulateResiduals(warm.fall_model_3)
plotQQunif(res.warm.fall_model_3)
plotResiduals(res.warm.fall_model_3)

# 4: Duration + Lifeform + Perc_dev_since + PlantSource2 + 
#   (1 | Site/Plot) + Lifeform:Perc_dev_since
summary(warm.fall_model_4)
r2(warm.fall_model_4)
res.warm.fall_model_4 <- simulateResiduals(warm.fall_model_4)
plotQQunif(res.warm.fall_model_4)
plotResiduals(res.warm.fall_model_4)

# 5: AridityIndex + Duration + Lifeform + Perc_dev_since + PlantSource2 + 
#   (1 | Site/Plot) + AridityIndex:Perc_dev_since + Lifeform:Perc_dev_since + 
#   Perc_dev_since:PlantSource2
summary(warm.fall_model_5)
r2(warm.fall_model_5) # can't compute
res.warm.fall_model_5 <- simulateResiduals(warm.fall_model_5)
plotQQunif(res.warm.fall_model_5)
plotResiduals(res.warm.fall_model_5)

# 6: AridityIndex + Lifeform + Perc_dev_since + PlantSource2 +  
#   (1 | Site/Plot) + Lifeform:Perc_dev_since
summary(warm.fall_model_6)
r2(warm.fall_model_6) # can't compute
res.warm.fall_model_6 <- simulateResiduals(warm.fall_model_6)
plotQQunif(res.warm.fall_model_6)
plotResiduals(res.warm.fall_model_6)

# 7: AridityIndex + Lifeform + Perc_dev_since + PlantSource2 +  
#   (1 | Site/Plot) + AridityIndex:Perc_dev_since + Lifeform:Perc_dev_since
summary(warm.fall_model_7)
r2(warm.fall_model_7) # can't compute
res.warm.fall_model_7 <- simulateResiduals(warm.fall_model_7)
plotQQunif(res.warm.fall_model_7)
plotResiduals(res.warm.fall_model_7)

# 8: Lifeform + Perc_dev_since + PlantSource2 + 
#   (1 | Site/Plot) + Lifeform:Perc_dev_since + Perc_dev_since:PlantSource2
summary(warm.fall_model_8)
r2(warm.fall_model_8)
res.warm.fall_model_8 <- simulateResiduals(warm.fall_model_8)
plotQQunif(res.warm.fall_model_8)
plotResiduals(res.warm.fall_model_8)

# 9: AridityIndex + Lifeform + Perc_dev_since + PlantSource2 +  
#   (1 | Site/Plot) + AridityIndex:Perc_dev_since + AridityIndex:PlantSource2 +      
#   Lifeform:Perc_dev_since
summary(warm.fall_model_9)
r2(warm.fall_model_9) # can't compute
res.warm.fall_model_9 <- simulateResiduals(warm.fall_model_9)
plotQQunif(res.warm.fall_model_9)
plotResiduals(res.warm.fall_model_9)

# 10: AridityIndex + Lifeform + Perc_dev_since + PlantSource2 +  
#   (1 | Site/Plot) + Lifeform:Perc_dev_since + Perc_dev_since:PlantSource2
summary(warm.fall_model_10)
r2(warm.fall_model_10)
res.warm.fall_model_10 <- simulateResiduals(warm.fall_model_10)
plotQQunif(res.warm.fall_model_10)
plotResiduals(res.warm.fall_model_10)

# 11: AridityIndex + Duration + Lifeform + Perc_dev_since + PlantSource2 + 
#   (1 | Site/Plot) + Lifeform:Perc_dev_since + Perc_dev_since:PlantSource2
summary(warm.fall_model_11)
r2(warm.fall_model_11)
res.warm.fall_model_11 <- simulateResiduals(warm.fall_model_11)
plotQQunif(res.warm.fall_model_11)
plotResiduals(res.warm.fall_model_11)



## Warm-Fall model averaging ----------------------------------------------

# Average top models
warm.fall_averaged <- model.avg(warm.fall_model_1, warm.fall_model_2,
                                warm.fall_model_3, warm.fall_model_4,
                                warm.fall_model_5, warm.fall_model_6,
                                warm.fall_model_7, warm.fall_model_8,
                                warm.fall_model_9, warm.fall_model_10,
                                warm.fall_model_11) 

summary(warm.fall_averaged)



# Year-All ----------------------------------------------------------------

# All variables 
#   Does not include AridityIndex because that was correlated with Perc_dev_since
year.all_full <- glmmTMB(Count ~ Perc_dev_since + Treatment + PlantSource2 +
                            Duration + Lifeform + 
                            Perc_dev_since * Treatment +
                            Perc_dev_since * PlantSource2 + 
                            Perc_dev_since * Duration + 
                            Perc_dev_since * Lifeform +
                            (1 | Site / Plot),
                          data = year.all,
                          family = nbinom2)
summary(year.all_full)
r2(year.all_full)
res.year.all_full <- simulateResiduals(year.all_full)
plotQQunif(res.year.all_full)
plotResiduals(res.year.all_full)


## Year-All top models ----------------------------------------------------

# Model selection on UA HPC (do not run)
#   options(na.action = "na.fail")
#   year.all_set <- dredge(year.all_full)

# Examine top model
year.all_best_model <- get.models(year.all_set, 1)[[1]]
summary(year.all_best_model)

# Examine models within top 2 AICc units
year.all_top_models <- subset(year.all_set, delta < 2) |> 
  filter(!is.na(delta)) 

# Assign each model to a separate object
for (i in 1:nrow(year.all_top_models)) {
  assign(paste0("year.all_model_", i), get.models(year.all_top_models, subset = i)[[1]])
}


# 1: Duration + Lifeform + Perc_dev_since + PlantSource2 + Treatment + 
#     (1 | Site/Plot) + Perc_dev_since:PlantSource2
summary(year.all_model_1)
r2(year.all_model_1)
res.year.all_model_1 <- simulateResiduals(year.all_model_1)
plotQQunif(res.year.all_model_1)
plotResiduals(res.year.all_model_1)


# 2: Duration + Lifeform + Perc_dev_since + PlantSource2 + Treatment + 
#   (1 | Site/Plot) + Lifeform:Perc_dev_since + Perc_dev_since:PlantSource2
summary(year.all_model_2)
r2(year.all_model_2)
res.year.all_model_2 <- simulateResiduals(year.all_model_2)
plotQQunif(res.year.all_model_2)
plotResiduals(res.year.all_model_2)



## Year-All model averaging -----------------------------------------------

# Average top models
year.all_averaged <- model.avg(year.all_model_1, year.all_model_2) 

summary(year.all_averaged)


save.image("RData/16_generalized-linear-models_Sonoran-seasonality.RData")
