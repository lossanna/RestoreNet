# Created: 2025-10-15
# Last updated: 2025-10-15

# Purpose: Examine distributions, outliers, and variable relationships for continuous
#   explanatory variables (applies to both subplot and 2x2 data response variables).

# There isn't much difference between the weedy and desirable data sets, or the full set (except for the
#   huge Perc_dev_cum outlier).
# Square-root transformation seemed to help normalize Cum_precip and Since_last_precip.
# Log transformation seemed to help normalize AridityIndex.

library(tidyverse)
library(GGally)
library(ggpubr)

# Load data ---------------------------------------------------------------

subplot.raw <- read_csv("Sonoran-data/cleaned/04.1_subplot-data_clean.csv")
prism.data <- read_csv("Sonoran-data/cleaned/03.1_monitoring-events-with-PRISM-climate-data_clean.csv")
since.pd <- read_csv("Sonoran-data/cleaned/03.2_since-last-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("Sonoran-data/cleaned/03.3_aridity-index-values_clean.csv")


# Data wrangling ----------------------------------------------------------

# Reorganize columns for left_join()
since.pd.subplot <- since.pd %>% 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Perc_deviation, Deviation_mm) %>% 
  rename(Perc_dev_since = Perc_deviation,
         Dev_mm_since = Deviation_mm)

# Combine all variables
subplot <- subplot.raw %>% 
  left_join(prism.data) %>% 
  left_join(ai) %>% 
  left_join(since.pd.subplot)

# Check for NAs
apply(subplot, 2, anyNA)

# Separate out continuous variables
pairs.cont <- subplot %>% 
  select(Region, Perc_dev_since, AridityIndex, MAT, MAP, Since_last_precip, Elevation_ft) 
pairs.cont <- pairs.cont %>% 
  distinct(.keep_all = TRUE)


# Since_last_precip & Perc_dev_since --------------------------------------

# Perc_dev_since
hist(pairs.cont$Perc_dev_since, breaks = 20) # not normally distributed
dotchart(pairs.cont$Perc_dev_since)
qqnorm(pairs.cont$Perc_dev_since)
qqline(pairs.cont$Perc_dev_since)

# Since_last_precip
hist(pairs.cont$Since_last_precip, breaks = 20)
dotchart(pairs.cont$Since_last_precip)
qqnorm(pairs.cont$Since_last_precip)
qqline(pairs.cont$Since_last_precip)
