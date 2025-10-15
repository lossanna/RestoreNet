# Created: 2025-04-01
# Last updated: 2025-10-06

# Purpose: Examine distributions, outliers, and variable relationships for Count response variable.

# Determine if the relationship between Count and precip variables is linear.

library(tidyverse)
library(GGally)

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

# 
