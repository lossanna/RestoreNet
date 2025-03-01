# Created: 2024-12-12
# Last updated: 2024-12-12

# Purpose: Look at correlations between continuous variables by seasonality. MAP & MAT should be dropped
#   from all; AridityIndex should be dropped from year.all (flowers year-round, monitoring from both seasons).

library(tidyverse)
library(GGally)

# Load data ---------------------------------------------------------------

sonoran.subplot.raw <- read_csv("data/cleaned/14.1_Sonoran-Desert_subplot_with-seasonality_clean.csv")
since.pd <- read_csv("data/cleaned/03.3_since-last-precip_percent-deviation-from-norm_clean.csv")
sonoran.monitor <- read_csv("data/cleaned/14.2_Sonoran-Desert_monitoring-events.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")

# Data wrangling ----------------------------------------------------------

## Combine ----------------------------------------------------------------

# Reorganize columns for left_join()
since.pd.join <- since.pd |> 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Perc_deviation, Deviation_mm) |> 
  rename(Perc_dev_since = Perc_deviation,
         Dev_mm_since = Deviation_mm)

# Combine all variables
dat <- sonoran.subplot.raw |> 
  left_join(sonoran.monitor) |> 
  left_join(prism.data) |> 
  left_join(ai) |> 
  left_join(since.pd.join)

# Separate datasets by seasonality
cool.spring <- dat |> 
  filter(Seasonality == "Cool") |> 
  filter(Monitor_season == "Spring")

warm.fall <- dat |> 
  filter(Seasonality == "Warm") |> 
  filter(Monitor_season == "Fall")

year.all <- dat |> 
  filter(Seasonality %in% c("Year round", "Unknown"))


# Histogram ---------------------------------------------------------------

hist(cool.spring$Count, breaks = 50)
hist(warm.fall$Count)
hist(year.all$Count)


# Correlation between continuous variables --------------------------------

# All Sonoran sites
dat |> 
  select(Perc_dev_since, AridityIndex, MAT, MAP) |>
  distinct(.keep_all = TRUE) |>
  ggpairs() # MAT & AI correlated; MAP & AridityIndex correlated; MAT & MAP correlated

# Cool-Spring
cool.spring |> 
  select(Perc_dev_since, AridityIndex, MAT, MAP) |>
  distinct(.keep_all = TRUE) |>
  ggpairs()

# Warm-Fall
warm.fall  |> 
  select(Perc_dev_since, AridityIndex, MAT, MAP) |>
  distinct(.keep_all = TRUE) |>
  ggpairs()

# Year-All
year.all  |> 
  select(Perc_dev_since, AridityIndex, MAT, MAP) |>
  distinct(.keep_all = TRUE) |>
  ggpairs() # AridityIndex correlated with Perc_dev_since
