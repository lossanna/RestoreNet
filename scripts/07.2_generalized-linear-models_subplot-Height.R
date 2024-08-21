# Created: 2024-07-30
# Last updated: 2024-07-30

# Purpose: Run generalized linear models for subplot data, with Height as response variable. 
#   Check for overdispersion and zero-inflation.

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