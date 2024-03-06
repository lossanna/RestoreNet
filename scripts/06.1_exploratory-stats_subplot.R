# Created: 2024-03-06
# Last updated: 2024-03-06

# Purpose: Begin to examine subplot trneds as they relate to precip.

library(tidyverse)

# Load data ---------------------------------------------------------------

subplot <- read_csv("data/cleaned/04.1_subplot-data_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.cv <- read_csv("data/cleaned/03.3_cumulative-precip_CV_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
since.cv <- read_csv("data/cleaned/03.3_since-last-precip_CV_clean.csv")
since.pd <- read_csv("data/cleaned/03.3_since-last-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")


# Data wrangling ----------------------------------------------------------

# Check for NAs
apply(subplot, 2, anyNA)
count.na <- subplot |> 
  filter(is.na(Count))
height.na <- subplot |> 
  filter(is.na(Height))

# Remove ones with NAs for now until I can look at raw data sheets and correct them
subplot <- subplot |> 
  filter(!is.na(Count)) |> 
  filter(!is.na(Height))
