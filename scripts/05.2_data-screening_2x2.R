# Created: 2024-05-29
# Last updated: 2024-05-29

# Purpose: Examine distributions, outliers, and variable relationships for 2x2 data response
#   variables, Seeded Cover and Total Cover.

library(tidyverse)
library(GGally)

# Load data ---------------------------------------------------------------

p2x2.rich.cover.raw <- read_csv("data/cleaned/04.2_2x2-richness-cover_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")
