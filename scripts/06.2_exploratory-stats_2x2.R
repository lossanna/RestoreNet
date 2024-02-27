# Created: 2024-02-27
# Last updated: 2024-02-27

# Purpose: Begin to examine 2x2 trends as they relate to precip.

library(tidyverse)

# Load data ---------------------------------------------------------------

richness.cover <- read_csv("data/cleaned/04.2_p2x2-richness-cover_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")


# Data wrangling ----------------------------------------------------------

# Remove monitoring events not included in 2x2 data
prism.data.2x2 <- prism.data |> 
  filter(SiteDateID %in% richness.cover$SiteDateID)

# Combine
dat <- left_join(richness.cover, prism.data.2x2)



# Visualize linear relationships ------------------------------------------

# SRER
dat |> 
  filter(Site == "SRER") |> 
  ggplot(aes(x = Cum_precip, y = Seeded_Cover)) +
  geom_point()
