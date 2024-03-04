# Created: 2024-02-27
# Last updated: 2024-02-27

# Purpose: Begin to examine 2x2 trends as they relate to precip.

library(tidyverse)

# Load data ---------------------------------------------------------------

richness.cover <- read_csv("data/cleaned/04.2_2x2-richness-cover_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.cv <- read_csv("data/cleaned/03.3_cumulative-precip_CV_clean.csv")
cum.pc <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
since.cv <- read_csv("data/cleaned/03.3_since-last-precip_CV_clean.csv")
since.pc <- read_csv("data/cleaned/03.3_since-last-precip_percent-deviation-from-norm_clean.csv")


# Data wrangling ----------------------------------------------------------

# Check for NAs
apply(richness.cover, 2, anyNA)
seeded.cover.na <- richness.cover |> 
  filter(is.na(Seeded_Cover))
total.cover.na <- richness.cover |> 
  filter(is.na(Total_Veg_Cover))
not.seeded.na <- richness.cover |> 
  filter(is.na(Not_Seeded_Cover))

# Remove ones with NAs for now until I can look at raw data sheets and correct them'
richness.cover <- richness.cover |> 
  filter(!SiteDatePlotID %in% not.seeded.na$SiteDatePlotID)

# Remove monitoring events not included in 2x2 data
prism.data.2x2 <- prism.data |> 
  filter(SiteDateID %in% richness.cover$SiteDateID)
cum.pc.2x2 <- cum.pc |> 
  filter(SiteDateID %in% richness.cover$SiteDateID) |> 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, perc_change) |> 
  rename(cum_perc_dev = perc_change)
since.pc.2x2 <- since.pc |> 
  filter(SiteDateID %in% richness.cover$SiteDateID) |> 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, perc_change) |> 
  rename(since_perc_dev = perc_change)

# Combine
dat <- richness.cover |> 
  left_join(prism.data.2x2) |> 
  left_join(cum.pc.2x2) |> 
  left_join(since.pc.2x2) |> 
  left_join(cum.cv) |> 
  left_join(since.cv)

# Check for NAs
apply(dat, 2, anyNA)

precip.na <- dat |> 
  filter(is.na(Cum_precip))


# Visualize linear relationships ------------------------------------------

# SRER
dat |> 
  ggplot(aes(x = since_perc_dev, y = Seeded_Cover)) +
  geom_point() +
  geom_line()
