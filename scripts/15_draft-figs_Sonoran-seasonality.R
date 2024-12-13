# Created: 2024-12-12
# Last updated: 2024-12-12

# Purpose:

library(tidyverse)
library(scales)
library(viridis)
library(ggbreak)
library(ggpmisc)
library(ggpubr)

# Load data ---------------------------------------------------------------

sonoran.subplot.raw <- read_csv("data/cleaned/14.1_Sonoran-Desert_subplot_with-seasonality_clean.csv")
since.pd <- read_csv("data/cleaned/03.3_since-last-precip_percent-deviation-from-norm_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
sonoran.present_species.raw <- read_csv("data/cleaned/14.1_Sonoran-Desert_present-species_with-seasonality_clean.csv")
sonoran.monitor <- read_csv("data/cleaned/14.2_Sonoran-Desert_monitoring-events.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")

# Data wrangling ----------------------------------------------------------

## Combine ----------------------------------------------------------------

# Reorganize columns for left_join()
cum.pd.join <- cum.pd |> 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Perc_deviation, Deviation_mm) |> 
  rename(Perc_dev_cum = Perc_deviation,
         Dev_mm_cum = Deviation_mm)

since.pd.join <- since.pd |> 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Perc_deviation, Deviation_mm) |> 
  rename(Perc_dev_since = Perc_deviation,
         Dev_mm_since = Deviation_mm)

# Combine all variables
dat <- sonoran.subplot.raw |> 
  left_join(sonoran.monitor) |> 
  left_join(prism.data) |> 
  left_join(ai) |> 
  left_join(cum.pd.join) |> 
  left_join(since.pd.join)

# Add Density column
dat <- dat |> 
  mutate(Density = Count / (0.25 * 0.25))

# Separate datasets by seasonality
cool.spring <- dat |> 
  filter(Seasonality == "Cool") |> 
  filter(Monitor_season == "Spring")

warm.fall <- dat |> 
  filter(Seasonality == "Warm") |> 
  filter(Monitor_season == "Fall")

year.all <- dat |> 
  filter(Seasonality %in% c("Year round", "Unknown"))


# Density -----------------------------------------------------------------

# Cool-Spring by PlantSource2
cool.spring.count <- cool.spring |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Sonoran Desert cool season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)  
cool.spring.count
