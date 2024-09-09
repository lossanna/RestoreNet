# Created: 2024-08-22
# Last updated: 2024-08-22

# Purpose: Examine subplot trends (with added 0s to track seeded species) as they relate to precip.

library(tidyverse)
library(scales)
library(viridis)
library(ggbreak)
library(ggpmisc)

# Load data ---------------------------------------------------------------

subplot <- read_csv("data/cleaned/04.15_subplot-data_clean-0-added.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")


# Data wrangling ----------------------------------------------------------

# Check for NAs
apply(subplot, 2, anyNA)
count.na <- subplot |> 
  filter(is.na(Count))
height.na <- subplot |> 
  filter(is.na(Height)) # some have no Height recorded but do have Count

# Reorganize columns for left_join()
cum.pd.subplot <- cum.pd |> 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Perc_deviation, Deviation_mm) |> 
  rename(Perc_dev_cum = Perc_deviation,
         Dev_mm_cum = Deviation_mm)

# Combine all variables
dat <- subplot |> 
  left_join(prism.data) |> 
  left_join(ai) |> 
  left_join(cum.pd.subplot) 

# Check for NAs
apply(dat, 2, anyNA)


# Reorder PlotMix_Climate
dat$PlotMix_Climate <- factor(dat$PlotMix_Climate,
                                 levels = c("None", "Current", "Projected"))

# Data without Infinity
#   Inf created when there was no rain in the time period (can't divide by 0),
#     but this only occurs when the time period is small and in all cases there is another
#     monitoring date less than 2 weeks away with a non-Inf percent change. Occurs once
#     at CO Plateau (BarTBar).
dat.noInf <- dat |> 
  filter(Perc_dev_cum != Inf)

# Data without percent deviation outliers for cumulative
dat2 <- dat |> 
  filter(Perc_dev_cum < 3)



# 2024-08 draft figures ---------------------------------------------------

## Density by Sonoran Desert, N AZ, Utah ----------------------------------

# Includes all treatments, outliers not removed

# Sonoran Desert
sonoran.des <- dat.noInf |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")  +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.des
sonoran.weed <- dat.noInf |> 
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, weedy species")  +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")  +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.weed


# Northern AZ (CO Plateau)
naz.des <- dat.noInf |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Northern Arizona Plateau, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")  +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.des
naz.weed <- dat.noInf |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Northern Arizona Plateau, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")  +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.weed


# Utah
utah.des <- dat.noInf |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Utah") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Utah-Colorado Plateau, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")  +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
utah.des
utah.weed <- dat.noInf |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Utah") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Utah-Colorado Plateau, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")  +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
utah.weed


save.image("RData/06.15_exploratory-graphs_subplot-0-added.RData")


