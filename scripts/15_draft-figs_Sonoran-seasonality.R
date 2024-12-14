# Created: 2024-12-12
# Last updated: 2024-12-13

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
sonoran.present_species.raw <- read_csv("data/cleaned/14.1_Sonoran-Desert_present-species_with-seasonality_clean.csv")
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


# Cool-Spring -------------------------------------------------------------

# Cool-Spring by Perc_dev_since and PlantSource2
cool.spring.since.plantsource <- cool.spring |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Sonoran Desert cool season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)  
cool.spring.since.plantsource

cool.spring.since.plantsource.wrap <- cool.spring |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Sonoran Desert cool season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~PlantSource2)
cool.spring.since.plantsource.wrap

# Cool-Spring by Perc_dev_since and Duration
cool.spring.since.duration <- cool.spring |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  labs(title = "Sonoran Desert cool season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)  
cool.spring.since.duration

cool.spring.since.duration.wrap <- cool.spring |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  labs(title = "Sonoran Desert cool season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~Duration)
cool.spring.since.duration.wrap


# Cool-Spring by AridityIndex and PlantSource2
cool.spring.ai.plantsource <- cool.spring |> 
  ggplot(aes(x = AridityIndex, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Sonoran Desert cool season plants",
       x = "Aridity index",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) 
cool.spring.ai.plantsource

cool.spring.ai.plantsource.wrap <- cool.spring |> 
  ggplot(aes(x = AridityIndex, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Sonoran Desert cool season plants",
       x = "Aridity index",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  facet_wrap(~PlantSource2)
cool.spring.ai.plantsource.wrap


# Cool-Spring by AridityIndex and Duration
cool.spring.ai.duration <- cool.spring |> 
  ggplot(aes(x = AridityIndex, y = Density)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  labs(title = "Sonoran Desert cool season plants",
       x = "Aridity index",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) 
cool.spring.ai.duration

cool.spring.ai.duration.wrap <- cool.spring |> 
  ggplot(aes(x = AridityIndex, y = Density)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  labs(title = "Sonoran Desert cool season plants",
       x = "Aridity index",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  facet_wrap(~Duration)
cool.spring.ai.duration.wrap


# Cool-Spring by Perc_dev_since and AridityIndex
cool.spring.since.ai <- cool.spring |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = AridityIndex),
             alpha = 0.7) +
  labs(title = "Sonoran Desert cool season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)  
cool.spring.since.ai



# Warm-Fall ---------------------------------------------------------------

# Warm-Fall by Perc_dev_since and Lifeform
warm.fall.since.lifeform <- warm.fall |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  labs(title = "Sonoran Desert warm season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)  
warm.fall.since.lifeform

warm.fall.since.lifeform.wrap <- warm.fall |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  labs(title = "Sonoran Desert warm season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~Lifeform)
warm.fall.since.lifeform.wrap

# Warm-Fall by Perc_dev_since and PlantSource2
warm.fall.since.plantsource <- warm.fall |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Sonoran Desert warm season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)  
warm.fall.since.plantsource

warm.fall.since.plantsource.wrap <- warm.fall |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Sonoran Desert warm season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~PlantSource2)
warm.fall.since.plantsource.wrap


# Warm-Fall by Perc_dev_since and AridityIndex
warm.fall.since.ai <- warm.fall |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = AridityIndex),
             alpha = 0.7) +
  labs(title = "Sonoran Desert warm season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)  
warm.fall.since.ai



# Year-All ----------------------------------------------------------------

# Year-All by Perc_dev_since and PlantSource2
year.all.since.plantsource <- year.all |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Sonoran Desert all-season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "dodgerblue4")) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)  
year.all.since.plantsource

year.all.since.plantsource.wrap <- year.all |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Sonoran Desert all-season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "dodgerblue4")) +
  facet_wrap(~PlantSource2)
year.all.since.plantsource.wrap


# Year-All by Perc_dev_since and Lifeform
year.all.since.lifeform <- year.all |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  labs(title = "Sonoran Desert all-season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)  
year.all.since.lifeform

year.all.since.lifeform.wrap <- year.all |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  labs(title = "Sonoran Desert all-season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~Lifeform)
year.all.since.lifeform.wrap


save.image("RData/15_draft-figs_Sonoran-seasonality.RData")
