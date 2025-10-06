# Created: 2024-12-12
# Last updated: 2024-12-13

# Purpose: Create draft figures for Sonoran-only analysis with seasonality.

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




# Precipitation conditions ------------------------------------------------

since.pd |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  left_join(sonoran.monitor) |> 
  ggplot(aes(x = Date_Monitored, y = Perc_deviation)) +
  geom_point(aes(color = Monitor_season, shape = Monitor_season),
             size = 2) +
  geom_line(aes(color = Monitor_season)) +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")



# Cool-Spring -------------------------------------------------------------

## Cool-Spring by Perc_dev_since ------------------------------------------

# Cool-Spring by Perc_dev_since and PlantSource2
cool.spring.since.plantsource <- cool.spring |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Cool season plants",
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
  labs(title = "Cool season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~PlantSource2) 
cool.spring.since.plantsource.wrap

tiff("figures/2024-12_draft-figures/Cool-Spring_precip-dev-plantsource2.tiff", units = "in", height = 4, width = 7, res = 150)
cool.spring.since.plantsource.wrap
dev.off()


# Cool-Spring by Perc_dev_since and Duration
cool.spring.since.duration <- cool.spring |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  labs(title = "Cool season plants",
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
  labs(title = "Cool season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~Duration)
cool.spring.since.duration.wrap

tiff("figures/2024-12_draft-figures/Cool-Spring_precip-dev-duration.tiff", units = "in", height = 4, width = 7, res = 150)
cool.spring.since.duration.wrap
dev.off()


# Cool-Spring by Perc_dev_since and Lifeform
cool.spring.since.lifeform <- cool.spring |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  labs(title = "Cool season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#E7298A", "#66A61E", "#E6AB02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)  
cool.spring.since.lifeform

cool.spring.since.lifeform.wrap <- cool.spring |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  labs(title = "Cool season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#E7298A", "#66A61E", "#E6AB02")) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~Lifeform)
cool.spring.since.lifeform.wrap

tiff("figures/2024-12_draft-figures/Cool-Spring_precip-dev-lifeform.tiff", units = "in", height = 4, width = 7, res = 150)
cool.spring.since.lifeform.wrap
dev.off()


# Cool-Spring by Perc_dev_since and Duration + PlantSource2
cool.spring.since.duration.plantsource2 <- cool.spring |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Cool season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  facet_wrap(~Duration)
cool.spring.since.duration.plantsource2

tiff("figures/2024-12_draft-figures/Cool-Spring_precip-dev-duration-plantsource2.tiff", units = "in", height = 4, width = 7, res = 150)
cool.spring.since.duration.plantsource2
dev.off()



## Cool-Spring by AridityIndex --------------------------------------------

# Cool-Spring by AridityIndex and PlantSource2
cool.spring.ai.plantsource <- cool.spring |> 
  ggplot(aes(x = AridityIndex, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Cool season plants",
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
  labs(title = "Cool season plants",
       x = "Aridity index",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  facet_wrap(~PlantSource2)
cool.spring.ai.plantsource.wrap

tiff("figures/2024-12_draft-figures/Cool-Spring_aridity-plantsource2.tiff", units = "in", height = 4, width = 7, res = 150)
cool.spring.ai.plantsource.wrap
dev.off()


# Cool-Spring by AridityIndex and Duration
cool.spring.ai.duration <- cool.spring |> 
  ggplot(aes(x = AridityIndex, y = Density)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  labs(title = "Cool season plants",
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
  labs(title = "Cool season plants",
       x = "Aridity index",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~Duration)
cool.spring.ai.duration.wrap

tiff("figures/2024-12_draft-figures/Cool-Spring_aridity-duration.tiff", units = "in", height = 4, width = 7, res = 150)
cool.spring.ai.duration.wrap
dev.off()


## Cool-Spring other ------------------------------------------------------

# Cool-Spring by Perc_dev_since and AridityIndex
cool.spring.since.ai <- cool.spring |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = AridityIndex),
             alpha = 0.7) +
  labs(title = "Cool season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)  
cool.spring.since.ai

tiff("figures/2024-12_draft-figures/Cool-Spring_precip-dev-aridity.tiff", units = "in", height = 4, width = 6, res = 150)
cool.spring.since.ai
dev.off()



# Warm-Fall ---------------------------------------------------------------

## Warm-Fall by Perc_dev_since --------------------------------------------

# Warm-Fall by Perc_dev_since and PlantSource2
warm.fall.since.plantsource <- warm.fall |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Warm season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)  
warm.fall.since.plantsource

warm.fall.since.plantsource.wrap <- warm.fall |> 
  filter(PlantSource2 != "Recruit") |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Warm season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~PlantSource2)
warm.fall.since.plantsource.wrap

tiff("figures/2024-12_draft-figures/Warm-Fall_precip-dev-plantsource2.tiff", units = "in", height = 4, width = 7, res = 150)
warm.fall.since.plantsource.wrap
dev.off()


# Warm-Fall by Perc_dev_since and Duration
warm.fall.since.duration <- warm.fall |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  labs(title = "Warm season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)  
warm.fall.since.duration

warm.fall.since.duration.wrap <- warm.fall |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  labs(title = "Warm season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~Duration)
warm.fall.since.duration.wrap

tiff("figures/2024-12_draft-figures/Warm-Fall_precip-dev-duration.tiff", units = "in", height = 4, width = 7, res = 150)
warm.fall.since.duration.wrap
dev.off()



# Warm-Fall by Perc_dev_since and Lifeform
warm.fall.since.lifeform <- warm.fall |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  labs(title = "Warm season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#E7298A", "#66A61E", "#E6AB02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)  
warm.fall.since.lifeform

warm.fall.since.lifeform.wrap <- warm.fall |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  labs(title = "Warm season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#E7298A", "#66A61E", "#E6AB02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~Lifeform)
warm.fall.since.lifeform.wrap

tiff("figures/2024-12_draft-figures/Warm-Fall_precip-dev-lifeform.tiff", units = "in", height = 4, width = 7, res = 150)
warm.fall.since.lifeform.wrap
dev.off()


# Warm-Fall by Perc_dev_since and Duration + PlantSource2
warm.fall.since.duration.plantsource2 <- warm.fall |> 
  filter(PlantSource2 != "Recruit") |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Warm season plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  facet_wrap(~Duration)
warm.fall.since.duration.plantsource2

tiff("figures/2024-12_draft-figures/Warm-Fall_precip-dev-duration-plantsource2.tiff", units = "in", height = 4, width = 7, res = 150)
warm.fall.since.duration.plantsource2
dev.off()



## Warm-Fall by AridityIndex ----------------------------------------------

# Warm-Fall by AridityIndex and PlantSource2
warm.fall.ai.plantsource <- warm.fall |> 
  ggplot(aes(x = AridityIndex, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Cool season plants",
       x = "Aridity index",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) 
warm.fall.ai.plantsource

warm.fall.ai.plantsource.wrap <- warm.fall |> 
  filter(PlantSource2 != "Recruit") |> 
  ggplot(aes(x = AridityIndex, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Warm season plants",
       x = "Aridity index",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  facet_wrap(~PlantSource2)
warm.fall.ai.plantsource.wrap

tiff("figures/2024-12_draft-figures/Warm-Fall_aridity-plantsource2.tiff", units = "in", height = 4, width = 7, res = 150)
warm.fall.ai.plantsource.wrap
dev.off()


## Warm-Fall other --------------------------------------------------------

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

## Year-All by Perc_dev_since ---------------------------------------------

# Year-All by Perc_dev_since and PlantSource2
year.all.since.plantsource <- year.all |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  labs(title = "Year-round and unknown plants",
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
  labs(title = "Year-round and unknown plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "dodgerblue4")) +
  facet_wrap(~PlantSource2)
year.all.since.plantsource.wrap

tiff("figures/2024-12_draft-figures/Year-All_precip-dev-plantsource2.tiff", units = "in", height = 4, width = 7, res = 150)
year.all.since.plantsource.wrap
dev.off()


# Year-All by Perc_dev_since and Duration
year.all.since.duration <- year.all |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  labs(title = "Year-round and unknown plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)  
year.all.since.duration

year.all.since.duration.wrap <- year.all |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  labs(title = "Year-round and unknown plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~Duration)
year.all.since.duration.wrap

tiff("figures/2024-12_draft-figures/Year-All_precip-dev-duration.tiff", units = "in", height = 4, width = 7, res = 150)
year.all.since.duration.wrap
dev.off()


# Year-All by Perc_dev_since and Lifeform
year.all.since.lifeform <- year.all |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  labs(title = "Year-round and unknown plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#E7298A", "#66A61E", "#E6AB02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)  
year.all.since.lifeform

year.all.since.lifeform.wrap <- year.all |> 
  ggplot(aes(x = Perc_dev_since, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  labs(title = "Year-round and unknown plants",
       x = "Precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_color_manual(values = c("#E7298A", "#66A61E", "#E6AB02")) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~Lifeform)
year.all.since.lifeform.wrap

tiff("figures/2024-12_draft-figures/Year-All_precip-dev-lifeform.tiff", units = "in", height = 4, width = 7, res = 150)
year.all.since.lifeform.wrap
dev.off()

save.image("RData/15_draft-figs_Sonoran-seasonality.RData")
