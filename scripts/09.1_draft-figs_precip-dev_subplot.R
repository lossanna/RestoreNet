# Created: 2024-09-09
# Last updated: 2024-09-18

# Purpose: Graph relationships of Perc_dev_cum vs. Count or Height for Sonoran Desert
#   and Northern Arizona Plateau. Examine based on groups, and also by single species.
#   Looking for most abundant species, and species that did well under variable precip.
#   Further investigation of species of interest in 09.1_identify-species-of-interest.R

# Sonoran Desert species of interest:
#   Current mix, most abundant (all conditions) and did well under var precip: SACO6, LUSP2
#   Projected mix, did well under var precip: PLOV, SECO10
#   Projected mix, most abundant: ARPU9, PLOV
#   Native volunteers, most abundant and did well under var precip: VUOC, LOAR12, CHPO12, LOHU2
#   Weedy species, most abundant and did well under var precip: SCBA, BRRU2, ERCI6

# Northern Arizona Plateau species of interest:
#   Current mix, did well under var precip: LECI4, HECO26
#   Current mix, higher frequency: LECI4, LILE3, PASM, DACA7
#   Projected mix, did well under var precip: BAMU
#   Projected mix, high frequency when wetter: SECO10, ASTU
#   Native volunteers, did well under var precip: CHAL11, SOEL
#   Native volunteers, higher frequency in wet conditions: LEPA6
#   Native volunteers, high frequency: CHAL11
#   Weedy species, did well under var precip: SATR12, HAGL, BRRU2
#   Weedy species, higher frequency: SATR12, ERCI6
#   Weedy species, high frequency when wettest: BRNI


library(tidyverse)
library(scales)
library(viridis)
library(ggbreak)
library(ggpmisc)

# Load data ---------------------------------------------------------------

subplot <- read_csv("data/cleaned/04.1_subplot-data_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")

# From 09.1_identify-species-of-interest
sonoran.interest <- read_csv("data/cleaned/09.1_Sonoran-Desert_frequency_species-of-interest.csv")
naz.interest <- read_csv("data/cleaned/09.1_Northern-AZ-Plateau_frequency_species-of-interest.csv")


# Data wrangling ----------------------------------------------------------

# Check for NAs
apply(subplot, 2, anyNA)
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


# Data without Infinity
#   Inf created when there was no rain in the time period (can't divide by 0),
#     but this only occurs when the time period is small and in all cases there is another
#     monitoring date less than 2 weeks away with a non-Inf percent change. Occurs once
#     at CO Plateau (BarTBar).
dat <- dat |> 
  filter(Perc_dev_cum != Inf)

# Reorder PlotMix_Climate
dat$PlotMix_Climate <- factor(dat$PlotMix_Climate,
                              levels = c("None", "Current", "Projected"))


# Sonoran Desert ----------------------------------------------------------

## Count ------------------------------------------------------------------

### Desirable & Weedy -----------------------------------------------------

# Single panel by PlantSource2
sonoran.des.count <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.des.count
sonoran.weed.count <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  ggtitle("Sonoran Desert, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.weed.count

# By PlotMix_Climate and PlantSource2
sonoran.des.count.plotmixclimate <- dat |> 
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
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.des.count.plotmixclimate
sonoran.weed.count.plotmixclimate <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.weed.count.plotmixclimate

# By Lifeform (forb & grass) and PlantSource2
#   There are hardly any shrubs
sonoran.des.count.forbgrass.plantsource2 <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Lifeform %in% c("Forb", "Grass")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Lifeform) +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(17, 15)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.des.count.forbgrass.plantsource2
sonoran.weed.count.forbgrass.plantsource2 <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Lifeform %in% c("Forb", "Grass")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Lifeform) +
  ggtitle("Sonoran Desert, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(15, 17)) +
  scale_color_manual(values = c("#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.weed.count.forbgrass.plantsource2

# By Duration (annual & perennial only) and Lifeform
sonoran.des.count.perennial.annual.lifeform <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Duration %in% c("Annual", "Perennial")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Duration) +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17, 20)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02", "#666666")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.des.count.perennial.annual.lifeform
sonoran.weed.count.perennial.annual.lifeform <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Duration %in% c("Annual", "Perennial")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Duration) +
  ggtitle("Sonoran Desert, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17, 20)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02", "#666666")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.weed.count.perennial.annual.lifeform

# By Treatment and PlantSource2
sonoran.des.count.treatment <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Treatment) +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.des.count.treatment
sonoran.weed.count.treatment <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Treatment) +
  ggtitle("Sonoran Desert, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.weed.count.treatment

# Single by AridityIndex
sonoran.des.count.ai <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = AridityIndex)) +
  geom_smooth() +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_color_viridis(direction = -1) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.des.count.ai
sonoran.weed.count.ai <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = AridityIndex)) +
  geom_smooth() +
  ggtitle("Sonoran Desert, weedy species") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_color_viridis(direction = -1) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.weed.count.ai


### Seeded species --------------------------------------------------------

# Seeded species by PlotMix_Climate
sonoran.seed.count.plotmixclimate <- dat |> 
  filter(PlotMix_Climate %in% c("Current", "Projected"),
         SpeciesSeeded == "Yes") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(color = "#D95F02",
             shape = 15,
             alpha = 0.7) +
  facet_wrap(~PlotMix_Climate) +
  geom_smooth() +
  ggtitle("Sonoran Desert, seeded species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed.count.plotmixclimate

# Seeded species by PlotMix_Climate and Duration
sonoran.seed.count.plotmixclimate.duration <- dat |> 
  filter(PlotMix_Climate %in% c("Current", "Projected"),
         SpeciesSeeded == "Yes") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~PlotMix_Climate) +
  geom_smooth() +
  ggtitle("Sonoran Desert, seeded species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed.count.plotmixclimate.duration

# Seeded species by PlotMix_Climate and Lifeform
sonoran.seed.count.plotmixclimate.lifeform <- dat |> 
  filter(PlotMix_Climate %in% c("Current", "Projected"),
         SpeciesSeeded == "Yes") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  facet_wrap(~PlotMix_Climate) +
  geom_smooth() +
  ggtitle("Sonoran Desert, seeded species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed.count.plotmixclimate.lifeform

# Seeded species by PlotMix_Climate and Duration/Lifeform
sonoran.seed.count.plotmixclimate.duration.lifeform <- dat |> 
  filter(PlotMix_Climate %in% c("Current", "Projected"),
         SpeciesSeeded == "Yes") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~PlotMix_Climate) +
  geom_smooth() +
  ggtitle("Sonoran Desert, seeded species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed.count.plotmixclimate.duration.lifeform


### Species facet_wrap ----------------------------------------------------

# By seeded species: Current
sonoran.seed.count.current.species <- dat |> 
  filter(PlotMix_Climate == "Current",
         SpeciesSeeded == "Yes") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Sonoran Desert, seeded species (Current)") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed.count.current.species

# By seeded species: Projected
sonoran.seed.count.projected.species <- dat |> 
  filter(PlotMix_Climate == "Projected",
         SpeciesSeeded == "Yes") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Sonoran Desert, seeded species (Projected)") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed.count.projected.species

# By known desirable native recruits: grasses
dat |> 
  filter(Weedy == "Desirable",
         SpeciesSeeded == "No",
         Lifeform == "Grass") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(!str_detect(Code, "SRER|Patagonia|Pleasant|Roosevelt|SCC")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Sonoran Desert, native recruits") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# By known desirable native recruits: forbs
dat |> 
  filter(Weedy == "Desirable",
         SpeciesSeeded == "No",
         Lifeform == "Forb") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(!str_detect(Code, "SRER|Patagonia|Pleasant|Roosevelt|SCC")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Sonoran Desert, native recruits") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# By known desirable native recruits: narrow down contenders for best under var precip
dat |> 
  filter(Weedy == "Desirable",
         SpeciesSeeded == "No") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Code %in% c("VUOC", "CHPO12", "CRCO34", "LAGR10", "LOAR12", "LOHU2", "LOSTT", 
                     "PEHE", "PEPL", "PERE", "PLAR", "PLPA2", "PSCSA11", "SIAN2")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Sonoran Desert, native recruits") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)

# By invasives
dat |> 
  filter(PlantSource2 == "Introduced/Invasive") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Sonoran Desert, invasives") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# Species of interest
sonoran.species.count <- dat |> 
  filter(Code %in% c("SACO6", "LUSP2", "PLOV", "SECO10", "ARPU9", "VUOC", "LOAR12", "CHPO12",
                     "LOHU2", "SCBA", "BRRU2", "ERCI6")) |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  mutate(Code = factor(Code, levels = c("SACO6", "LUSP2", "PLOV", "SECO10", "ARPU9", "VUOC", "LOAR12", "CHPO12",
                                        "LOHU2", "SCBA", "BRRU2", "ERCI6"))) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = Lifeform),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Sonoran Desert species of interest") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.species.count



## Height -----------------------------------------------------------------

### Desirable & Weedy -----------------------------------------------------

# Single panel by PlantSource2
sonoran.des.height <- dat |> 
  filter(!is.na(Height)) |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.des.height
sonoran.weed.height <- dat |> 
  filter(!is.na(Height)) |>
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  ggtitle("Sonoran Desert, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.weed.height

# By PlotMix_Climate and PlantSource2
sonoran.des.height.plotmixclimate <- dat |> 
  filter(!is.na(Height)) |>
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.des.height.plotmixclimate
sonoran.weed.height.plotmixclimate <- dat |> 
  filter(!is.na(Height)) |>
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.weed.height.plotmixclimate

# By Lifeform (forb & grass) and PlantSource2
#   There are hardly any shrubs
sonoran.des.height.forbgrass.plantsource2 <- dat |> 
  filter(!is.na(Height)) |>
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Lifeform %in% c("Forb", "Grass")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Lifeform) +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(17, 15)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.des.height.forbgrass.plantsource2
sonoran.weed.height.forbgrass.plantsource2 <- dat |> 
  filter(!is.na(Height)) |>
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Lifeform %in% c("Forb", "Grass")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Lifeform) +
  ggtitle("Sonoran Desert, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(15, 17)) +
  scale_color_manual(values = c("#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.weed.height.forbgrass.plantsource2

# By Duration (annual & perennial only) and Lifeform
sonoran.des.height.perennial.annual.lifeform <- dat |> 
  filter(!is.na(Height)) |>
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Duration %in% c("Annual", "Perennial")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Duration) +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.des.height.perennial.annual.lifeform
sonoran.weed.height.perennial.annual.lifeform <- dat |> 
  filter(!is.na(Height)) |>
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Duration %in% c("Annual", "Perennial")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Duration) +
  ggtitle("Sonoran Desert, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.weed.height.perennial.annual.lifeform

# By Treatment and PlantSource2
sonoran.des.height.treatment <- dat |> 
  filter(!is.na(Height)) |>
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Treatment) +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.des.height.treatment
sonoran.weed.height.treatment <- dat |> 
  filter(!is.na(Height)) |>
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Treatment) +
  ggtitle("Sonoran Desert, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.weed.height.treatment

# Single by AridityIndex
sonoran.des.height.ai <- dat |> 
  filter(!is.na(Height)) |>
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = AridityIndex)) +
  geom_smooth() +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_color_viridis(direction = -1) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.des.height.ai
sonoran.weed.height.ai <- dat |>
  filter(!is.na(Height)) |>
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = AridityIndex)) +
  geom_smooth() +
  ggtitle("Sonoran Desert, weedy species") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_color_viridis(direction = -1) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.weed.height.ai


### Seeded species --------------------------------------------------------

# Seeded species by PlotMix_Climate
sonoran.seed.height.plotmixclimate <- dat |> 
  filter(PlotMix_Climate %in% c("Current", "Projected"),
         SpeciesSeeded == "Yes") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(color = "#D95F02",
             shape = 15,
             alpha = 0.7) +
  facet_wrap(~PlotMix_Climate) +
  geom_smooth() +
  ggtitle("Sonoran Desert, seeded species") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed.height.plotmixclimate

# Seeded species by PlotMix_Climate and Duration
sonoran.seed.height.plotmixclimate.duration <- dat |> 
  filter(PlotMix_Climate %in% c("Current", "Projected"),
         SpeciesSeeded == "Yes") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~PlotMix_Climate) +
  geom_smooth() +
  ggtitle("Sonoran Desert, seeded species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed.height.plotmixclimate.duration


### Species facet_wrap ----------------------------------------------------

# By seeded species: Current
sonoran.seed.height.current.species <- dat |> 
  filter(PlotMix_Climate == "Current",
         SpeciesSeeded == "Yes") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Sonoran Desert, seeded species (Current)") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed.height.current.species

# By seeded species: Projected
sonoran.seed.height.projected.species <- dat |> 
  filter(PlotMix_Climate == "Projected",
         SpeciesSeeded == "Yes") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Sonoran Desert, seeded species (Projected)") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed.height.projected.species

# By known desirable native recruits: grasses
dat |> 
  filter(Weedy == "Desirable",
         SpeciesSeeded == "No",
         Lifeform == "Grass") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(!str_detect(Code, "SRER|Patagonia|Pleasant|Roosevelt|SCC")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Sonoran Desert, native recruits") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# By known desirable native recruits: forbs
dat |> 
  filter(Weedy == "Desirable",
         SpeciesSeeded == "No",
         Lifeform == "Forb") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(!str_detect(Code, "SRER|Patagonia|Pleasant|Roosevelt|SCC")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Sonoran Desert, native recruits") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# By invasives
dat |> 
  filter(PlantSource2 == "Introduced/Invasive") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Sonoran Desert, invasives") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# Species of interest
sonoran.species.height <- dat |> 
  filter(Code %in% c("SACO6", "LUSP2", "PLOV", "SECO10", "ARPU9", "VUOC", "LOAR12", "CHPO12",
                     "LOHU2", "SCBA", "BRRU2", "ERCI6")) |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  mutate(Code = factor(Code, levels = c("SACO6", "LUSP2", "PLOV", "SECO10", "ARPU9", "VUOC", "LOAR12", "CHPO12",
                                        "LOHU2", "SCBA", "BRRU2", "ERCI6"))) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = Lifeform),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Sonoran Desert species of interest") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.species.height


## Frequency (occurrence in plots) ----------------------------------------

# Histogram
sonoran.species.freq <- dat |> 
  filter(Code %in% c("SACO6", "LUSP2", "PLOV", "SECO10", "ARPU9", "VUOC", "LOAR12", "CHPO12",
                     "LOHU2", "SCBA", "BRRU2", "ERCI6")) |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  mutate(Code = factor(Code, levels = c("SACO6", "LUSP2", "PLOV", "SECO10", "ARPU9", "VUOC", "LOAR12", "CHPO12",
                                        "LOHU2", "SCBA", "BRRU2", "ERCI6"))) |> 
  ggplot(aes(x = Perc_dev_cum,
             fill = PlantSource2)) +
  geom_histogram() +
  facet_wrap(~Code) +
  ggtitle("Sonoran Desert species of interest") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  ylab("Frequency (presence in plots)") +
  scale_fill_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.species.freq


# Bar graph (total)
sonoran.interest.total <- sonoran.interest |> 
  filter(Plot == "Total") |> 
  mutate(Plant = factor(Plant, levels = c("Current mix", "Projected mix",
                                          "Native recruit", "Invasive",
                                          "Empty")),
         Code = factor(Code, levels = c("SACO6", "LUSP2", "PLOV", "SECO10", "ARPU9", 
                                        "VUOC", "LOAR12", "CHPO12",
                                        "LOHU2", "SCBA", "BRRU2", "ERCI6", "Empty")))
sonoran.species.total <- sonoran.interest.total |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plant)) +
  geom_bar(stat = "identity") +
  ggtitle("Sonoran Desert species of interest") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#E5C494", "#FC8D62", "#66C2A5", "#8DA0CB", "#B3B3B3")) +
  theme(axis.text.x = element_text(color = "black"))
sonoran.species.total
  
# Paired bar graph (wetter & drier)
sonoran.interest.wetdry <- sonoran.interest |> 
  filter(Plot != "Total") 
unique(sonoran.interest.wetdry$Type)
sonoran.interest.wetdry$Type <- factor(sonoran.interest.wetdry$Type, 
                                       levels = c("Current mix, Drier", "Current mix, Wetter",
                                                  "Projected mix, Drier", "Projected mix, Wetter",
                                                  "Native recruit, Drier", "Native recruit, Wetter",
                                                  "Invasive, Drier", "Invasive, Wetter",
                                                  "Empty, Drier", "Empty, Wetter"))
sonoran.interest.wetdry$Code <- factor(sonoran.interest.wetdry$Code,
                                       levels = c("SACO6", "LUSP2", "PLOV", "SECO10", "ARPU9", 
                                                  "VUOC", "LOAR12", "CHPO12",
                                                  "LOHU2", "SCBA", "BRRU2", "ERCI6", "Empty"))
sonoran.species.wetdry <- sonoran.interest.wetdry |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert species of interest") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#E5D4A7", "#A6761D", "#FBB4AE", "#D95F02",
                               "#B3E2D6", "#1B9E77", "#B3C8E8", "#7570B3",
                               "#D9D9D9", "#666666")) +
  theme(axis.text.x = element_text(color = "black"))
sonoran.species.wetdry
  
  
  

# Northern Arizona Plateau ------------------------------------------------

## Count ------------------------------------------------------------------

### Desirable & Weedy -----------------------------------------------------

# Single panel by PlantSource2
naz.des.count <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.des.count
naz.weed.count <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.weed.count

# By PlotMix_Climate and PlantSource2
naz.des.count.plotmixclimate <- dat |> 
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
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.des.count.plotmixclimate
naz.weed.count.plotmixclimate <- dat |> 
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
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.weed.count.plotmixclimate

# By Lifeform (forb, grass, shrub) and PlantSource2
#   Very little weedy shrubs
naz.des.count.forbgrassshrub.plantsource2 <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Lifeform %in% c("Forb", "Grass", "Shrub")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Lifeform) +
  ggtitle("Northern Arizona Plateau, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(17, 15)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.des.count.forbgrassshrub.plantsource2
naz.weed.count.forbgrass.plantsource2 <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Lifeform %in% c("Forb", "Grass")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Lifeform) +
  ggtitle("Northern Arizona Plateau, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(15, 17)) +
  scale_color_manual(values = c("#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.weed.count.forbgrass.plantsource2

# By Duration (annual & perennial only) and Lifeform
#   Almost all weedy species are annuals
naz.des.count.perennial.annual.lifeform <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Duration %in% c("Annual", "Perennial")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Duration) +
  ggtitle("Northern Arizona Plateau, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.des.count.perennial.annual.lifeform
naz.weed.count.perennial.annual.lifeform <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Duration %in% c("Annual", "Perennial")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Duration) +
  ggtitle("Northern Arizona Plateau, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.weed.count.perennial.annual.lifeform

# By Treatment and PlantSource2
naz.des.count.treatment <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Treatment) +
  ggtitle("Northern Arizona Plateau, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.des.count.treatment
naz.weed.count.treatment <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Treatment) +
  ggtitle("Northern Arizona Plateau, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.weed.count.treatment

# Single by AridityIndex
naz.des.count.ai <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = AridityIndex)) +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, desirable species") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_color_viridis(direction = -1) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.des.count.ai
naz.weed.count.ai <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = AridityIndex)) +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, weedy species") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_color_viridis(direction = -1) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.weed.count.ai


### Seeded species --------------------------------------------------------

# Seeded species by PlotMix_Climate
naz.seed.count.plotmixclimate <- dat |> 
  filter(PlotMix_Climate %in% c("Current", "Projected"),
         SpeciesSeeded == "Yes") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(color = "#D95F02",
             shape = 15,
             alpha = 0.7) +
  facet_wrap(~PlotMix_Climate) +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, seeded species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.seed.count.plotmixclimate

# Seeded species by PlotMix_Climate and Duration
naz.seed.count.plotmixclimate.duration <- dat |> 
  filter(PlotMix_Climate %in% c("Current", "Projected"),
         SpeciesSeeded == "Yes") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~PlotMix_Climate) +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, seeded species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.seed.count.plotmixclimate.duration


### Species facet_wrap ----------------------------------------------------

# By seeded species: Current
naz.seed.count.current.species <- dat |> 
  filter(PlotMix_Climate == "Current",
         SpeciesSeeded == "Yes") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(!str_detect(Code, "SPP|SUNGR|UNFO|UNGR")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau, seeded species (Current)") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.seed.count.current.species

# By seeded species: Projected
naz.seed.count.projected.species <- dat |> 
  filter(PlotMix_Climate == "Projected",
         SpeciesSeeded == "Yes") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(!str_detect(Code, "SPP|SUNGR|UNFO|UNGR")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau, seeded species (Projected)") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.seed.count.projected.species

# By known native recruit: grasses
dat |> 
  filter(Weedy == "Desirable",
         SpeciesSeeded == "No",
         Lifeform == "Grass") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(!str_detect(Code, "FlyingM|MOWE|TLE|BarTBar|PEFO|AguaFria|BabbittPJ|Spiderweb")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau, native recruits") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# By known native recruit: forbs
dat |> 
  filter(Weedy == "Desirable",
         SpeciesSeeded == "No",
         Lifeform == "Forb") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(!str_detect(Code, "FlyingM|MOWE|TLE|BarTBar|PEFO|AguaFria|BabbittPJ|Spiderweb")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau, native recruits") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# By known native recruit: shrubs
dat |> 
  filter(Weedy == "Desirable",
         SpeciesSeeded == "No",
         Lifeform == "Shrub") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(!str_detect(Code, "FlyingM|MOWE|TLE|BarTBar|PEFO|AguaFria|BabbittPJ|Spiderweb")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(color = "#D95F02",
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau, native recruits (shrubs)") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# By known invasives
dat |> 
  filter(PlantSource2 == "Introduced/Invasive") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(!str_detect(Code, "FlyingM|MOWE|TLE|BarTBar|PEFO|AguaFria|BabbittPJ|Spiderweb")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau, invasives") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# Species of interest
naz.species.count <- dat |> 
  filter(Code %in% c("LECI4", "HECO26", "LILE3", "PASM", "DACA7", "BAMU", "SECO10", "ASTU",
                     "CHAL11", "SOEL", "LEPA6",
                     "SATR12", "HAGL", "BRRU2", "ERCI6", "BRNI")) |> 
  filter(Region == "Colorado Plateau") |> 
  mutate(Code = factor(Code, levels = c("LECI4", "HECO26", "LILE3", "PASM", "DACA7", "BAMU", "SECO10", "ASTU",
                                        "CHAL11", "SOEL", "LEPA6",
                                        "SATR12", "HAGL", "BRRU2", "ERCI6", "BRNI"))) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = Lifeform),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau species of interest") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.species.count

# Desirable species of interest
dat |> 
  filter(Code %in% c("LECI4", "HECO26", "BAMU", "PASM", "LILE3", "DACA7", "CHAL11", "SOEL")) |> 
  filter(Region == "Colorado Plateau") |> 
  mutate(Code = factor(Code, levels = c("LECI4", "HECO26", "BAMU", "PASM", "LILE3", "DACA7", 
                                        "CHAL11", "SOEL"))) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = Lifeform),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau, desirable species") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 17)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# Weedy species of interest
dat |> 
  filter(Code %in% c("SATR12", "HAGL", "BRRU2")) |> 
  filter(Region == "Colorado Plateau") |> 
  mutate(Code = factor(Code, levels = c("SATR12", "HAGL", "BRRU2"))) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = Lifeform),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau species") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 17)) +
  scale_color_manual(values = c("#7570B3")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 



## Height -----------------------------------------------------------------

### Desirable & Weedy -----------------------------------------------------

# Single panel by PlantSource2
naz.des.height <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.des.height
naz.weed.height <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.weed.height

# By PlotMix_Climate and PlantSource2
naz.des.height.plotmixclimate <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Northern Arizona Plateau, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.des.height.plotmixclimate
naz.weed.height.plotmixclimate <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Northern Arizona Plateau, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.weed.height.plotmixclimate

# By Lifeform (forb, grass, shrub) and PlantSource2
#   Very little weedy shrubs
naz.des.height.forbgrassshrub.plantsource2 <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Lifeform %in% c("Forb", "Grass", "Shrub")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Lifeform) +
  ggtitle("Northern Arizona Plateau, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(17, 15)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.des.height.forbgrassshrub.plantsource2
naz.weed.height.forbgrass.plantsource2 <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Lifeform %in% c("Forb", "Grass")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Lifeform) +
  ggtitle("Northern Arizona Plateau, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(15, 17)) +
  scale_color_manual(values = c("#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.weed.height.forbgrass.plantsource2

# By Duration (annual & perennial only) and Lifeform
#   Almost all weedy species are annuals
naz.des.height.perennial.annual.lifeform <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Duration %in% c("Annual", "Perennial")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Duration) +
  ggtitle("Northern Arizona Plateau, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17, 20)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02", "#666666")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.des.height.perennial.annual.lifeform
naz.weed.height.perennial.annual.lifeform <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Duration %in% c("Annual", "Perennial")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Duration) +
  ggtitle("Northern Arizona Plateau, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17, 20)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02", "#666666")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.weed.height.perennial.annual.lifeform

# By Treatment and PlantSource2
naz.des.height.treatment <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Treatment) +
  ggtitle("Northern Arizona Plateau, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.des.height.treatment
naz.weed.height.treatment <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Treatment) +
  ggtitle("Northern Arizona Plateau, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.weed.height.treatment

# Single by AridityIndex
naz.des.height.ai <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = AridityIndex)) +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, desirable species") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_color_viridis(direction = -1) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.des.height.ai
naz.weed.height.ai <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = AridityIndex)) +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, weedy species") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_color_viridis(direction = -1) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.weed.height.ai


### Seeded species --------------------------------------------------------

# Seeded species by PlotMix_Climate
naz.seed.height.plotmixclimate <- dat |> 
  filter(PlotMix_Climate %in% c("Current", "Projected"),
         SpeciesSeeded == "Yes") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(color = "#D95F02",
             shape = 15,
             alpha = 0.7) +
  facet_wrap(~PlotMix_Climate) +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, seeded species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.seed.height.plotmixclimate

# Seeded species by PlotMix_Climate and Duration
naz.seed.height.plotmixclimate.duration <- dat |> 
  filter(PlotMix_Climate %in% c("Current", "Projected"),
         SpeciesSeeded == "Yes") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~PlotMix_Climate) +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, seeded species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.seed.height.plotmixclimate.duration


### Species facet_wrap ----------------------------------------------------

# By seeded species: Current
naz.seed.height.current.species <- dat |> 
  filter(PlotMix_Climate == "Current",
         SpeciesSeeded == "Yes") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(!str_detect(Code, "SPP|SUNGR|UNFO|UNGR")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau, seeded species (Current)") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.seed.height.current.species

# By seeded species: Projected
naz.seed.height.projected.species <- dat |> 
  filter(PlotMix_Climate == "Projected",
         SpeciesSeeded == "Yes") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(!str_detect(Code, "SPP|SUNGR|UNFO|UNGR")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau, seeded species (Projected)") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.seed.height.projected.species

# By known native recruit: grasses
dat |> 
  filter(Weedy == "Desirable",
         SpeciesSeeded == "No",
         Lifeform == "Grass") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(!str_detect(Code, "FlyingM|MOWE|TLE|BarTBar|PEFO|AguaFria|BabbittPJ|Spiderweb")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau, native recruits") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# By known native recruit: forbs
dat |> 
  filter(Weedy == "Desirable",
         SpeciesSeeded == "No",
         Lifeform == "Forb") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(!str_detect(Code, "FlyingM|MOWE|TLE|BarTBar|PEFO|AguaFria|BabbittPJ|Spiderweb")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Duration,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau, native recruits") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# By known native recruit: shrubs
dat |> 
  filter(Weedy == "Desirable",
         SpeciesSeeded == "No",
         Lifeform == "Shrub") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(!str_detect(Code, "FlyingM|MOWE|TLE|BarTBar|PEFO|AguaFria|BabbittPJ|Spiderweb")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(color = "#D95F02",
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau, native recruits (shrubs)") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# By known invasives
dat |> 
  filter(PlantSource2 == "Introduced/Invasive") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(!str_detect(Code, "FlyingM|MOWE|TLE|BarTBar|PEFO|AguaFria|BabbittPJ|Spiderweb")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau, invasives") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# Species of interest
naz.species.height <- dat |> 
  filter(Code %in% c("LECI4", "HECO26", "LILE3", "PASM", "DACA7", "BAMU", "SECO10", "ASTU",
                     "CHAL11", "SOEL", "LEPA6",
                     "SATR12", "HAGL", "BRRU2", "ERCI6", "BRNI")) |> 
  filter(Region == "Colorado Plateau") |> 
  mutate(Code = factor(Code, levels = c("LECI4", "HECO26", "LILE3", "PASM", "DACA7", "BAMU", "SECO10", "ASTU",
                                        "CHAL11", "SOEL", "LEPA6",
                                        "SATR12", "HAGL", "BRRU2", "ERCI6", "BRNI"))) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = Lifeform),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau species of interest") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.species.height



## Frequency (occurrence in plots) ----------------------------------------

# Histogram
naz.species.freq <- dat |> 
  filter(Code %in% c("LECI4", "HECO26", "LILE3", "PASM", "DACA7", "BAMU", "SECO10", "ASTU",
                     "CHAL11", "SOEL", "LEPA6",
                     "SATR12", "HAGL", "BRRU2", "ERCI6", "BRNI")) |> 
  filter(Region == "Colorado Plateau") |> 
  mutate(Code = factor(Code, levels = c("LECI4", "HECO26", "LILE3", "PASM", "DACA7", "BAMU", "SECO10", "ASTU",
                                        "CHAL11", "SOEL", "LEPA6",
                                        "SATR12", "HAGL", "BRRU2", "ERCI6", "BRNI"))) |> 
  ggplot(aes(x = Perc_dev_cum,
             fill = PlantSource2)) +
  geom_histogram() +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau species of interest") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  ylab("Frequency (presence in plots)") +
  scale_fill_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.species.freq


# Bar graph (total)
naz.interest.total <- naz.interest |> 
  filter(Plot == "Total") |> 
  mutate(Plant = factor(Plant, levels = c("Current mix", "Projected mix",
                                          "Native recruit", "Invasive",
                                          "Empty")),
         Code = factor(Code, levels = c("LECI4", "HECO26", "LILE3", "PASM", "DACA7", "BAMU", "SECO10", "ASTU",
                                        "CHAL11", "SOEL", "LEPA6",
                                        "SATR12", "HAGL", "BRRU2", "ERCI6", "BRNI", "Empty")))
naz.species.total <- naz.interest.total |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plant)) +
  geom_bar(stat = "identity") +
  ggtitle("Northern Arizona Plateau species of interest") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  scale_fill_manual(values = c("#E5C494", "#FC8D62", "#66C2A5", "#8DA0CB", "#B3B3B3")) +
  theme(axis.text.x = element_text(color = "black"))
naz.species.total

# Paired bar graph (wetter & drier)
naz.interest.wetdry <- naz.interest |> 
  filter(Plot != "Total") 
unique(naz.interest.wetdry$Type)
naz.interest.wetdry$Type <- factor(naz.interest.wetdry$Type, 
                                   levels = c("Current mix, Drier", "Current mix, Wetter",
                                              "Projected mix, Drier", "Projected mix, Wetter",
                                              "Native recruit, Drier", "Native recruit, Wetter",
                                              "Invasive, Drier", "Invasive, Wetter",
                                              "Empty, Drier", "Empty, Wetter"))
naz.interest.wetdry$Code <- factor(naz.interest.wetdry$Code,
                                   levels = c("LECI4", "HECO26", "LILE3", "PASM", "DACA7", "BAMU", "SECO10", "ASTU",
                                              "CHAL11", "SOEL", "LEPA6",
                                              "SATR12", "HAGL", "BRRU2", "ERCI6", "BRNI", "Empty"))
naz.species.wetdry <- naz.interest.wetdry |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona Plateau species of interest") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#E5D4A7", "#A6761D", "#FBB4AE", "#D95F02",
                               "#B3E2D6", "#1B9E77", "#B3C8E8", "#7570B3",
                               "#D9D9D9", "#666666")) +
  theme(axis.text.x = element_text(angle = 40)) +
  theme(axis.text.x = element_text(color = "black"))
naz.species.wetdry




# Write out draft figures -------------------------------------------------

## Sonoran Desert ---------------------------------------------------------

# Sonoran Desert: Count
tiff("figures/2024-09_draft-figures/Sonoran_desirable_Count-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.des.count
dev.off()
tiff("figures/2024-09_draft-figures/Sonoran_weedy_Count-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.weed.count
dev.off()

tiff("figures/2024-09_draft-figures/Sonoran_desirable_Count-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.des.count.plotmixclimate
dev.off()
tiff("figures/2024-09_draft-figures/Sonoran_weedy_Count-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.weed.count.plotmixclimate
dev.off()

tiff("figures/2024-09_draft-figures/Sonoran_desirable_Count-by-forb-grass-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.des.count.forbgrass.plantsource2
dev.off()
tiff("figures/2024-09_draft-figures/Sonoran_weedy_Count-by-forb-grass-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.weed.count.forbgrass.plantsource2
dev.off()

tiff("figures/2024-09_draft-figures/Sonoran_desirable_Count-by-duration-and-lifeform.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.des.count.perennial.annual.lifeform
dev.off()
tiff("figures/2024-09_draft-figures/Sonoran_weedy_Count-by-duration-and-lifeform.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.weed.count.perennial.annual.lifeform
dev.off()

tiff("figures/2024-09_draft-figures/Sonoran_seeded_Count-by-PlotMix_Climate-and-duration.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.seed.count.plotmixclimate.duration
dev.off()
tiff("figures/2024-09_draft-figures/Sonoran_seeded_Count-by-seeded-species_Current.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.seed.count.current.species
dev.off()
tiff("figures/2024-09_draft-figures/Sonoran_seeded_Count-by-seeded-species_Projected.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.seed.count.projected.species
dev.off()

# Sonoran Desert: Height
tiff("figures/2024-09_draft-figures/Sonoran_desirable_Height-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.des.height
dev.off()
tiff("figures/2024-09_draft-figures/Sonoran_weedy_Height-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.weed.height
dev.off()

tiff("figures/2024-09_draft-figures/Sonoran_desirable_Height-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.des.height.plotmixclimate
dev.off()
tiff("figures/2024-09_draft-figures/Sonoran_weedy_Height-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.weed.height.plotmixclimate
dev.off()

tiff("figures/2024-09_draft-figures/Sonoran_desirable_Height-by-forb-grass-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.des.height.forbgrass.plantsource2
dev.off()
tiff("figures/2024-09_draft-figures/Sonoran_weedy_Height-by-forb-grass-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.weed.height.forbgrass.plantsource2
dev.off()

tiff("figures/2024-09_draft-figures/Sonoran_desirable_Height-by-duration-and-lifeform.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.des.height.perennial.annual.lifeform
dev.off()
tiff("figures/2024-09_draft-figures/Sonoran_weedy_Height-by-duration-and-lifeform.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.weed.height.perennial.annual.lifeform
dev.off()

tiff("figures/2024-09_draft-figures/Sonoran_seeded_Height-by-PlotMix_Climate-and-duration.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.seed.height.plotmixclimate.duration
dev.off()
tiff("figures/2024-09_draft-figures/Sonoran_seeded_Height-by-seeded-species_Current.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.seed.height.current.species
dev.off()
tiff("figures/2024-09_draft-figures/Sonoran_seeded_Height-by-seeded-species_Projected.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.seed.height.projected.species
dev.off()

# Sonoran Desert: Species of interest
tiff("figures/2024-09_draft-figures/Sonoran_species-of-interest_Count.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.species.count
dev.off()
tiff("figures/2024-09_draft-figures/Sonoran_species-of-interest_Height.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.species.height
dev.off()
tiff("figures/2024-09_draft-figures/Sonoran_species-of-interest_frequency-histogram.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.species.freq
dev.off()
tiff("figures/2024-09_draft-figures/Sonoran_species-of-interest_frequency-wet-dry.tiff", units = "in", height = 4, width = 9, res = 150)
sonoran.species.wetdry
dev.off()
tiff("figures/2024-09_draft-figures/Sonoran_species-of-interest_frequency-total.tiff", units = "in", height = 4, width = 9, res = 150)
sonoran.species.total
dev.off()



## Northern Arizona Plateau -----------------------------------------------

# Northern AZ: Count
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_desirable_Count-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
naz.des.count
dev.off()
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_weedy_Count-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
naz.weed.count
dev.off()

tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_desirable_Count-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
naz.des.count.plotmixclimate
dev.off()
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_weedy_Count-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
naz.weed.count.plotmixclimate
dev.off()

tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_desirable_Count-by-forb-grass-shrub-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
naz.des.count.forbgrassshrub.plantsource2
dev.off()
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_weedy_Count-by-forb-grass-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
naz.weed.count.forbgrass.plantsource2
dev.off()

tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_desirable_Count-by-duration-and-lifeform.tiff", units = "in", height = 4, width = 5, res = 150)
naz.des.count.perennial.annual.lifeform
dev.off()
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_weedy_Count-by-duration-and-lifeform.tiff", units = "in", height = 4, width = 5, res = 150)
naz.weed.count.perennial.annual.lifeform
dev.off()


tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_seeded_Count-by-PlotMix_Climate-and-duration.tiff", units = "in", height = 5, width = 7, res = 150)
naz.seed.count.plotmixclimate.duration
dev.off()
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_seeded_Count-by-seeded-species_Current.tiff", units = "in", height = 5, width = 7, res = 150)
naz.seed.count.current.species
dev.off()
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_seeded_Count-by-seeded-species_Projected.tiff", units = "in", height = 5, width = 7, res = 150)
naz.seed.count.projected.species
dev.off()


# Northern AZ: Height
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_desirable_Height-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
naz.des.height
dev.off()
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_weedy_Height-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
naz.weed.height
dev.off()

tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_desirable_Height-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
naz.des.height.plotmixclimate
dev.off()
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_weedy_Height-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
naz.weed.height.plotmixclimate
dev.off()

tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_desirable_Height-by-forb-grass-shrub-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
naz.des.height.forbgrassshrub.plantsource2
dev.off()
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_weedy_Height-by-forb-grass-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
naz.weed.height.forbgrass.plantsource2
dev.off()

tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_desirable_Height-by-duration-and-lifeform.tiff", units = "in", height = 4, width = 5, res = 150)
naz.des.height.perennial.annual.lifeform
dev.off()
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_weedy_Height-by-duration-and-lifeform.tiff", units = "in", height = 4, width = 5, res = 150)
naz.weed.height.perennial.annual.lifeform
dev.off()

tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_seeded_Height-by-PlotMix_Climate-and-duration.tiff", units = "in", height = 5, width = 7, res = 150)
naz.seed.height.plotmixclimate.duration
dev.off()
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_seeded_Height-by-seeded-species_Current.tiff", units = "in", height = 5, width = 7, res = 150)
naz.seed.height.current.species
dev.off()
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_seeded_Height-by-seeded-species_Projected.tiff", units = "in", height = 5, width = 7, res = 150)
naz.seed.height.projected.species
dev.off()


# Northern AZ: Species of interest
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_species-of-interest_Count.tiff", units = "in", height = 5, width = 7, res = 150)
naz.species.count
dev.off()
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_species-of-interest_Height.tiff", units = "in", height = 5, width = 7, res = 150)
naz.species.height
dev.off()
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_species-of-interest_frequency-histogram.tiff", units = "in", height = 5, width = 7, res = 150)
naz.species.freq
dev.off()
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_species-of-interest_frequency-wet-dry.tiff", units = "in", height = 4, width = 9, res = 150)
naz.species.wetdry
dev.off()
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_species-of-interest_frequency-total.tiff", units = "in", height = 4, width = 9, res = 150)
naz.species.total
dev.off()


save.image("RData/09.1_draft-figs_precip-dev_subplot.RData")


