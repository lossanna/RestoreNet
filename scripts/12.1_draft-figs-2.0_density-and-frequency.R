# Created: 2024-09-25
# Last updated: 2024-09-27

# Purpose: Narrow down and improve figures from 09.1_draft-figs_precip-dev_subplot.R. In particular,
#   change Count to density (individuals per m2), and change N AZ Plateau to just Northern Arizona
#   (it contains both the Mountains and Plateau Level III ecoregions). Also frequency has changed a bit
#   since I updated (corrected) present_species.

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

sonoran.freq <- read_csv("data/cleaned/11.1_Sonoran-Desert_frequency_all_clean.csv")
sonoran.freq.interest <- read_csv("data/cleaned/11.1_Sonoran-Desert_frequency_interest_clean.csv")
sonoran.freq.nativevolun <- read_csv("data/cleaned/11.1_Sonoran-Desert_frequency_native-volunteer_clean.csv")
sonoran.freq.weed <- read_csv("data/cleaned/11.1_Sonoran-Desert_frequency_weed_clean.csv")
sonoran.freq.current <- read_csv("data/cleaned/11.1_Sonoran-Desert_frequency_seeded-current_clean.csv")
sonoran.freq.projected <- read_csv("data/cleaned/11.1_Sonoran-Desert_frequency_seeded-projected_clean.csv")

naz.freq <- read_csv("data/cleaned/11.1_Northern-AZ_frequency_all_clean.csv")
naz.freq.interest <- read_csv("data/cleaned/11.1_Northern-AZ_frequency_interest_clean.csv")
naz.freq.nativevolun <- read_csv("data/cleaned/11.1_Northern-AZ_frequency_native-volunteer_clean.csv")
naz.freq.weed <- read_csv("data/cleaned/11.1_Northern-AZ_frequency_weed_clean.csv")
naz.freq.current <- read_csv("data/cleaned/11.1_Northern-AZ_frequency_seeded-current_clean.csv")
naz.freq.projected <- read_csv("data/cleaned/11.1_Northern-AZ_frequency_seeded-projected_clean.csv")


# Data wrangling ----------------------------------------------------------

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
dat <- dat |> 
  filter(Perc_dev_cum != Inf)

# Create Density column
dat <- dat |> 
  mutate(Density = Count / (0.25 * 0.25))

# Rename and reorder PlotMix_Climate
dat <- dat |> 
  mutate(PlotMix_Climate = case_when(
    PlotMix_Climate == "None" ~ "Not seeded",
    PlotMix_Climate == "Current" ~ "Current-adapted mix",
    PlotMix_Climate == "Projected" ~ "Projected-adapted mix"))
dat$PlotMix_Climate <- factor(dat$PlotMix_Climate,
                              levels = c("Not seeded", "Current-adapted mix", "Projected-adapted mix"))

# Rename weedy PlantSource2
dat <- dat |> 
  mutate(PlantSource2 = case_when(
    PlantSource2 == "Introduced/Invasive" ~ "Invasive",
    PlantSource2 == "Recruit" ~ "Unknown recruit",
    TRUE ~ PlantSource2))


# Reorder frequency for species of interest
sonoran.freq.interest <- sonoran.freq.interest |> 
  mutate(Plant = factor(Plant, levels = c("Current mix", "Projected mix",
                                          "Native recruit", "Invasive",
                                          "Empty")),
         Code = factor(Code, levels = c("SACO6", "LUSP2", "PLOV", "SECO10", "ARPU9", 
                                        "VUOC", "LOAR12", "CHPO12",
                                        "LOHU2", "SCBA", "BRRU2", "ERCI6", "Empty"))) 


# Sonoran Desert ----------------------------------------------------------

## Count ------------------------------------------------------------------

# Desirable by PlotMix_Climate and PlantSource2 (native recruit outliers removed)
sonoran.des.count.plotmixclimate <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Density < 1000) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  labs(title = "Sonoran Desert, desirable species",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.des.count.plotmixclimate

# Weedy as single panel by PlantSource2
sonoran.weed.count <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  labs(title = "Sonoran Desert, weedy species",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.weed.count

# Seeded species by PlotMix_Climate and Duration/Lifeform
sonoran.seed.count.plotmixclimate.lifeform <- dat |> 
  filter(PlotMix_Climate %in% c("Current-adapted mix", "Projected-adapted mix"),
         SpeciesSeeded == "Yes") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  facet_wrap(~PlotMix_Climate) +
  geom_smooth() +
  labs(title = "Sonoran Desert, seeded species",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed.count.plotmixclimate.lifeform


# Seeded, by species
# By seeded species: Current
sonoran.seed.count.current.species <- dat |> 
  filter(PlotMix_Climate == "Current-adapted mix",
         SpeciesSeeded == "Yes") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  labs(title = "Sonoran Desert, seeded species (Current-adapted mix)",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed.count.current.species

# By seeded species: Projected
sonoran.seed.count.projected.species <- dat |> 
  filter(PlotMix_Climate == "Projected-adapted mix",
         SpeciesSeeded == "Yes") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  labs(title = "Sonoran Desert, seeded species (Projected-adapted mix)",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed.count.projected.species


## Frequency --------------------------------------------------------------

# Native recruit, all plots
sonoran.freq.nativevolun |> 
  filter(Plot == "Total") |> 
  ggplot(aes(x = Code, y = Frequency)) +
  geom_bar(stat = "identity") +
  ggtitle("Sonoran Desert, native recruit, all plots") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))

# Native recruit, extremes
sonoran.freq.nativevolun |> 
  filter(Plot %in% c("Wettest", "Driest")) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert, native recruit") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom")

# Weedy, all plots
sonoran.freq.weed |> 
  filter(Plot == "Total") |> 
  ggplot(aes(x = Code, y = Frequency)) +
  geom_bar(stat = "identity") +
  ggtitle("Sonoran Desert, weeds, all plots") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) 

# Weedy, extremes
sonoran.freq.weed |> 
  filter(Plot %in% c("Wettest", "Driest")) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert, weeds") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom")

# Current mix, all plots
sonoran.freq.current |> 
  filter(Plot == "Total") |> 
  ggplot(aes(x = Code, y = Frequency)) +
  geom_bar(stat = "identity") +
  ggtitle("Sonoran Desert, Current mix, all plots") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35))

# Current mix, extremes
sonoran.freq.current |> 
  filter(Plot %in% c("Wettest", "Driest")) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert, Current mix") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom")

# Projected mix, all plots
sonoran.freq.projected |> 
  filter(Plot == "Total") |> 
  ggplot(aes(x = Code, y = Frequency)) +
  geom_bar(stat = "identity") +
  ggtitle("Sonoran Desert, Projected mix, all plots") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35))

# Projected mix, extremes
sonoran.freq.projected |> 
  filter(Plot %in% c("Wettest", "Driest")) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert, Projected mix") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom")


# Species of interest, all plots
sonoran.species.total <- sonoran.freq.interest |> 
  filter(Plot == "Total") |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plant)) +
  geom_bar(stat = "identity") +
  ggtitle("Sonoran Desert species of interest") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#E5C494", "#FC8D62", "#66C2A5", "#8DA0CB", "#B3B3B3")) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom")
sonoran.species.total

# Species of interest, paired wetter & drier
sonoran.species.wetdry <- sonoran.freq.interest |> 
  filter(Plot %in% c("Wetter", "Drier")) |> 
  mutate(Type = factor(Type, 
                       levels = c("Current mix, Drier", "Current mix, Wetter",
                                  "Projected mix, Drier", "Projected mix, Wetter",
                                  "Native recruit, Drier", "Native recruit, Wetter",
                                  "Invasive, Drier", "Invasive, Wetter",
                                  "Empty, Drier", "Empty, Wetter"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert species of interest") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#E5D4A7", "#A6761D", "#FBB4AE", "#D95F02",
                               "#B3E2D6", "#1B9E77", "#B3C8E8", "#7570B3",
                               "#D9D9D9", "#666666")) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom")
sonoran.species.wetdry

# Species of interest, wettest & driest (extremes)
sonoran.species.ex <- sonoran.freq.interest |> 
  filter(Plot %in% c("Wettest", "Driest")) |> 
  mutate(Type = factor(Type, 
                       levels = c("Current mix, Driest", "Current mix, Wettest",
                                  "Projected mix, Driest", "Projected mix, Wettest",
                                  "Native recruit, Driest", "Native recruit, Wettest",
                                  "Invasive, Driest", "Invasive, Wettest",
                                  "Empty, Driest", "Empty, Wettest"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert, species of interest under variable precipitation") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#E5D4A7", "#A6761D", "#FBB4AE", "#D95F02",
                               "#B3E2D6", "#1B9E77", "#B3C8E8", "#7570B3",
                               "#D9D9D9", "#666666")) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom")
sonoran.species.ex



# Northern Arizona --------------------------------------------------------

## Count ------------------------------------------------------------------

# Desirable by PlotMix_Climate and PlantSource2 (native recruit outliers removed)
naz.des.count.plotmixclimate <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Density < 500) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  labs(title = "Northern Arizona, desirable species",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.des.count.plotmixclimate

# Weedy as single panel by PlantSource2 (unknown outliers removed)
naz.weed.count <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Density < 1800) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  labs(title = "Northern Arizona, weedy species",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.weed.count


# Seeded species by PlotMix_Climate and Duration/Lifeform
naz.seed.count.plotmixclimate <- dat |> 
  filter(PlotMix_Climate %in% c("Current-adapted mix", "Projected-adapted mix"),
         SpeciesSeeded == "Yes") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  facet_wrap(~PlotMix_Climate) +
  geom_smooth() +
  labs(title = "Northern Arizona, seeded species",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.seed.count.plotmixclimate


# Seeded species
# By seeded species: Current
naz.seed.count.current.species <- dat |> 
  filter(PlotMix_Climate == "Current-adapted mix",
         SpeciesSeeded == "Yes") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(!str_detect(Code, "SPP|SUNGR|UNFO|UNGR")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  labs(title = "Northern Arizona, seeded species (Current-adapted)",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals / ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
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



## Frequency --------------------------------------------------------------

# Native recruit, all plots
naz.freq.nativevolun |> 
  filter(Plot == "Total") |> 
  ggplot(aes(x = Code, y = Frequency)) +
  geom_bar(stat = "identity") +
  ggtitle("Northern Arizona, top native recruits, all plots") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))

# Native recruit, wetter/drier
naz.freq.nativevolun |> 
  filter(Plot %in% c("Wetter", "Drier")) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona, native recruit") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))

# Native recruit, extremes
naz.freq.nativevolun |> 
  filter(Plot %in% c("Wettest", "Driest")) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona, native recruit") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))

# Weedy, all plots
naz.freq.weed |> 
  filter(Plot == "Total") |> 
  filter(Code != "Empty") |> 
  ggplot(aes(x = Code, y = Frequency)) +
  geom_bar(stat = "identity") +
  ggtitle("Northern Arizona, weeds, all plots") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))

# Weedy, extremes
naz.freq.weed |> 
  filter(Plot %in% c("Wettest", "Driest")) |> 
  filter(Code != "Empty") |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona, weeds") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))

# Current mix, all plots
naz.freq.current |> 
  filter(Plot == "Total") |> 
  ggplot(aes(x = Code, y = Frequency)) +
  geom_bar(stat = "identity") +
  ggtitle("Northern Arizona, Current mix, all plots") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))

# Current mix, wetter/drier
naz.freq.current |> 
  filter(Plot %in% c("Wetter", "Drier")) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona, Current mix") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))

# Current mix, extremes
naz.freq.current |> 
  filter(Plot %in% c("Wettest", "Driest")) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona, Current mix") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))

# Projected mix, all plots
naz.freq.projected |> 
  filter(Plot == "Total") |> 
  ggplot(aes(x = Code, y = Frequency)) +
  geom_bar(stat = "identity") +
  ggtitle("Northern Arizona, Projected mix, all plots") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))

# Projected mix, wetter/drier
naz.freq.projected |> 
  filter(Plot %in% c("Wetter", "Drier")) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona, Projected mix") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))

# Projected mix, extremes
naz.freq.projected |> 
  filter(Plot %in% c("Wettest", "Driest")) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona, Projected mix") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))




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

save.image("RData/12.1_draft-figs-2.0_density-and-frequency.RData")
