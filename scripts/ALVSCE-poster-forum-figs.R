# Created: 2024-03-28
# Last updated: 2024-03-28

# Purpose: Create figures for ALVSCE 2024 poster.

library(tidyverse)
library(tidyverse)
library(scales)
library(ggpubr)

# Load data ---------------------------------------------------------------

subplot <- read_csv("data/cleaned/04.1_subplot-data_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")


# Data wrangling ----------------------------------------------------------

# Remove ones with NA counts for now 
subplot <- subplot |> 
  filter(!is.na(Count)) 

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


# Add PlotMix_Climate col
#   Look at seed mix to see which is the current-adapted and which is the projected mix;
#     PlotMix names alone cannot be used to group.
dat <- dat |> 
  mutate(PlotMix_Climate = case_when(
    str_detect(dat$Site, "Creosote|Mesquite|Patagonia|SRER") & 
      dat$PlotMix == "Medium" ~ "Current climate mix",
    str_detect(dat$Site, "Creosote|Mesquite|Patagonia|SRER") & 
      dat$PlotMix == "Warm" ~ "Projected climate mix",
    str_detect(dat$Site, "AguaFria|MOWE|PEFO|Spiderweb") & 
      dat$PlotMix == "Med-Warm" ~ "Current climate mix",
    str_detect(dat$Site, "AguaFria|MOWE|PEFO|Spiderweb") & 
      dat$PlotMix == "Warm" ~ "Projected climate mix",
    str_detect(dat$Site, "BarTBar|FlyingM|CRC|Salt_Desert") & 
      dat$PlotMix == "Cool-Med" ~ "Current climate mix",
    str_detect(dat$Site, "BarTBar|FlyingM|CRC|Salt_Desert") & 
      dat$PlotMix == "Med-Warm" ~ "Projected climate mix",
    str_detect(dat$Site, "BabbittPJ|UtahPJ") & 
      dat$PlotMix == "Cool" ~ "Current climate mix",
    str_detect(dat$Site, "BabbittPJ|UtahPJ") & 
      dat$PlotMix == "Cool-Med" ~ "Projected climate mix",
    str_detect(dat$Site, "29_Palms|AVRCD|Preserve|SCC|Roosevelt|Pleasant|TLE") & 
      dat$PlotMix == "Cool" ~ "Current climate mix",
    str_detect(dat$Site, "29_Palms|AVRCD|Preserve|SCC|Roosevelt|Pleasant|TLE") & 
      dat$PlotMix == "Warm" ~ "Projected climate mix",
    dat$PlotMix == "None" ~ "Not seeded"))
dat$PlotMix_Climate <- factor(dat$PlotMix_Climate, 
                              levels = c("Not seeded", "Current climate mix", 
                                         "Projected climate mix"))

# Add Weedy column
unique(dat$PlantSource)
dat <- dat |> 
  mutate(Weedy = case_when(
    str_detect(dat$PlantSource, "Unknown_recruit|Introduced/Invasive") ~ "Weedy",
    str_detect(dat$PlantSource, "Native_recruit|Likely native_recruit|Seeded") ~ "Desirable",
    TRUE ~ dat$PlantSource))

# Add PlantSource2 column
unique(dat$PlantSource)
dat <- dat |> 
  mutate(PlantSource2 = case_when(
    dat$PlantSource == "Unknown_recruit" ~ "Recruit",
    str_detect(dat$PlantSource, "Native_recruit|Likely native_recruit") ~ "Native recruit",
    TRUE ~ dat$PlantSource))

# Remove Infinities and keep Control & Seed treatments only
dat.seed.trt <- dat |> 
  filter(Perc_dev_cum != Inf) |> 
  filter(Treatment %in% c("Seed", "Control"))


# Data without percent deviation outliers for cumulative
dat2 <- dat |> 
  filter(Perc_dev_cum < 3)

dat2.seed.trt <- dat2 |> 
  filter(Treatment %in% c("Seed", "Control"))


# Make figures ------------------------------------------------------------

# There will be 4 figures of Count by Perc_dev_cum (density by percent deviation from cumulative): 
#   1) Sonoran Desert, desirable species
#   2) Sonoran Desert, weedy species
#   3) CO Plateau, desirable species
#   4) CO Plateau, weedy species

# Sonoran Desert
#   Desirable
seedcon.sonoran.des <- dat2.seed.trt |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")  +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.2, l = 0.1, "in"))
seedcon.sonoran.des

#   Weedy
seedcon.sonoran.weed <- dat2.seed.trt |> 
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, weedy species")  +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")  +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.2, l = 0.1, "in"))
seedcon.sonoran.weed


# CO Plateau
#   Desirable
seedcon.co.des <- dat2.seed.trt |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Arizona-Colorado Plateau, desirable species") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")  +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02"))  +
  theme(legend.title = element_blank()) +
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.2, l = 0.1, "in")) +
  theme(axis.text.x = element_text(angle = 35))
seedcon.co.des

#   Weedy
seedcon.co.weed <- dat2.seed.trt |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Count < 100) |> # removing outliers
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Arizona-Colorado Plateau, weedy species") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")  +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77"))  +
  theme(legend.title = element_blank()) +
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.2, l = 0.1, "in"))  +
  theme(axis.text.x = element_text(angle = 35))
seedcon.co.weed


# Write to TIFF -----------------------------------------------------------

# Combine into single figure
tiff("figures/2024-04_ALVSCE/Sonoran_CO-Plateau_combined.tiff", units = "in", height = 9, width = 16.5, res = 300)
ggarrange(seedcon.sonoran.des, seedcon.sonoran.weed, seedcon.co.des, seedcon.co.weed,
          nrow = 2, ncol = 2,
          labels = c("(A)", "(B)", "(C)", "(D)"))
dev.off()


save.image("RData/ALVSCE-poster-forum-figs.RData")
