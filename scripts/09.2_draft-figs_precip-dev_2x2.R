# Created: 2024-09-11
# Last updated: 2024-09-11

# Purpose: Graph relationships of Perc_dev_cum vs. Seeded_Cover for Sonoran Desert
#   and Northern Arizona Plateau.

library(tidyverse)
library(scales)
library(viridis)

# Load data ---------------------------------------------------------------

richness.cover <- read_csv("data/cleaned/04.2_2x2-richness-cover_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")

# Data wrangling ----------------------------------------------------------

# Remove rows with NA for Seeded_Cover
richness.cover <- richness.cover |> 
  filter(!is.na(Seeded_Cover))

# Remove monitoring events not included in 2x2 data
prism.data.2x2 <- prism.data |> 
  filter(SiteDateID %in% richness.cover$SiteDateID)
cum.pd.2x2 <- cum.pd |> 
  filter(SiteDateID %in% richness.cover$SiteDateID) |> 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Perc_deviation, Deviation_mm) |> 
  rename(Perc_dev_cum = Perc_deviation,
         Dev_mm_cum = Deviation_mm)

# Combine all variables
dat <- richness.cover |> 
  left_join(prism.data.2x2) |> 
  left_join(ai) |> 
  left_join(cum.pd.2x2)

# Add PlotMix_Climate col
dat <- dat |> 
  mutate(PlotMix_Climate = case_when(
    str_detect(dat$Site, "Creosote|Mesquite|Patagonia|SRER") & 
      dat$PlotMix == "Medium" ~ "Current",
    str_detect(dat$Site, "Creosote|Mesquite|Patagonia|SRER") & 
      dat$PlotMix == "Warm" ~ "Projected",
    str_detect(dat$Site, "AguaFria|MOWE|PEFO|Spiderweb") & 
      dat$PlotMix == "Med-Warm" ~ "Current",
    str_detect(dat$Site, "AguaFria|MOWE|PEFO|Spiderweb") & 
      dat$PlotMix == "Warm" ~ "Projected",
    str_detect(dat$Site, "BarTBar|FlyingM|CRC|Salt_Desert") & 
      dat$PlotMix == "Cool-Med" ~ "Current",
    str_detect(dat$Site, "BarTBar|FlyingM|CRC|Salt_Desert") & 
      dat$PlotMix == "Med-Warm" ~ "Projected",
    str_detect(dat$Site, "BabbittPJ|UtahPJ") & 
      dat$PlotMix == "Cool" ~ "Current",
    str_detect(dat$Site, "BabbittPJ|UtahPJ") & 
      dat$PlotMix == "Cool-Med" ~ "Projected",
    str_detect(dat$Site, "29_Palms|AVRCD|Preserve|SCC|Roosevelt|Pleasant|TLE") & 
      dat$PlotMix == "Cool" ~ "Current",
    str_detect(dat$Site, "29_Palms|AVRCD|Preserve|SCC|Roosevelt|Pleasant|TLE") & 
      dat$PlotMix == "Warm" ~ "Projected",
    TRUE ~ dat$PlotMix))
dat$PlotMix_Climate <- factor(dat$PlotMix_Climate, 
                              levels = c("None", "Current", "Projected"))

# Remove infinity
dat <- dat |> 
  filter(Perc_dev_cum != Inf)



# Sonoran Desert ----------------------------------------------------------

# Single panel
sonoran.seed <- dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point(aes(color = Desirable)) +
  geom_smooth() +
  ggtitle("Sonoran Desert, seeded cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed

# By PlotMix_Climate
sonoran.seed.plotmixclimate <- dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(PlotMix_Climate != "None") |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point(aes(color = Seeded)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, seeded cover") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(color = "Seeded richness") +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed.plotmixclimate


# Northern Arizona Plateau ------------------------------------------------

# Single panel
naz.seed <- dat |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point(aes(color = Desirable)) +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, seeded cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.seed

# By PlotMix_Climate
naz.seed.plotmixclimate <- dat |> 
  filter(Region == "Colorado Plateau") |> 
  filter(PlotMix_Climate != "None") |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point(aes(color = Seeded)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Northern Arizona Plateau, seeded cover") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(color = "Seeded richness") +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.seed.plotmixclimate



# Write out draft figures -------------------------------------------------

# Sonoran Desert
tiff("figures/2024-09_draft-figures/Sonoran_2x2-seeded-cover.tiff", units = "in", height = 4, width = 6, res = 150)
sonoran.seed
dev.off()

tiff("figures/2024-09_draft-figures/Sonoran_2x2-seeded-cover-by-PlotMix_Climate.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.seed.plotmixclimate
dev.off()

# Northern AZ
tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_2x2-seeded-cover.tiff", units = "in", height = 4, width = 6, res = 150)
naz.seed
dev.off()

tiff("figures/2024-09_draft-figures/Northern-AZ-Plateau_2x2-seeded-cover-by-PlotMix_Climate.tiff", units = "in", height = 5, width = 7, res = 150)
naz.seed.plotmixclimate
dev.off()


save.image("09.2_draft-figs_precip-dev_2x2.RData")
