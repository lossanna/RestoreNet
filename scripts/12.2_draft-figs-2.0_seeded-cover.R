# Created: 2024-10-18
# Last updated: 2024-10-18

# Purpose: Graph Seeded_Cover as a function of precip var for 2x2 plots, for supp figs.

library(tidyverse)
library(scales)
library(viridis)
library(ggpubr)

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

# Reorganize columns for left_join()
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
      dat$PlotMix == "Medium" ~ "Current-adapted mix",
    str_detect(dat$Site, "Creosote|Mesquite|Patagonia|SRER") & 
      dat$PlotMix == "Warm" ~ "Projected-adapted mix",
    str_detect(dat$Site, "AguaFria|MOWE|PEFO|Spiderweb") & 
      dat$PlotMix == "Med-Warm" ~ "Current-adapted mix",
    str_detect(dat$Site, "AguaFria|MOWE|PEFO|Spiderweb") & 
      dat$PlotMix == "Warm" ~ "Projected-adapted mix",
    str_detect(dat$Site, "BarTBar|FlyingM|CRC|Salt_Desert") & 
      dat$PlotMix == "Cool-Med" ~ "Current-adapted mix",
    str_detect(dat$Site, "BarTBar|FlyingM|CRC|Salt_Desert") & 
      dat$PlotMix == "Med-Warm" ~ "Projected-adapted mix",
    str_detect(dat$Site, "BabbittPJ|UtahPJ") & 
      dat$PlotMix == "Cool" ~ "Current-adapted mix",
    str_detect(dat$Site, "BabbittPJ|UtahPJ") & 
      dat$PlotMix == "Cool-Med" ~ "Projected-adapted mix",
    str_detect(dat$Site, "29_Palms|AVRCD|Preserve|SCC|Roosevelt|Pleasant|TLE") & 
      dat$PlotMix == "Cool" ~ "Current-adapted mix",
    str_detect(dat$Site, "29_Palms|AVRCD|Preserve|SCC|Roosevelt|Pleasant|TLE") & 
      dat$PlotMix == "Warm" ~ "Projected-adapted mix",
    TRUE ~ "None"))
dat$PlotMix_Climate <- factor(dat$PlotMix_Climate, 
                              levels = c("None", "Current-adapted mix", "Projected-adapted mix"))

# Remove infinity
dat <- dat |> 
  filter(Perc_dev_cum != Inf)

# Add cover col as decimal
dat <- dat |> 
  mutate(seeded_cover = Seeded_Cover / 100)


# Sonoran Desert ----------------------------------------------------------

# Single panel
sonoran.seed <- dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = seeded_cover)) +
  geom_point(aes(color = Seeded)) +
  geom_smooth() +
  ggtitle("Sonoran Desert, seeded species cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precipitation deviation from normals") +
  ylab("Cover") +
  labs(color = "Seeded species richness") +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed

# By PlotMix_Climate
sonoran.seed.plotmixclimate <- dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(PlotMix_Climate != "None") |> 
  ggplot(aes(x = Perc_dev_cum, y = seeded_cover)) +
  geom_point(aes(color = Seeded)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, seeded species cover") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(color = "Seeded species richness") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precipitation deviation from normals") +
  ylab("Cover") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) 
sonoran.seed.plotmixclimate


# Northern Arizona Plateau ------------------------------------------------

# Single panel
naz.seed <- dat |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = seeded_cover)) +
  geom_point(aes(color = Desirable)) +
  geom_smooth() +
  ggtitle("Northern Arizona, seeded species cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precipitation deviation from normals") +
  ylab("Cover") +
  labs(color = "Seeded species richness") +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.seed

# By PlotMix_Climate
naz.seed.plotmixclimate <- dat |> 
  filter(Region == "Colorado Plateau") |> 
  filter(PlotMix_Climate != "None") |> 
  ggplot(aes(x = Perc_dev_cum, y = seeded_cover)) +
  geom_point(aes(color = Seeded)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Northern Arizona, seeded species cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precipitation deviation from normals") +
  ylab("Cover") +
  labs(color = "Seeded species richness") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in"))
naz.seed.plotmixclimate



# Write out draft figures -------------------------------------------------

# Sonoran Desert
tiff("figures/2024-09_draft-figures-2.0/Sonoran_2x2-seeded-cover.tiff", units = "in", height = 4, width = 6, res = 150)
sonoran.seed
dev.off()

tiff("figures/2024-09_draft-figures-2.0/Sonoran_2x2-seeded-cover-by-PlotMix_Climate.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.seed.plotmixclimate
dev.off()

# Northern AZ
tiff("figures/2024-09_draft-figures-2.0/Northern-AZ_2x2-seeded-cover.tiff", units = "in", height = 4, width = 6, res = 150)
naz.seed
dev.off()

tiff("figures/2024-09_draft-figures-2.0/Northern-AZ_2x2-seeded-cover-by-PlotMix_Climate.tiff", units = "in", height = 5, width = 7, res = 150)
naz.seed.plotmixclimate
dev.off()


## Figure S3: Seeded species cover ----------------------------------------

tiff("figures/2024-09_draft-figures-2.0/FigS3_seeded-cover_2x2.tiff", units = "in", height = 5, width = 11, res = 150)
ggarrange(sonoran.seed.plotmixclimate, naz.seed.plotmixclimate,
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)"),
          common.legend = TRUE, legend = "bottom") 
dev.off()

save.image("RData/12.2_draft-figs-2.0_seeded-cover.RData")
