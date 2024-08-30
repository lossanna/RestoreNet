# Created: 2024-02-27
# Last updated: 2024-05-26

# Purpose: Begin to examine 2x2 trends as they relate to precip.

library(tidyverse)
library(scales)
library(viridis)


# Load data ---------------------------------------------------------------

richness.cover <- read_csv("data/cleaned/04.2_2x2-richness-cover_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.cv <- read_csv("data/cleaned/03.3_cumulative-precip_CV_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
since.cv <- read_csv("data/cleaned/03.3_since-last-precip_CV_clean.csv")
since.pd <- read_csv("data/cleaned/03.3_since-last-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")


# Data wrangling ----------------------------------------------------------

# Check for NAs
apply(richness.cover, 2, anyNA)
seeded.cover.na <- richness.cover |> 
  filter(is.na(Seeded_Cover))
total.cover.na <- richness.cover |> 
  filter(is.na(Total_Veg_Cover))
not.seeded.na <- richness.cover |> 
  filter(is.na(Not_Seeded_Cover))

# Remove ones with NAs for now until I can look at raw data sheets and correct them
richness.cover <- richness.cover |> 
  filter(!SiteDatePlotID %in% not.seeded.na$SiteDatePlotID)

# Remove monitoring events not included in 2x2 data
prism.data.2x2 <- prism.data |> 
  filter(SiteDateID %in% richness.cover$SiteDateID)
cum.pd.2x2 <- cum.pd |> 
  filter(SiteDateID %in% richness.cover$SiteDateID) |> 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Perc_deviation, Deviation_mm) |> 
  rename(Perc_dev_cum = Perc_deviation,
         Dev_mm_cum = Deviation_mm)
since.pd.2x2 <- since.pd |> 
  filter(SiteDateID %in% richness.cover$SiteDateID) |> 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Perc_deviation, Deviation_mm) |> 
  rename(Perc_dev_since = Perc_deviation,
         Dev_mm_since = Deviation_mm)

# Combine all variables
dat <- richness.cover |> 
  left_join(prism.data.2x2) |> 
  left_join(ai) |> 
  left_join(cum.pd.2x2) |> 
  left_join(since.pd.2x2) |> 
  left_join(cum.cv) |> 
  left_join(since.cv)

# Check for NAs
apply(dat, 2, anyNA)


# Add PlotMix_Climate col
#   Look at seed mix to see which is the current-adapted and which is the projected mix;
#     PlotMix names alone cannot be used to group.
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


# Control & Seed treatments only
dat.seed.trt <- dat|> 
  filter(Treatment %in% c("Seed", "Control"))



# Visualize relationships -------------------------------------------------

## Cover by PlotMix_Climate and Cumulative ---------------------------------

# All sites
dat |> 
  filter(Perc_dev_cum != Inf) |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites")
dat |> 
  filter(Perc_dev_cum != Inf) |>
  ggplot(aes(x = Perc_dev_cum, y = Total_Veg_Cover)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) + 
  ggtitle("All sites")


# Sonoran Desert
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert")
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Total_Veg_Cover)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert")


# CO Plateau
dat |> 
  filter(Region == "Colorado Plateau") |>
  filter(Perc_dev_cum != Inf) |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Colorado Plateau")
dat |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum != Inf) |> 
  ggplot(aes(x = Perc_dev_cum, y = Total_Veg_Cover)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Colorado Plateau")


# Mojave
dat |> 
  filter(Region == "Mojave") |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Mojave")
dat |> 
  filter(Region == "Mojave") |> 
  ggplot(aes(x = Perc_dev_cum, y = Total_Veg_Cover)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Mojave")


# Chihuahuan
dat |> 
  filter(Region == "Chihuahuan") |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Chihuahuan")
dat |> 
  filter(Region == "Chihuahuan") |> 
  ggplot(aes(x = Perc_dev_cum, y = Total_Veg_Cover)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Chihuahuan")


# Utah
#   Utah is weird because they didn't take 2x2 data for a lot of it
dat |> 
  filter(Region == "Utah") |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Utah")
dat |> 
  filter(Region == "Utah") |> 
  ggplot(aes(x = Perc_dev_cum, y = Total_Veg_Cover)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Utah")


# CO Plateau & Utah
dat |> 
  filter(Region %in% c("Colorado Plateau", "Utah")) |> 
  filter(Perc_dev_cum != Inf) |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Colorado Plateau & Utah")
dat |> 
  filter(Region %in% c("Colorado Plateau", "Utah")) |> 
  filter(Perc_dev_cum != Inf) |> 
  ggplot(aes(x = Perc_dev_cum, y = Total_Veg_Cover)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Colorado Plateau & Utah")



## Number of Weedy by PlotMix_Climate and Cumulative -----------------------

# All sites
dat |> 
  ggplot(aes(x = Perc_dev_cum, y = Desirable)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites")
dat |> 
  filter(Perc_dev_cum != Inf) |>
  ggplot(aes(x = Perc_dev_cum, y = Weedy)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) + 
  ggtitle("All sites")


# Sonoran Desert
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Desirable)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert")
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Weedy)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert")


# CO Plateau
dat |> 
  filter(Region == "Colorado Plateau") |>
  filter(Perc_dev_cum != Inf) |> 
  ggplot(aes(x = Perc_dev_cum, y = Desirable)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Colorado Plateau")
dat |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum != Inf) |> 
  ggplot(aes(x = Perc_dev_cum, y = Weedy)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Colorado Plateau")


# Chihuahuan
dat |> 
  filter(Region == "Chihuahuan") |> 
  ggplot(aes(x = Perc_dev_cum, y = Desirable)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Chihuahuan")
dat |> 
  filter(Region == "Chihuahuan") |> 
  ggplot(aes(x = Perc_dev_cum, y = Weedy)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Chihuahuan")

# Mojave
dat |> 
  filter(Region == "Mojave") |> 
  ggplot(aes(x = Perc_dev_cum, y = Desirable)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Mojave")
dat |> 
  filter(Region == "Mojave") |> 
  ggplot(aes(x = Perc_dev_cum, y = Weedy)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Mojave")


# 2024-03 draft figures ---------------------------------------------------

# Sonoran Desert
seedcon.sonoran.seed <- dat.seed.trt |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point(aes(color = Desirable)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, seeded cover") +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  theme(legend.position = "bottom")
seedcon.sonoran.seed
seedcon.sonoran.total <- dat.seed.trt |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Total_Veg_Cover)) +
  geom_point(aes(color = Weedy)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, total cover") +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  theme(legend.position = "bottom")
seedcon.sonoran.total

# CO Plateau
seedcon.co.seed <- dat.seed.trt |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point(aes(color = Desirable)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Colorado Plateau, seeded cover") +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  theme(legend.position = "bottom")
seedcon.co.seed
seedcon.co.total <- dat.seed.trt |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Total_Veg_Cover)) +
  geom_point(aes(color = Weedy)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Colorado Plateau, total cover") +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  theme(legend.position = "bottom")
seedcon.co.total


# Write out draft figures
tiff("figures/2024-03_draft-figures/Sonoran_seeded-cover_seed-control-only.tiff", units = "in", height = 5, width = 7, res = 150)
seedcon.sonoran.seed
dev.off()
tiff("figures/2024-03_draft-figures/Sonoran_total-cover_seed-control-only.tiff", units = "in", height = 5, width = 7, res = 150)
seedcon.sonoran.total
dev.off()

tiff("figures/2024-03_draft-figures/CO-Plateau_seeded-cover_seed-control-only.tiff", units = "in", height = 5, width = 7, res = 150)
seedcon.co.seed
dev.off()
tiff("figures/2024-03_draft-figures/CO-Plateau_total-cover_seed-control-only.tiff", units = "in", height = 5, width = 7, res = 150)
seedcon.co.total
dev.off()


# 2024-08 draft figures ---------------------------------------------------

## Seeded cover -----------------------------------------------------------

# Sonoran Desert, single panel
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
sonoran.total <- dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Total_Veg_Cover)) +
  geom_point(aes(color = Weedy)) +
  geom_smooth() +
  ggtitle("Sonoran Desert, total cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.total

sonoran.seed.quad <- dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point(aes(color = Desirable)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  ggtitle("Sonoran Desert, seeded cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed.quad

# Sonoran Desert, by PlotMix_Climate
sonoran.seed.plotmixclimate <- dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point(aes(color = Desirable)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, seeded cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.seed.plotmixclimate
sonoran.total.plotmixclimate <- dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Total_Veg_Cover)) +
  geom_point(aes(color = Weedy)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, total cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.total.plotmixclimate


# CO Plateau, single panel
co.seed <- dat |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point(aes(color = Desirable)) +
  geom_smooth() +
  ggtitle("Nothern Arizona Plateau, seeded cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
co.seed
co.total <- dat |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Total_Veg_Cover)) +
  geom_point(aes(color = Weedy)) +
  geom_smooth() +
  ggtitle("Nothern Arizona Plateau, total cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
co.total

# CO Plateau, by PlotMix_Climate
co.seed.plotmixclimate <- dat |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point(aes(color = Desirable)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Northern Arizona Plateau, seeded cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
co.seed.plotmixclimate
co.total.plotmixclimate <- dat |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Total_Veg_Cover)) +
  geom_point(aes(color = Weedy)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Northern Arizona Plateau, total cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
co.total.plotmixclimate


# All sites, single panel
all.seed <- dat |> 
  filter(Perc_dev_cum < 8) |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point(aes(color = Desirable)) +
  geom_smooth() +
  ggtitle("All sites, seeded cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.seed
all.total <- dat |> 
  filter(Perc_dev_cum < 8) |> 
  ggplot(aes(x = Perc_dev_cum, y = Total_Veg_Cover)) +
  geom_point(aes(color = Weedy)) +
  geom_smooth() +
  ggtitle("All sites, total cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.total

all.seed.quad <- dat |> 
  filter(Perc_dev_cum < 8) |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point(aes(color = Desirable)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  ggtitle("All sites, seeded cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.seed.quad

# All sites, by PlotMix_Climate
all.seed.plotmixclimate <- dat |> 
  filter(Perc_dev_cum < 8) |> 
  ggplot(aes(x = Perc_dev_cum, y = Seeded_Cover)) +
  geom_point(aes(color = Desirable)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, seeded cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.seed.plotmixclimate
all.total.plotmixclimate <- dat |> 
  filter(Perc_dev_cum < 8) |> 
  ggplot(aes(x = Perc_dev_cum, y = Total_Veg_Cover)) +
  geom_point(aes(color = Weedy)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, total cover") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(direction = -1) +
  xlab("Cumulative precip deviation from normals") +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.total.plotmixclimate


# Richness ----------------------------------------------------------------

# All sites, single panel
all.des <- dat |> 
  filter(Perc_dev_cum < 8) |> 
  ggplot(aes(x = Perc_dev_cum, y = Desirable)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, desirable species richness") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.des
all.weed <- dat |> 
  filter(Perc_dev_cum < 8) |> 
  ggplot(aes(x = Perc_dev_cum, y = Weedy)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, weedy species richness") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.weed

all.des.quad <- dat |> 
  filter(Perc_dev_cum < 8) |> 
  ggplot(aes(x = Perc_dev_cum, y = Desirable)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  ggtitle("All sites, desirable species richness") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.des.quad

all.des.linear <- dat |> 
  filter(Perc_dev_cum < 8) |> 
  ggplot(aes(x = Perc_dev_cum, y = Desirable)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("All sites, desirable species richness") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)
all.des.linear

# Sonoran Desert, single panel
sonoran.des <- dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Desirable)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Sonoran Desert, desirable species richness") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.des
sonoran.weed <- dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Weedy)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Sonoran Desert, weedy species richness") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.weed

# CO Plateau, single panel
naz.des <- dat |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Desirable)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, desirable species richness") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.des
naz.weed <- dat |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Weedy)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, weedy species richness") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
naz.weed



## Write out figures ------------------------------------------------------

# All sites
tiff("figures/2024-08_draft-figures/All-sites_2x2-seeded-cover.tiff", units = "in", height = 4, width = 6, res = 150)
all.seed
dev.off()

tiff("figures/2024-08_draft-figures/All-sites_2x2-desirable-richness.tiff", units = "in", height = 4, width = 5, res = 150)
all.des
dev.off()

tiff("figures/2024-08_draft-figures/All-sites_2x2-weedy-richness.tiff", units = "in", height = 4, width = 5, res = 150)
all.weed
dev.off()

# Sonoran Desert
tiff("figures/2024-08_draft-figures/Sonoran_2x2-seeded-cover.tiff", units = "in", height = 4, width = 6, res = 150)
sonoran.seed
dev.off()

tiff("figures/2024-08_draft-figures/Sonoran_2x2-desirable-richness.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.des
dev.off()

tiff("figures/2024-08_draft-figures/Sonoran_2x2-weedy-richness.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.weed
dev.off()

# CO Plateau
tiff("figures/2024-08_draft-figures/CO-Plateau_2x2-seeded-cover.tiff", units = "in", height = 4, width = 6, res = 150)
co.seed
dev.off()

tiff("figures/2024-08_draft-figures/CO-Plateau_2x2-desirable-richness.tiff", units = "in", height = 4, width = 5, res = 150)
naz.des
dev.off()

tiff("figures/2024-08_draft-figures/CO-Plateau_2x2-weedy-richness.tiff", units = "in", height = 4, width = 5, res = 150)
naz.weed
dev.off()


save.image("RData/06.2_exploratory-graphs_2x2.RData")
