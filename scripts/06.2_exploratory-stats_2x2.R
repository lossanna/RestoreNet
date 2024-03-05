# Created: 2024-02-27
# Last updated: 2024-03-05

# Purpose: Begin to examine 2x2 trends as they relate to precip.

library(tidyverse)

# Load data ---------------------------------------------------------------

richness.cover <- read_csv("data/cleaned/04.2_2x2-richness-cover_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.cv <- read_csv("data/cleaned/03.3_cumulative-precip_CV_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
since.cv <- read_csv("data/cleaned/03.3_since-last-precip_CV_clean.csv")
since.pd <- read_csv("data/cleaned/03.3_since-last-precip_percent-deviation-from-norm_clean.csv")


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

# Combine
dat <- richness.cover |> 
  left_join(prism.data.2x2) |> 
  left_join(cum.pd.2x2) |> 
  left_join(since.pd.2x2) |> 
  left_join(cum.cv) |> 
  left_join(since.cv)

# Check for NAs
apply(dat, 2, anyNA)


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