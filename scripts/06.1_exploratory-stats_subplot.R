# Created: 2024-03-06
# Last updated: 2024-03-27

# Purpose: Begin to examine subplot trends as they relate to precip.

library(tidyverse)
library(scales)
library(viridis)
library(ggbreak)
library(ggpmisc)

# Load data ---------------------------------------------------------------

subplot <- read_csv("data/cleaned/04.1_subplot-data_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.cv <- read_csv("data/cleaned/03.3_cumulative-precip_CV_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
since.cv <- read_csv("data/cleaned/03.3_since-last-precip_CV_clean.csv")
since.pd <- read_csv("data/cleaned/03.3_since-last-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")


# Data wrangling ----------------------------------------------------------

# Check for NAs
apply(subplot, 2, anyNA)
count.na <- subplot |> 
  filter(is.na(Count))
height.na <- subplot |> 
  filter(is.na(Height))

# Remove ones with NAs for now until I can look at raw data sheets and correct them
subplot <- subplot |> 
  filter(!is.na(Count)) |> 
  filter(!is.na(Height))

# Reorganize columns for left_join()
cum.pd.subplot <- cum.pd |> 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Perc_deviation, Deviation_mm) |> 
  rename(Perc_dev_cum = Perc_deviation,
         Dev_mm_cum = Deviation_mm)
since.pd.subplot <- since.pd |> 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Perc_deviation, Deviation_mm) |> 
  rename(Perc_dev_since = Perc_deviation,
         Dev_mm_since = Deviation_mm)

# Combine all variables
dat <- subplot |> 
  left_join(prism.data) |> 
  left_join(ai) |> 
  left_join(cum.pd.subplot) |> 
  left_join(since.pd.subplot) |> 
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

# Data without Infinity
dat.noInf <- dat |> 
  filter(Perc_dev_cum != Inf)

# Control & Seed treatments only
dat.seed.trt <- dat.noInf |> 
  filter(Treatment %in% c("Seed", "Control"))


# Data without percent deviation outliers for cumulative
dat2 <- dat |> 
  filter(Perc_dev_cum < 3)

dat2.seed.trt <- dat2 |> 
  filter(Treatment %in% c("Seed", "Control"))



# Initial: Density by PlotMix, Climate, Weedy and Cum precip --------------

# All sites
#   geom_smooth() default
dat.noInf |> 
  filter(Weedy != "Weedy") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, desirable species")
dat.noInf |> 
  filter(Weedy != "Desirable") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, weedy")
#   Color gradient by aridity index
dat2.seed.trt |> 
  filter(Weedy != "Weedy") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = AridityIndex)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, desirable species") +
  scale_color_viridis() +
  scale_x_continuous(labels = scales::percent)
dat2.seed.trt |> 
  filter(Weedy != "Desirable") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = AridityIndex)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, weedy") +
  scale_color_viridis() +
  scale_x_continuous(labels = scales::percent)
#   Linear model
dat.noInf |> 
  filter(Weedy != "Weedy") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, desirable species")
dat.noInf |> 
  filter(Weedy != "Desirable") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, weedy")
#   Remove high Perc_dev_cum to see lower trends
dat2 |> 
  filter(Weedy != "Weedy") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, desirable species (<3 deviation)")
dat2 |> 
  filter(Weedy != "Desirable") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, weedy (<3 deviation)")


# Sonoran Desert
dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, desirable species") +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent)
dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, weedy")  +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent)
dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth(aes(color = PlantSource)) +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, desirable species")  +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent)
dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth(aes(color = PlantSource)) +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, weedy")  +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent)

# Utah
dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Utah") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Utah, desirable species") +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent)
dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Utah") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Utah, weedy") +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent)

# Chihuahuan
dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Chihuahuan") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Chihuahuan, desirable species")
dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Chihuahuan") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Chihuahuan, weedy")

# CO Plateau
dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Colorado Plateau, desirable species") +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent)
dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Colorado Plateau, weedy") +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent)
#   Remove very high Count values to better see lower trends
dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Count < 150) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Colorado Plateau, weedy (<150 count)")


# Mojave
dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Mojave") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Mojave, desirable species")
dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Mojave") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Mojave, weedy")




# 2024-03 draft figures ---------------------------------------------------

## Density, Control & Seed only --------------------------------------------


# All sites
#   Although tbh there is probably too much going on in these graphs
dat2.seed.trt |> 
  filter(Weedy != "Weedy") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = AridityIndex,
                 shape = PlantSource2)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_color_viridis(option ="magma", direction = -1) +
  scale_shape_manual(values = c(20, 17, 15)) 

seedcon.all.des <- dat2.seed.trt |> 
  filter(Weedy != "Weedy") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") 
seedcon.all.des
seedcon.all.weed <- dat2.seed.trt |> 
  filter(Weedy != "Desirable") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, weedy")  +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")
seedcon.all.weed
seedcon.all.weed.outrm <- dat2.seed.trt |> 
  filter(Weedy != "Desirable") |> 
  filter(Count < 100) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, weedy (outlier removed)")  +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")
seedcon.all.weed.outrm


# Sonoran Desert
seedcon.sonoran.des <- dat2.seed.trt |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")  +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02"))
seedcon.sonoran.des
seedcon.sonoran.weed <- dat2.seed.trt |> 
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, weedy species")  +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")  +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77"))
seedcon.sonoran.weed
#   Linear model
dat.seed.trt |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth(method = "lm") +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, desirable species") +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent)
dat.seed.trt |> 
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth(method = "lm") +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, weedy")  +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent)

# CO Plateau
seedcon.co.des <- dat2.seed.trt |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Colorado Plateau, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")  +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02"))
seedcon.co.des
seedcon.co.weed <- dat2.seed.trt |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Count < 100) |> # removing outliers
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Colorado Plateau, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")  +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77"))
seedcon.co.weed

# Utah
seedcon.utah.des <- dat2.seed.trt |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Utah") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Utah, desirable species") +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")
seedcon.utah.des
seedcon.utah.weed <- dat2.seed.trt |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Utah") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Utah, weedy") +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")
seedcon.utah.weed

# Chihuahuan
#   Chihuahuan is weird because there's not a lot of sampling, nothing grew, and
#   they were always in drought.
seedcon.chihua.des <- dat2.seed.trt |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Chihuahuan") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Chihuahuan, desirable species") +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")
seedcon.chihua.des
seedcon.chihua.weed <- dat2.seed.trt |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Chihuahuan") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Chihuahuan, weedy") +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")
seedcon.chihua.weed

# Mojave
#   Mojave is super weird because there aren't a lot of points
seedcon.mojave.des <- dat2.seed.trt |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Mojave") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Mojave, desirable species") +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")
seedcon.mojave.des
seedcon.mojave.weed <- dat2.seed.trt |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Mojave") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Mojave, weedy") +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals")
seedcon.mojave.weed


# Write out draft figures
tiff("figures/2024-03_draft-figures/Sonoran_weedy_seed-control-only.tiff", units = "in", height = 5, width = 7, res = 150)
seedcon.sonoran.weed
dev.off()
tiff("figures/2024-03_draft-figures/Sonoran_desirable_seed-control-only.tiff", units = "in", height = 5, width = 7, res = 150)
seedcon.sonoran.des
dev.off()

tiff("figures/2024-03_draft-figures/CO-Plateau_weedy_seed-control-only.tiff", units = "in", height = 5, width = 7, res = 150)
seedcon.co.weed
dev.off()
tiff("figures/2024-03_draft-figures/CO-Plateau_desirable_seed-control-only.tiff", units = "in", height = 5, width = 7, res = 150)
seedcon.co.des
dev.off()

tiff("figures/2024-03_draft-figures/Utah_weedy_seed-control-only.tiff", units = "in", height = 5, width = 7, res = 150)
seedcon.utah.weed
dev.off()
tiff("figures/2024-03_draft-figures/Utah_desirable_seed-control-only.tiff", units = "in", height = 5, width = 7, res = 150)
seedcon.utah.des
dev.off()

tiff("figures/2024-03_draft-figures/Chihuahuan_weedy_seed-control-only.tiff", units = "in", height = 5, width = 7, res = 150)
seedcon.chihua.weed
dev.off()
tiff("figures/2024-03_draft-figures/Chihuahuan_desirable_seed-control-only.tiff", units = "in", height = 5, width = 7, res = 150)
seedcon.chihua.des
dev.off()

tiff("figures/2024-03_draft-figures/Mojave_weedy_seed-control-only.tiff", units = "in", height = 5, width = 7, res = 150)
seedcon.mojave.weed
dev.off()
tiff("figures/2024-03_draft-figures/Mojave_desirable_seed-control-only.tiff", units = "in", height = 5, width = 7, res = 150)
seedcon.mojave.des
dev.off()





# Linear models -----------------------------------------------------------

# Count
# Control & Seed treatments only
dat.seed.trt |>
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  facet_wrap(~Treatment)
lm.seeded1 <- lm(Seeded_Cover ~ Perc_dev_cum + Treatment, 
                 data = dat.seed.trt)
summary(lm.seeded1)

lm.seeded2 <- lm(Seeded_Cover ~ Perc_dev_cum + Treatment + AridityIndex, 
                 data = dat.seed.trt)
summary(lm.seeded2)

save.image("RData/06.1_exploratory-stats_subplot.RData")


