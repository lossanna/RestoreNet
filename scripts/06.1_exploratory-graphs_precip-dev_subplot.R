# Created: 2024-03-06
# Last updated: 2024-09-05

# Purpose: Begin to examine subplot trends as they relate to precip. Focus on Perc_dev_cum
#   and relationship to response variables Count & Height. Identify what seeded species from 
#   mixes are doing well.

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
  filter(is.na(Height)) # some have no Height recorded but do have Count

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


# Data without Infinity
#   Inf created when there was no rain in the time period (can't divide by 0),
#     but this only occurs when the time period is small and in all cases there is another
#     monitoring date less than 2 weeks away with a non-Inf percent change. Occurs once
#     at CO Plateau (BarTBar).
dat <- dat |> 
  filter(Perc_dev_cum != Inf)

# Control & Seed treatments only
dat.seed.trt <- dat |> 
  filter(Treatment %in% c("Seed", "Control"))


# Data without percent deviation outliers for cumulative
dat2 <- dat |> 
  filter(Perc_dev_cum < 3)

dat2.seed.trt <- dat2 |> 
  filter(Treatment %in% c("Seed", "Control"))



# Initial: Density by PlotMix, Climate, Weedy and Cum precip --------------

# All sites
#   geom_smooth() default
dat |> 
  filter(Weedy != "Weedy") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, desirable species")
dat |> 
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
dat |> 
  filter(Weedy != "Weedy") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, desirable species")
dat |> 
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
  ggtitle("Sonoran Desert, weedy") +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent)
dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth(aes(color = PlantSource)) +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, desirable species") +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent)
dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource)) +
  geom_smooth(aes(color = PlantSource)) +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Sonoran Desert, weedy") +
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
  ggtitle("All sites, weedy") +
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
  ggtitle("All sites, weedy (outlier removed)") +
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
  xlab("Cumulative precip deviation from normals") +
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
  ggtitle("Sonoran Desert, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
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
  ggtitle("Sonoran Desert, weedy") +
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
  xlab("Cumulative precip deviation from normals") +
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
  xlab("Cumulative precip deviation from normals") +
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



# 2024-08 draft figures ---------------------------------------------------

## Count -------------------------------------------------------------------

# Includes all treatments, outliers not removed (except for 800% precip deviation in all data)

### All sites -------------------------------------------------------------

# Extreme precip deviation outliers removed (800% deviation) for better visualization
dat |> 
  filter(Perc_dev_cum > 1) |> 
  select(Region, Site, Date_Monitored, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE)

# Single panel by PlantSource2
all.des.count <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Perc_dev_cum < 8) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  ggtitle("All sites, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.des.count
all.weed.count <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Perc_dev_cum < 8) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  ggtitle("All sites, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.weed.count

# By PlotMix_Climate and PlantSource2
all.des.count.plotmixclimate <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Perc_dev_cum < 8) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.des.count.plotmixclimate
all.weed.count.plotmixclimate <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Perc_dev_cum < 8) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.weed.count.plotmixclimate

# By Lifeform (forb, grass, shrub) and PlantSource2
#   Almost no weedy shrubs, so they aren't included
all.des.count.forbgrassshrub.plantsource2 <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Perc_dev_cum < 8) |>
  filter(Lifeform %in% c("Forb", "Grass", "Shrub")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Lifeform) +
  ggtitle("All sites, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(17, 15)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.des.count.forbgrassshrub.plantsource2
all.weed.count.forbgrass.plantsource2 <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Perc_dev_cum < 8) |>
  filter(Lifeform %in% c("Forb", "Grass")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Lifeform) +
  ggtitle("All sites, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(15, 17)) +
  scale_color_manual(values = c("#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.weed.count.forbgrass.plantsource2

# By Duration (annual & perennial only) and Lifeform
all.des.count.perennial.annual.lifeform <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Perc_dev_cum < 8) |>
  filter(Duration %in% c("Annual", "Perennial")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Duration) +
  ggtitle("All sites, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17, 20)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02", "#666666")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.des.count.perennial.annual.lifeform
all.weed.count.perennial.annual.lifeform <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Perc_dev_cum < 8) |>
  filter(Duration %in% c("Annual", "Perennial")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Duration) +
  ggtitle("All sites, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17, 20)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02", "#666666")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.weed.count.perennial.annual.lifeform

# By Treatment and PlantSource2
all.des.count.treatment <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Perc_dev_cum < 8) |>
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Treatment) +
  ggtitle("All sites, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.des.count.treatment
all.weed.count.treatment <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Perc_dev_cum < 8) |>
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Treatment) +
  ggtitle("All sites, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.weed.count.treatment

# Single by AridityIndex
all.des.count.ai <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Perc_dev_cum < 8) |>
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = AridityIndex)) +
  geom_smooth() +
  ggtitle("All sites, desirable species") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_color_viridis(direction = -1) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.des.count.ai
all.weed.count.ai <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Perc_dev_cum < 8) |>
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = AridityIndex)) +
  geom_smooth() +
  ggtitle("All sites, weedy species") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_color_viridis(direction = -1) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.weed.count.ai



### Sonoran Desert --------------------------------------------------------

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


### Northern Arizona Plateau ----------------------------------------------

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
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
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
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
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
  scale_shape_manual(values = c(19, 15, 17, 20)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02", "#666666")) +
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
  scale_shape_manual(values = c(19, 15, 17, 20)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02", "#666666")) +
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



### Utah ------------------------------------------------------------------

# By PlotMix_Climate and PlantSource2
utah.des.count.plotmixclimate <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Utah") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Utah-Colorado Plateau, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
utah.des.count.plotmixclimate
utah.weed.count.plotmixclimate <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Utah") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Utah-Colorado Plateau, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
utah.weed.count.plotmixclimate



### Write out draft figures -----------------------------------------------

# Sonoran Desert
tiff("figures/2024-08_draft-figures/Sonoran_desirable_Count-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.des.count
dev.off()
tiff("figures/2024-08_draft-figures/Sonoran_weedy_Count-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.weed.count
dev.off()

tiff("figures/2024-08_draft-figures/Sonoran_desirable_Count-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.des.count.plotmixclimate
dev.off()
tiff("figures/2024-08_draft-figures/Sonoran_weedy_Count-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.weed.count.plotmixclimate
dev.off()

tiff("figures/2024-08_draft-figures/Sonoran_desirable_Count-by-forb-grass-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.des.count.forbgrass.plantsource2
dev.off()
tiff("figures/2024-08_draft-figures/Sonoran_weedy_Count-by-forb-grass-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.weed.count.forbgrass.plantsource2
dev.off()

tiff("figures/2024-08_draft-figures/Sonoran_desirable_Count-by-duration-and-lifeform.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.des.count.perennial.annual.lifeform
dev.off()
tiff("figures/2024-08_draft-figures/Sonoran_weedy_Count-by-duration-and-lifeform.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.weed.count.perennial.annual.lifeform
dev.off()

# N AZ
tiff("figures/2024-08_draft-figures/CO-Plateau_desirable_Count-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
naz.des.count
dev.off()
tiff("figures/2024-08_draft-figures/CO-Plateau_weedy_Count-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
naz.weed.count
dev.off()

tiff("figures/2024-08_draft-figures/CO-Plateau_desirable_Count-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
naz.des.count.plotmixclimate
dev.off()
tiff("figures/2024-08_draft-figures/CO-Plateau_weedy_Count-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
naz.weed.count.plotmixclimate
dev.off()

tiff("figures/2024-08_draft-figures/CO-Plateau_desirable_Count-by-forb-grass-shrub-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
naz.des.count.forbgrassshrub.plantsource2
dev.off()
tiff("figures/2024-08_draft-figures/CO-Plateau_weedy_Count-by-forb-grass-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
naz.weed.count.forbgrass.plantsource2
dev.off()

# All sites
tiff("figures/2024-08_draft-figures/All-sites_desirable_Count-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
all.des.count
dev.off()
tiff("figures/2024-08_draft-figures/All-sites_weedy_Count-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
all.weed.count
dev.off()

tiff("figures/2024-08_draft-figures/All-sites_desirable_Count-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
all.des.count.plotmixclimate
dev.off()
tiff("figures/2024-08_draft-figures/All-sites_weedy_Count-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
all.weed.count.plotmixclimate
dev.off()

tiff("figures/2024-08_draft-figures/All-sites_desirable_Count-by-forb-grass-shrub-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
all.des.count.forbgrassshrub.plantsource2
dev.off()
tiff("figures/2024-08_draft-figures/All-sites_weedy_Count-by-forb-grass-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
all.weed.count.forbgrass.plantsource2
dev.off()



## Height -----------------------------------------------------------------

### All sites -------------------------------------------------------------

# Extreme precip deviation outliers removed (800% deviation) for better visualization

# Single panel by PlantSource2
all.des.height <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Perc_dev_cum < 8) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  ggtitle("All sites, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.des.height
all.weed.height <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Perc_dev_cum < 8) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  ggtitle("All sites, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.weed.height

# By PlotMix_Climate and PlantSource2
all.des.height.plotmixclimate <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Perc_dev_cum < 8) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.des.height.plotmixclimate
all.weed.height.plotmixclimate <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Perc_dev_cum < 8) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("All sites, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.weed.height.plotmixclimate

# By Lifeform (forb, grass, shrub) and PlantSource2
all.des.height.forbgrassshrub.plantsource2 <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Perc_dev_cum < 8) |>
  filter(Lifeform %in% c("Forb", "Grass", "Shrub")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Lifeform) +
  ggtitle("All sites, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(17, 15)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.des.height.forbgrassshrub.plantsource2
all.weed.height.forbgrassshrub.plantsource2 <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Perc_dev_cum < 8) |>
  filter(Lifeform %in% c("Forb", "Grass", "Shrub")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Lifeform) +
  ggtitle("All sites, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(15, 17)) +
  scale_color_manual(values = c("#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.weed.height.forbgrassshrub.plantsource2

# By Duration (annual & perennial only) and Lifeform
all.des.height.perennial.annual.lifeform <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Perc_dev_cum < 8) |>
  filter(Duration %in% c("Annual", "Perennial")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Duration) +
  ggtitle("All sites, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17, 20)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02", "#666666")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.des.height.perennial.annual.lifeform
all.weed.height.perennial.annual.lifeform <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Perc_dev_cum < 8) |>
  filter(Duration %in% c("Annual", "Perennial")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Duration) +
  ggtitle("All sites, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 15, 17, 20)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02", "#666666")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.weed.height.perennial.annual.lifeform

# By Treatment and PlantSource2
all.des.height.treatment <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Perc_dev_cum < 8) |>
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Treatment) +
  ggtitle("All sites, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.des.height.treatment
all.weed.height.treatment <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Perc_dev_cum < 8) |>
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~Treatment) +
  ggtitle("All sites, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.weed.height.treatment

# Single by AridityIndex
all.des.height.ai <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Perc_dev_cum < 8) |>
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = AridityIndex)) +
  geom_smooth() +
  ggtitle("All sites, desirable species") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_color_viridis(direction = -1) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.des.height.ai
all.weed.height.ai <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Perc_dev_cum < 8) |>
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = AridityIndex)) +
  geom_smooth() +
  ggtitle("All sites, weedy species") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_color_viridis(direction = -1) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
all.weed.height.ai


### Sonoran Desert --------------------------------------------------------

# Single panel by PlantSource2
sonoran.des.height <- dat |> 
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
  scale_shape_manual(values = c(19, 15, 17, 20)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02", "#666666")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.des.height.perennial.annual.lifeform
sonoran.weed.height.perennial.annual.lifeform <- dat |> 
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
  scale_shape_manual(values = c(19, 15, 17, 20)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02", "#666666")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
sonoran.weed.height.perennial.annual.lifeform

# By Treatment and PlantSource2
sonoran.des.height.treatment <- dat |> 
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



### Northern Arizona Plateau ----------------------------------------------

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
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
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
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
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



### Utah ------------------------------------------------------------------

# By PlotMix_Climate and PlantSource2
utah.des.height <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Utah") |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Utah-Colorado Plateau, desirable species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
utah.des.height
utah.weed.height <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Utah") |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  ggtitle("Utah-Colorado Plateau, weedy species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
utah.weed.height




### Write out draft figures -----------------------------------------------

# Sonoran Desert
tiff("figures/2024-08_draft-figures/Sonoran_desirable_Height-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.des.height
dev.off()
tiff("figures/2024-08_draft-figures/Sonoran_weedy_Height-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.weed.height
dev.off()

tiff("figures/2024-08_draft-figures/Sonoran_desirable_Height-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.des.height.plotmixclimate
dev.off()
tiff("figures/2024-08_draft-figures/Sonoran_weedy_Height-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.weed.height.plotmixclimate
dev.off()

tiff("figures/2024-08_draft-figures/Sonoran_desirable_Height-by-forb-grass-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.des.height.forbgrass.plantsource2
dev.off()
tiff("figures/2024-08_draft-figures/Sonoran_weedy_Height-by-forb-grass-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.weed.height.forbgrass.plantsource2
dev.off()

tiff("figures/2024-08_draft-figures/Sonoran_desirable_Height-by-duration-and-lifeform.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.des.height.perennial.annual.lifeform
dev.off()
tiff("figures/2024-08_draft-figures/Sonoran_weedy_Height-by-duration-and-lifeform.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.weed.height.perennial.annual.lifeform
dev.off()

# N AZ
tiff("figures/2024-08_draft-figures/CO-Plateau_desirable_Height-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
naz.des.height
dev.off()
tiff("figures/2024-08_draft-figures/CO-Plateau_weedy_Height-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
naz.weed.height
dev.off()

tiff("figures/2024-08_draft-figures/CO-Plateau_desirable_Height-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
naz.des.height.plotmixclimate
dev.off()
tiff("figures/2024-08_draft-figures/CO-Plateau_weedy_Height-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
naz.weed.height.plotmixclimate
dev.off()

tiff("figures/2024-08_draft-figures/CO-Plateau_desirable_Height-by-forb-grass-shrub-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
naz.des.height.forbgrassshrub.plantsource2
dev.off()
tiff("figures/2024-08_draft-figures/CO-Plateau_weedy_Height-by-forb-grass-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
naz.weed.height.forbgrass.plantsource2
dev.off()

# All sites
tiff("figures/2024-08_draft-figures/All-sites_desirable_Height-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
all.des.height
dev.off()
tiff("figures/2024-08_draft-figures/All-sites_weedy_Height-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
all.weed.height
dev.off()

tiff("figures/2024-08_draft-figures/All-sites_desirable_Height-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
all.des.height.plotmixclimate
dev.off()
tiff("figures/2024-08_draft-figures/All-sites_weedy_Height-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
all.weed.height.plotmixclimate
dev.off()

tiff("figures/2024-08_draft-figures/All-sites_desirable_Height-by-forb-grass-shrub-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
all.des.height.forbgrassshrub.plantsource2
dev.off()
tiff("figures/2024-08_draft-figures/All-sites_weedy_Height-by-forb-grass-shrub-and-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
all.weed.height.forbgrassshrub.plantsource2
dev.off()



## Identify outliers (frequency across plots & sites) ---------------------

### Sonoran Desert --------------------------------------------------------

# Desirable (all plots)
#   Highest species frequency (showed up in most plots and sites) when wetter
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum > 0) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Name) |> 
  arrange(desc(n)) |> 
  print(n = 25)

#   All plots: Highest species frequency (showed up in most plots and sites) when drier
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum < 0) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Name) |> 
  arrange(desc(n)) |> 
  print(n = 25)


# Weedy (all plots)
#   Highest species frequency (showed up in most plots and sites) when wetter
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum > 0) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Name) |> 
  arrange(desc(n)) |> 
  print(n = 15)

#   All plots: Highest species frequency (showed up in most plots and sites) when drier
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum < 0) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Name) |> 
  arrange(desc(n)) |> 
  print(n = 15)


### CO Plateau ------------------------------------------------------------

# Desirable (all plots)
#   Highest species frequency (showed up in most plots and sites) when wetter
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         Perc_dev_cum > 0) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Name) |> 
  arrange(desc(n)) |> 
  print(n = 25)

#   All plots: Highest species frequency (showed up in most plots and sites) when drier
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         Perc_dev_cum < 0) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Name) |> 
  arrange(desc(n)) |> 
  print(n = 25)


# Weedy (all plots)
#   Highest species frequency (showed up in most plots and sites) when wetter
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau",
         Perc_dev_cum > 0) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Name) |> 
  arrange(desc(n)) |> 
  print(n = 15)

#   All plots: Highest species frequency (showed up in most plots and sites) when drier
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau",
         Perc_dev_cum < 0) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Name) |> 
  arrange(desc(n)) |> 
  print(n = 15)




## Identify outliers (Count) -----------------------------------------------

### Sonoran Desert --------------------------------------------------------

# Sonoran Central precip dev: ranged from -43% to +64%
dat |> 
  filter(Region == "Sonoran Central") |> 
  count(Perc_dev_cum)

# Sonoran SE precip dev: ranged from -23% to +46%
dat |> 
  filter(Region == "Sonoran SE") |> 
  count(Perc_dev_cum)

# Desirable (all plots)
# All plots: High count recruit
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Count > 50) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Site) |> 
  print(n = 24)

# All plots: Highest wet deviation
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum > 0.35) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Site)

# All plots: Highest dry deviation
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum < -0.35) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Site)


# Desirable, by PlotMix_Climate
# None: High count recruit
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "None") |> 
  filter(Count > 50) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Current: High count recruit
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Count > 50) |> 
  arrange(desc(Count)) |> 
  arrange(Site) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Current: Seeded highest wet deviation
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum > 0.35) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Site) |> 
  print(n = 48)

# Current: Seeded highest dry deviation
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.35) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Site)

# Current: Seeded, species of highest Count
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  print(n = 45)

# Current: Seeded, species frequency (showed up in most plots and sites)
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Name) |> 
  arrange(desc(n))

# Projected: High count recruit
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  filter(Count > 50) |> 
  arrange(desc(Count)) |> 
  arrange(Site) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Projected: Seeded highest wet deviation
#     Plantago ovata & ARPU9 doing well
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum > 0.35) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 42)

# Projected: Seeded highest dry deviation
#     Plantago ovata still doing well
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.35) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Projected: Seeded, species of highest Count
#     Plantago ovata very common in Central
#     Aristida purpurea did best in SE
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  print(n = 49)

# Projected: Seeded, species frequency (showed up in most plots and sites)
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Name) |> 
  arrange(desc(n))

# ARPU9 driest conditions
dat |> 
  filter(Code == "ARPU9",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Perc_dev_cum)


# Weedy (all plots)
# All plots: High count recruit
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Count > 50) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Site) |> 
  print(n = 29)

# All plots: Highest wet deviation
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum > 0.35) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Site)

# All plots: Highest dry deviation
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum < -0.35) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Site)


# Weedy, by PlotMix_Climate
# None: High count recruit (also highest dry deviation)
dat |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "None") |> 
  filter(Count > 50) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# None: Invasive highest wet deviation
dat |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "None") |> 
  filter(Perc_dev_cum > 0.5) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Current: High count recruit 
dat |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Count > 50) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Current: Invasive highest dry deviation
dat |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum < -0.35) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count))

# Current: Invasive highest wet deviation
dat |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum > 0.5) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) 

# Projected: High count recruit 
dat |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  filter(Count > 50) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Projected: Invasive highest dry deviation
dat |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum < -0.35) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count))

# Projected: Invasive highest wet deviation
dat |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum > 0.5) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)



### Colorado Plateau ------------------------------------------------------

# Desirable (all plots)
# All plots: High count recruit
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau") |> 
  filter(Count > 50) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Site) |> 
  print(n = 24)

# All plots: Highest wet deviation
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau")|> 
  filter(Perc_dev_cum > 0.35) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Site)

# All plots: Highest dry deviation
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau")|> 
  filter(Perc_dev_cum < -0.35) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Site)


# Desirable, by PlotMix_Climate
# None: Highest count recruit
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "None") |> 
  filter(Count > 20) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Current: High count recruit
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Count > 50) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Current: High count Seeded
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Count > 20,
         PlantSource2 == "Seeded") |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Current: Seeded highest wet deviation
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum > 1) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Current: Seeded highest dry deviation
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.5) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Projected: High count recruit
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Count > 40) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Projected: Seeded highest wet deviation
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum > 1) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Projected: Seeded highest dry deviation
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.5) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)


# Weedy (all plots)
# All plots: High count recruit
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  filter(Count > 50) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Site) |> 
  print(n = 24)

# All plots: Highest wet deviation
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau")|> 
  filter(Perc_dev_cum > 0.35) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Site)

# All plots: Highest dry deviation
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau")|> 
  filter(Perc_dev_cum < -0.35) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Site)


# Weedy, by PlotMix_Climate
# None: High count seeded (also highest wet deviation)
dat |> 
  filter(Weedy == "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "None") |> 
  filter(Perc_dev_cum > 1.5) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# None: Invasive highest dry deviation
dat |> 
  filter(Weedy == "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "None") |> 
  filter(Perc_dev_cum < -0.5) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Current: High count recruit 
dat |> 
  filter(Weedy == "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Count > 40) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Current: High count seeded (also highest wet deviation) 
dat |> 
  filter(Weedy == "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum > 1.5) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Current: Invasive highest dry deviation
dat |> 
  filter(Weedy == "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum < -0.5) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count))

# Projected: High count recruit 
dat |> 
  filter(Weedy == "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Count > 30) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Projected: Invasive highest dry deviation
dat |> 
  filter(Weedy == "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum < -0.5) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count))

# Projected: Invasive highest wet deviation
dat |> 
  filter(Weedy == "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum > 1.5) |> 
  select(Site, Name, Duration, Lifeform, Count, Perc_dev_cum)


## Identify outliers (Height) ---------------------------------------------

### Sonoran Desert --------------------------------------------------------

# Desirable, by PlotMix_Climate
# None: Tallest
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "None") |> 
  filter(Height > 600) |> 
  select(Site, Name, Duration, Lifeform, Height, Perc_dev_cum)


# Current: Tallest
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Height > 600) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum)

# Current: Tallest seeded
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         SpeciesSeeded == "Yes") |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  print(n = 50)

# Current: Seeded highest wet deviation
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum > 0.35) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 48)

# Current: Seeded highest dry deviation
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.35) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum)

# Projected: Tallest
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  filter(Height > 600) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum)

# Projected: Seeded highest wet deviation
#   None of them are that tall
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum > 0.35) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 42)

# Projected: Seeded highest dry deviation
#   These are also pretty short
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.35) |> 
  select(Site, Name, Duration, Lifeform, Height, Perc_dev_cum)

# Projected: Seeded, tallest species
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded",
         Height > 200)|> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  print(n = 39)


# Weedy, None
#   High Height recruit (also highest dry deviation)
dat |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "None") |> 
  filter(Height > 50) |> 
  select(Site, Name, Duration, Lifeform, Height, Perc_dev_cum)

#   Invasive highest wet deviation
dat |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "None") |> 
  filter(Perc_dev_cum > 0.5) |> 
  select(Site, Name, Duration, Lifeform, Height, Perc_dev_cum)


# Weedy, Current
#   High Height recruit 
dat |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Height > 50) |> 
  select(Site, Name, Duration, Lifeform, Height, Perc_dev_cum)

#   Invasive highest dry deviation
dat |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum < -0.35) |> 
  select(Site, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height))

#   Invasive highest wet deviation
dat |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum > 0.5) |> 
  select(Site, Name, Duration, Lifeform, Height, Perc_dev_cum) 


# Weedy, Projected
#   High Height recruit 
dat |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  filter(Height > 50) |> 
  select(Site, Name, Duration, Lifeform, Height, Perc_dev_cum)

#   Invasive highest dry deviation
dat |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum < -0.35) |> 
  select(Site, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height))

#   Invasive highest wet deviation
dat |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum > 0.5) |> 
  select(Site, Name, Duration, Lifeform, Height, Perc_dev_cum)



## Top species in mixes (Count) -------------------------------------------

# Most common seeded species across all Treatments for Current or Projected mixes

### Sonoran Desert --------------------------------------------------------

# Current
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         PlantSource == "Seeded") |> 
  group_by(Code, Name) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount))

# Projected
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         PlantSource == "Seeded") |> 
  group_by(Code, Name) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount))


### CO Plateau ------------------------------------------------------------

# Current: All sites
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         PlantSource == "Seeded") |> 
  group_by(Code, Name) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 30)

# Projected: All sites
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         PlantSource == "Seeded") |> 
  group_by(Code, Name) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 28)


# Current: By site/mix
dat |> 
  filter(Site %in% c("BarTBar", "FlyingM"),
         PlotMix_Climate == "Current",
         PlantSource == "Seeded") |> 
  group_by(Code, Name) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount))

dat |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb"),
         PlotMix_Climate == "Current",
         PlantSource == "Seeded") |> 
  group_by(Code, Name) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount))

dat |> 
  filter(Site == "BabbittPJ",
         PlotMix_Climate == "Current",
         PlantSource == "Seeded") |> 
  group_by(Code, Name) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount))

dat |> 
  filter(Site == "TLE",
         PlotMix_Climate == "Current",
         PlantSource == "Seeded") |> 
  group_by(Code, Name) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) # lol nothing grew


# Projected: By site/mix
dat |> 
  filter(Site %in% c("BarTBar", "FlyingM"),
         PlotMix_Climate == "Projected",
         PlantSource == "Seeded") |> 
  group_by(Code, Name) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount))

dat |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb"),
         PlotMix_Climate == "Projected",
         PlantSource == "Seeded") |> 
  group_by(Code, Name) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount))

dat |> 
  filter(Site == "BabbittPJ",
         PlotMix_Climate == "Projected",
         PlantSource == "Seeded") |> 
  group_by(Code, Name) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount))

dat |> 
  filter(Site == "TLE",
         PlotMix_Climate == "Projected",
         PlantSource == "Seeded") |> 
  group_by(Code, Name) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) # lol nothing grew





save.image("RData/06.1_exploratory-graphs_precip-dev_subplot.RData")


