# Created: 2024-09-25
# Last updated: 2024-09-26

# Purpose: Calculate frequency for all species. 

# First find number of plots needed for denominator of various groups.
# Calculate frequency considering entire plot (subplot + additional 2x2 species),
#   identify most frequent, and format to make a table for bar graphs.
# Calculate frequencies for total (all plots/conditions), wetter conditions, extremely
#   wet conditions (+24% and wetter for Sonoran Desert; 48% and wetter for N AZ), drier
#   conditions, and extremely dry conditions (-23% and drier for SD and -50% and drier for NAZ).

# Sonoran Desert precip deviation extremes (to determine performance in variable precip):
#   Wet: +24% and wetter includes all sites but Roosevelt.
#   Dry: -23% and drier includes all sites but Patagonia.
# Northern Arizona Plateau precip deviation extremes:
#   Wet: +48% and wetter includes all sites but FlyingM and TLE.
#   Dry: -50% and drier includes all sites.

# Sonoran Desert, best performers based on frequency:
#   Current mix, most frequent (all conditions) and did well under var precip: SACO6, LUSP2
#   Projected mix, most frequent (all conditions) and did well under var precip: PLOV
#   Projected mix, most frequent (but not present in both extremes): ARPU9, SECO10
#   Projected mix, frequent in extremely wet conditions: PLOV, ARPU9
#   Projected mix, frequent in extremely dry conditions: PLOV, SECO10
#   Native volunteers, most frequent and did well under var precip: LOAR12, LOHU2
#   Weedy species, most frequent and did well under var precip: SCBA, BRRU2, ERCI6

# Northern Arizona Plateau species of interest:
#   Current mix, most frequent (all conditions) and did well under var precip: LECI4, HEBO, HECO26
#   Projected mix, most frequent (all conditions) and did well under var precip: BAMU, PASM
#   Projected mix, large precip range, grew in extremely wet and moderate dry: ASTU, SECO10
#   Native volunteers, most frequent (all conditions) and did well under var precip: ATCO, SOEL
#   Weedy species, most frequent (all conditions) and did well under var precip: SATR12, ERCI6



library(tidyverse)

# Load data ---------------------------------------------------------------

subplot <- read_csv("data/cleaned/04.1_subplot-data_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")
present_species <- read_csv("data/cleaned/04.2_2x2-species-present_clean.csv")

# Data wrangling ----------------------------------------------------------

# Check for NAs
apply(subplot, 2, anyNA)

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

# Add Perc_dev_cum to present_species
present_species <- present_species |> 
  left_join(cum.pd.subplot)


# Number of plots ---------------------------------------------------------

## Sonoran Desert ---------------------------------------------------------

### All plots (Native recruit, Weedy) -------------------------------------

# Total number of plots: 1152
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()
36 * (6 + 6 + 5 + 4 + 6 + 5) # 36 plots * number of monitoring events at each site

# Number of wetter plots: 360
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  select(Region, Site, Date_Monitored, Plot, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE) |> 
  filter(Perc_dev_cum > 0) |> 
  nrow()

# Number of extremely wet plots (+24% and wetter): 180
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  select(Region, Site, Date_Monitored, Plot, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE) |> 
  filter(Perc_dev_cum > 0.24) |> 
  nrow()

# Number of drier plots: 792
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  select(Region, Site, Date_Monitored, Plot, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE) |> 
  filter(Perc_dev_cum < 0) |> 
  nrow()

# Number of extremely dry plots (-23% and drier): 288
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  select(Region, Site, Date_Monitored, Plot, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE) |> 
  filter(Perc_dev_cum < -0.23) |> 
  nrow()


### Seeded plots ----------------------------------------------------------

# 4 types of seeded species, based on how they were seeded:
# (Note that Current vs. Projected doesn't affect plot number, just the site, because
# some sites were monitored more frequently than others. No species was included at the same site
# as both Current and Projected, so for all species, they were seeded in half the seeded plots,
# which is the same number for Current and Projected.)
#   1: Seeded only at Sonoran Central in just one mix
#   2: Seeded at only Sonoran SE in just one mix
#   3: Seeded at both regions in the same mix type: SPCR, ARPU9, BOCU, SECO10
#   4: Seeded at both regions in opposite mix types: BAMU
#   0: Empty plot comparison, both regions and both mixes

# 1: Sonoran Central only, total number of plots for species in just 1 mix: 320
dat |> 
  filter(Region == "Sonoran Central",
         PlotMix_Climate == "Current") |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 1: Sonoran Central only, when wetter: 96
dat |> 
  filter(Region == "Sonoran Central", 
         PlotMix_Climate == "Current",
         Perc_dev_cum > 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 1: Sonoran Central only, when extremely wet (+24% and wetter): 48
dat |> 
  filter(Region == "Sonoran Central", 
         PlotMix_Climate == "Current",
         Perc_dev_cum > 0.24) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 1: Sonoran Central only, when drier: 224
dat |> 
  filter(Region == "Sonoran Central", 
         PlotMix_Climate == "Current",
         Perc_dev_cum < 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 1: Sonoran Central only, when extremely dry (-23% and drier): 112
dat |> 
  filter(Region == "Sonoran Central", 
         PlotMix_Climate == "Current",
         Perc_dev_cum < -0.23) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()


# 2: Sonoran SE only, total number of plots for species in just 1 mix: 192
dat |> 
  filter(Region == "Sonoran SE",
         PlotMix_Climate == "Current") |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 2: Sonoran SE only, when wetter: 64
dat |> 
  filter(Region == "Sonoran SE",
         PlotMix_Climate == "Current",
         Perc_dev_cum > 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 2: Sonoran SE only, when extremely wet (+24% and wetter): 32
dat |> 
  filter(Region == "Sonoran SE",
         PlotMix_Climate == "Current",
         Perc_dev_cum > 0.24) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 2: Sonoran SE only, when drier: 128
dat |> 
  filter(Region == "Sonoran SE",
         PlotMix_Climate == "Current",
         Perc_dev_cum < 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 2: Sonoran SE only, when extremely dry (-23% and drier): 16
dat |> 
  filter(Region == "Sonoran SE",
         PlotMix_Climate == "Current",
         Perc_dev_cum < -0.23) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 3: Both sites, species in same mix type, total plots: 512
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 3: Both sites, same mix, wetter: 160
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         Perc_dev_cum > 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 3: Both sites, same mix, extremely wet (+24% and wetter): 80
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         Perc_dev_cum > 0.24) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 3. Both sites, same mix, drier: 352
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         Perc_dev_cum < 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 3. Both sites, same mix, extremely dry (-23% and drier): 128
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         Perc_dev_cum < -0.23) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 4. Both sites, species in opposite mixes, total plots: 512
#     when wetter: 160; when drier: 352; when extremely wet: 80; when extremely dry: 128
#   Same as 3 because BAMU was still seeded in only half the plots at each site


# 0. Empty plot comparison (Current or Projected), total: 512
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 0: Empty plot comparison, wetter: 160
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         Perc_dev_cum > 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 0: Empty plot comparison, extremely wet (+24% and wetter): 80
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         Perc_dev_cum > 0.24) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 0. Empty plot comparison, drier: 352
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         Perc_dev_cum < 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 0. Empty plot comparison, extremely dry (-23% and drier): 128
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         Perc_dev_cum < -0.23) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()


## Northern Arizona -------------------------------------------------------

### All plots (Native recruit, Weedy) -------------------------------------

# Total number of plots: 3492
dat |> 
  filter(Region == "Colorado Plateau") |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()
36 * (7 + 16 + 15 + 14 + 13 + 14 + 14 + 4) # 36 plots * number of monitoring events at each site

# Number of wetter plots: 2196
dat |> 
  filter(Region == "Colorado Plateau") |> 
  select(Region, Site, Date_Monitored, Plot, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE) |> 
  filter(Perc_dev_cum > 0) |> 
  nrow()

# Number of extremely wet plots (+48% and wetter): 792
dat |> 
  filter(Region == "Colorado Plateau") |> 
  select(Region, Site, Date_Monitored, Plot, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE) |> 
  filter(Perc_dev_cum > 0.48) |> 
  nrow()

# Number of drier plots: 1296
dat |> 
  filter(Region == "Colorado Plateau") |> 
  select(Region, Site, Date_Monitored, Plot, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE) |> 
  filter(Perc_dev_cum < 0) |> 
  nrow()

# Number of extremely dry plots (-50% and drier): 540
dat |> 
  filter(Region == "Colorado Plateau") |> 
  select(Region, Site, Date_Monitored, Plot, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE) |> 
  filter(Perc_dev_cum < -0.5) |> 
  nrow()


### Seeded plots ----------------------------------------------------------

# 4 seed mixes:
# (Note that Current vs. Projected doesn't affect plot number, just the site, because
# some sites were monitored more frequently than others. No species was included 
# more than one in each mix, so for all species, they were seeded in half the seeded plots,
# which is the same number for Current and Projected.)
#   1. Warm: seeded at AguaFria, MOWE, PEFO, Spiderweb, TLE as Projected
#   2. Med-Warm: seeded at AguaFria, MOWE, PEFO, Spiderweb as Current; BarTBar, FlyingM as Projected
#   3. Cool-Med: seeded at BarTBar, FlyingM as Current; BabbittPJ as Projected
#   4. Cool: seeded at BabbittPJ, TLE as Current

# 1. Warm, total: 831
dat |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "TLE"),
         PlotMix_Climate == "Projected") |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 2. Med-Warm, total: 1231
dat |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "BarTBar", "FlyingM"),
         PlotMix_Climate == "Projected") |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 3. Cool-Med, total: 720
dat |> 
  filter(Site %in% c("BarTBar", "FlyingM", "BabbittPJ"),
         PlotMix_Climate == "Projected") |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 4. Cool, total: 320
dat |> 
  filter(Site %in% c("BabbittPJ", "TLE"),
         PlotMix_Climate == "Projected") |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()


# 1. Warm, when wetter: 479
dat |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "TLE"),
         PlotMix_Climate == "Projected",
         Perc_dev_cum > 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 2. Med-Warm, when wetter: 783
dat |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "BarTBar", "FlyingM"),
         PlotMix_Climate == "Projected",
         Perc_dev_cum > 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 3. Cool-Med, when wetter: 496
dat |> 
  filter(Site %in% c("BarTBar", "FlyingM", "BabbittPJ"),
         PlotMix_Climate == "Projected", 
         Perc_dev_cum > 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 4. Cool, when wetter: 192
dat |> 
  filter(Site %in% c("BabbittPJ", "TLE"),
         PlotMix_Climate == "Projected", 
         Perc_dev_cum > 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 1. Warm, when extremely wet (+48% and wetter): 191
dat |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "TLE"),
         PlotMix_Climate == "Projected",
         Perc_dev_cum > 0.48) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 2. Med-Warm, when extremely wet (+48% and wetter): 207
dat |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "BarTBar", "FlyingM"),
         PlotMix_Climate == "Projected",
         Perc_dev_cum > 0.48) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 3. Cool-Med, when extremely wet (+48% and wetter): 160
dat |> 
  filter(Site %in% c("BarTBar", "FlyingM", "BabbittPJ"),
         PlotMix_Climate == "Projected", 
         Perc_dev_cum > 0.48) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 4. Cool, when extremely wet (+48% and wetter): 144
dat |> 
  filter(Site %in% c("BabbittPJ", "TLE"),
         PlotMix_Climate == "Projected", 
         Perc_dev_cum > 0.48) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()


# 1. Warm, when drier: 352
dat |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "TLE"),
         PlotMix_Climate == "Projected",
         Perc_dev_cum < 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 2. Med-Warm, when drier: 448
dat |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "BarTBar", "FlyingM"),
         PlotMix_Climate == "Projected",
         Perc_dev_cum < 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 3. Cool-Med, when drier: 224
dat |> 
  filter(Site %in% c("BarTBar", "FlyingM", "BabbittPJ"),
         PlotMix_Climate == "Projected", 
         Perc_dev_cum < 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 4. Cool, when drier: 128
dat |> 
  filter(Site %in% c("BabbittPJ", "TLE"),
         PlotMix_Climate == "Projected", 
         Perc_dev_cum < 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 1. Warm, when extremely dry (-50% and drier): 144
dat |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "TLE"),
         PlotMix_Climate == "Projected",
         Perc_dev_cum < -0.5) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 2. Med-Warm, when extremely dry (-50% and drier): 192
dat |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "BarTBar", "FlyingM"),
         PlotMix_Climate == "Projected",
         Perc_dev_cum < -0.5) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 3. Cool-Med, when extremely dry (-50% and drier): 96
dat |> 
  filter(Site %in% c("BarTBar", "FlyingM", "BabbittPJ"),
         PlotMix_Climate == "Projected", 
         Perc_dev_cum < -0.5) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 4. Cool, when extremely dry (-50% and drier): 48
dat |> 
  filter(Site %in% c("BabbittPJ", "TLE"),
         PlotMix_Climate == "Projected", 
         Perc_dev_cum < -0.5) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()


# 0. Empty plot comparison total: 1553
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 0. Empty plot comparison, wetter: 977
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  select(Region, Site, Date_Monitored, Plot, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE) |> 
  filter(Perc_dev_cum > 0) |> 
  nrow()

# 0. Empty plot comparison, extremely wet (+48% and wetter): 353
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  select(Region, Site, Date_Monitored, Plot, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE) |> 
  filter(Perc_dev_cum > 0.48) |> 
  nrow()

# 0. Empty plot comparison, drier: 576
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  select(Region, Site, Date_Monitored, Plot, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE) |> 
  filter(Perc_dev_cum < 0) |> 
  nrow()

# 0. Empty plot comparison, extremely dry (-50% and drier): 240
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  select(Region, Site, Date_Monitored, Plot, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE) |> 
  filter(Perc_dev_cum < -0.5) |> 
  nrow()



# Sonoran Desert: Frequency -----------------------------------------------

## Native recruit ---------------------------------------------------------

# Native volunteers (all plots): Highest species frequency overall
#   LOHU2, LOAR12, CHPO12, PERE, VUOC, PLAR, LAGR10
sonoran.total.nativevolun <- present_species |> 
  filter(Weedy == "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "No") |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 1152) * 100) |> 
  print(n = 30) 

# Native volunteers: Highest species frequency when wetter
#   LOAR12, LAGR10, PLPR3, LOHU2, PSCA11, CHPO12, VUOC
sonoran.wet.nativevolun <- present_species |> 
  filter(Weedy == "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "No",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 360) * 100) |> 
  print(n = 25) 

# Native volunteers: Highest species frequency extremely wet (+24% and more)
#   LOAR12, VUOC, LOHU2
sonoran.wettest.nativevolun <- present_species |> 
  filter(Weedy == "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "No",
         Perc_dev_cum > 0.24) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 180) * 100) |> 
  print(n = 25) 

# Native volunteers: Highest species frequency when drier
#   LOHU2, LOAR12, CHPO12, AMCO3, PLAR, PERE, VUOC 
sonoran.dry.nativevolun <- present_species |> 
  filter(Weedy == "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "No",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 792) * 100) |> 
  print(n = 25) 

# Native volunteers: Highest species frequency when extremely dry (-23% and less)
#   LOHU2, PLAR, PERE, LOAR12, CHPO12 
sonoran.driest.nativevolun <- present_species |> 
  filter(Weedy == "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "No",
         Perc_dev_cum < -0.23) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 288) * 100) |> 
  print(n = 25) 



## Weedy ------------------------------------------------------------------

# Weedy (all plots): Highest species frequency overall
#   ERCI6, SCBA, BRRU2, ERLE
sonoran.total.weedy <- present_species |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 1152) * 100) |> 
  print(n = 15)

# Weedy: Highest species frequency when wetter
#   ERCI6, SCBA, BRRU2, ERLE
sonoran.wet.weedy <- present_species |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 360) * 100) |> 
  print(n = 15)

# Weedy: Highest species frequency when extremely wet (+24% and more)
#   SCBA, ERCI6, MAPA5, SIIR, HEHIC, BRRU2, UNFO3.SRER
sonoran.wettest.weedy <- present_species |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum > 0.24) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 180) * 100) |> 
  print(n = 15)

# Weedy: Highest species frequency when drier
#   ERCI6, SCBA, BRRU2, ERLE
sonoran.dry.weedy <- present_species |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 792) * 100) |> 
  print(n = 15)

# Weedy: Highest species frequency when extremely dry (-23% and less)
#   ERCI6, SCBA, BRRU2
sonoran.driest.weedy <- present_species |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum < -0.23) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 288) * 100) |> 
  print(n = 15)



## Seeded species ---------------------------------------------------------

# Total: Seeded version 1
sonoran.total.seed1 <- present_species |> 
  filter(Code %in% c("AMDE4", "DICA8", "LUSP2", "MUPO2", "SACO6", "SPAM2", 
                     "BOAR", "BORO2", "CAER", "ENFA", "PLOV"),
         Region == "Sonoran Central",
         SpeciesSeeded == "Yes") |> 
  count(Code) |> 
  mutate(perc_freq = (n / 320) * 100) |> 
  arrange(desc(n)) 
sonoran.total.seed1
sonoran.total.seed1 <- sonoran.total.seed1 |> 
  mutate(mix = c("Current", "Current", "Projected", "Current", "Projected", "Current", "Current",
                 "Projected", "Projected"))
sonoran.total.seed1

# Total: Seeded version 2
sonoran.total.seed2 <- present_species |> 
  filter(Code %in% c("BOGR2", "ELEL5", "HEMU3", "HENE5", "MATA2", "POSE", 
                     "ASTU", "PLJA", "PEPA8"),
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes") |> 
  count(Code) |> 
  mutate(perc_freq = (n / 192) * 100) |> 
  arrange(desc(n)) 
sonoran.total.seed2
sonoran.total.seed2 <- sonoran.total.seed2 |> 
  mutate(mix = c("Current", "Projected", "Current", "Current", "Current", "Current"))
sonoran.total.seed2

# Total: Seeded version 3
sonoran.total.seed3 <- present_species |> 
  filter(Code %in% c("SPCR", "ARPU9", "BOCU", "SECO10"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "Yes") |> 
  count(Code) |> 
  mutate(perc_freq = (n / 512) * 100) |> 
  arrange(desc(n))
sonoran.total.seed3
sonoran.total.seed3 <- sonoran.total.seed3 |> 
  mutate(mix = c("Projected", "Projected", "Projected", "Current"))
sonoran.total.seed3

# Total: Seeded version 4, Current
sonoran.total.seed4c <- present_species |> 
  filter(Code == "BAMU",
         Region == "Sonoran Central",
         SpeciesSeeded == "Yes") |> 
  count(Code) |> 
  mutate(perc_freq = (n / 512) * 100,
         mix = "Current") 
sonoran.total.seed4c

# Total: Seeded version 4, Projected
sonoran.total.seed4p <- present_species |> 
  filter(Code == "BAMU",
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes") |> 
  count(Code) |> 
  mutate(perc_freq = (n / 512) * 100,
         mix = "Projected") 
sonoran.total.seed4p

# Wetter: Seeded version 1
sonoran.wet.seed1 <- present_species |> 
  filter(Code %in% c("AMDE4", "DICA8", "LUSP2", "MUPO2", "SACO6", "SPAM2", 
                     "BOAR", "BORO2", "CAER", "ENFA", "PLOV"),
         Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 96) * 100) |> 
  arrange(desc(n))
sonoran.wet.seed1
sonoran.wet.seed1 <- sonoran.wet.seed1 |> 
  mutate(mix = c("Current", "Current", "Projected", "Current", "Current",
                 "Projected", "Projected", "Projected", "Current"))
sonoran.wet.seed1

# Wetter: Seeded version 2
sonoran.wet.seed2 <- present_species |> 
  filter(Code %in% c("BOGR2", "ELEL5", "HEMU3", "HENE5", "MATA2", "POSE", 
                     "ASTU", "PLJA", "PEPA8"),
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 64) * 100) |> 
  arrange(desc(n))
sonoran.wet.seed2
sonoran.wet.seed2 <- sonoran.wet.seed2 |> 
  mutate(mix = c("Current", "Current", "Current", "Current", "Projected", "Current"))
sonoran.wet.seed2

# Wetter: Seeded version 3
sonoran.wet.seed3 <- present_species |> 
  filter(Code %in% c("SPCR", "ARPU9", "BOCU", "SECO10"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 160) * 100) |> 
  arrange(desc(n))
sonoran.wet.seed3
sonoran.wet.seed3 <- sonoran.wet.seed3 |> 
  mutate(mix = c("Projected", "Projected", "Current", "Projected"))
sonoran.wet.seed3

# Wetter: Seeded version 4, Current
sonoran.wet.seed4c <- present_species |> 
  filter(Code == "BAMU",
         Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 160) * 100,
         mix = "Current") 
sonoran.wet.seed4c

# Wetter: Seeded version 4, Projected
sonoran.wet.seed4p <- present_species |> 
  filter(Code == "BAMU",
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 160) * 100,
         mix = "Projected") 
sonoran.wet.seed4p

# Extremely wet: Seeded version 1
sonoran.wettest.seed1 <- present_species |> 
  filter(Code %in% c("AMDE4", "DICA8", "LUSP2", "MUPO2", "SACO6", "SPAM2", 
                     "BOAR", "BORO2", "CAER", "ENFA", "PLOV"),
         Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0.24) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 48) * 100) |> 
  arrange(desc(n))
sonoran.wettest.seed1
sonoran.wettest.seed1 <- sonoran.wettest.seed1 |> 
  mutate(mix = c("Current", "Current", "Projected", "Current", "Current",
                 "Projected", "Current"))
sonoran.wettest.seed1

# Extremely wet: Seeded version 2
sonoran.wettest.seed2 <- present_species |> 
  filter(Code %in% c("BOGR2", "ELEL5", "HEMU3", "HENE5", "MATA2", "POSE", 
                     "ASTU", "PLJA", "PEPA8"),
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0.24) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 32) * 100) |> 
  arrange(desc(n))
sonoran.wettest.seed2
sonoran.wettest.seed2 <- sonoran.wettest.seed2 |> 
  mutate(mix = c("Current", "Current", "Current", "Projected", "Current"))
sonoran.wettest.seed2

# Extremely wet: Seeded version 3
sonoran.wettest.seed3 <- present_species |> 
  filter(Code %in% c("SPCR", "ARPU9", "BOCU", "SECO10"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0.24) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 80) * 100) |> 
  arrange(desc(n))
sonoran.wettest.seed3
sonoran.wettest.seed3 <- sonoran.wettest.seed3 |> 
  mutate(mix = c("Projected", "Projected", "Projected", "Current"))
sonoran.wettest.seed3

# Extremely wet: Seeded version 4, Current
sonoran.wettest.seed4c <- present_species |> 
  filter(Code == "BAMU",
         Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0.24) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 80) * 100,
         mix = "Current") 
sonoran.wettest.seed4c

# Extremely wet: Seeded version 4, Projected
sonoran.wettest.seed4p <- present_species |> 
  filter(Code == "BAMU",
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0.24) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 80) * 100,
         mix = "Projected") 
sonoran.wettest.seed4p

# Drier: Seeded version 1
sonoran.dry.seed1 <- present_species |> 
  filter(Code %in% c("AMDE4", "DICA8", "LUSP2", "MUPO2", "SACO6", "SPAM2", 
                     "BOAR", "BORO2", "CAER", "ENFA", "PLOV"),
         Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 224) * 100) |> 
  arrange(desc(n))
sonoran.dry.seed1
sonoran.dry.seed1 <- sonoran.dry.seed1 |> 
  mutate(mix = c("Current", "Current", "Current", "Projected",
                 "Current", "Projected", "Current", "Projected"))
sonoran.dry.seed1

# Drier: Seeded version 2
sonoran.dry.seed2 <- present_species |> 
  filter(Code %in% c("BOGR2", "ELEL5", "HEMU3", "HENE5", "MATA2", "POSE", 
                     "ASTU", "PLJA", "PEPA8"),
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 128) * 100) |> 
  arrange(desc(n))
sonoran.dry.seed2
sonoran.dry.seed2 <- sonoran.dry.seed2 |> 
  mutate(mix = c("Current", "Projected", "Current"))
sonoran.dry.seed2

# Drier: Seeded version 3
sonoran.dry.seed3 <- present_species |> 
  filter(Code %in% c("SPCR", "ARPU9", "BOCU", "SECO10"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "Yes",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 352) * 100) |> 
  arrange(desc(n))
sonoran.dry.seed3
sonoran.dry.seed3 <- sonoran.dry.seed3 |> 
  mutate(mix = c("Projected", "Projected", "Projected", "Current"))
sonoran.dry.seed3

# Drier: Seeded version 4, Current
sonoran.dry.seed4c <- present_species |> 
  filter(Code == "BAMU",
         Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 352) * 100,
         mix = "Current") 
sonoran.dry.seed4c

# Drier: Seeded version 4, Projected
sonoran.dry.seed4p <- present_species |> 
  filter(Code == "BAMU",
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 352) * 100,
         mix = "Projected") 
sonoran.dry.seed4p

# Extremely dry: Seeded version 1
sonoran.driest.seed1 <- present_species |> 
  filter(Code %in% c("AMDE4", "DICA8", "LUSP2", "MUPO2", "SACO6", "SPAM2", 
                     "BOAR", "BORO2", "CAER", "ENFA", "PLOV"),
         Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         Perc_dev_cum < -0.23) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 112) * 100) |> 
  arrange(desc(n))
sonoran.driest.seed1
sonoran.driest.seed1 <- sonoran.driest.seed1 |> 
  mutate(mix = c("Current", "Current", "Current", "Projected",
                 "Projected", "Current", "Current", "Projected"))
sonoran.driest.seed1

# Extremely dry: Seeded version 2
present_species |> 
  filter(Code %in% c("BOGR2", "ELEL5", "HEMU3", "HENE5", "MATA2", "POSE", 
                     "ASTU", "PLJA", "PEPA8"),
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         Perc_dev_cum < -0.23) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 16) * 100) |> 
  arrange(desc(n)) # none

# Extremely dry: Seeded version 3
sonoran.driest.seed3 <- present_species |> 
  filter(Code %in% c("SPCR", "ARPU9", "BOCU", "SECO10"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "Yes",
         Perc_dev_cum < -0.23) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 128) * 100) |> 
  arrange(desc(n))
sonoran.driest.seed3
sonoran.driest.seed3 <- sonoran.driest.seed3 |> 
  mutate(mix = c("Projected", "Projected", "Projected"))
sonoran.driest.seed3

# Extremely dry: Seeded version 4, Current
sonoran.driest.seed4c <- present_species |> 
  filter(Code == "BAMU",
         Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         Perc_dev_cum < -0.23) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 128) * 100,
         mix = "Current") 
sonoran.driest.seed4c

# Extremely dry: Seeded version 4, Projected
present_species |> 
  filter(Code == "BAMU",
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         Perc_dev_cum < -0.23) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 128) * 100,
         mix = "Projected") # nothing grew


# Total: combined
#   Current: SACO6, LUSP2
#   Projected: ARPU9, SECO10, PLOV
sonoran.total.seed <- bind_rows(sonoran.total.seed1, sonoran.total.seed2, sonoran.total.seed3, 
                                sonoran.total.seed4c, sonoran.total.seed4p) |> 
  arrange(desc(perc_freq))
sonoran.total.seed

# Wetter: combined
#   Current: LUSP2, SACO6
#   Projected: ARPU9, PLOV
sonoran.wet.seed <- bind_rows(sonoran.wet.seed1, sonoran.wet.seed2, sonoran.wet.seed3,
                              sonoran.wet.seed4c, sonoran.wet.seed4p) |> 
  arrange(desc(perc_freq))
sonoran.wet.seed

# Extremely wet (+24% and more): combined
#   Current: LUSP2, SACO6
#   Projected: PLOV, ARPU9
sonoran.wettest.seed <- bind_rows(sonoran.wettest.seed1, sonoran.wettest.seed2, sonoran.wettest.seed3,
                                  sonoran.wettest.seed4c, sonoran.wettest.seed4p) |> 
  arrange(desc(perc_freq))
sonoran.wettest.seed

# Drier: combined
#   Current: SACO6, LUSP2
#   Projected: SECO10, ARPU9, SPAM2, PLOV
sonoran.dry.seed <- bind_rows(sonoran.dry.seed1, sonoran.dry.seed2, sonoran.dry.seed3,
                              sonoran.dry.seed4c) |> 
  arrange(desc(perc_freq))
sonoran.dry.seed

# Extremely dry (-23% and less): combined
#   Current: SACO6, LUSP2, SPAM2
#   Projected: PLOV, SECO10
sonoran.driest.seed <- bind_rows(sonoran.driest.seed1, sonoran.driest.seed3,
                                 sonoran.driest.seed4c) |> 
  arrange(desc(perc_freq))
sonoran.driest.seed


## Empty plots ------------------------------------------------------------

# Empty plots (out of all plots)
sonoran.total.empty <- present_species |> 
  filter(Code == "0",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 1152) * 100)
sonoran.wet.empty <- present_species |> 
  filter(Code == "0",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 360) * 100)
sonoran.wettest.empty <- present_species |> 
  filter(Code == "0",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum > 0.24) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 180) * 100)
sonoran.dry.empty <- present_species |> 
  filter(Code == "0",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 792) * 100)
sonoran.driest.empty <- present_species |> 
  filter(Code == "0",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum < -0.23) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 288) * 100)


# Empty seeded plots (Current or Projected)
sonoran.total.empty.seed <- present_species |> 
  filter(PlotMix_Climate == "Current",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  count(Code) |> 
  filter(Code == "0") |> 
  mutate(perc_freq = (n / 512) * 100) 
sonoran.wet.empty.seed <- present_species |> 
  filter(PlotMix_Climate == "Current",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  filter(Code == "0") |> 
  mutate(perc_freq = (n / 160) * 100)
sonoran.wettest.empty.seed <- present_species |> 
  filter(PlotMix_Climate == "Current",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum > 0.24) |> 
  count(Code) |> 
  filter(Code == "0") |> 
  mutate(perc_freq = (n / 80) * 100)
sonoran.dry.empty.seed <- present_species |> 
  filter(PlotMix_Climate == "Current",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  filter(Code == "0") |> 
  mutate(perc_freq = (n / 352) * 100) 
sonoran.driest.empty.seed <- present_species |> 
  filter(PlotMix_Climate == "Current",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum < -0.23) |> 
  count(Code) |> 
  filter(Code == "0") |> 
  mutate(perc_freq = (n / 128) * 100) 



## Construct tables -------------------------------------------------------

# Combine Native recruit
sonoran.nativevolun <- bind_rows(sonoran.total.nativevolun, sonoran.wet.nativevolun, sonoran.dry.nativevolun,
                                 sonoran.wettest.nativevolun, sonoran.driest.nativevolun) |> 
  mutate(Frequency = perc_freq / 100) |> 
  mutate(Plant = "Native recruit",
         Plot = c(rep("Total", nrow(sonoran.total.nativevolun)), rep("Wetter", nrow(sonoran.wet.nativevolun)),
                  rep("Drier", nrow(sonoran.dry.nativevolun)), rep("Wettest", nrow(sonoran.wettest.nativevolun)),
                  rep("Driest", nrow(sonoran.driest.nativevolun)))) 

# Combine Weedy
sonoran.weedy <- bind_rows(sonoran.total.weedy, sonoran.wet.weedy, sonoran.dry.weedy,
                           sonoran.wettest.weedy, sonoran.driest.weedy) |> 
  mutate(Frequency = perc_freq / 100) |> 
  mutate(Plant = "Weed",
         Plot = c(rep("Total", nrow(sonoran.total.weedy)), rep("Wetter", nrow(sonoran.wet.weedy)),
                  rep("Drier", nrow(sonoran.dry.weedy)), rep("Wettest", nrow(sonoran.wettest.weedy)),
                  rep("Driest", nrow(sonoran.driest.weedy)))) 

# Combine Seeded
sonoran.seed <- bind_rows(sonoran.total.seed, sonoran.wet.seed, sonoran.dry.seed,
                          sonoran.wettest.seed, sonoran.driest.seed) |> 
  mutate(Frequency = perc_freq / 100) |> 
  mutate(Plant = paste(mix, "mix"),
         Plot = c(rep("Total", nrow(sonoran.total.seed)), rep("Wetter", nrow(sonoran.wet.seed)),
                  rep("Drier", nrow(sonoran.dry.seed)), rep("Wettest", nrow(sonoran.wettest.seed)),
                  rep("Driest", nrow(sonoran.driest.seed)))) |> 
  select(-mix)

# Combine Empty (all)
sonoran.empty <- bind_rows(sonoran.total.empty, sonoran.wet.empty, sonoran.dry.empty,
                          sonoran.wettest.empty, sonoran.driest.empty) |> 
  mutate(Frequency = perc_freq / 100) |> 
  mutate(Plant = "Empty",
         Plot = c("Total", "Wetter", "Drier", "Wettest", "Driest"))

# Combine Empty (seeded plots)
sonoran.empty.seed <- bind_rows(sonoran.total.empty.seed, sonoran.wet.empty.seed, sonoran.dry.empty.seed,
                           sonoran.wettest.empty.seed, sonoran.driest.empty.seed) |> 
  mutate(Frequency = perc_freq / 100) |> 
  mutate(Plant = "Empty seeded",
         Plot = c("Total", "Wetter", "Drier", "Wettest", "Driest"))

# All
sonoran.freq <- bind_rows(sonoran.nativevolun, sonoran.weedy, sonoran.seed,
                          sonoran.empty, sonoran.empty.seed) |> 
  mutate(Type = paste0(Plant, ", ", Plot))
sonoran.freq$Code[sonoran.freq$Code == "0"] <- "Empty"  

# Write to csv
write_csv(sonoran.freq,
          file = "data/cleaned/11.1_Sonoran-Desert_frequency_all_clean.csv")

# Species of interest
sonoran.freq.interest <- sonoran.freq |> 
  filter(Code == "SACO6" & Plant == "Current mix"| 
           Code == "LUSP2" & Plant == "Current mix" | 
           Code == "PLOV" & Plant == "Projected mix" | 
           Code == "SECO10" & Plant == "Projected mix" | 
           Code == "ARPU9" & Plant == "Projected mix" |
           Code %in% c("VUOC", "LOAR12", "CHPO12",
                       "LOHU2", "SCBA", "BRRU2", "ERCI6") |
           Code == "Empty" & Plant == "Empty") |> 
  mutate(Plant = str_replace(Plant, "Weed", "Invasive"),
         Type = str_replace(Type, "Weed", "Invasive"))

# Top 10 native volunteers
sonoran.freq.nativevolun <- sonoran.freq |> 
  filter(Plant == "Native recruit") |> 
  group_by(Plot, Type) |> 
  arrange(desc(Frequency)) |> 
  slice_head(n = 10)

# Top 10 weeds
sonoran.freq.weed <- sonoran.freq |> 
  filter(Plant == "Weed") |> 
  group_by(Plot, Type) |> 
  arrange(desc(Frequency)) |> 
  slice_head(n = 10)

# Current mix
sonoran.freq.current <- sonoran.freq |> 
  filter(Plant %in% c("Current mix", "Empty seeded"))

# Projected mix
sonoran.freq.projected <- sonoran.freq |> 
  filter(Plant %in% c("Current mix", "Empty seeded"))

# Write to csv
write_csv(sonoran.freq.interest,
          file = "data/cleaned/11.1_Sonoran-Desert_frequency_interest_clean.csv")
write_csv(sonoran.freq.nativevolun,
          file = "data/cleaned/11.1_Sonoran-Desert_frequency_native-volunteer_clean.csv")
write_csv(sonoran.freq.weed,
          file = "data/cleaned/11.1_Sonoran-Desert_frequency_weed_clean.csv")
write_csv(sonoran.freq.current,
          file = "data/cleaned/11.1_Sonoran-Desert_frequency_seeded-current_clean.csv")
write_csv(sonoran.freq.projected,
          file = "data/cleaned/11.1_Sonoran-Desert_frequency_seeded-projected_clean.csv")


# Sonoran Desert: Precip range --------------------------------------------

# Native volunteer
#   MONU, ASNU4, DICA14, ESCAM, LOAR12, LOHU2, LOSTT, PEHE, PERE, PLPA2, BAMU, 
#     AMMEI2, CHPO12, DAPU3, PLAR, SIAN2, VUOC, LAGR10, PEPL
present_species |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Weedy != "Weedy",
         SpeciesSeeded == "No") |> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum)) |> 
  mutate(range = max - min) |> 
  arrange(desc(range)) |> 
  print(n = 30)

# Weedy
#   ERCI6, SCBA, MAPA5, ONPI, SIIR, SATR12, SOOL, BRRU2
present_species |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Weedy != "Desirable") |> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum)) |> 
  mutate(range = max - min) |> 
  arrange(desc(range)) |> 
  print(n = 20)

# Seeded, Current
#   BAMU, LUSP2, SACO6, AMDE4
present_species |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Weedy != "Weedy",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current") |> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum)) |> 
  mutate(range = max - min) |> 
  arrange(desc(range))

# Seeded, Projected
#   PLOV, ENFA, SECO10, ARPU9
present_species |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Weedy != "Weedy",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected") |> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum)) |> 
  mutate(range = max - min) |> 
  arrange(desc(range))




# Northern Arizona: Frequency ---------------------------------------------

## Native recruit ---------------------------------------------------------

# Native volunteer (all plots): Highest species frequency overall
#   ATCO, SPSP.BarTBar, HESP.BabbittPJ, SOEL, CHAL11, SCMU6
naz.total.nativevolun <- present_species |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         SpeciesSeeded == "No") |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 3492) * 100) |> 
  print(n = 25)

# Native volunteer: when wetter
#   HESP.BabbittPJ, SPSP.BarTBar, ATCO, CHAL11, SCMU6, LEPA6, SAAB, SOEL
naz.wet.nativevolun <- present_species |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         SpeciesSeeded == "No",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 2196) * 100) |> 
  print(n = 25)

# Native volunteer: when extremely wet (+48% and wetter)
#   HESP.BabbittPJ, LEPA6, CHAL11, ATCO
naz.wettest.nativevolun <- present_species |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         SpeciesSeeded == "No",
         Perc_dev_cum > 0.48) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 2196) * 100) |> 
  print(n = 25)

# Native volunteer: when drier
#   ATCO, SOEL, SPSP.TLE, HECI
naz.dry.nativevolun <- present_species |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         SpeciesSeeded == "No",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 1296) * 100) |> 
  print(n = 25)

# Native volunteer: when extremely dry (-50% and drier)
#   SOEL, ATCO, CHEN.BabbittPJ, PHNE3
naz.driest.nativevolun <- present_species |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         SpeciesSeeded == "No",
         Perc_dev_cum < -0.5) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 1296) * 100) |> 
  print(n = 25)



## Weedy ------------------------------------------------------------------

# Weedy (all plots): Highest species frequency overall
#   SATR12, UNFO1.FlyingM, ERCI6, UNGR.FlyingM, UNGR.BarTBar, UNFO8.BarTBar, HAGL
naz.total.weedy <- present_species |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 3492) * 100) |> 
  print(n = 20)

# Weedy: when wetter
#   SATR12, UNFO1.FlyingM, UNGR.BarTBar, UNGR.FlyingM, UNFO8.BarTBar, ERCI6, UNGR1.MOWE
naz.wet.weedy <- present_species |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 2196) * 100) |> 
  print(n = 25)

# Weedy: when extremely wet (+48% and wetter)
#   BRINI, SATR12, ERCI6, UNFO1.PEFO, UNGR1.MOWE, HAGL
naz.wettest.weedy <- present_species |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau",
         Perc_dev_cum > 0.48) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 972) * 100) |> 
  print(n = 25)

# Weedy: when drier
#   SATR12, UNFO1.FlyingM, BRRU2, HAGL, ERCI6, TRTE
naz.dry.weedy <- present_species |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 1296) * 100) |> 
  print(n = 15)

# Weedy: when extremely dry (-50% and drier)
#   SATR12, BRRU2, ERCI6, TRTE
naz.driest.weedy <- present_species |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau",
         Perc_dev_cum < -0.5) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 540) * 100) |> 
  print(n = 15)


## Seeded species ---------------------------------------------------------

# Total: (1) Warm mix 
naz.total.seed1p <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "TLE"),
         Code %in% c("ACHY", "ARPU9", "ASTU", "BAMU", "BOCU", "PLMU3", "SECO10")) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 831) * 100,
         Sites = "AguaFria, MOWE, PEFO, Spiderweb, TLE",
         mix = "Projected")
naz.total.seed1p

# Total: (2) Med-Warm mix, Current 
naz.total.seed2c <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb"),
         Code %in% c("BOER4", "KRLA2", "MATA2", "PEPA8", "PLJA", "POSE", "SPCR")) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 1231) * 100,
         Sites = "AguaFria, MOWE, PEFO, Spiderweb",
         mix = "Current")
naz.total.seed2c

# Total: (2) Med-Warm mix, Projected
naz.total.seed2p <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("BarTBar", "FlyingM"),
         Code %in% c("BOER4", "KRLA2", "MATA2", "PEPA8", "PLJA", "POSE", "SPCR")) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 1231) * 100,
         Sites = "BarTBar, FlyingM",
         mix = "Projected")
naz.total.seed2p

# Total: (3) Cool-Med mix, Current
naz.total.seed3c <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("BarTBar", "FlyingM"),
         Code %in% c("ACMI2", "BOGR2", "DACA7", "ELEL5", "HEMU3", "LILE3", "PASM")) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 720) * 100,
         Sites = "BarTBar, FlyingM",
         mix = "Current")
naz.total.seed3c

# Total: (3) Cool-Med mix, Projected
naz.total.seed3p <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site == "BabbittPJ",
         Code %in% c("ACMI2", "BOGR2", "DACA7", "ELEL5", "HEMU3", "LILE3", "PASM")) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 720) * 100,
         Sites = "BabbitPJ",
         mix = "Projected")
naz.total.seed3p

# Total: (4) Cool mix, Current
naz.total.seed4c <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("BabbittPJ", "TLE"),
         Code %in% c("ELTR7", "ELWA2", "HEBO", "HECO26", "LECI4", "PSSP6", "SPGR2")) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 320) * 100,
         Sites = "BabbittPJ, TLE",
         mix = "Current")
naz.total.seed4c

# Wetter: (1) Warm mix 
naz.wet.seed1p <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "TLE"),
         Code %in% c("ACHY", "ARPU9", "ASTU", "BAMU", "BOCU", "PLMU3", "SECO10"),
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 479) * 100,
         Sites = "AguaFria, MOWE, PEFO, Spiderweb, TLE",
         mix = "Projected")
naz.wet.seed1p

# Wetter: (2) Med-Warm mix, Current 
naz.wet.seed2c <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb"),
         Code %in% c("BOER4", "KRLA2", "MATA2", "PEPA8", "PLJA", "POSE", "SPCR"),
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 783) * 100,
         Sites = "AguaFria, MOWE, PEFO, Spiderweb",
         mix = "Current")
naz.wet.seed2c

# Wetter: (2) Med-Warm mix, Projected
naz.wet.seed2p <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("BarTBar", "FlyingM"),
         Code %in% c("BOER4", "KRLA2", "MATA2", "PEPA8", "PLJA", "POSE", "SPCR"),
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 783) * 100,
         Sites = "BarTBar, FlyingM",
         mix = "Projected")
naz.wet.seed2p

# Wetter: (3) Cool-Med mix, Current
naz.wet.seed3c <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("BarTBar", "FlyingM"),
         Code %in% c("ACMI2", "BOGR2", "DACA7", "ELEL5", "HEMU3", "LILE3", "PASM"),
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 496) * 100,
         Sites = "BarTBar, FlyingM",
         mix = "Current")
naz.wet.seed3c

# Wetter: (3) Cool-Med mix, Projected
naz.wet.seed3p <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site == "BabbittPJ",
         Code %in% c("ACMI2", "BOGR2", "DACA7", "ELEL5", "HEMU3", "LILE3", "PASM"),
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 496) * 100,
         Sites = "BabbitPJ",
         mix = "Projected")
naz.wet.seed3p

# Wetter: (4) Cool mix, Current
naz.wet.seed4c <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("BabbittPJ", "TLE"),
         Code %in% c("ELTR7", "ELWA2", "HEBO", "HECO26", "LECI4", "PSSP6", "SPGR2"),
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 192) * 100,
         Sites = "BabbittPJ, TLE",
         mix = "Current")
naz.wet.seed4c

# Extremely wet: (1) Warm mix 
naz.wettest.seed1p <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "TLE"),
         Code %in% c("ACHY", "ARPU9", "ASTU", "BAMU", "BOCU", "PLMU3", "SECO10"),
         Perc_dev_cum > 0.48) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 191) * 100,
         Sites = "AguaFria, MOWE, PEFO, Spiderweb, TLE",
         mix = "Projected")
naz.wettest.seed1p

# Extremely wet: (2) Med-Warm mix, Current 
naz.wettest.seed2c <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb"),
         Code %in% c("BOER4", "KRLA2", "MATA2", "PEPA8", "PLJA", "POSE", "SPCR"),
         Perc_dev_cum > 0.48) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 207) * 100,
         Sites = "AguaFria, MOWE, PEFO, Spiderweb",
         mix = "Current")
naz.wettest.seed2c

# Extremely wet: (2) Med-Warm mix, Projected
naz.wettest.seed2p <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("BarTBar", "FlyingM"),
         Code %in% c("BOER4", "KRLA2", "MATA2", "PEPA8", "PLJA", "POSE", "SPCR"),
         Perc_dev_cum > 0.48) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 207) * 100,
         Sites = "BarTBar, FlyingM",
         mix = "Projected")
naz.wettest.seed2p

# Extremely wet: (3) Cool-Med mix, Current
naz.wettest.seed3c <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("BarTBar", "FlyingM"),
         Code %in% c("ACMI2", "BOGR2", "DACA7", "ELEL5", "HEMU3", "LILE3", "PASM"),
         Perc_dev_cum > 0.48) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 160) * 100,
         Sites = "BarTBar, FlyingM",
         mix = "Current")
naz.wettest.seed3c

# Extremely wet: (3) Cool-Med mix, Projected
naz.wettest.seed3p <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site == "BabbittPJ",
         Code %in% c("ACMI2", "BOGR2", "DACA7", "ELEL5", "HEMU3", "LILE3", "PASM"),
         Perc_dev_cum > 0.48) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 160) * 100,
         Sites = "BabbitPJ",
         mix = "Projected")
naz.wettest.seed3p

# Extremely wet: (4) Cool mix, Current
naz.wettest.seed4c <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("BabbittPJ", "TLE"),
         Code %in% c("ELTR7", "ELWA2", "HEBO", "HECO26", "LECI4", "PSSP6", "SPGR2"),
         Perc_dev_cum > 0.48) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 144) * 100,
         Sites = "BabbittPJ, TLE",
         mix = "Current")
naz.wettest.seed4c

# Drier: (1) Warm mix 
naz.dry.seed1p <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "TLE"),
         Code %in% c("ACHY", "ARPU9", "ASTU", "BAMU", "BOCU", "PLMU3", "SECO10"),
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 352) * 100,
         Sites = "AguaFria, MOWE, PEFO, Spiderweb, TLE",
         mix = "Projected")
naz.dry.seed1p

# Drier: (2) Med-Warm mix, Current 
naz.dry.seed2c <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb"),
         Code %in% c("BOER4", "KRLA2", "MATA2", "PEPA8", "PLJA", "POSE", "SPCR"),
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 448) * 100,
         Sites = "AguaFria, MOWE, PEFO, Spiderweb",
         mix = "Current")
naz.dry.seed2c

# Drier: (2) Med-Warm mix, Projected
naz.dry.seed2p <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("BarTBar", "FlyingM"),
         Code %in% c("BOER4", "KRLA2", "MATA2", "PEPA8", "PLJA", "POSE", "SPCR"),
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 448) * 100,
         Sites = "BarTBar, FlyingM",
         mix = "Projected")
naz.dry.seed2p

# Drier: (3) Cool-Med mix, Current
naz.dry.seed3c <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("BarTBar", "FlyingM"),
         Code %in% c("ACMI2", "BOGR2", "DACA7", "ELEL5", "HEMU3", "LILE3", "PASM"),
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 224) * 100,
         Sites = "BarTBar, FlyingM",
         mix = "Current")
naz.dry.seed3c

# Drier: (3) Cool-Med mix, Projected
naz.dry.seed3p <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site == "BabbittPJ",
         Code %in% c("ACMI2", "BOGR2", "DACA7", "ELEL5", "HEMU3", "LILE3", "PASM"),
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 224) * 100,
         Sites = "BabbitPJ",
         mix = "Projected")
naz.dry.seed3p

# Drier: (4) Cool mix, Current
naz.dry.seed4c <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("BabbittPJ", "TLE"),
         Code %in% c("ELTR7", "ELWA2", "HEBO", "HECO26", "LECI4", "PSSP6", "SPGR2"),
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 128) * 100,
         Sites = "BabbittPJ, TLE",
         mix = "Current")
naz.dry.seed4c

# Extremely dry: (1) Warm mix 
naz.driest.seed1p <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "TLE"),
         Code %in% c("ACHY", "ARPU9", "ASTU", "BAMU", "BOCU", "PLMU3", "SECO10"),
         Perc_dev_cum < -0.5) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 144) * 100,
         Sites = "AguaFria, MOWE, PEFO, Spiderweb, TLE",
         mix = "Projected")
naz.driest.seed1p

# Extremely dry: (2) Med-Warm mix, Current 
naz.driest.seed2c <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb"),
         Code %in% c("BOER4", "KRLA2", "MATA2", "PEPA8", "PLJA", "POSE", "SPCR"),
         Perc_dev_cum < -0.5) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 192) * 100,
         Sites = "AguaFria, MOWE, PEFO, Spiderweb",
         mix = "Current")
naz.driest.seed2c

# Extremely dry: (2) Med-Warm mix, Projected
naz.driest.seed2p <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("BarTBar", "FlyingM"),
         Code %in% c("BOER4", "KRLA2", "MATA2", "PEPA8", "PLJA", "POSE", "SPCR"),
         Perc_dev_cum < -0.5) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 192) * 100,
         Sites = "BarTBar, FlyingM",
         mix = "Projected")
naz.driest.seed2p

# Extremely dry: (3) Cool-Med mix, Current
naz.driest.seed3c <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("BarTBar", "FlyingM"),
         Code %in% c("ACMI2", "BOGR2", "DACA7", "ELEL5", "HEMU3", "LILE3", "PASM"),
         Perc_dev_cum < -0.5) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 96) * 100,
         Sites = "BarTBar, FlyingM",
         mix = "Current")
naz.driest.seed3c

# Extremely dry: (3) Cool-Med mix, Projected
naz.driest.seed3p <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site == "BabbittPJ",
         Code %in% c("ACMI2", "BOGR2", "DACA7", "ELEL5", "HEMU3", "LILE3", "PASM"),
         Perc_dev_cum < -0.5) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 96) * 100,
         Sites = "BabbitPJ",
         mix = "Projected")
naz.driest.seed3p

# Extremely dry: (4) Cool mix, Current
naz.driest.seed4c <- present_species |> 
  filter(SpeciesSeeded == "Yes",
         Site %in% c("BabbittPJ", "TLE"),
         Code %in% c("ELTR7", "ELWA2", "HEBO", "HECO26", "LECI4", "PSSP6", "SPGR2"),
         Perc_dev_cum < -0.5) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 148) * 100,
         Sites = "BabbittPJ, TLE",
         mix = "Current")
naz.driest.seed4c


# Current: total combined
#   LILE3, PASM, DACA7, LECI4, HEBO, AMCI2, HECO26, ELEL5
naz.total.seedc <- bind_rows(naz.total.seed2c, naz.total.seed3c, naz.total.seed4c) |> 
  arrange(desc(perc_freq))
naz.total.seedc

# Current: wetter combined
#   LILE3, DACA7, PASM, LECI4, HEBO, AMCI2, ELEL5, ELTR7
naz.wet.seedc <- bind_rows(naz.wet.seed2c, naz.wet.seed3c, naz.wet.seed4c) |> 
  arrange(desc(perc_freq))
naz.wet.seedc

# Current: Extremely wet (+48% and wetter) combined
#   LECI4, HEBO, ELTR7, HECO26, ELWA2
naz.wettest.seedc <- bind_rows(naz.wettest.seed2c, naz.wettest.seed3c, naz.wettest.seed4c) |> 
  arrange(desc(perc_freq))
naz.wettest.seedc

# Current: drier combined
#   HECO26, PASM, HEBO, LECI4, LILE3
naz.dry.seedc <- bind_rows(naz.dry.seed2c, naz.dry.seed3c, naz.dry.seed4c) |> 
  arrange(desc(perc_freq))
naz.dry.seedc

# Current: Extremely dry (-50% and drier) combined
#   HECO26, LECI4, HEBO, DACA7, PASM
naz.driest.seedc <- bind_rows(naz.driest.seed2c, naz.driest.seed3c, naz.driest.seed4c) |> 
  arrange(desc(perc_freq))
naz.driest.seedc


# Projected: total combined
#   BAMU, ASTU, SECO10, LILE3, PASM, ACHY
naz.total.seedp <- bind_rows(naz.total.seed1p, naz.total.seed2p, naz.total.seed3p) |> 
  arrange(desc(perc_freq))
naz.total.seedp 

# Projected: wetter combined
#   BAMU, ASTU, SECO10, PASM
naz.wet.seedp <- bind_rows(naz.wet.seed1p, naz.dry.seed2p, naz.dry.seed3p) |> 
  arrange(desc(perc_freq))
naz.wet.seedp

# Projected: Extremely wet (+48% and wetter) combined
#   BAMU, ASTU, SECO10, PASM
naz.wettest.seedp <- bind_rows(naz.wettest.seed1p, naz.dry.seed2p, naz.dry.seed3p) |> 
  arrange(desc(perc_freq))
naz.wettest.seedp

# Projected: drier combined
#   PASM, ACHY, BAMU
naz.dry.seedp <- bind_rows(naz.dry.seed1p, naz.dry.seed2p, naz.dry.seed3p) |> 
  arrange(desc(perc_freq))
naz.dry.seedp

# Projected: Extremely dry (-50% and drier) combined
#   PASM, BAMU, ELEL5, HEMU3
naz.driest.seedp <- bind_rows(naz.driest.seed1p, naz.driest.seed2p, naz.driest.seed3p) |> 
  arrange(desc(perc_freq))
naz.driest.seedp



## Empty plots ------------------------------------------------------------

# Empty plots (out of all plots)
naz.total.empty <- present_species |> 
  filter(Code == "0",
         Region == "Colorado Plateau") |> 
  count(Code) |> 
  mutate(perc_freq = (n / 3492) * 100)
naz.wet.empty <- present_species |> 
  filter(Code == "0",
         Region == "Colorado Plateau",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 2196) * 100)
naz.wettest.empty <- present_species |> 
  filter(Code == "0",
         Region == "Colorado Plateau",
         Perc_dev_cum > 0.48) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 792) * 100)
naz.dry.empty <- present_species |> 
  filter(Code == "0",
         Region == "Colorado Plateau",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 1296) * 100)
naz.driest.empty <- present_species |> 
  filter(Code == "0",
         Region == "Colorado Plateau",
         Perc_dev_cum < -0.5) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 540) * 100)


# Empty seeded plots (Current or Projected)
naz.total.empty.seed <- present_species |> 
  filter(PlotMix_Climate != "None",
         Region == "Colorado Plateau",
         Code == "0") |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 1553) * 100)
naz.wet.empty.seed <- present_species |> 
  filter(PlotMix_Climate != "None",
         Region == "Colorado Plateau",
         Code == "0",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 977) * 100)
naz.wettest.empty.seed <- present_species |> 
  filter(PlotMix_Climate != "None",
         Region == "Colorado Plateau",
         Code == "0",
         Perc_dev_cum > 0.48) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 353) * 100)
naz.dry.empty.seed <- present_species |> 
  filter(PlotMix_Climate != "None",
         Region == "Colorado Plateau",
         Code == "0",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 576) * 100)
naz.driest.empty.seed <- present_species |> 
  filter(PlotMix_Climate != "None",
         Region == "Colorado Plateau",
         Code == "0",
         Perc_dev_cum < -0.5) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 240) * 100)


## Construct tables -------------------------------------------------------

# Combine Native recruit
naz.nativevolun <- bind_rows(naz.total.nativevolun, naz.wet.nativevolun, naz.dry.nativevolun,
                             naz.wettest.nativevolun, naz.driest.nativevolun) |> 
  mutate(Frequency = perc_freq / 100) |> 
  mutate(Plant = "Native recruit",
         Plot = c(rep("Total", nrow(naz.total.nativevolun)), rep("Wetter", nrow(naz.wet.nativevolun)),
                  rep("Drier", nrow(naz.dry.nativevolun)), rep("Wettest", nrow(naz.wettest.nativevolun)),
                  rep("Driest", nrow(naz.driest.nativevolun)))) 

# Combine Weedy
naz.weedy <- bind_rows(naz.total.weedy, naz.wet.weedy, naz.dry.weedy,
                       naz.wettest.weedy, naz.driest.weedy) |> 
  mutate(Frequency = perc_freq / 100) |> 
  mutate(Plant = "Weed",
         Plot = c(rep("Total", nrow(naz.total.weedy)), rep("Wetter", nrow(naz.wet.weedy)),
                  rep("Drier", nrow(naz.dry.weedy)), rep("Wettest", nrow(naz.wettest.weedy)),
                  rep("Driest", nrow(naz.driest.weedy)))) 

# Combine Seeded
naz.seed <- bind_rows(naz.total.seedc, naz.wet.seedc, naz.dry.seedc,
                      naz.wettest.seedc, naz.driest.seedc,
                      naz.total.seedp, naz.wet.seedp, naz.dry.seedp,
                      naz.wettest.seedp, naz.driest.seedp) |> 
  mutate(Frequency = perc_freq / 100) |> 
  mutate(Plant = paste(mix, "mix"),
         Plot = c(rep("Total", nrow(naz.total.seedc)), rep("Wetter", nrow(naz.wet.seedc)),
                  rep("Drier", nrow(naz.dry.seedc)), rep("Wettest", nrow(naz.wettest.seedc)),
                  rep("Driest", nrow(naz.driest.seedc)),
                  rep("Total", nrow(naz.total.seedp)), rep("Wetter", nrow(naz.wet.seedp)),
                  rep("Drier", nrow(naz.dry.seedp)), rep("Wettest", nrow(naz.wettest.seedp)),
                  rep("Driest", nrow(naz.driest.seedp)))) |> 
  select(-mix)

# Combine Empty (all)
naz.empty <- bind_rows(naz.total.empty, naz.wet.empty, naz.dry.empty,
                       naz.wettest.empty, naz.driest.empty) |> 
  mutate(Frequency = perc_freq / 100) |> 
  mutate(Plant = "Empty",
         Plot = c("Total", "Wetter", "Drier", "Wettest", "Driest"))

# Combine Empty (seeded plots)
naz.empty.seed <- bind_rows(naz.total.empty.seed, naz.wet.empty.seed, naz.dry.empty.seed,
                                naz.wettest.empty.seed, naz.driest.empty.seed) |> 
  mutate(Frequency = perc_freq / 100) |> 
  mutate(Plant = "Empty seeded",
         Plot = c("Total", "Wetter", "Drier", "Wettest", "Driest"))

# All
naz.freq <- bind_rows(naz.nativevolun, naz.weedy, naz.seed,
                      naz.empty) |> 
  mutate(Type = paste0(Plant, ", ", Plot))
naz.freq$Code[naz.freq$Code == "0"] <- "Empty"  

# Write to csv
write_csv(naz.freq,
          file = "data/cleaned/11.1_Northern-AZ_frequency_all_clean.csv")

# Species of interest
naz.freq.interest <- naz.freq |> 
  filter(Code == "LECI4" & Plant == "Current mix"| 
           Code == "HEBO" & Plant == "Current mix" | 
           Code == "HECO26" & Plant == "Current mix" | 
           Code == "BAMU" & Plant == "Projected mix" | 
           Code == "PASM" & Plant == "Projected mix" |
           Code == "ASTU" & Plant == "Projected mix" |
           Code == "SECO10" & Plant == "Projected mix" |
           Code %in% c("ATCO", "SOEL", "CHALL11",
                       "SAR12", "ERCI6", "Empty")) |> 
  mutate(Plant = str_replace(Plant, "Weed", "Invasive"),
         Type = str_replace(Type, "Weed", "Invasive"))

# Top 10 native volunteers
naz.freq.nativevolun <- naz.freq |> 
  filter(Plant == "Native recruit") |> 
  group_by(Plot, Type) |> 
  arrange(desc(Frequency)) |> 
  slice_head(n = 10)

# Top 10 weeds
naz.freq.weed <- naz.freq |> 
  filter(Plant == "Weed") |> 
  group_by(Plot, Type) |> 
  arrange(desc(Frequency)) |> 
  slice_head(n = 10)

# Current mix
naz.freq.current <- naz.freq |> 
  filter(Plant %in% c("Current mix", "Empty seeded"))

# Projected mix
naz.freq.projected <- naz.freq |> 
  filter(Plant %in% c("Current mix", "Empty seeded"))

# Write to csv
write_csv(naz.freq.interest,
          file = "data/cleaned/11.1_Northern-AZ_frequency_interest_clean.csv")
write_csv(naz.freq.nativevolun,
          file = "data/cleaned/11.1_Northern-AZ_frequency_native-volunteer_clean.csv")
write_csv(naz.freq.weed,
          file = "data/cleaned/11.1_Northern-AZ_frequency_weed_clean.csv")
write_csv(naz.freq.current,
          file = "data/cleaned/11.1_Northern-AZ_frequency_seeded-current_clean.csv")
write_csv(naz.freq.projected,
          file = "data/cleaned/11.1_Northern-AZ_frequency_seeded-projected_clean.csv")




# Northern Arizona: Precip range ------------------------------------------

# Native volunteer
#   CHAL11, HESP.BabbittPJ, LECI4, BOGR2, BOSI2
present_species |> 
  filter(Region == "Colorado Plateau",
         Weedy != "Weedy",
         SpeciesSeeded == "No") |> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum)) |> 
  mutate(range = max - min) |> 
  arrange(desc(range)) |> 
  print(n = 30)

# Weedy
#   UNSH.BarTBar, SATR12, UNGR.BabbittPJ, ERCI6, TRTE
present_species |> 
  filter(Region == "Colorado Plateau",
         Weedy != "Desirable") |> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum)) |> 
  mutate(range = max - min) |> 
  arrange(desc(range)) |> 
  print(n = 20)

# Seeded, Current
#   LECI4, KRLA2, PEPA8
present_species |> 
  filter(Region == "Colorado Plateau",
         Weedy != "Weedy",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current") |> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum)) |> 
  mutate(range = max - min) |> 
  arrange(desc(range))

# Seeded, Projected
#   ASTU, SECO10, ACHY, BAMU
present_species |> 
  filter(Region == "Colorado Plateau",
         Weedy != "Weedy",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected") |> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum)) |> 
  mutate(range = max - min) |> 
  arrange(desc(range))



save.image("RData/11.1_calculate-frequency-by-species.RData")
