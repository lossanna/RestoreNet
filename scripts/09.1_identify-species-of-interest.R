# Created: 2024-09-17
# Last updated: 2024-09-18

# Purpose: Identify what species are doing well based on Count in Sonoran Desert and
#   Northern Arizona Plateau. Calculate total number of plots for various groups 
#   to determine percent frequency in which the species shows up in a plot.
#   Investigate species of interest (seeded, native volunteer, and weedy
#   that are doing well in variable precip by Count and plot frequency). Use graphs from
#   09.1_drafit-figs_precip-dev_subplot.R to determine precip extremes, tall heights, and high
#   count values, as well as graphs of individual species to visualize performance.

# Sonoran Desert precip deviation extremes (to determine performance in variable precip):
#   Wet: +24% and wetter includes all sites but Roosevelt.
#   Dry: -23% and drier includes all sites but Patagonia.
# Northern Arizona Plateau precip deviation extremes:
#   Wet: +48% and wetter includes all sites but FlyingM and TLE.
#   Dry: -50% and drier includes all sites.

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

# Load data ---------------------------------------------------------------

subplot <- read_csv("data/cleaned/04.1_subplot-data_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")


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
dat <- dat |> 
  filter(Perc_dev_cum != Inf)



# Site info ---------------------------------------------------------------

## Sonoran Desert ---------------------------------------------------------

# Sonoran Central precip dev: ranged from -43% to +64%
dat |> 
  filter(Region == "Sonoran Central") |> 
  count(Perc_dev_cum)

# Sonoran SE precip dev: ranged from -23% to +46%
dat |> 
  filter(Region == "Sonoran SE") |> 
  count(Perc_dev_cum)


### Weedy & Native recruit ------------------------------------------------

# Values for weedy species, native recruits, and empty plot comparison

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

# Number of drier plots: 792
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  select(Region, Site, Date_Monitored, Plot, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE) |> 
  filter(Perc_dev_cum < 0) |> 
  nrow()


### Seeded species --------------------------------------------------------

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

# 1: Sonoran Central only, when drier: 224
dat |> 
  filter(Region == "Sonoran Central", 
         PlotMix_Climate == "Current",
         Perc_dev_cum < 0) |> 
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

# 2: Sonoran SE only, when drier: 128
dat |> 
  filter(Region == "Sonoran SE",
         PlotMix_Climate == "Current",
         Perc_dev_cum < 0) |> 
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

# 3. Both sites, same mix, drier: 352
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         Perc_dev_cum < 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 4. Both sites, species in opposite mixes, total plots: 512
#     when wetter: 160; when drier: 352
#   Same as 3 because BAMU was still seeded in only half the plots at each site

# 0. Empty plot comparison, total: 1024
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate != "None") |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 0: Empty plot comparison, wetter: 320
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate != "None",
         Perc_dev_cum > 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 0. Empty plot comparison, drier: 704
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate != "None",
         Perc_dev_cum < 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()



## Northern Arizona Plateau -----------------------------------------------

# Precip dev
#   Ranges from -99% to 193%
summary(filter(dat, Region == "Colorado Plateau")$Perc_dev_cum)
hist(filter(dat, Region == "Colorado Plateau")$Perc_dev_cum)
count(filter(dat, Region == "Colorado Plateau"), Perc_dev_cum) |> 
  print(n = 97)

### Weedy & Native recruit ------------------------------------------------

# Values for weedy species, native recruits, and empty plot comparison

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

# Number of drier plots: 1296
dat |> 
  filter(Region == "Colorado Plateau") |> 
  select(Region, Site, Date_Monitored, Plot, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE) |> 
  filter(Perc_dev_cum < 0) |> 
  nrow()


### Seeded species --------------------------------------------------------

# 4 seed mixes:
# (Note that Current vs. Projected doesn't affect plot number, just the site, because
# some sites were monitored more frequently than others.No species was included 
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


# 0. Empty plot comparison total: 3104
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate != "None") |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 0. Empty plot comparison, wetter: 1952
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate != "None") |> 
  select(Region, Site, Date_Monitored, Plot, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE) |> 
  filter(Perc_dev_cum > 0) |> 
  nrow()

# 0. Empty plot comparison, drier: 1152
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate != "None") |> 
  select(Region, Site, Date_Monitored, Plot, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE) |> 
  filter(Perc_dev_cum < 0) |> 
  nrow()



# Sonoran Desert ----------------------------------------------------------

## Most frequent species (in highest # of plots) --------------------------

### Weedy & Native recruit ------------------------------------------------

# Native volunteers (all plots): Highest species frequency overall
#   LOAR12, CHPO12, LOHU2, VUOC
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "No") |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 1152) * 100) |> 
  print(n = 30)

# Native volunteers: Highest species frequency when wetter
#   LOAR12, PSCA11, CHPO12, VUOC, GIST, MEAL6
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "No",
         Perc_dev_cum > 0) |> 
  select(Site, Code, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 360) * 100) |> 
  print(n = 25)

# Native volunteers: Highest species frequency when drier
#   LOAR12, LOHU2, AMCO3, CHPO12, VUOC
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "No",
         Perc_dev_cum < 0) |> 
  select(Site, Code, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 792) * 100) |> 
  print(n = 25)


# Weedy (all plots): Highest species frequency overall
#   ERCI6, SCBA, BRRU2, ERLE
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  select(Site, Code, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 1152) * 100) |> 
  print(n = 15)

# Weedy: Highest species frequency when wetter
#   SCBA, ERCI6, ERLE, BRRU2
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum > 0) |> 
  select(Site, Code, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 360) * 100) |> 
  print(n = 15)

# Weedy: Highest species frequency when drier
#   ERCI6, SCBA, BRRU2, ERLE
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum < 0) |> 
  select(Site, Code, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 792) * 100) |> 
  print(n = 15)


### Seeded species --------------------------------------------------------

# Total: Seeded version 1 
sonoran.total.seed1 <- dat |> 
  filter(Code %in% c("AMDE4", "DICA8", "LUSP2", "MUPO2", "SACO6", "SPAM2", 
                     "BOAR", "BORO2", "CAER", "ENFA", "PLOV"),
         Region == "Sonoran Central",
         SpeciesSeeded == "Yes") |> 
  count(Code) |> 
  mutate(perc_freq = (n / 320) * 100) |> 
  arrange(desc(n)) 
sonoran.total.seed1
sonoran.total.seed1 <- sonoran.total.seed1 |> 
  mutate(mix = c("Current", "Current", "Projected", "Current", "Projected", "Current",
                 "Projected", "Current"))
sonoran.total.seed1

# Total: Seeded version 2
sonoran.total.seed2 <- dat |> 
  filter(Code %in% c("BOGR2", "ELEL5", "HEMU3", "HENE5", "MATA2", "POSE", 
                     "ASTU", "PLJA", "PEPA8"),
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes") |> 
  count(Code) |> 
  mutate(perc_freq = (n / 192) * 100) |> 
  arrange(desc(n)) 
sonoran.total.seed2
sonoran.total.seed2 <- sonoran.total.seed2 |> 
  mutate(mix = c("Projected", "Current", "Current", "Current"))

# Total: Seeded version 3
sonoran.total.seed3 <- dat |> 
  filter(Code %in% c("SPCR", "ARPU9", "BOCU", "SECO10"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "Yes") |> 
  count(Code) |> 
  mutate(perc_freq = (n / 512) * 100) |> 
  arrange(desc(n))
sonoran.total.seed3
sonoran.total.seed3 <- sonoran.total.seed3 |> 
  mutate(mix = c("Projected", "Projected", "Current", "Projected"))

# Total: Seeded version 4, Current
sonoran.total.seed4c <- dat |> 
  filter(Code == "BAMU",
         Region == "Sonoran Central",
         SpeciesSeeded == "Yes") |> 
  count(Code) |> 
  mutate(perc_freq = (n / 512) * 100,
         mix = "Current") 
sonoran.total.seed4c

# Total: Seeded version 4, Projected
sonoran.total.seed4p <- dat |> 
  filter(Code == "BAMU",
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes") |> 
  count(Code) |> 
  mutate(perc_freq = (n / 512) * 100,
         mix = "Projected") 
sonoran.total.seed4p

# Wetter: Seeded version 1
sonoran.wet.seed1 <- dat |> 
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
  mutate(mix = c("Projected", "Current", "Current", "Projected", "Current", "Projected",
         "Current", "Current"))
sonoran.wet.seed1

# Wetter: Seeded version 2
sonoran.wet.seed2 <- dat |> 
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
  mutate(mix = c("Current", "Current", "Current", "Projected"))

# Wetter: Seeded version 3
sonoran.wet.seed3 <- dat |> 
  filter(Code %in% c("SPCR", "ARPU9", "BOCU", "SECO10"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 160) * 100) |> 
  arrange(desc(n))
sonoran.wet.seed3
sonoran.wet.seed3 <- sonoran.wet.seed3 |> 
  mutate(mix = c("Projected", "Projected", "Current"))

# Wetter: Seeded version 4, Current
sonoran.wet.seed4c <- dat |> 
  filter(Code == "BAMU",
         Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 160) * 100,
         mix = "Current") 
sonoran.wet.seed4c

# Wetter: Seeded version 4, Projected
sonoran.wet.seed4p <- dat |> 
  filter(Code == "BAMU",
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 160) * 100,
         mix = "Projected") 
sonoran.wet.seed4p

# Drier: Seeded version 1
sonoran.dry.seed1 <- dat |> 
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
  mutate(mix = c("Current", "Current", "Projected", "Current",
                 "Current", "Projected", "Current"))
sonoran.dry.seed1

# Drier: Seeded version 2
sonoran.dry.seed2 <- dat |> 
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
  mutate(mix = c("Projected", "Current", "Current"))

# Drier: Seeded version 3
sonoran.dry.seed3 <- dat |> 
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

# Drier: Seeded version 4, Current
sonoran.dry.seed4c <- dat |> 
  filter(Code == "BAMU",
         Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 160) * 100,
         mix = "Current") 
sonoran.dry.seed4c

# Drier: Seeded version 4, Projected
sonoran.dry.seed4p <- dat |> 
  filter(Code == "BAMU",
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 160) * 100,
         mix = "Projected") 
sonoran.dry.seed4p

# Total: combined
#   Current: SACO6, LUSP2
#   Projected: PLOV, ARPU9
sonoran.total.seed <- bind_rows(sonoran.total.seed1, sonoran.total.seed2, sonoran.total.seed3, 
                                sonoran.total.seed4c, sonoran.total.seed4p) |> 
  arrange(desc(perc_freq))
sonoran.total.seed

# Wetter: combined
#   Current: LUSP2, SACO6
#   Projected: PLOV, ARPU9
sonoran.wet.seed <- bind_rows(sonoran.wet.seed1, sonoran.wet.seed2, sonoran.wet.seed3,
                              sonoran.wet.seed4c, sonoran.wet.seed4p) |> 
  arrange(desc(perc_freq))
sonoran.wet.seed

# Drier: combined
#   Current: SACO6, LUSP2
#   Projected: PLOV, SECO10
sonoran.dry.seed <- bind_rows(sonoran.dry.seed1, sonoran.dry.seed2, sonoran.dry.seed3,
                              sonoran.dry.seed4c) |> 
  arrange(desc(perc_freq))
sonoran.dry.seed


# Empty seeded plots (Current + Projected): total
dat |> 
  filter(PlotMix_Climate != "None",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  count(Code) |> 
  filter(Code == "0") |> 
  mutate(perc_freq = (n / 1024) * 100) 

# Empty seeded plots (Current + Projected): when wetter
dat |> 
  filter(PlotMix_Climate != "None",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  filter(Code == "0") |> 
  mutate(perc_freq = (n / 320) * 100)

# Empty seeded plots (Current + Projected): when drier
dat |> 
  filter(PlotMix_Climate != "None",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  filter(Code == "0") |> 
  mutate(perc_freq = (n / 704) * 100) 



### Construct table -------------------------------------------------------

# Native volunteers
sonoran.total.nativevolun <- dat |> 
  filter(Code %in% c("VUOC", "LOAR12", "CHPO12", "LOHU2"),
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 1152),
         Plant = "Native recruit",
         Plot = "Total")
sonoran.wet.nativevolun <- dat |> 
  filter(Code %in% c("VUOC", "LOAR12", "CHPO12", "LOHU2"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 360),
         Plant = "Native recruit",
         Plot = "Wetter")
sonoran.dry.nativevolun <- dat |> 
  filter(Code %in% c("VUOC", "LOAR12", "CHPO12", "LOHU2"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 792),
         Plant = "Native recruit",
         Plot = "Drier")

# Weedy
sonoran.total.weedy <- dat |> 
  filter(Code %in% c("SCBA", "BRRU2", "ERCI6"),
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 1152),
         Plant = "Invasive",
         Plot = "Total")
sonoran.wet.weedy <- dat |> 
  filter(Code %in% c("SCBA", "BRRU2", "ERCI6"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 360),
         Plant = "Invasive",
         Plot = "Wetter")
sonoran.dry.weedy <- dat |> 
  filter(Code %in% c("SCBA", "BRRU2", "ERCI6"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 792),
         Plant = "Invasive",
         Plot = "Drier")

# Empty plots (out of all plots)
sonoran.total.empty <- dat |> 
  filter(Code == "0",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 1152),
         Plant = "Empty",
         Plot = "Total")
sonoran.wet.empty <- dat |> 
  filter(Code == "0",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 360),
         Plant = "Empty",
         Plot = "Wetter")
sonoran.dry.empty <- dat |> 
  filter(Code == "0",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 792),
         Plant = "Empty",
         Plot = "Drier")

# Seeded
sonoran.seed.interest <- bind_rows(sonoran.total.seed, sonoran.wet.seed, sonoran.dry.seed) |> 
  mutate(Frequency = perc_freq / 100) |> 
  mutate(Plant = paste(mix, "mix"),
         Plot = c(rep("Total", nrow(sonoran.total.seed)), rep("Wetter", nrow(sonoran.wet.seed)),
                  rep("Drier", nrow(sonoran.dry.seed)))) |> 
  select(-mix, -perc_freq) |> 
  filter(Code %in% c("SACO6", "LUSP2", "PLOV", "SECO10", "ARPU9"))

# Combine all
sonoran.interest <- bind_rows(sonoran.total.nativevolun, sonoran.wet.nativevolun, sonoran.dry.nativevolun,
                              sonoran.total.weedy, sonoran.wet.weedy, sonoran.dry.weedy,
                              sonoran.seed.interest, sonoran.total.empty, sonoran.wet.empty, sonoran.dry.empty) |> 
  mutate(Type = paste0(Plant, ", ", Plot))
sonoran.interest$Code[sonoran.interest$Code == "0"] <- "Empty"  

# Write to csv
write_csv(sonoran.interest,
          file = "data/cleaned/09.1_Sonoran-Desert_frequency_species-of-interest.csv")

  
## Precip range -----------------------------------------------------------

# Native volunteer
#   MONU, LOAR12, PEHE, CHPO12, LOHU2, LOSTT, PERE, PLAR, PLPA2, SIAN2, VUOC, PEPL
dat |> 
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
#   ERCI6, SCBA, MAPA5, BRRU2
dat |> 
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
dat |> 
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
#   PLOV, SECO10, ARPU9
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Weedy != "Weedy",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected") |> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum)) |> 
  mutate(range = max - min) |> 
  arrange(desc(range))



## Count ------------------------------------------------------------------

### Native recruit --------------------------------------------------------

# Native volunteers (all plots): All conditions, high count per plot
#   CRYSPP/CRAN4, PECTO, GIST, CHPO12, VUOC, MENSPP, MEAL6
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Count > 50) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  print(n = 24)

# Native volunteers: +24% and wetter, count of all individuals 
#   GIST, VUOC, LOAR12, MEAL6, CRAN4
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum > 0.24) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 40)

# Native volunteers: +24% and wetter, with at least 10 individuals in a plot
#   PSCA11, BAMU, VUOC, LOAR12 - high count per plot when wettest
#   GIST, LOAR12, VUOC - high frequency of plots
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum > 0.24) |> 
  filter(Count > 9) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 99)
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum > 0.24) |> 
  filter(Count > 9) |> 
  count(Code) |> 
  arrange(desc(n))

# Native volunteers: -23% and drier, count of all individuals 
#   CRYSPP, MENSPP, VUOC, LOHU2, LOAR12, PERE
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum < -0.23) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 51)

# Native volunteers: -23% and drier, with at least 10 individuals in a plot
#   CHPO12, VUOC, MENSPP, CRYSPP - high count per plot when driest
#   CRYSPP, MENSPP, LOHU2, VUOC - high frequency of plots
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Count > 9) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 73)
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Count > 9) |> 
  count(Code) |> 
  arrange(desc(n))


# LOAR12, VUOC, CHPO12, LOHU2 max Count
dat |> 
  filter(Code %in% c("LOAR12", "VUOC", "CHPO12", "LOHU2"),
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Count = max(Count)) |> 
  arrange(desc(max_Count)) |> 
  arrange(Code) |> 
  print(n = 37)



### Weedy -----------------------------------------------------------------

# Weedy (all plots): All conditions, high count recruit per plot
#   ERCI6, BRRU2, SCBA
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Count > 50) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  print(n = 29)

# Weedy: Highest wet deviation
#   MAPA5
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum > 0.35) |> 
  filter(Count > 5) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 29)

# Weedy: +24% and wetter, count of all individuals
#   BRRU2, MAPA5, UNFO3.SRER, SCBA, UNFO2.Patagonia, ERCI6
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum > 0.24) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 30)

# Weedy: +24% and wetter, with at least 11 individuals in a plot
#   MAPA5, UNFO.Patagonia, BRRU2, SCBA - high count per plot when wettest
#   BRRU2, SCBA, MAPA5, UNFO3.SRER - high frequency of plots
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum > 0.24) |> 
  filter(Count > 10) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 41)
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum > 0.24) |> 
  filter(Count > 10) |> 
  count(Code) |> 
  arrange(desc(n))

# Weedy: Highest dry deviation
#   ERCI6, SCBA, BRRU2
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum < -0.35) |> 
  filter(Count > 50) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count))

# Weedy: -23% and drier, count of all individuals
#   ERCI6, SCBA, BRRU2
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum < -0.23) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

# Weedy: -23% and drier, with at least 51 individuals in a plot
#   It's just endless SCBA, BRRU2, ERCI6
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Count > 50) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum)
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Count > 50) |> 
  count(Code) |> 
  arrange(desc(n))

# Weedy: highest number of individuals across all sites, plots, and conditions
#   SCBA, ERCI6, BRRU2
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Weedy != "Desirable") |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 30)

# Sonoran SE plots: High dry deviation
#   SATR12, ERLE, lots of unknowns
dat |> 
  filter(Weedy != "Desirable",
         Region == "Sonoran SE") |> 
  filter(Perc_dev_cum < 0) |> 
  filter(Count > 10) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Site) |> 
  print(n = 22)

# SCBA, BRRU2, ERCI6 max count
dat |> 
  filter(Code %in% c("SCBA", "BRRU2", "ERCI6"),
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Count = max(Count)) |> 
  arrange(desc(max_Count)) |> 
  arrange(Code) |> 
  print(n = 34)



### Seeded ----------------------------------------------------------------

# Current: All conditions, high count per plot
#   SACO6
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Count > 10) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) 

# Current: +24% and wetter, with at least 3 individuals in a plot
#   SACO6, BAMU, MATA2, ELEL5, POSE - high count per plot when wettest
#   SACO6, LUSP2 - high frequency of plots
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum > 0.24,
         Count > 2) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 42)
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum > 0.24,
         Count > 2) |> 
  count(Code) |> 
  arrange(desc(n))

# Current: -23% and drier, with at least 2 individuals in a plot
#   SACO6, LUSP2 
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Count > 1) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 24)
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Count > 1) |> 
  count(Code) |> 
  arrange(desc(n))


# Projected: All conditions, high count per plot
#   PLOV, BAMU
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  filter(Count > 10) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) 
  
# Projected: +24% and wetter, with at least 3 individuals in a plot
#   ARPU9, BAMU, PLOV
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum > 0.24,
         Count > 2) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 35)
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum > 0.24,
         Count > 2) |> 
  count(Code) |> 
  arrange(desc(n))
  
# Projected: -23% and drier, with at least 1 individual in a plot
#     PLOV
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Count > 0) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 29)
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Count > 0) |>
  count(Code) |> 
  arrange(desc(n))


# SACO6, LUSP2 max count in Current
dat |> 
  filter(Code %in% c("SACO6", "LUSP2"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         SpeciesSeeded == "Yes") |> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Count = max(Count)) |> 
  arrange(desc(max_Count)) |> 
  arrange(Code) |> 
  print(n = 22)

# PLOV, SECO10, ARPU9 max count in Projected
dat |> 
  filter(Code %in% c("PLOV", "SECO10", "ARPU9"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         SpeciesSeeded == "Yes") |> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Count = max(Count)) |> 
  arrange(desc(max_Count)) |> 
  arrange(Code) |> 
  print(n = 32)



## Height -----------------------------------------------------------------

### Native recruit --------------------------------------------------------

# All plots: Tallest
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "No") |> 
  filter(Height > 600) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  print(n = 17)

# LOAR12, VUOC, CHPO12, LOHU2 tallest heights
dat |> 
  filter(Code %in% c("LOAR12", "VUOC", "CHPO12", "LOHU2"),
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Height = max(Height)) |> 
  arrange(desc(max_Height)) |> 
  arrange(Code) |> 
  print(n = 37)


### Weedy -----------------------------------------------------------------

# Weedy: All conditions, tallest
#   ERCU2, ERLE SATR12
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Height > 600) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) 

# Weedy: 24% or wetter, 15 cm and taller
#   MAPA5, SIIR
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum > 0.24) |> 
  filter(Height > 149) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 42)
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum > 0.24) |> 
  filter(Height > 149) |> 
  count(Code) |> 
  arrange(desc(n))

# Weedy: -23% and drier, 5 cm and taller
#   ERCI6, BRRU2
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Height > 49) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 41)
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Height > 49) |>
  count(Code) |> 
  arrange(desc(n))

# SCBA at 10 cm and 24%+ wetter
#   Present at wettest (64%)
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Code == "SCBA") |> 
  filter(Perc_dev_cum > 0.24,
         Height > 99) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) 

# SCBA at 6 cm and -23% drier
#   Present at 31% drier
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Code == "SCBA") |> 
  filter(Perc_dev_cum < -0.23,
         Height > 59) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) 

# SCBA height at driest
#   Tallest it gets is 50 mm (5 cm)
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Code == "SCBA") |> 
  filter(Perc_dev_cum < -0.35) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) 

# BRRU2 at 10 cm and 24%+ wetter
#   Present at +34%
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Code == "BRRU2") |> 
  filter(Perc_dev_cum > 0.24,
         Height > 99) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) 

# BRRU2 at 10 cm and -23% drier
#   Present at 31% drier
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Code == "BRRU2") |> 
  filter(Perc_dev_cum < -0.23,
         Height > 99) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) 

# BRRU2 height at driest
#   Tallest is 75 mm 
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Code == "BRRU2") |> 
  filter(Perc_dev_cum < -0.35) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) 

# ERCI6 at 10 cm and 24%+ wetter
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Code == "ERCI6") |> 
  filter(Perc_dev_cum > 0.24,
         Height > 99) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) 

# ERCI6 at 5 cm and -23% drier
#   Present at 31% drier
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Code == "ERCI6") |> 
  filter(Perc_dev_cum < -0.23,
         Height > 49) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) 

# BRRU2 height at driest
#   Tallest is 75 mm 
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Code == "ERCI6") |> 
  filter(Perc_dev_cum < -0.35) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 36)

# SCBA, BRRU2, ERCI6 tallest heights
dat |> 
  filter(Code %in% c("SCBA", "BRRU2", "ERCI6"),
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Height = max(Height)) |> 
  arrange(desc(max_Height)) |> 
  arrange(Code) |> 
  print(n = 34)


### Seeded ----------------------------------------------------------------

# Current: All conditions, tallest
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         SpeciesSeeded == "Yes") |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  print(n = 50)

# Current: +24% and wetter, 3 cm and taller
#   SACO6, LUSP2, ELEL5
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         SpeciesSeeded == "Yes") |> 
  filter(Perc_dev_cum > 0.24) |> 
  filter(Height > 29) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 39)
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         SpeciesSeeded == "Yes") |> 
  filter(Perc_dev_cum > 0.24) |> 
  filter(Height > 29) |> 
  count(Code) |> 
  arrange(desc(n))

# Current: -23% and drier, 2 cm and taller
#   LUSP2, SACO6
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         SpeciesSeeded == "Yes") |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Height > 19) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 41)
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         SpeciesSeeded == "Yes") |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Height > 19) |> 
  count(Code) |> 
  arrange(desc(n))


# Projected: All conditions, tallest
#   ARPU9, BORO2
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  filter(Height > 500) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height))

# Projected: +24% and wetter, 3 cm and taller
#   PLOV, ARPU9
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         SpeciesSeeded == "Yes") |> 
  filter(Perc_dev_cum > 0.24) |> 
  filter(Height > 29) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 36)
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         SpeciesSeeded == "Yes") |> 
  filter(Perc_dev_cum > 0.24) |> 
  filter(Height > 29) |> 
  count(Code) |> 
  arrange(desc(n))

# Projected: -23% and drier, 2 cm and taller
#   PLOV, SECO10
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         SpeciesSeeded == "Yes") |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Height > 19) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 23)
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         SpeciesSeeded == "Yes") |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Height > 19) |> 
  count(Code) |> 
  arrange(desc(n))


# SACO6, LUSP2 tallest heights in Current
dat |> 
  filter(Code %in% c("SACO6", "LUSP2"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         SpeciesSeeded == "Yes") |> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Height = max(Height)) |> 
  arrange(desc(max_Height)) |> 
  arrange(Code) |> 
  print(n = 22)

# PLOV, SECO10, ARPU9 tallest heights in Projected
dat |> 
  filter(Code %in% c("PLOV", "SECO10", "ARPU9"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         SpeciesSeeded == "Yes") |> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Height = max(Height)) |> 
  arrange(desc(max_Height)) |> 
  arrange(Code) |> 
  print(n = 32)





# Northern Arizona Plateau ------------------------------------------------

## Most frequent species (in highest # of plots) --------------------------

### Weedy & Native recruit ------------------------------------------------

# Native volunteer (all plots): Highest species frequency overall
#   CHAL11, SCMU6, LEPA6, ATCO, SAAB, SOEL
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         SpeciesSeeded == "No") |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 3492) * 100) |> 
  print(n = 25)

# Native volunteer: when wetter
#   CHAL11, SCMU6, LEPA6, SAAB
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         SpeciesSeeded == "No",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 2196) * 100) |> 
  print(n = 25)

# Native volunteer: when drier
#   MONU, GUSA2, HECI, PHNE3
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         SpeciesSeeded == "No",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 1296) * 100) |> 
  print(n = 47)


# Weedy (all plots): Highest species frequency overall
#   SATR12, BRNI, ERCI6, BRRU2, HAGL
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 3492) * 100) |> 
  print(n = 20)

# Weedy: when wetter
#   SATR12, BRNI, ERCI6
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau",
         Perc_dev_cum > 0) |> 
  select(Site, Code, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 2196) * 100) |> 
  print(n = 25)

# Weedy: when drier
#   SATR12, BRRU2, HAGL
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau",
         Perc_dev_cum < 0) |> 
  select(Site, Code, Duration, Lifeform, Count, Perc_dev_cum) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 1296) * 100) |> 
  print(n = 15)


### Seeded species --------------------------------------------------------

# Total: (1) Warm mix 
naz.total.seed1p <- dat |> 
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
naz.total.seed2c <- dat |> 
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
naz.total.seed2p <- dat |> 
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
naz.total.seed3c <- dat |> 
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
naz.total.seed3p <- dat |> 
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
naz.total.seed4c <- dat |> 
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
naz.wet.seed1p <- dat |> 
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
naz.wet.seed2c <- dat |> 
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
naz.wet.seed2p <- dat |> 
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
naz.wet.seed3c <- dat |> 
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
naz.wet.seed3p <- dat |> 
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
naz.wet.seed4c <- dat |> 
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

# Drier: (1) Warm mix 
naz.dry.seed1p <- dat |> 
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

# Drier: Med-Warm mix, Current 
naz.dry.seed2c <- dat |> 
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

# Drier: Med-Warm mix, Projected
naz.dry.seed2p <- dat |> 
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

# Drier: Cool-Med mix, Current
naz.dry.seed3c <- dat |> 
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

# Drier: Cool-Med mix, Projected
naz.dry.seed3p <- dat |> 
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

# Drier: Cool mix, Current
naz.dry.seed4c <- dat |> 
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


# Current: total combined
#   PASM, DACA7, LILE3, LECI4
naz.total.seedc <- bind_rows(naz.total.seed2c, naz.total.seed3c, naz.total.seed4c) |> 
  arrange(desc(perc_freq))
naz.total.seedc

# Current: wetter combined
#   LILE3, DACA7, LECI4, PASM
naz.wet.seedc <- bind_rows(naz.wet.seed2c, naz.wet.seed3c, naz.wet.seed4c) |> 
  arrange(desc(perc_freq))
naz.wet.seedc

# Current: drier combined
#   LECI4, PASM, HECO26
naz.dry.seedc <- bind_rows(naz.dry.seed2c, naz.dry.seed3c, naz.dry.seed4c) |> 
  arrange(desc(perc_freq))
naz.dry.seedc

# Projected: total combined
#   BAMU, ASTU, SECO10
naz.total.seedp <- bind_rows(naz.total.seed1p, naz.total.seed2p, naz.total.seed3p) |> 
  arrange(desc(perc_freq))
naz.total.seedp 

# Projected: wetter combined
#   BAMU, ASTU, SECO10
naz.wet.seedp <- bind_rows(naz.wet.seed1p, naz.dry.seed2p, naz.dry.seed3p) |> 
  arrange(desc(perc_freq))
naz.wet.seedp

# Projected: drier combined
#   PASM, BAMU, BOCU
naz.dry.seedp <- bind_rows(naz.dry.seed1p, naz.dry.seed2p, naz.dry.seed3p) |> 
  arrange(desc(perc_freq))
naz.dry.seedp


# Empty seeded plots (Current + Projected): total
dat |> 
  filter(PlotMix_Climate != "None",
         Region == "Colorado Plateau") |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 3104) * 100)

# Empty seeded plots (Current + Projected): when wetter
dat |> 
  filter(PlotMix_Climate != "None",
         Region == "Colorado Plateau",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 1952) * 100)


# Empty seeded plots (Current + Projected): when drier
dat |> 
  filter(PlotMix_Climate != "None",
         Region == "Colorado Plateau",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 1152) * 100)


### Construct table -------------------------------------------------------

# Native volunteers
naz.total.nativevolun <- dat |> 
  filter(Code %in% c("CHAL11", "SOEL", "LEPA6"),
         Region == "Colorado Plateau") |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 3492),
         Plant = "Native recruit",
         Plot = "Total")
naz.wet.nativevolun <- dat |> 
  filter(Code %in% c("CHAL11", "SOEL", "LEPA6"),
         Region == "Colorado Plateau",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 2196),
         Plant = "Native recruit",
         Plot = "Wetter")
naz.dry.nativevolun <- dat |> 
  filter(Code %in% c("CHAL11", "SOEL", "LEPA6"),
         Region == "Colorado Plateau",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 1296),
         Plant = "Native recruit",
         Plot = "Drier")
add0 <- data.frame(Code = "LEPA6", n = 0, Frequency = 0,
                   Plant = "Native recruit", Plot = "Drier")
naz.dry.nativevolun <- bind_rows(naz.dry.nativevolun, add0)

# Weedy
naz.total.weedy <- dat |> 
  filter(Code %in% c("SATR12", "HAGL", "BRRU2", "ERCI6", "BRNI"),
         Region == "Colorado Plateau") |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 3492),
         Plant = "Invasive",
         Plot = "Total")
naz.wet.weedy <- dat |> 
  filter(Code %in% c("SATR12", "HAGL", "BRRU2", "ERCI6", "BRNI"),
         Region == "Colorado Plateau",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 2196),
         Plant = "Invasive",
         Plot = "Wetter")
naz.dry.weedy <- dat |> 
  filter(Code %in% c("SATR12", "HAGL", "BRRU2", "ERCI6", "BRNI"),
         Region == "Colorado Plateau",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 1296),
         Plant = "Invasive",
         Plot = "Drier")
add0 <- data.frame(Code = "BRNI", n = 0, Frequency = 0,
                   Plant = "Invasive", Plot = "Drier")
naz.dry.weedy <- bind_rows(naz.dry.weedy, add0)

# Empty plots (out of all plots)
naz.total.empty <- dat |> 
  filter(Code == "0",
         Region == "Colorado Plateau") |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 3492),
         Plant = "Empty",
         Plot = "Total")
naz.wet.empty <- dat |> 
  filter(Code == "0",
         Region == "Colorado Plateau",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 2196),
         Plant = "Empty",
         Plot = "Wetter")
naz.dry.empty <- dat |> 
  filter(Code == "0",
         Region == "Colorado Plateau",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 1296),
         Plant = "Empty",
         Plot = "Drier")

# Seeded, Current
naz.seedc.interest <- bind_rows(naz.total.seedc, naz.wet.seedc, naz.dry.seedc) |> 
  mutate(Frequency = perc_freq / 100) |> 
  mutate(Plant = paste(mix, "mix"),
         Plot = c(rep("Total", nrow(naz.total.seedc)), rep("Wetter", nrow(naz.wet.seedc)),
                  rep("Drier", nrow(naz.dry.seedc)))) |> 
  select(-mix, -perc_freq, -Sites) |> 
  filter(Code %in% c("LECI4", "HECO26", "LILE3", "PASM", "DACA7"))

# Seeded, Projected
naz.seedp.interest <- bind_rows(naz.total.seedp, naz.wet.seedp, naz.dry.seedp) |> 
  mutate(Frequency = perc_freq / 100) |> 
  mutate(Plant = paste(mix, "mix"),
         Plot = c(rep("Total", nrow(naz.total.seedp)), rep("Wetter", nrow(naz.wet.seedp)),
                  rep("Drier", nrow(naz.dry.seedp)))) |> 
  select(-mix, -perc_freq, -Sites) |> 
  filter(Code %in% c("BAMU", "SECO10", "ASTU")) 
add0 <- data.frame(Code = "ASTU", n = 0, Frequency = 0,
                   Plant = "Projected mix", Plot = "Drier")
naz.seedp.interest <- bind_rows(naz.seedp.interest, add0)
  

# Combine all
naz.interest <- bind_rows(naz.total.nativevolun, naz.wet.nativevolun, naz.dry.nativevolun,
                          naz.total.weedy, naz.wet.weedy, naz.dry.weedy, naz.seedc.interest,
                          naz.seedp.interest, naz.total.empty, naz.wet.empty, naz.dry.empty) |> 
  mutate(Type = paste0(Plant, ", ", Plot))
naz.interest$Code[naz.interest$Code == "0"] <- "Empty"  

# Write to csv
write_csv(naz.interest,
          file = "data/cleaned/09.1_Northern-AZ-Plateau_frequency_species-of-interest.csv")




## Precip range -----------------------------------------------------------

# Native volunteer
#   CHAL11, BOSI2, SOEL, BOGR2
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region == "Colorado Plateau") |> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum)) |> 
  mutate(range = max - min) |> 
  arrange(desc(range)) |> 
  print(n = 30)

dat |> 
  filter(Code %in% c("CHAL11", "SOEL", "LEPA6", "0"),
         Region == "Colorado Plateau") |> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum))

# Weedy
#   SATR12, ERCI6, TRTE, BRNI (but only when wetter), HAGL
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum)) |> 
  mutate(range = max - min) |> 
  arrange(desc(range)) |> 
  print(n = 30)
dat |> 
  filter(Code %in% c("SATR12", "ERCI6", "HAGL", "BRRU2", "BRNI"),
         Region == "Colorado Plateau") |> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum))

# Current mix
#   LECI4, HEBO, HECO26, POSE, KRLA2
dat |> 
  filter(SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current",
         Region == "Colorado Plateau") |> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum)) |> 
  mutate(range = max - min) |> 
  arrange(desc(range)) |> 
  print(n = 30)
dat |> 
  filter(Code %in% c("PASM", "LECI4", "HECO26", "LILE3", "DACA7"),
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum))

# Projected mix
#   SECO10, ACHY, BAMU, ASTU, BOGR2
dat |> 
  filter(SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected",
         Region == "Colorado Plateau") |> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum)) |> 
  mutate(range = max - min) |> 
  arrange(desc(range)) |> 
  print(n = 28)
dat |> 
  filter(Code %in% c("BAMU", "ASTU", "SECO10"),
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum))



## Count ------------------------------------------------------------------

### Native recruit --------------------------------------------------------

# Native volunteers: All conditions, high count per plot
#   LEPA6, TOIN, PLPA2, CHEN.BabbittPJ, MONU
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         SpeciesSeeded == "No") |> 
  filter(Count > 25) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  print(n = 66)

# Native volunteers: +48% and wetter, count of all individuals
#   LEPA6, HESP.BabbittPJ, CHEN.BabbittPJ, CHALL11
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum > 0.48) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 44)

# Native volunteers: +48% and wetter, with at least 15 individuals in a plot
#   LEPA6
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum > 0.48) |> 
  filter(Count > 14) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 99)
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum > 0.48) |> 
  filter(Count > 14) |> 
  count(Code) |> 
  arrange(desc(n))

# Native volunteers: -50% and drier, count of all individuals
#   CHEN.BabbittPJ, PHNE3, DAPU7
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum < -0.5) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 21)

# Native volunteers: -50% and drier, with at least 3 individuals in a plot
#   CHEN.BabbittPJ, DAPU7, PHNE3
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Count > 2) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 62)
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Count > 2) |> 
  count(Code) |> 
  arrange(desc(n))

# CHAL11, SOEL, LEPA6 max count
dat |> 
  filter(Code %in% c("CHAL11", "SOEL", "LEPA6"),
         Region == "Colorado Plateau") |> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Count = max(Count)) |> 
  arrange(desc(max_Count)) |> 
  arrange(Code) |> 
  print(n = 40)



### Weedy -----------------------------------------------------------------

# Weedy: All conditions, high count per plot
#   UNFO1.FlyingM, SATR12, UNGR1.MOWE
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  filter(Count > 50) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Site) |> 
  print(n = 57)

# Weedy: +48% and wetter, count of all individuals
#   UNGR1.MOWE, BRNI, ERCI6, SATR12
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum > 0.48) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 35)

# Weedy: +48% and wetter, with at least 15 individuals in a plot
#   UNGR1.MOWE, BRNI
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum > 0.48) |> 
  filter(Count > 14) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 57)
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum > 0.48) |> 
  filter(Count > 14) |> 
  count(Code) |> 
  arrange(desc(n))

# Weedy: -50% and drier, count of all individuals
#   BRRU2, ERCI6, TRTE
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum < -0.5) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

# Weedy: -50% and drier, with at least 3 individuals in a plot
#   BRRU2
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Count > 2) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 32)
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Count > 2) |> 
  count(Code) |> 
  arrange(desc(n))


# SATR12, ERCI6, HAGL, BRRU2, BRNI, ERCI6 max count
dat |> 
  filter(Code %in% c("SATR12", "ERCI6", "HAGL", "BRRU2", "BRNI", "ERCI6"),
         Region == "Colorado Plateau") |> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Count = max(Count)) |> 
  arrange(desc(max_Count)) |> 
  arrange(Code) |> 
  print(n = 82)



### Seeded ----------------------------------------------------------------

# Current: All conditions, high count per plot
#   ELEL5, BOGR2
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Count > 15) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 17)

# Current: +48% and wetter, with at least 4 individuals in a plot
#   LECI4
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum > 0.48,
         Count > 3) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 41)
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum > 0.48,
         Count > 3) |> 
  count(Code) |> 
  arrange(desc(n))

# Current: -50% and drier, with at least 2 individuals in a plot
#   LECI4, HECO26
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Count > 1) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum)
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Count > 1) |> 
  count(Code) |> 
  arrange(desc(n))


# Projected: All conditions, high count per plot
#   BAMU
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Count > 10) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum))

# Projected: +48% and wetter, with at least 4 individuals in a plot
#   BAMU, ASTU
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum > 0.48,
         Count > 3) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 27)
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum > 0.48,
         Count > 3) |> 
  count(Code) |> 
  arrange(desc(n))

# Projected: -50% and drier, with at least 2 individuals in a plot
#   PASM
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Count > 1) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum)
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Count > 1) |> 
  count(Code) |> 
  arrange(desc(n))


# LECI4, HECO26, PASM, LILE3, DACA7 max count
dat |> 
  filter(Code %in% c("LECI4", "HECO26", "PASM", "LILE3", "DACA7"),
         Region == "Colorado Plateau",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current") |> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Count = max(Count)) |> 
  arrange(desc(max_Count)) |> 
  arrange(Code) |> 
  print(n = 77)

# BAMU, ASTU, SECO10 max count
dat |> 
  filter(Code %in% c("BAMU", "ASTU", "SECO10"),
         Region == "Colorado Plateau",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected") |> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Count = max(Count)) |> 
  arrange(desc(max_Count)) |> 
  arrange(Code) |>
  print(n = 44)



## Height -----------------------------------------------------------------

### Native recruit --------------------------------------------------------

# Native volunteers: All conditions, tallest
#   HPOU, PLPA2
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region == "Colorado Plateau") |> 
  filter(Height > 300) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(Site) |> 
  print(n = 34)
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region == "Colorado Plateau") |> 
  filter(Height > 300) |> 
  count(Code) |> 
  arrange(desc(n))

# Native volunteers: +48% and wetter, 20 cm or taller
#   ELEL5, ELTR7, PASM
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum > 0.48) |> 
  filter(Height > 199) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 23)
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum > 0.48) |> 
  filter(Height > 199) |> 
  count(Code) |> 
  arrange(desc(n))

# Native volunteers: -50% and drier, 10 cm or taller
#   MEMO4, ELEL5, POSE, HECO26, PASM
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Height > 99) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum)

# CHAL11, SOEL, LEPA6 tallest height
dat |> 
  filter(Code %in% c("CHAL11", "SOEL", "LEPA6"),
         Region == "Colorado Plateau") |> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Height = max(Height)) |> 
  arrange(desc(max_Height)) |> 
  arrange(Code) |> 
  print(n = 40)



### Weedy -----------------------------------------------------------------

# Weedy: All conditions, tallest height
#   BRNI, AVFA
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  filter(Height > 300) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(Site) |> 
  print(n = 53)
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  filter(Height > 300) |> 
  count(Code) |> 
  arrange(desc(n))

# Weedy: +48% and wetter, 10 cm or taller
#   SATR12, BRNI
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum > 0.48) |> 
  filter(Height > 99) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum)) 

# Weedy: -50% and drier, 10 cm or taller
#   BRTE, BRRU2, HOMU
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Height > 99) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum)


# SATR12, HAGL, BRRU2, ERCI6, BRNI tallest heights
dat |> 
  filter(Code %in% c("SATR12", "HAGL", "BRRU2", "ERCI6", "BRNI"),
         Region == "Colorado Plateau") |> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Height = max(Height)) |> 
  arrange(desc(max_Height)) |> 
  arrange(Code) |> 
  print(n = 82)



### Seeded ----------------------------------------------------------------

# Current: All conditions, tallest height
#   ELTR7
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         SpeciesSeeded == "Yes") |> 
  filter(Height > 199) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum))
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         SpeciesSeeded == "Yes") |> 
  filter(Height > 199) |>
  count(Code) |> 
  arrange(desc(n))

# Current: +48% and wetter, 10 cm and taller
#   ELTR7, ELWA2
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         SpeciesSeeded == "Yes") |> 
  filter(Perc_dev_cum > 0.48) |> 
  filter(Height > 99) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum)) 

# Current: -50% and wetter, 5 cm and taller
#   LECI4, HECO26
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         SpeciesSeeded == "Yes") |>
  filter(Perc_dev_cum < -0.5) |> 
  filter(Height > 49) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 16)
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         SpeciesSeeded == "Yes") |>
  filter(Perc_dev_cum < -0.5) |> 
  filter(Height > 49) |> 
  count(Code) |> 
  arrange(desc(n))


# Projected: All conditions, tallest height
#   PASM, ELEL5, PLJA
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         SpeciesSeeded == "Yes") |> 
  filter(Height > 199) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 21)
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         SpeciesSeeded == "Yes") |> 
  filter(Height > 199) |>
  count(Code) |> 
  arrange(desc(n))

# Projected: +48% and wetter, 10 cm and taller
#   ELEL5, PASM
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         SpeciesSeeded == "Yes") |> 
  filter(Perc_dev_cum > 0.48) |> 
  filter(Height > 99) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum)) 
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         SpeciesSeeded == "Yes") |> 
  filter(Perc_dev_cum > 0.48) |> 
  filter(Height > 99) |> 
  count(Code) |> 
  arrange(desc(n))

# Projected: -50% and wetter, 5 cm and taller
#   PASM
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         SpeciesSeeded == "Yes") |>
  filter(Perc_dev_cum < -0.5) |> 
  filter(Height > 49) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) 
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         SpeciesSeeded == "Yes") |>
  filter(Perc_dev_cum < -0.5) |> 
  filter(Height > 49) |> 
  count(Code) |> 
  arrange(desc(n))


# LECI4, HECO26, PASM, LILE3, DACA7 tallest height
dat |> 
  filter(Code %in% c("LECI4", "HECO26", "PASM", "LILE3", "DACA7"),
         Region == "Colorado Plateau",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current") |> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Height = max(Height)) |> 
  arrange(desc(max_Height)) |> 
  arrange(Code) |> 
  print(n = 77)

# BAMU, ASTU, SECO10 tallest height
dat |> 
  filter(Code %in% c("BAMU", "ASTU", "SECO10"),
         Region == "Colorado Plateau",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected") |> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Height = max(Height)) |> 
  arrange(desc(max_Height)) |> 
  arrange(Code) |>
  print(n = 44)



# Unsuccessful seeded species ---------------------------------------------

# Sonoran Desert
# Current, none grew: MUPO2, SPCR (Central) | BOGR2, HEMU3, HENE5 (SE)
# Current, pretty unsuccessful: AMDE4, SPAM2 (Central)
# Projected, none grew: BOCU, CAER, ENFA (Central) | PLJA, PEPA8 (SE; BOCU was successful at SE)
# Projected, pretty unsuccessful: BOAR (Central)

# Northern AZ
# Current, none grew: PSSP6, SPGR2 (Cool)
# Current, pretty unsuccessful: BOER4, KRLA2, PLJA (Med-Warm) | HEMU3 (Cool-Med) | ELWA2 (Cool)
# Projected, none grew: PLMU3 (Warm) | HEMU3 (Cool-Med)
# Projected, pretty unsuccessful: ARPU9 (Warm) | MATA2, PEPA8, SPCR (Med-Warm) 


## Sonoran Desert ---------------------------------------------------------

# Sonoran Central
# Current mix (8): AMDE4, BAMU, DICA8, LUSP2, MUPO2, SACO6, SPAM2, SPCR
dat |> 
  filter(Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current") |> 
  group_by(Site, Code) |> 
  summarise(Presence = n())
#   missing from Pleasant: MUPO2, SPAM2, SPCR
#   missing from Preserve: AMDE4, SPCR
#   missing from Roosevelt: AMDE4, DICA8, SPCR
#   missing from SCC: DICA8, MUPO2, SPAM2, SPCR

# Sonoran Central, Current: appeared at no sites: MUPO2, SPCR
dat |> 
  filter(Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current",
         Code %in% c("MUPO2", "SPCR")) 

# Sonoran Central, Current: did not appear at all sites
#   Would say overall that SPAM2 and AMDE4 were not very successful
sonoran.total.seed |> 
  filter(Code %in% c("AMDE4", "DICA8", "SPAM2"))
dat |> 
  filter(Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current",
         Code %in% c("AMDE4", "DICA8", "SPAM2")) |> 
  group_by(Code) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 


# Sonoran Central, Projected mix (8): ARPU9, BOAR, BORO2, BOCU, CAER, ENFA, PLOV, SECO10
dat |> 
  filter(Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected") |> 
  group_by(Site, Code) |> 
  summarise(Presence = n())
#   missing from Pleasant: BOAR, BORO2, BOCU, CAER, ENFA
#   missing from Preserve: BOCU, CAER, ENFA
#   missing from Roosevelt: ARPU9, BOAR, BOCU, CAER, ENFA
#   missing from SCC: BOAR, BOCU, CAER, ENFA

# Sonoran Central, Projected: appeared at no sites: BOCU, CAER, ENFA
dat |> 
  filter(Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current",
         Code %in% c("BOCU", "CAER", "ENFA")) 

# Sonoran Central, Current: did not appear at all sites
#   Would say overall that BOAR was not very successful
sonoran.total.seed |> 
  filter(Code %in% c("ARPU9", "BOAR", "BORO2"))
dat |> 
  filter(Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected",
         Code %in% c("ARPU9", "BOAR", "BORO2")) |> 
  group_by(Code) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 


# Sonoran SE
# Current mix (7): BOGR2, ELEL5, HEMU3, HENE5, MATA2, POSE, SPCR
dat |> 
  filter(Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current") |> 
  group_by(Site, Code) |> 
  summarise(Presence = n())
#   missing from Patagonia: BOGR2, HEMU3, HENE5
#   missing from SRER: BOGR2, HEMU3, HENE5, POSE, SPCR

# Sonoran SE, Current: appeared at no sites: BOGR2, HEMU3, HENE5
dat |> 
  filter(Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current",
         Code %in% c("BOGR2", "HEMU3", "HENE5")) 

# Sonoran SE, Current: did not appear at all sites
sonoran.total.seed |> 
  filter(Code %in% c("POSE", "SPCR"))
dat |> 
  filter(Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current",
         Code %in% c("POSE", "SPCR")) |> 
  group_by(Code) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 


# Sonoran SE, Projected mix (7): ARPU9, ASTU, BAMU, BOCU, PLJA, PEPA8, SECO10
dat |> 
  filter(Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected") |> 
  group_by(Site, Code) |> 
  summarise(Presence = n())
#   missing from Patagonia: PLJA, PEPA8
#   missing from SRER: BAMU, PLJA, PEPA8

# Sonoran SE, Projected: appeared at no sites: PLJA, PEPA8
dat |> 
  filter(Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current",
         Code %in% c("PLJA", "PEPA8")) 

# Sonoran SE, Current: did not appear at all sites
sonoran.total.seed |> 
  filter(Code == "BAMU")
dat |> 
  filter(Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected",
         Code == "BAMU") |> 
  group_by(Code) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 



## Northern Arizona Plateau -----------------------------------------------

# Warm mix, Projected (7): ACHY, ARPU9, ASTU, BAMU, BOCU, PLMU3, SECO10
dat |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "TLE"),
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected") |> 
  group_by(Site, Code) |> 
  summarise(Presence = n()) |> 
  print(n = 25)
#   missing from AguaFria: ARPU9, BOCU, PLMU3
#   missing from MOWE: BOCU, PLMU3
#   missing from PEFO: ASTU, PLMU3, SECO10
#   missing from Spiderweb: ARPU9, PLMU3
#   missing from TLE: ARPU9, ASTU, BAMU, BOCU, PLMU3, SECO10

# Warm mix, Projected: appeared at no sites: PLMU3
dat |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "TLE"),
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected",
         Code == "PLMU3") 

# Warm mix, Projected: did not appear at all sites
#   Would say that ARPU9 did not do that well
naz.total.seedp |> 
  filter(Code %in% c("ARPU9", "ASTU", "BAMU", "BOCU", "SECO10"))
dat |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "TLE"),
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected",
         Code %in% c("ARPU9", "ASTU", "BAMU", "BOCU", "SECO10")) |> 
  group_by(Code) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 


# Med-Warm mix, Current (7): BOER4, KRLA2, MATA2, PEPA8, PLJA, POSE, SPCR
dat |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb"),
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current") |> 
  group_by(Site, Code) |> 
  summarise(Presence = n()) |> 
  print(n = 19)
#   missing from AguaFria: BOER4, KRLA2, MATA2, PEPA8, PLJA, POSE, SPCR
#   missing from MOWE: BOER4, POSE
#   missing from PEFO: PEPA8, PLJA
#   missing from Spiderweb: KRLA2, POSE, SPCR

# Med-Warm mix, Current: appeared at no sites: none

# Med-Warm mix, Current: did not appear at MOWE, PEFO, and Spiderweb (AguaFria just did not do well)
#   Would say that BOER4, KRLA2, PLJA did not do that well
naz.total.seedc |> 
  filter(Code %in% c("BOER4", "KRLA2", "PEPA8", "PLJA", "POSE", "SPCR"))
dat |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb"),
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current",
         Code %in% c("BOER4", "KRLA2", "PEPA8", "PLJA", "POSE", "SPCR")) |> 
  group_by(Code) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

# Med-Warm mix, Projected (7): BOER4, KRLA2, MATA2, PEPA8, PLJA, POSE, SPCR
dat |> 
  filter(Site %in% c("BarTBar", "FlyingM"),
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected") |> 
  group_by(Site, Code) |> 
  summarise(Presence = n())
#   missing from BarTBar: SPCR
#   missing from FlyingM: MATA2, PEPA8

# Med-Warm mix, Projected: appeared at no sites: none

# Med-Warm mix, Projected: did not appear at all sites
#   Would say that MATA2, PEPA8, SPCR did not do that well
naz.total.seedp |> 
  filter(Code %in% c("MATA2", "PEPA8", "SPCR"))
dat |> 
  filter(Site %in% c("BarTBar", "FlyingM"),
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected",
         Code %in% c("MATA2", "PEPA8", "SPCR")) |> 
  group_by(Code) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 


# Cool-Med mix, Current (7): ACMI2, BOGR2, DACA7, ELEL5, HEMU3, LILE3, PASM
dat |> 
  filter(Site %in% c("BarTBar", "FlyingM"),
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current") |> 
  group_by(Site, Code) |> 
  summarise(Presence = n())
#   missing from BarTBar: HEMU3
#   missing from FlyingM: none

# Cool-Med mix, Current: appeared at no sites: none

# Cool-Med mix, Current: did not appear at all sites
#   Would say that HEMU3 did terribly
naz.total.seedc |> 
  filter(Code == "HEMU3")
dat |> 
  filter(Site %in% c("BarTBar", "FlyingM"),
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current",
         Code == "HEMU3") |> 
  group_by(Code) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

# Cool-Med mix, Projected (7): ACMI2, BOGR2, DACA7, ELEL5, HEMU3, LILE3, PASM
dat |> 
  filter(Site == "BabbittPJ",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected") |> 
  group_by(Site, Code) |> 
  summarise(Presence = n())
#   missing from BabbittPJ: HEMU3

# Cool-Med mix, Projected: appeared at no sites: HEMU3
dat |> 
  filter(Site == "BabbittPJ",
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected",
         Code == "HEMU3") 


# Cool mix, Current (7): ELTR7, ELWA2, HEBO, HECO26, LECI4, PSSP6, SPGR2
dat |> 
  filter(Site %in% c("BabbittPJ", "TLE"),
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current") |> 
  group_by(Site, Code) |> 
  summarise(Presence = n())
#   missing from BabbittPJ: PSSP6, SPGR2
#   missing from TLE: ELTR7, ELWA2, HEBO, HECO26, LECI4, PSSP6, SPGR2

# Cool mix, Current: appeared at no sites: PSSP6, SPGR2
dat |> 
  filter(Site %in% c("BabbittPJ", "TLE"),
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Projected",
         Code %in% c("PSSP6, SPGR2")) 

# Cool mix, Current: did not appear at all sites
#   Would say that ELWA2 did not do that well
naz.total.seedc |> 
  filter(Code %in% c("ELTR7", "ELWA2", "HEBO", "HECO26", "LECI4", "PSSP6", "SPGR2"))
dat |> 
  filter(Site %in% c("BabbittPJ", "TLE"),
         SpeciesSeeded == "Yes",
         PlotMix_Climate == "Current",
         Code %in% c("ELTR7", "ELWA2", "HEBO", "HECO26", "LECI4", "PSSP6", "SPGR2")) |> 
  group_by(Code) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

save.image("RData/09.1_identify-species-of-interest.RData")

