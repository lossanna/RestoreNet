# Created: 2024-09-25
# Last updated: 2024-09-25

# Purpose: Calculate frequency for all species. 

# First find number of plots needed for denominator of various groups.
# Calculate frequency considering entire plot (subplot + additional 2x2 species),
#   identify most frequent, and format to make a table for bar graphs.
# Calculate frequencies for total (all plots/conditions), wetter conditions, extremely
#   wet conditions (+24% and wetter), drier conditions, and extremely dry conditions (-23% and drier).

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

### Native recruit & Weedy ------------------------------------------------

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

### Native recruit & Weedy ------------------------------------------------

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



# Sonoran Desert: Frequency -----------------------------------------------

## Native recruit ---------------------------------------------------------

# Native volunteers (all plots): Highest species frequency overall
#   LOHU2, LOAR12, CHPO12, PERE, PLAR, VUOC, LAGR10
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
  mutate(perc_freq = (n / 360) * 100) |> 
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
  mutate(perc_freq = (n / 792) * 100) |> 
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
#   SCBA, ERCI6, MAPA5, SIIR
sonoran.wettest.weedy <- present_species |> 
  filter(Weedy == "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum > 0.24) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 360) * 100) |> 
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
  mutate(perc_freq = (n / 792) * 100) |> 
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
  mutate(perc_freq = (n / 96) * 100) |> 
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
  mutate(perc_freq = (n / 64) * 100) |> 
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
  mutate(perc_freq = (n / 160) * 100) |> 
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
  mutate(perc_freq = (n / 160) * 100,
         mix = "Current") 
sonoran.wettest.seed4c

# Extremely wet: Seeded version 4, Projected
sonoran.wettest.seed4p <- present_species |> 
  filter(Code == "BAMU",
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0.24) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 160) * 100,
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
  mutate(perc_freq = (n / 160) * 100,
         mix = "Current") 
sonoran.dry.seed4c

# Drier: Seeded version 4, Projected
sonoran.dry.seed4p <- present_species |> 
  filter(Code == "BAMU",
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 160) * 100,
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
  mutate(perc_freq = (n / 224) * 100) |> 
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
  mutate(perc_freq = (n / 128) * 100) |> 
  arrange(desc(n)) # none

# Extremely dry: Seeded version 3
sonoran.driest.seed3 <- present_species |> 
  filter(Code %in% c("SPCR", "ARPU9", "BOCU", "SECO10"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "Yes",
         Perc_dev_cum < -0.23) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 352) * 100) |> 
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
  mutate(perc_freq = (n / 160) * 100,
         mix = "Current") 
sonoran.driest.seed4c

# Extremely dry: Seeded version 4, Projected
present_species |> 
  filter(Code == "BAMU",
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         Perc_dev_cum < -0.23) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 160) * 100,
         mix = "Projected") # nothing grew


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

# Extremely wet (+24% and more): combined
#   Current: LUSP2, SACO6
#   Projected: PLOV, ARPU9
sonoran.wettest.seed <- bind_rows(sonoran.wettest.seed1, sonoran.wettest.seed2, sonoran.wettest.seed3,
                                  sonoran.wettest.seed4c, sonoran.wettest.seed4p) |> 
  arrange(desc(perc_freq))
sonoran.wettest.seed

# Drier: combined
#   Current: SACO6, LUSP2
#   Projected: PLOV, SECO10
sonoran.dry.seed <- bind_rows(sonoran.dry.seed1, sonoran.dry.seed2, sonoran.dry.seed3,
                              sonoran.dry.seed4c) |> 
  arrange(desc(perc_freq))
sonoran.dry.seed

# Extremely dry (-23% and less): combined
#   Current: SACO6, BAMU, LUSP2
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
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 1152),
         Plant = "Empty",
         Plot = "Total")
sonoran.wet.empty <- present_species |> 
  filter(Code == "0",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 360),
         Plant = "Empty",
         Plot = "Wetter")
sonoran.wettest.empty <- present_species |> 
  filter(Code == "0",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum > 0.24) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 360),
         Plant = "Empty",
         Plot = "Extremely wet")
sonoran.dry.empty <- present_species |> 
  filter(Code == "0",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 792),
         Plant = "Empty",
         Plot = "Drier")
sonoran.driest.empty <- present_species |> 
  filter(Code == "0",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         Perc_dev_cum < -0.23) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(Frequency = (n / 792),
         Plant = "Empty",
         Plot = "Extremely dry")


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



## Construct table --------------------------------------------------------

# Native recruit
sonoran.nativevolun <- bind_rows(sonoran.total.nativevolun, sonoran.wet.nativevolun, sonoran.dry.nativevolun,
                                 sonoran.wettest.nativevolun, sonoran.driest.nativevolun) |> 
  mutate(Frequency = perc_freq / 100) |> 
  mutate(Plant = "Native recruit",
         Plot = c(rep("Total", nrow(sonoran.total.nativevolun)), rep("Wetter", nrow(sonoran.wet.nativevolun)),
                  rep("Drier", nrow(sonoran.dry.nativevolun)), rep("Extremely wet", nrow(sonoran.wettest.nativevolun)),
                  rep("Extremely dry", nrow(sonoran.driest.nativevolun)))) 

# Weedy
sonoran.weedy <- bind_rows(sonoran.total.weedy, sonoran.wet.weedy, sonoran.dry.weedy,
                           sonoran.wettest.weedy, sonoran.driest.weedy) |> 
  mutate(Frequency = perc_freq / 100) |> 
  mutate(Plant = "Weed",
         Plot = c(rep("Total", nrow(sonoran.total.weedy)), rep("Wetter", nrow(sonoran.wet.weedy)),
                  rep("Drier", nrow(sonoran.dry.weedy)), rep("Extremely wet", nrow(sonoran.wettest.weedy)),
                  rep("Extremely dry", nrow(sonoran.driest.weedy)))) 

# Seeded
sonoran.seed <- bind_rows(sonoran.total.seed, sonoran.wet.seed, sonoran.dry.seed,
                          sonoran.wettest.seed, sonoran.driest.seed) |> 
  mutate(Frequency = perc_freq / 100) |> 
  mutate(Plant = paste(mix, "mix"),
         Plot = c(rep("Total", nrow(sonoran.total.seed)), rep("Wetter", nrow(sonoran.wet.seed)),
                  rep("Drier", nrow(sonoran.dry.seed)), rep("Extremely wet", nrow(sonoran.wettest.seed)),
                  rep("Extremely dry", nrow(sonoran.driest.seed)))) |> 
  select(-mix)

# Combine all
sonoran.freq <- bind_rows(sonoran.nativevolun, sonoran.weedy, sonoran.seed,
                              sonoran.total.empty, sonoran.wet.empty, sonoran.dry.empty,
                              sonoran.wettest.empty, sonoran.driest.empty) |> 
  mutate(Type = paste0(Plant, ", ", Plot))
sonoran.freq$Code[sonoran.freq$Code == "0"] <- "Empty"  

# Write to csv
write_csv(sonoran.freq,
          file = "data/cleaned/11.1_Sonoran-Desert_frequency-by-species.csv")


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
