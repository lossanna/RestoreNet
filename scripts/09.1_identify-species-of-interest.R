# Created: 2024-09-17
# Last updated: 2024-09-17

# Purpose: Identify what species are doing well based on Count in Sonoran Desert and
#   Northern Arizona Plateau. Calculate total number of plots for various groups 
#   to determine percent frequency in which the species shows up in a plot.
#   Investigate species of interest (seeded, native volunteer, and weedy
#   that are doing well in variable precip by Count and plot frequency). Use graphs from
#   09.1_drafit-figs_precip-dev_subplot.R to determine precip extremes, tall heights, and high
#   count values, as well as graphs of individual species to visualize performance.

# Sonoran Desert species of interest:
#   Current mix, most abundant (all conditions) and did well under var precip: SACO6, LUSP2
#   Projected mix, did well under var precip: PLOV, SECO10
#   Projected mix, most abundant: ARPU9, PLOV
#   Native volunteers, most abundant and did well under var precip: VUOC, LOAR12, CHPO12
#   Weedy species, most abundant and did well under var precip: SCBA, BRRU2, ERCI6

# Northern Arizona Plateau species of interest:
#   Current mix, did well under var precip: LECI4, HECO26
#   Projected mix, did well under var precip: BAMU
#   Seeded (both mixes), most abundant: PASM, LILE3, DACA7
#   Native volunteers, most abundant and did well under var precip: CHAL11
#   Native volunteers, did well under var precip: SOEL
#   Weedy species, most abundant and did well under var precip: SATR12, BRRU2
#   Weedy species, did well under var precip: HAGL


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
#   Inf created when there was no rain in the time period (can't divide by 0),
#     but this only occurs when the time period is small and in all cases there is another
#     monitoring date less than 2 weeks away with a non-Inf percent change. Occurs once
#     at CO Plateau (BarTBar).
dat <- dat |> 
  filter(Perc_dev_cum != Inf)

# Reorder PlotMix_Climate
dat$PlotMix_Climate <- factor(dat$PlotMix_Climate,
                              levels = c("None", "Current", "Projected"))



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
#   1: Seeded only at Sonoran Central in just one mix
#   2: Seeded at only Sonoran SE in just one mix
#   3: Seeded at both regions in the same mix type: SPCR, ARPU9, BOCU, SECO10
#   4: Seeded at both regions in opposite mix types: BAMU

# 1: Sonoran Central only, total number of plots for species in just 1 mix: 720
dat |> 
  filter(Region == "Sonoran Central") |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()
36 * (5 + 4 + 6 + 5)

# 1: Sonoran Central only, when wetter: 216
dat |> 
  filter(Region == "Sonoran Central", 
         Perc_dev_cum > 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 1: Sonoran Central only, when drier: 504
dat |> 
  filter(Region == "Sonoran Central", 
         Perc_dev_cum < 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 2: Sonoran SE only, total number of plots for species in just 1 mix: 432
dat |> 
  filter(Region == "Sonoran SE") |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()
36 * (6 + 6)

# 2: Sonoran SE only, when wetter: 144
dat |> 
  filter(Region == "Sonoran SE",
         Perc_dev_cum > 0) |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# 2: Sonoran SE only, when drier: 288
dat |> 
  filter(Region == "Sonoran SE",
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




## Northern Arizona Plateau -----------------------------------------------

# Precip dev
#   Ranges from -99% to 193%
summary(filter(dat, Region == "Colorado Plateau")$Perc_dev_cum)
hist(filter(dat, Region == "Colorado Plateau")$Perc_dev_cum)
count(filter(dat, Region == "Colorado Plateau"), Perc_dev_cum) |> 
  print(n = 97)

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

# Number of seeded plots, Current or Projected: 1553
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  select(Region, Site, Date_Monitored, Plot) |> 
  distinct(.keep_all = TRUE) |> 
  nrow()

# Number of seeded wetter plots, Current or Projected: 977
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  select(Region, Site, Date_Monitored, Plot, Perc_dev_cum) |> 
  distinct(.keep_all = TRUE) |> 
  filter(Perc_dev_cum > 0) |> 
  nrow()

# Number of seeded plots, Current or Projected: 576
dat |> 
  filter(Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
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
#   SACO6, LUSP2, PLOV (8-10%)
sonoran.total.seed1 <- dat |> 
  filter(Code %in% c("AMDE4", "DICA8", "LUSP2", "MUPO2", "SACO6", "SPAM2", 
                     "BOAR", "BORO2", "CAER", "ENFA", "PLOV"),
         Region == "Sonoran Central",
         SpeciesSeeded == "Yes") |> 
  count(Code) |> 
  mutate(perc_freq = (n / 720) * 100) |> 
  arrange(desc(n))
sonoran.total.seed1

# Total: Seeded version 2
#   ASTU, MATA2, ELEL5 (1-2%)
sonoran.total.seed2 <- dat |> 
  filter(Code %in% c("BOGR2", "ELEL5", "HEMU3", "HENE5", "MATA2", "POSE", 
                     "ASTU", "PLJA", "PEPA8"),
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes") |> 
  count(Code) |> 
  mutate(perc_freq = (n / 432) * 100) |> 
  arrange(desc(n))
sonoran.total.seed2

# Total: Seeded version 3-4
#   ARPU9, SECO10 (7-12%)
sonoran.total.seed34 <- dat |> 
  filter(Code %in% c("SPCR", "ARPU9", "BOCU", "SECO10", "BAMU"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "Yes") |> 
  count(Code) |> 
  mutate(perc_freq = (n / 512) * 100) |> 
  arrange(desc(n))
sonoran.total.seed34

# Wetter: Seeded version 1
#   PLOV, LUSP2, SACO6 (18-20%)
sonoran.wet.seed1 <- dat |> 
  filter(Code %in% c("AMDE4", "DICA8", "LUSP2", "MUPO2", "SACO6", "SPAM2", 
                     "BOAR", "BORO2", "CAER", "ENFA", "PLOV"),
         Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 216) * 100) |> 
  arrange(desc(n))
sonoran.wet.seed1

# Wetter: Seeded version 2
#   ELEL5, MATA2, POSE (2-4%)
sonoran.wet.seed2 <- dat |> 
  filter(Code %in% c("BOGR2", "ELEL5", "HEMU3", "HENE5", "MATA2", "POSE", 
                     "ASTU", "PLJA", "PEPA8"),
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 144) * 100) |> 
  arrange(desc(n))
sonoran.wet.seed2

# Wetter: Seeded version 3-4
#   ARPU9, BAMU (12-27%)
sonoran.wet.seed34 <- dat |> 
  filter(Code %in% c("SPCR", "ARPU9", "BOCU", "SECO10", "BAMU"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 160) * 100) |> 
  arrange(desc(n))
sonoran.wet.seed34

# Drier: Seeded version 1
#   PLOV, LUSP2, SACO6 (7-9%)
sonoran.dry.seed1 <- dat |> 
  filter(Code %in% c("AMDE4", "DICA8", "LUSP2", "MUPO2", "SACO6", "SPAM2", 
                     "BOAR", "BORO2", "CAER", "ENFA", "PLOV"),
         Region == "Sonoran Central",
         SpeciesSeeded == "Yes",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 504) * 100) |> 
  arrange(desc(n))
sonoran.dry.seed1

# Drier: Seeded version 2
#   ASTU, MATA2, POSE (1-2%)
sonoran.dry.seed2 <- dat |> 
  filter(Code %in% c("BOGR2", "ELEL5", "HEMU3", "HENE5", "MATA2", "POSE", 
                     "ASTU", "PLJA", "PEPA8"),
         Region == "Sonoran SE",
         SpeciesSeeded == "Yes",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 288) * 100) |> 
  arrange(desc(n))
sonoran.dry.seed2

# Drier: Seeded version 3-4
#   ARPU9, BAMU (12-27%)
sonoran.dry.seed34 <- dat |> 
  filter(Code %in% c("SPCR", "ARPU9", "BOCU", "SECO10", "BAMU"),
         Region %in% c("Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "Yes",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 352) * 100) |> 
  arrange(desc(n))
sonoran.dry.seed34


# Total: combined
#   Current: SACO6, LUSP2
#   Projected: ARPU9, PLOV
sonoran.total.seed <- bind_rows(sonoran.total.seed1, sonoran.total.seed2, sonoran.total.seed34) |> 
  arrange(desc(perc_freq))
sonoran.total.seed

# Wetter: combined
#   Current: LUSP2, SACO6
#   Projected: ARPU9, PLOV
sonoran.wet.seed <- bind_rows(sonoran.wet.seed1, sonoran.wet.seed2, sonoran.wet.seed34) |> 
  arrange(desc(perc_freq))
sonoran.wet.seed

# Drier: combined
#   Current: LUSP2, SACO6
#   Projected: PLOV, SECO10
sonoran.dry.seed <- bind_rows(sonoran.dry.seed1, sonoran.dry.seed2, sonoran.dry.seed34) |> 
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


## Northern Arizona Plateau -----------------------------------------------

# Desirable
# All plots: Highest species frequency overall
#   BAMU, CHAL11, SCMU6, LILE3, PASM, LEPA6, DACA7
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau") |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  print(n = 25)

# All plots: Highest species frequency (showed up in most plots and sites) when wetter
#   BAMU, CHAL11, SCMU6, LILE3, LEPA6, DACA7, PASM
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  print(n = 25)

# All plots: Highest species frequency (showed up in most plots and sites) when drier
#   MONU, GUSA2, PASM, SPCR, HECI, PHNE3, DAPU7
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  print(n = 40)

# Native volunteer
# All plots: Highest species frequency overall
#   CHAL11, SCMU6, LEPA6, ATCO, SAAB, SOEL
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         SpeciesSeeded == "No") |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 3492) * 100) |> 
  print(n = 25)

# All plots: when wetter
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

# All plots: when drier
#   MONU, GUSA2, HECI, PHNE3
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         SpeciesSeeded == "No",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 1296) * 100) |> 
  print(n = 40)


# Weedy
# All plots: Highest species frequency overall
#   SATR12, BRNI, ERCI6, BRRU2, HAGL
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 3492) * 100) |> 
  print(n = 20)

# All plots: Highest species frequency (showed up in most plots and sites) when wetter
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

# All plots: Highest species frequency (showed up in most plots and sites) when drier
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


# Seeded
# All plots: Highest species frequency across all monitoring times and plots
#   BAMU (only in Projected), PASM, LILE3, DACA7
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau") |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 3106) * 100) |> 
  print(n = 38)

# All plots: when wetter
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  print(n = 36)

# All plots: when drier
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  print(n = 23)


# Current, all seeded plots
#   PASM, LILE3, DACA7
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  count(Code) |> 
  mutate(perc_freq = (n / 1553) * 100) |> 
  arrange(desc(n)) |> 
  print(n = 20)

# Current: when wetter
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 977) * 100) |> 
  arrange(desc(n)) |> 
  print(n = 20)

# Current: when drier
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 576) * 100) |> 
  arrange(desc(n))

# Projected
#   BAMU, ASTU, SECO10, BOER4
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 1553) * 100) |> 
  print(n = 28)

# Projected: when wetter
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n)) |> 
  mutate(perc_freq = (n / 977) * 100) |> 
  print(n = 27)

# Projected: when drier
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  mutate(perc_freq = (n / 576) * 100) |> 
  arrange(desc(n))


# Empty seeded plots (Current + Projected): total
dat |> 
  filter(PlotMix_Climate != "None",
         Region == "Colorado Plateau") |> 
  count(Code) |> 
  arrange(desc(n))

# Empty seeded plots (Current + Projected): when wetter
dat |> 
  filter(PlotMix_Climate != "None",
         Region == "Colorado Plateau",
         Perc_dev_cum > 0) |> 
  count(Code) |> 
  arrange(desc(n))


# Empty seeded plots (Current + Projected): when drier
dat |> 
  filter(PlotMix_Climate != "None",
         Region == "Colorado Plateau",
         Perc_dev_cum < 0) |> 
  count(Code) |> 
  arrange(desc(n))



# Identify outliers (Count) -----------------------------------------------

## Sonoran Desert: Desirable & Seeded -------------------------------------

# All plots: High count recruit
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Count > 50) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  print(n = 24)

# All plots: Highest wet deviation
#   ARPU9, BAMU, PLOV, PSCA11
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum > 0.35) |> 
  filter(Count > 2) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 47)

# All plots: Highest dry deviation
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum < -0.35) |> 
  filter(Count > 2) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum)

# All plots: High dry deviation (-23% or drier)
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Count > 4) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 100)


# All plots: most frequent in observations across all sites and plots (total)
#   CHPO12, VUOC, LOAR12
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Weedy != "Weedy") |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 30)


# All plots, volunteers: High wet deviation, total count
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum > 0.25) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 28)
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum > 0.25) |> 
  filter(Count > 2) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 77)

# All plots, volunteers: High dry deviation 
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum < -0.23) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 51)
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Count > 3) |> 
  filter(!str_detect(Code, ".SRER")) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 77)

# LOAR12, VUOC, CHPO12 conditions
dat |> 
  filter(Code %in% c("LOAR12", "VUOC", "CHPO12"),
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum))
dat |> 
  filter(Code %in% c("LOAR12", "VUOC", "CHPO12"),
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Count = max(Count)) |> 
  arrange(desc(max_Count)) |> 
  arrange(Code) |> 
  print(n = 28)


# Desirable, by PlotMix_Climate
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
#   SACO6, LUSP2, ELEL5, DICA8
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum > 0.35) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 48)

# Current: Seeded highest dry deviation
#   SACO6, LUSP2
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.35) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum)

# Current: Seeded high dry deviation (-23% and drier)
#   SACO6, LUSP2 (Sonoran SE Current seed mix not very successful in dry conditions)
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Count > 1) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 24)

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
#     ARPU9, BAMU, PLOV
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum > 0.35) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 42)

# Projected: Seeded highest dry deviation
#     PLOV
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.35) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Projected: Seeded high dry deviation (-23% and drier)
#   PLOV (by far most successful), SECO10, BORO2
#     Sonoran SE Projected mix did not seem to do that well, either
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.23) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 29)

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
#   ARPU9, PLOV, SECO10
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

# SACO6, LUSP2, PLOV, SECO10, ARPU9 conditions
dat |> 
  filter(Code %in% c("SACO6", "LUSP2", "PLOV", "SECO10", "ARPU9", "0"),
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum))
dat |> 
  filter(Code %in% c("SACO6", "LUSP2", "PLOV", "SECO10", "ARPU9"),
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Count = max(Count)) |> 
  arrange(desc(max_Count)) |> 
  arrange(Code) |> 
  print(n = 56)


## Sonoran Desert: Weedy --------------------------------------------------

# All plots: High count recruit
#   SCBA, BRRU2, ERCI6
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Count > 50) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Site) |> 
  print(n = 29)

# All plots: Highest wet deviation
#   MAPA5
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum > 0.35) |> 
  filter(Count > 5) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Site) |> 
  print(n = 29)

# All plots: High wet deviation (+25% and wetter)
#   BRRU2, SCBA, ERCI6, MAPA5
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum > 0.25) |> 
  filter(Count > 10) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 31)

# All plots: Highest dry deviation
#   ERCI6, SCBA, BRRU2
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum < -0.35) |> 
  filter(Count > 50) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Site) 

# All plots: High dry deviation (-23% and drier)
#   It's just endless SCBA, BRRU2, ERCI6
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Count > 50) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 25)

# All plots: most frequent across all sites and plots
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

# SCBA, BRRU2, ERCI6 conditions
dat |> 
  filter(Code %in% c("SCBA", "BRRU2", "ERCI6"),
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum))
dat |> 
  filter(Code %in% c("SCBA", "BRRU2", "ERCI6"),
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Count = max(Count)) |> 
  arrange(desc(max_Count)) |> 
  arrange(Code) |> 
  print(n = 34)


## Northern Arizona Plateau: Desirable & Seeded ---------------------------

# All plots: High count recruit
#   LEPA6, PLPA2
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau") |> 
  filter(Count > 25) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Site) |> 
  print(n = 68)

# All plots: High wet deviation
#   CHAL11, LEPA6, BAMU
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau")|> 
  filter(Perc_dev_cum > 1) |> 
  filter(Count > 2) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 97)

# All plots: High dry deviation
#   DAPU7, PHNE3, PASM
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau")|> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Count > 2) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 75)

# All plots: most frequent in observations across all sites and plots (total)
#   LEPA6, SCMU6, PLPA2, CHAL11, MONU, SAAB
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region == "Colorado Plateau")|> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 40)

# All plots, volunteers: High wet deviation, total count
#   LEPA6, CHA11, SOEL
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region == "Colorado Plateau")|> 
  filter(Perc_dev_cum > 0.5) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 29)

# All plots, volunteers: High dry deviation, total count 
#   PHNE3, DAPU7, HECI, SOEL
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region == "Colorado Plateau")|> 
  filter(Perc_dev_cum < -0.5) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 21)


# By PlotMix_Climate
# Current: High count recruit
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Count > 25) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 26)

# Current: High count Seeded
#   ELEL5, BOGR2, PASM, DACA7
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Count > 10,
         PlantSource2 == "Seeded") |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  print(n = 32)

# Current: Total sum of seeded
#   DACA7, PASM, ELEL5, LILE3, BOGR2
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current")|> 
  group_by(Code, Name, Duration, Lifeform) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 30)

# Current: Seeded high wet deviation (+80% and wetter)
#   LECI4
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum > 0.8) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 26)

# Current: Sum of seeded at high wet deviation (+80% and wetter)
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum > 0.8) |> 
  group_by(Code, Name, Duration, Lifeform) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

# Current: Sum of seeded at wet deviation (+50% and wetter)
#   LECI4, POSE, SPCR, HECO26, PEPA8
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum > 0.5) |> 
  group_by(Code, Name, Duration, Lifeform) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 


# Current: Seeded high dry deviation
#   BOER4, HECO26, LECI4, HEBO
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.5) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 26)

# Current: Sum of seeded at dry deviation (-50% and drier)
#   BOER4, HECO26, LECI4
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.5) |> 
  group_by(Code, Name, Duration, Lifeform) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 


# Projected: High count recruit
#   LEPA6
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Count > 40) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum)

# Projected: High count Seeded
#   BAMU
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Count > 5,
         PlantSource2 == "Seeded") |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  print(n = 28)

# Projected: Seeded high wet deviation (+80% and wetter)
#   ASTU, BAMU, SECO10
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum > 0.8) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 56)

# Projected: Sum of seeded at high wet deviation (+80% and wetter)
#   BAMU, ASTU, SECO10
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum > 0.8) |> 
  group_by(Code, Name, Duration, Lifeform) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

# Projected: Seeded high dry deviation
#   SPCR, BAMU, PASM
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.5) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 23)

# Projected: Sum of seeded at high dry deviation (-50% and drier)
#   PASM, ELEL5, BAMU
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.5) |> 
  group_by(Code, Name, Duration, Lifeform) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 



# DACA7 conditions
#   -74% to + 49% precip dev
dat |> 
  filter(Region == "Colorado Plateau",
         Code == "DACA7") |> 
  count(Perc_dev_cum) |> 
  arrange(desc(n)) |> 
  print(n = 23)
range(filter(dat, Region == "Colorado Plateau", Code == "DACA7")$Perc_dev_cum)
dat |> 
  filter(Region == "Colorado Plateau",
         Code == "DACA7",
         Perc_dev_cum < 0) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Height, Perc_dev_cum) 

# ELEL5 conditions
#   -74% to + 49% precip dev
dat |> 
  filter(Region == "Colorado Plateau",
         Code == "ELEL5") |> 
  count(Perc_dev_cum) |> 
  arrange(desc(n)) |> 
  print(n = 21)
range(filter(dat, Region == "Colorado Plateau", Code == "ELEL5")$Perc_dev_cum)
dat |> 
  filter(Region == "Colorado Plateau",
         Code == "ELEL5",
         Perc_dev_cum < 0) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Height, Perc_dev_cum) 


# LECI4, HECO26, BAMU, PASM, LILE3, DACA7, CAHL11, SOEL conditions
dat |> 
  filter(Code %in% c("LECI4", "HECO26", "BAMU", "PASM", "LILE3", "DACA7",
                     "CHAL11", "SOEL", "0"),
         Region == "Colorado Plateau")|> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum))
dat |> 
  filter(Code %in% c("LECI4", "HECO26", "BAMU", "PASM", "LILE3", "DACA7",
                     "CHAL11", "SOEL"),
         Region == "Colorado Plateau")|> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Count = max(Count)) |> 
  arrange(desc(max_Count)) |> 
  arrange(Code) |> 
  print(n = 148)


## Northern Arizona Plateau: Weedy ----------------------------------------

# Overall not that much differs between Current and Projected for weedy species;
#   most prominent invasives are SATR12, BRNI, BRRU2

# All plots: High count recruit
#   Mostly unknowns, SATR12
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  filter(Count > 50) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Site) |> 
  print(n = 57)

# All plots: High wet deviation
#   BRNI & unknown
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau")|> 
  filter(Perc_dev_cum > 0.8) |> 
  filter(Count > 30) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum))

# All plots: High dry deviation
#   BRRU2, ERCI6
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau")|> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Count > 10) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum)


# Weedy, by PlotMix_Climate
# Current: High count recruit 
#   Mostly unknowns, SATR12
dat |> 
  filter(Weedy == "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Count > 40) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Site) |> 
  print(n = 50)

# Current: High wet deviation
#   SATR12, BRNI
dat |> 
  filter(Weedy == "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum > 1.5) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 25)

# Current: High dry deviation
#   SATR12, BRRU2, ERCI6, TRTE
dat |> 
  filter(Weedy == "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum < -0.5) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 43)

# Projected: High count recruit 
#   Mostly unknowns, SAT12
dat |> 
  filter(Weedy == "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Count > 30) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Site) |> 
  print(n = 46)

# Projected: High wet deviation
#   BRNI
dat |> 
  filter(Weedy == "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum > 1.5) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 25)

# Projected: High dry deviation
#   BRRU2, SATR12, ERCI6, TRTE
dat |> 
  filter(Weedy == "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum < -0.5) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 35)

# SATR12, HAGL, BRRU2 conditions
dat |> 
  filter(Code %in% c("SATR12", "HAGL", "BRRU2"),
         Region == "Colorado Plateau")|> 
  group_by(Code, Name) |> 
  summarise(min = min(Perc_dev_cum),
            max = max(Perc_dev_cum))
dat |> 
  filter(Code %in% c("SATR12", "HAGL", "BRRU2"),
         Region == "Colorado Plateau")|> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Count = max(Count)) |> 
  arrange(desc(max_Count)) |> 
  arrange(Code) |> 
  print(n = 66)


# Identify outliers (Height) ----------------------------------------------

## Sonoran Desert: Desirable & Seeded -----------------------------------

# All plots: Tallest
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Height > 600) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  print(n = 22)

# LOAR12, VUOC, CHPO12 tallest heights
dat |> 
  filter(Code %in% c("LOAR12", "VUOC", "CHPO12"),
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Height = max(Height)) |> 
  arrange(desc(max_Height)) |> 
  arrange(Code) |> 
  print(n = 28)

# Desirable, by PlotMix_Climate
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
#   SACO6, LUSP2, ELEL5, POSE
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

# Current: Seeded high dry deviation (-23% and drier)
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Height > 19) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 41)

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

# Projected: Seeded high dry deviation (-23% and drier)
#   PLOV, SECO10, 
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Height > 19) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 23)

# Projected: Seeded, tallest species
#   ARPU9 & BORO2
dat |> 
  filter(Weedy != "Weedy",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded",
         Height > 200) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  print(n = 39)

# ARPU9 at driest
#   Present at 10% drier 
dat |> 
  filter(Code == "ARPU9",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded") |>
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum)

# SACO6, LUSP2, PLOV, SECO10, ARPU9 tallest heights
dat |> 
  filter(Code %in% c("SACO6", "LUSP2", "PLOV", "SECO10", "ARPU9"),
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Height = max(Height)) |> 
  arrange(desc(max_Height)) |> 
  arrange(Code) |> 
  print(n = 56)


## Sonoran Desert: Weedy --------------------------------------------------

# All plots: Tallest
#   ERCU2, SATR12
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Height > 600) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(Site) 

# All plots: Highest wet deviation
#   ERCU2, SIIR, MAPA5
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum > 0.35) |> 
  filter(Height > 200) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum))

# All plots: High wet deviation (+25% or wetter)
#   ERCU2, ERCI6, SIIR, MAPA5
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum > 0.25) |> 
  filter(Height > 150) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 29)

# All plots: Highest dry deviation
#   BRRU2, SCBA
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum < -0.35) |> 
  filter(Height > 20) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Site) |> 
  print(n = 31)

# All plots: High dry deviation (-23% and drier)
#   ERCI6, BRRU2, SCBA, ERLE
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Height > 40) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 49)

# SCBA at 10 cm and 25%+ wetter
#   Present at wettest (64%)
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Code == "SCBA") |> 
  filter(Perc_dev_cum > 0.25,
         Height > 99) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) 

# SCBA at 6 cm and -25% drier
#   Present at 31% drier
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Code == "SCBA") |> 
  filter(Perc_dev_cum < -0.25,
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

# BRRU2 at 10 cm and 25%+ wetter
#   Present at +34%
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Code == "BRRU2") |> 
  filter(Perc_dev_cum > 0.25,
         Height > 99) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) 

# BRRU2 at 10 cm and -25% drier
#   Present at 31% drier
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Code == "BRRU2") |> 
  filter(Perc_dev_cum < -0.25,
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

# ERCI6 at 10 cm and 25%+ wetter
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Code == "ERCI6") |> 
  filter(Perc_dev_cum > 0.25,
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
         Region %in% c("Sonoran Central", "Sonoran SE"))|> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Height = max(Height)) |> 
  arrange(desc(max_Height)) |> 
  arrange(Code) |> 
  print(n = 34)



## Northern Arizona Plateau: Desirable & Seeded ---------------------------

# All plots: Tall recruit
#   HPOU, PLPA2, DICA14
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau") |> 
  filter(Height > 200) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(Site) |> 
  print(n = 68)

# All plots: High wet deviation
#   BOGR, ACHY, ATCO
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau")|> 
  filter(Perc_dev_cum > 0.5) |> 
  filter(Height > 100) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 18)

# All plots: High dry deviation
#   MEMO4, ELEL5, POSE, HECO26, PASM
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau")|> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Height > 100) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum)


# By PlotMix_Climate
# Current: Tall recruit
#   BOGR, LILE3, HOPU, PLPA2, DEPI
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         SpeciesSeeded == "No") |> 
  filter(Height > 200) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 43)

# Current: Tall Seeded
#   ELTR7, PASM, LILE3, BOGR2
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Height > 150,
         PlantSource2 == "Seeded") |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  print(n = 23)

# Current: Seeded high wet deviation (+80% and wetter)
#   LECI4, KRLA2, PEPA8
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum > 0.8) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 26)


# Current: Seeded high dry deviation
#   PASM, ELEL5, DACA7
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.5) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 26)


# Projected: Tall recruit
#   HOPU, DICA14, DEPI
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         SpeciesSeeded == "No") |> 
  filter(Height > 300) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum)

# Projected: Tall Seeded
#   PLJA, PASM, ELEL5
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Height > 200,
         PlantSource2 == "Seeded") |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  print(n = 20)

# Projected: Seeded high wet deviation (+80% and wetter)
#   BOGR2, ACHY, ASTU
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum > 0.8) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum)) |> 
  print(n = 56)

# Projected: Seeded high dry deviation
#   PLJA, SPCR, PASM, ELEL5
dat |> 
  filter(Weedy != "Weedy",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         PlantSource2 == "Seeded")|> 
  filter(Perc_dev_cum < -0.5) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum) |> 
  print(n = 23)

# LECI4, HECO26, BAMU, PASM, LILE3, DACA7, CHAL11, SOEL tallest
dat |> 
  filter(Code %in% c("LECI4", "HECO26", "BAMU", "PASM", "LILE3", "DACA7",
                     "CHAL11", "SOEL"),
         Region == "Colorado Plateau")|> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Height = max(Height)) |> 
  arrange(desc(max_Height)) |> 
  arrange(Code) |> 
  print(n = 148)


## Northern Arizona Plateau: Weedy ----------------------------------------

# All plots: Tall weed
#   BRNI, AVFA
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  filter(Height > 300) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |> 
  arrange(desc(Height)) |> 
  arrange(Site) |> 
  print(n = 53)

# All plots: High wet deviation
#   SATR12, BRNI
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau")|> 
  filter(Perc_dev_cum > 0.5) |> 
  filter(Height > 100) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(desc(Perc_dev_cum)) 

# All plots: High dry deviation
#   BRTE, BRRU2, HOMU
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau")|> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Height > 100) |> 
  select(Site, Code, Name, Duration, Lifeform, Height, Perc_dev_cum) |>
  arrange(desc(Height)) |> 
  arrange(Perc_dev_cum)


# SATR12, HAGL, BRRU2 tallest heights
dat |> 
  filter(Code %in% c("SATR12", "HAGL", "BRRU2"),
         Region == "Colorado Plateau")|> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Height = max(Height)) |> 
  arrange(desc(max_Height)) |> 
  arrange(Code) |> 
  print(n = 66)



# Top species in mixes (Count) --------------------------------------------

# Most common seeded species across all Treatments for Current or Projected mixes

## Sonoran Desert ---------------------------------------------------------

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


## Northern Arizona Plateau ------------------------------------------------

# Current: All sites
#   DACA7, PASM, ELEL5, LILE3, BOGR2
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
#   BAMU, ASTU
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
  arrange(desc(SumCount)) # did best of Current (DACA7, PASM, ELE5, LILE3)

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
  arrange(desc(SumCount)) # did best of Projected (BAMU, ASTU)

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
  arrange(desc(SumCount)) # lol basically nothing grew but two ACHY


save.image("RData/09.1_identify-species-of-interest.RData")

