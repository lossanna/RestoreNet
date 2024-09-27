# Created: 2024-09-25
# Last updated: 2024-09-27

# Purpose: Identify what species are doing well based on Count for Sonoran Desert 
#   and Northern Arizona.
# Investigate which seeded, native volunteer, and weedy species are doing well by Count
#   in variable precip. 

# Sonoran Desert precip deviation extremes (to determine performance in variable precip):
#   Wet: +24% and wetter includes all sites but Roosevelt.
#   Dry: -23% and drier includes all sites but Patagonia.
# Northern Arizona Plateau precip deviation extremes:
#   Wet: +48% and wetter includes all sites but FlyingM and TLE.
#   Dry: -50% and drier includes all sites.

# Sonoran Desert species of interest:
#   Current mix, most abundant (all conditions) and did well under var precip: SACO6, LUSP2
#   Projected mix, did well under var precip: PLOV
#   Projected mix, most abundant: ARPU9, PLOV
#   Native volunteers, most abundant and did well under var precip: VUOC 
#   Weedy species, most abundant and did well under var precip: SCBA, BRRU2

# Northern Arizona Plateau species of interest:
#   Current mix, did well under var precip: LECI4, HECO26
#   Current mix, higher count across fairly wide precip deviation range: DACA7, PASM, ELEL5
#   Projected mix, did well under var precip and had high count: BAMU
#   Projected mix, high count when wetter: ASTU
#   Projected mix, high count when drier: PASM
#   Native volunteers, high count and did well under var precip: LEPA6
#   Weedy species, higher count and did well under var precip: SATR12


library(tidyverse)

# Load data ---------------------------------------------------------------

subplot <- read_csv("data/cleaned/04.1_subplot-data_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")

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



# Sonoran Desert ----------------------------------------------------------

## Native recruit ---------------------------------------------------------

# Native volunteers (all plots): All conditions, count of all individuals
#   CHPO12, CRYSPP.SRER, VUOC, LOAR12, MENSPP.SRER, GIST
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Weedy != "Weedy",
         SpeciesSeeded == "No") |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 30)

# Native volunteers: All conditions, high count per plot
#   CRYSPP/CRAN4, PECTO, GIST, CHPO12, VUOC, MENSPP, MEAL6
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
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
#   GIST, LOAR12, VUOC - high occurrence of plots
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
#   CRYSPP.SRER, MENSPP.SRER, VUOC, LOHU2, LOAR12, PERE
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
#   VUOC, MENSPP.SRER, CRYSPP.SRER - high count per plot when driest
#   CRYSPP, MENSPP, LOHU2, VUOC - high occurrence of plots
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Count > 9) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
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



## Weedy ------------------------------------------------------------------

# Weedy (all plots): All conditions, count of all individuals
#   SCBA, ERCI6, BRRU2, ERLE
dat |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE"),
         Weedy != "Desirable") |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) |> 
  print(n = 30)


# Weedy (all plots): All conditions, high count recruit per plot
#   ERCI6, BRRU2, SCBA
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Count > 50) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
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
#   UNFO.Patagonia, BRRU2, SCBA, UNFO1.SRER, MAPA5 - high count per plot when wettest
#   BRRU2, SCBA, MAPA5, UNFO3.SRER - high occurrence of plots
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum > 0.24) |> 
  filter(Count > 10) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  print(n = 41)
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum > 0.24) |> 
  filter(Count > 10) |> 
  count(Code) |> 
  arrange(desc(n))

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
#   ERCI6, BRRU2, SCBA - all have high count per plot and high occurrence of plots
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Count > 50) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  print(n = 25)
dat |> 
  filter(Weedy != "Desirable",
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Count > 50) |> 
  count(Code) |> 
  arrange(desc(n))


# SCBA, BRRU2, ERCI6 max count
dat |> 
  filter(Code %in% c("SCBA", "BRRU2", "ERCI6"),
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  group_by(Code, Name, Perc_dev_cum) |> 
  summarise(max_Count = max(Count)) |> 
  arrange(desc(max_Count)) |> 
  arrange(Code) |> 
  print(n = 34)



## Seeded -----------------------------------------------------------------

# Current: All conditions, count of all individuals
#   SACO6, LUSP2
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

# Current: All conditions, high count per plot
#   SACO6
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Count > 10) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) 

# Current: +24% and wetter, count of all individuals
#   SACO6, LUSP2
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum > 0.24) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

# Current: +24% and wetter, with at least 3 individuals in a plot
#   SACO6, MATA2, LUSP2, ELEL5 - high count per plot when wettest
#   SACO6, LUSP2 - high occurrence of 3+ individuals 
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum > 0.24,
         Count > 2) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  print(n = 42)
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum > 0.24,
         Count > 2) |> 
  count(Code) |> 
  arrange(desc(n))

# Current: -23% and drier, count of all individuals
#   SACO6, LUSP2
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum < -0.23) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

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
  print(n = 24)
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum < -0.23) |> 
  filter(Count > 1) |> 
  count(Code) |> 
  arrange(desc(n))


# Projected: All conditions, count of all individuals
#   PLOV, ARPU9
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

# Projected: All conditions, high count per plot
#   PLOV, BAMU
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  filter(Count > 10) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) 

# Projected: +24% and wetter, count of all individuals
#   PLOV, ARPU9, BAMU
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum > 0.24) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

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

# Projected: -23% and drier, count of all individuals
#   PLOV
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region %in% c("Sonoran Central", "Sonoran SE"),
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum < -0.23) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

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




# Northern Arizona --------------------------------------------------------

## Native recruit ---------------------------------------------------------

# Native volunteers: All conditions, count of all individuals
#   LEPA6, CHEN.BabbittPJ, HESP.BabbittPJ, SPSP.BarTBar, SCMU6, PLPA2, CHAL11
dat |> 
  filter(Weedy != "Weedy",
         SpeciesSeeded == "No",
         Region == "Colorado Plateau") |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

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
#   LEPA6, HESP.BabbittPJ, CHEN.BabbittPJ, CHALL11, UNGR3.AguaFria, SCMU6
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



## Weedy ------------------------------------------------------------------

# Weedy: All conditions, count of all individuals
#   UNFO1.FlyingM, UNGR1.MOWE, SATR12, BRNI, UNFO8.BarTBar, UNGR.BarTBar, UNGR.FlyingM
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount))

# Weedy: All conditions, high count per plot
#   UNFO1.FlyingM, HAGL, UNGR1.MOWE, SATR12 
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  filter(Count > 50) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
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
#  UNGR.Spiderweb, HAGL, UNGR1.MOWE, SATR12, BRNI - high count per plot
#   UNGR1.MOWE, BRNI - high occurrence of 15+ individuals
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum > 0.48) |> 
  filter(Count > 14) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |>  
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
#   BRRU2, ERCI6 - high count per plot
#   BRRU2 - high occurrence of 3+ individuals
dat |> 
  filter(Weedy != "Desirable",
         Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Count > 2) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
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



## Seeded -----------------------------------------------------------------

# Current: All conditions, count of all individuals
#   DACA7, PASM, UNGR2.PEFO, ELEL5, LILE3
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

# Current: All conditions, high count per plot
#   BOGR2, UNGR2.PEFO, ELEL5
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Count > 15) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) |> 
  print(n = 17)

# Current: +48% and wetter, count of all individuals
#   UNGR2.PEFO, LECI4, UNGR1.BabbittPJ, ELSPP.BabbittPJ
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         Perc_dev_cum > 0.48) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

# Current: +48% and wetter, with at least 4 individuals in a plot
#   UNGR2.PEFO, POSE, DACA7 - high count per plot
#   UNGR2.PEFO, ELSPP.BabbittPJ, LECI4 - high occurrence of 4+ individuals
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum > 0.48,
         Count > 3) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |>
  arrange(desc(Count)) |> 
  print(n = 41)
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum > 0.48,
         Count > 3) |> 
  count(Code) |> 
  arrange(desc(n))

# Current: -50% and wetter, count of all individuals
#   BOER4, HECO26, LECI4
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current",
         Perc_dev_cum < -0.5) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

# Current: -50% and drier, with at least 2 individuals in a plot
#   LECI4, HECO26
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Count > 1) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count))
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Current") |> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Count > 1) |> 
  count(Code) |> 
  arrange(desc(n))


# Projected: All conditions, count of all individuals
#   BAMU, ASTU
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

# Projected: All conditions, high count per plot
#   BAMU
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Count > 10) |> 
  select(Site, Code, Name, Duration, Lifeform, SpeciesSeeded, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) 

# Projected: +48% and wetter, count of all individuals
#   BAMU, ASTU
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         Perc_dev_cum > 0.48) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

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
  print(n = 27)
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum > 0.48,
         Count > 3) |> 
  count(Code) |> 
  arrange(desc(n))

# Projected: -50% and drier, count of all individuals
#   PASM
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected",
         Perc_dev_cum < -0.5) |> 
  group_by(Code, Name, Duration, Lifeform, SpeciesSeeded) |> 
  summarise(SumCount = sum(Count),
            .groups = "keep") |> 
  arrange(desc(SumCount)) 

# Projected: -50% and drier, with at least 2 individuals in a plot
#   PASM
dat |> 
  filter(SpeciesSeeded == "Yes",
         Region == "Colorado Plateau",
         PlotMix_Climate == "Projected") |> 
  filter(Perc_dev_cum < -0.5) |> 
  filter(Count > 1) |> 
  select(Site, Code, Name, Duration, Lifeform, Count, Perc_dev_cum) |> 
  arrange(desc(Count)) 
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

save.image("RData/11.2_examine-species-by-Count.RData")

