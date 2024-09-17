# Created: 2024-09-17
# Last updated: 2024-09-17

# Purpose: Identify what species are doing well based on Count in Sonoran Desert and
#   Northern Arizona Plateau. Calculate total number of plots for various groups 
#   to determine percent frequency in which the species shows up in a plot.
#   Investigate species of interest (seeded, native volunteer, and weedy
#   that are doing well in variable precip by Count and plot frequency).

# Number of plots for frequency (36 plots * number of sampling events at each site):
# Sonoran Desert:
#   Total: 1152; when wetter: 360; when drier: 972
#   Seeded (no Control plots): 1024 for Current + Projected, 512 for Current/Projected alone
#   Seeded, when wetter: 320 (C+P) or 160 (C/P alone); when drier: 704 or 352
# Northern Arizona Plateau:
#   Total: 3492; when wetter: 2196; when drier: 1296
#   Seeded: 3106 (C+P) or 1553 (C/P alone); when wetter: 1954 or 977; when drier: 1152 or 576

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
library(scales)
library(viridis)
library(ggbreak)
library(ggpmisc)

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

