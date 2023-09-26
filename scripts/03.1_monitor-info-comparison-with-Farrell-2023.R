# Created: 2023-09-25
# Last updated: 2023-09-26

# Purpose: Investigate discrepancies between my monitoring info and
#   Hannah Farrell's 2023 dataset, write out list of monitoring events specific
#   to date and site (not plot).
# Did not make any changes to my monitoring info after comparing with Farrell's.


library(tidyverse)

# Load data ---------------------------------------------------------------

h.subplot.raw <- read_csv("data/Farrell_2023_EcologicalApplications_supp_RestoreNetsubpl/RestoreNet_Subplots_Data.csv")
monitor.info <- read_csv("data/cleaned/corrected-monitoring-info_clean.csv")


# Data wrangling ----------------------------------------------------------

# Standardize Site and convert Date_Seeded & Date_Monitored to date 
unique(h.subplot.raw$Site)
unique(monitor.info$Site)
h.subplot <- h.subplot.raw |> 
  mutate(Site = case_when(
    Site == "29 Palms" ~ "29_Palms",
    Site == "Agua Fria NM" ~ "AguaFria",
    Site == "AVRCD/Antelope Valley" ~ "AVRCD",
    Site == "Babbitt PJ" ~ "BabbittPJ",
    Site == "Babbitt PJ2020" ~ "BabbittPJ",
    Site == "BarTBar Ranch" ~ "BarTBar",
    Site == "BarTBar Ranch 2020" ~ "BarTBar",
    Site == "Canyonlands Research Center" ~ "CRC",
    Site == "Creosote (CDRRC)" ~ "Creosote",
    Site == "Flying M Ranch" ~ "FlyingM",
    Site == "Flying M Ranch2020" ~ "FlyingM",
    Site == "La Sal" ~ "UtahPJ",
    Site == "Lake Pleasant" ~ "Pleasant",
    Site == "McDowell-Sonoran Preserve" ~ "Preserve",
    Site == "Mesquite (CDRRC)" ~ "Mesquite",
    Site == "Montezuma Well NM" ~ "MOWE",
    Site == "Montezuma Well NM2020" ~ "MOWE",
    Site == "Petrified Forest NM" ~ "PEFO",
    Site == "Petrified Forest NM2020" ~ "PEFO",
    Site == "Roosevelt Lake" ~ "Roosevelt",
    Site == "Salt Desert" ~ "Salt_Desert",
    Site == "Santa Rita Experimental Range" ~ "SRER",
    Site == "Scottsdale CC" ~ "SCC",
    Site == "Spiderweb2020" ~ "Spiderweb",
    Site == "Tolani Lake Enterprises" ~ "TLE",
    TRUE ~ Site),
    Date_Seeded = as.Date(Date_Seeded, format = "%m/%d/%Y"),
    Date_Monitored = as.Date(Date_Monitored, format = "%m/%d/%Y"))


# Create Relative_Seed_Mix col for monitor.info
unique(h.subplot$Relative_Seed_Mix)
unique(monitor.info$PlotMix)
monitor.info <- monitor.info |> 
  mutate(Relative_Seed_Mix = case_when(
    Region == "Chihuahuan" & PlotMix == "Medium" ~ "Cool",
    str_detect(Site, "AguaFria|MOWE|PEFO|Spiderweb") & PlotMix == "Med-Warm" ~ "Cool",
    str_detect(Site, "BarTBar|FlyingM|CRC|Salt_Desert") & PlotMix == "Cool-Med" ~ "Cool",
    str_detect(Site, "BarTBar|FlyingM|CRC|Salt_Desert") & PlotMix == "Med-Warm" ~ "Warm",
    str_detect(Site, "BabbittPJ|UtahPJ") & PlotMix == "Cool-Med" ~ "Warm",
    Region == "Sonoran SE" & PlotMix == "Medium" ~ "Cool",
    PlotMix == "None" ~ NA,
    TRUE ~ PlotMix))
unique(monitor.info$Relative_Seed_Mix)


# Standardize Treatment
unique(h.subplot$Treatment)
unique(monitor.info$Treatment)
h.subplot <- h.subplot |> 
  mutate(Treatment = case_when(
    Treatment == "A.Control" ~ "Control",
    Treatment == "Seed Only" ~ "Seed",
    TRUE ~ Treatment))
unique(h.subplot$Treatment)


# Standardize Chihuahuan Plot number
unique(filter(monitor.info, Region == "Chihuahuan")$Plot)
unique(filter(h.subplot, Site %in% c("Creosote", "Mesquite"))$Plot)
monitor.info <- monitor.info |> 
  mutate(Plot = case_when(
    Region == "Chihuahuan" & Plot == 201 ~ 1,
    Region == "Chihuahuan" & Plot == 202 ~ 2,
    Region == "Chihuahuan" & Plot == 203 ~ 3,
    Region == "Chihuahuan" & Plot == 204 ~ 4,
    Region == "Chihuahuan" & Plot == 205 ~ 5,
    Region == "Chihuahuan" & Plot == 206 ~ 6,
    Region == "Chihuahuan" & Plot == 207 ~ 7,
    Region == "Chihuahuan" & Plot == 208 ~ 8,
    Region == "Chihuahuan" & Plot == 209 ~ 9,
    Region == "Chihuahuan" & Plot == 210 ~ 10,
    Region == "Chihuahuan" & Plot == 211 ~ 11,
    Region == "Chihuahuan" & Plot == 212 ~ 12,
    Region == "Chihuahuan" & Plot == 213 ~ 13,
    Region == "Chihuahuan" & Plot == 214 ~ 14,
    Region == "Chihuahuan" & Plot == 215 ~ 15,
    Region == "Chihuahuan" & Plot == 216 ~ 16,
    Region == "Chihuahuan" & Plot == 217 ~ 17,
    Region == "Chihuahuan" & Plot == 218 ~ 18,
    Region == "Chihuahuan" & Plot == 219 ~ 19,
    Region == "Chihuahuan" & Plot == 220 ~ 20,
    Region == "Chihuahuan" & Plot == 221 ~ 21,
    Region == "Chihuahuan" & Plot == 222 ~ 22,
    Region == "Chihuahuan" & Plot == 223 ~ 23,
    Region == "Chihuahuan" & Plot == 224 ~ 24,
    Region == "Chihuahuan" & Plot == 225 ~ 25,
    Region == "Chihuahuan" & Plot == 226 ~ 26,
    Region == "Chihuahuan" & Plot == 227 ~ 27,
    Region == "Chihuahuan" & Plot == 228 ~ 28,
    Region == "Chihuahuan" & Plot == 229 ~ 29,
    Region == "Chihuahuan" & Plot == 230 ~ 30,
    Region == "Chihuahuan" & Plot == 231 ~ 31,
    Region == "Chihuahuan" & Plot == 232 ~ 32,
    Region == "Chihuahuan" & Plot == 233 ~ 33,
    Region == "Chihuahuan" & Plot == 234 ~ 34,
    Region == "Chihuahuan" & Plot == 235 ~ 35,
    Region == "Chihuahuan" & Plot == 236 ~ 36,
    TRUE ~ Plot))



# Compare Site, Date_Seeded, Date_Monitored -------------------------------

# Narrow down columns
monitor.site <- monitor.info |> 
  select(Region, Site, Date_Seeded, Date_Monitored) |> 
  distinct(.keep_all = TRUE)

h.monitor.site <- h.subplot |> 
  select(Site, MAP, MAT, Cumulative_Precip, Precip_since_monitor,
         Date_Seeded, Date_Monitored) |> 
  distinct(.keep_all = TRUE)


# View discrepancies with left_join() that creates NAs
monitor.site <- left_join(monitor.site, h.monitor.site)

monitor.diff1 <- monitor.site |> 
  filter(is.na(MAP)) |> 
  arrange(Date_Monitored)

# New observations (not included in H. Farrell dataset)
range(h.monitor.site$Date_Monitored) # last monitor date 2021-06-29
monitor.new1 <- monitor.diff1 |> 
  filter(Date_Monitored > as.Date("2021-06-29"))

# Conflicting monitoring info
monitor.conflict1 <- monitor.diff1 |> 
  filter(!Date_Monitored %in% monitor.new1$Date_Monitored) |> 
  arrange(Site)


# Compare conflicting monitoring info
# 29_Palms
#   Farrell doesn't go past spring 2020 monitoring
filter(monitor.site, Site == "29_Palms") 
filter(h.monitor.site, Site == "29_Palms")

# AVRCD
#   Farrell doesn't go past spring 2020 monitoring
filter(monitor.site, Site == "AVRCD") 
filter(h.monitor.site, Site == "AVRCD") 

# Creosote
#   Farrell doesn't go past winter 2020 monitoring
filter(monitor.site, Site == "Creosote") 
filter(h.monitor.site, Site == "Creosote")

# Mesquite
#   Date_Monitored conflicting, and Farrell doesn't go past winter 2020 monitoring
filter(monitor.site, Site == "Mesquite") # monitored 2020-12-12
filter(h.monitor.site, Site == "Mesquite") # monitored 2020-12-13

# Patagonia
#   Farrell doesn't go past winter 2020 monitoring
filter(monitor.site, Site == "Patagonia") 
filter(h.monitor.site, Site == "Patagonia")

# Pleasant
#   Farrell doesn't go past fall 2020 monitoring
filter(monitor.site, Site == "Pleasant") 
filter(h.monitor.site, Site == "Pleasant")

# Preserve
#   Farrell doesn't go past fall 2020 monitoring
filter(monitor.site, Site == "Preserve") 
filter(h.monitor.site, Site == "Preserve")

# Roosevelt
#   Farrell doesn't go past fall 2020 monitoring
filter(monitor.site, Site == "Roosevelt") 
filter(h.monitor.site, Site == "Roosevelt")

# SCC
#   Farrell doesn't go past fall 2020 monitoring
filter(monitor.site, Site == "SCC") 
filter(h.monitor.site, Site == "SCC")

# SRER
#   Farrell doesn't go past fall 2020 monitoring
filter(monitor.site, Site == "SRER") 
filter(h.monitor.site, Site == "SRER")


# Actual conflicts
# Mesquite
filter(monitor.site, Site == "Mesquite") # monitored 2020-12-12
filter(h.monitor.site, Site == "Mesquite") # monitored 2020-12-13

monitor.conflict1 <- filter(monitor.site, Site == "Mesquite", Date_Monitored == "2020-12-12")


# Write CSV of monitoring events by site & date ---------------------------

# Add MAP, MAT, and precip manually for conflicting Date_Monitored
monitor.site$MAP[monitor.site$Site == "Mesquite" & monitor.site$Date_Monitored == as.Date("2020-12-12")] <- 274.76
monitor.site$MAT[monitor.site$Site == "Mesquite" & monitor.site$Date_Monitored == as.Date("2020-12-12")] <- 15.7
monitor.site$Cumulative_Precip[monitor.site$Site == "Mesquite" & monitor.site$Date_Monitored == as.Date("2020-12-12")] <- 56.120
monitor.site$Precip_since_monitor[monitor.site$Site == "Mesquite" & monitor.site$Date_Monitored == as.Date("2020-12-12")] <- 0

# Add MonitorSiteID
monitor.site <- monitor.site |> 
  arrange(Date_Monitored) |> 
  arrange(Site) |> 
  arrange(Region) |> 
  mutate(MonitorSiteID = 1:nrow(monitor.site)) |> 
  rename(Farrell_MAP = MAP,
         Farrell_MAT = MAT,
         Farrell_cum_precip = Cumulative_Precip,
         Farrell_precip_since_monitor = Precip_since_monitor)

# Write to CSV
write_csv(monitor.site,
          file = "data/data-wrangling-intermediate/03.1_monitoring-events-by-date-and-site.csv")



# Compare Treatment, PlotMix, Plot ----------------------------------------

# There is not much point to this section, because I just found some mistakes and differences
#   with Hannah's data, but I'm not changing any of my monitoring info.
# This may be useful later if I want to actually use the Farrell subplot data itself,
#   as there are known issues with all of the seed mix at Creosote marked Warm.

# Narrow down columns
monitor <- monitor.info |> 
  select(Region, Site, Date_Seeded, Date_Monitored, Treatment, Relative_Seed_Mix, Plot) |> 
  distinct(.keep_all = TRUE)

h.monitor <- h.subplot |> 
  select(Site, MAP, MAT, Cumulative_Precip, Precip_since_monitor,
         Date_Seeded, Date_Monitored, Treatment, Relative_Seed_Mix, Plot) |> 
  distinct(.keep_all = TRUE)



# View discrepancies with left_join() that creates NAs
monitor <- left_join(monitor, h.monitor)

monitor.diff2 <- monitor |> 
  filter(is.na(MAP)) |> 
  arrange(Date_Monitored)

monitor.conflict2 <- monitor.diff2 |> 
  filter(Date_Monitored < as.Date("2020-12-31")) |> 
  filter(Treatment != "Pellets") |> 
  filter(Date_Monitored != as.Date("2020-12-12"))


# Creosote and Mesquite are very different
#   Inspect dates
monitor.conflict2 |> 
  filter(Site == "Creosote") |> 
  count(Date_Monitored) # why are there not 36 plots

monitor.conflict2 |> 
  filter(Site == "Mesquite") |> 
  count(Date_Monitored) # why are there not 36


# Inspect 2020-09-20 as example
filter(monitor, Site == "Creosote", Date_Monitored == as.Date("2020-09-20")) |> 
  print(n = 36)
filter(h.monitor, Site == "Creosote", Date_Monitored == as.Date("2020-09-20")) |> 
  arrange(Plot) |> 
  print(n = 36)

filter(monitor, Site == "Mesquite", Date_Monitored == as.Date("2020-09-20")) |> 
  print(n = 36)
filter(h.monitor, Site == "Mesquite", Date_Monitored == as.Date("2020-09-20")) |> 
  arrange(Plot) |> 
  print(n = 36)

monitor |> 
  filter(Site == "Creosote") |> 
  count(Treatment) 
h.monitor |> 
  filter(Site == "Creosote") |> 
  count(Treatment)

monitor |> 
  filter(Site == "Creosote") |> 
  count(Relative_Seed_Mix) 
h.monitor |> 
  filter(Site == "Creosote") |> 
  count(Relative_Seed_Mix) # why are there no cool mix

monitor |> 
  filter(Site == "Mesquite") |> 
  count(Treatment)
h.monitor |> 
  filter(Site == "Mesquite") |> 
  count(Treatment)

monitor |> 
  filter(Site == "Mesquite") |> 
  count(Relative_Seed_Mix)
h.monitor |> 
  filter(Site == "Mesquite") |> 
  count(Relative_Seed_Mix) # why are there no cool


# Remove Creosote & Mesquite and check others
monitor.conflict2.1 <- monitor.conflict2 |> 
  filter(Region != "Chihuahuan")


# Salt_Desert
filter(monitor, Site == "Salt_Desert", Plot == 33)
filter(h.monitor, Site == "Salt_Desert", Plot == 33) # missing monitoring on 2019-03-29

# AguaFria
filter(monitor, Site == "AguaFria", Plot == 20)
filter(h.monitor, Site == "AguaFria", Plot == 20) # only sampled until spring 2019

# Pleasant
filter(monitor, Site == "Pleasant", Plot == 29)
filter(h.monitor, Site == "Pleasant", Plot == 29) # missing multiple monitoring events

# BarTBar
filter(monitor, Site == "BarTBar", Plot == 25)
filter(h.monitor, Site == "BarTBar", Plot == 25) # missing monitoring on 2020-07-15

# BarTBar
filter(monitor, Site == "BarTBar", Plot == 28)
filter(h.monitor, Site == "BarTBar", Plot == 28) # missing monitoring on 2020-07-15

# AVRCD
filter(monitor, Site == "AVRCD", Plot == 14)
filter(h.monitor, Site == "AVRCD", Plot == 14) # Relative_Seed_Mix conflicting
h.monitor |> 
  filter(Site == "AVRCD") |> 
  count(Relative_Seed_Mix) # Farrell is probably wrong, there are probably equal number of
#     plots with warm and cool mix


# Patagonia
#   Site was sampled on 2 different days but Farrell marked them all as one day (2020-10-01)
filter(monitor, Site == "Patagonia", Plot == 21)
filter(h.monitor, Site == "Patagonia", Plot == 21)
filter(monitor, Site == "Patagonia", Date_Monitored == as.Date("2020-10-01"))
filter(monitor, Site == "Patagonia", Date_Monitored == as.Date("2020-10-04"))
filter(h.monitor, Site == "Patagonia", Date_Monitored == as.Date("2020-10-01"))


save.image("RData/03.1_monitor-info-comparison-with-Farrell-2023.RData")
