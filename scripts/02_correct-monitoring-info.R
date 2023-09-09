# Created: 2022-12-09
# Last updated: 2023-09-09

# Purpose: In comparing the monitoring information from the subplot vs. 2x2 plot data, 
#   there were discrepancies, but there should be only one correct version. 
#   This script shows what corrections were made and why, to standardize information 
#   for all the monitoring events. 

# Monitoring events were assigned a MonitorID, where the ID is unique for each plot monitored 
#   at each time point (Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix cols),
#   without taking into account any actual data collection (species present or species measurements).


library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

p2x2.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")
subplot.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllSubplotData")


# Set up subplot data -----------------------------------------------------

# Narrow down subplot.raw columns
subplot <- subplot.raw %>% 
  rename(PlotMix = Seed_Mix) %>% 
  select(Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix) %>% 
  distinct(.keep_all = TRUE)

# Add Region col
subplot <- subplot %>% 
  mutate(Region = case_when(
    str_detect(subplot$Site, c("AguaFria|BabbittPJ|MOWE|Spiderweb|BarTBar|FlyingM|PEFO|TLE")) ~ "Colorado Plateau",
    str_detect(subplot$Site, c("CRC|UtahPJ|Salt_Desert")) ~ "Utah",
    str_detect(subplot$Site, c("29_Palms|AVRCD")) ~ "Mojave",
    str_detect(subplot$Site, c("Creosote|Mesquite")) ~ "Chihuahuan",
    str_detect(subplot$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(subplot$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central")) |> 
  select(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix)


# Set up 2x2 plot data ----------------------------------------------------

# Narrow down cols
p2x2 <- p2x2.raw %>% 
  rename(PlotMix = Seed_Mix) %>% 
  select(Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix) %>% 
  distinct(.keep_all = TRUE)

# Add Region
p2x2 <- p2x2 |> 
  mutate(Region = case_when(
    str_detect(p2x2$Site, c("AguaFria|BabbittPJ|MOWE|Spiderweb|BarTBar|FlyingM|PEFO|TLE")) ~ "Colorado Plateau",
    str_detect(p2x2$Site, c("CRC|UtahPJ|Salt_Desert")) ~ "Utah",
    str_detect(p2x2$Site, c("29_Palms|AVRCD")) ~ "Mojave",
    str_detect(p2x2$Site, c("Creosote|Mesquite")) ~ "Chihuahuan",
    str_detect(p2x2$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(p2x2$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central"))   |> 
  select(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix)


# Assign MonitorID --------------------------------------------------------

# Assign monitoring events an ID based on subplot monitoring info
#   use subplot monitoring info because some monitoring events were not recorded in 2x2 data
monitor.sub <- subplot %>% 
  mutate(across(everything(), as.character)) %>% 
  mutate(MonitorID = 1:nrow(subplot))

# Add monitoring IDs to 2x2 plot monitoring information
monitor.2x2 <- p2x2 %>% 
  mutate(across(everything(), as.character)) %>% 
  left_join(monitor.sub)


# Examine conflicts between subplot & 2x2 monitoring info -----------------

# Examine differences by looking at NAs
monitor.diff <- monitor.2x2 %>% 
  filter(is.na(MonitorID)) %>% 
  arrange(Site) 

# Remove line of all NA at the bottom
monitor.diff <- monitor.diff %>% 
  filter(!is.na(Site))

# Need to manually inspect monitor.diff for differences by site, then write df of 
#   corresponding rows using subplot monitoring info
# Compare monitor.diff (2x2 monitoring info) values with relevant obs from 
#   subplot monitoring info to determine correct value


# AVRCD conflicts ---------------------------------------------------------

# AVRCD (Mojave): 2 issues

# 1. PlotMix conflicts
# Extract differing rows
filter(monitor.diff, Site == "AVRCD", Date_Monitored == "2020-04-30", Plot == "14") # Cool
filter(monitor.sub, Site == "AVRCD", Date_Monitored == "2020-04-30", Plot == "14") # Warm 
filter(monitor.2x2, Site == "AVRCD", Date_Monitored == "2020-04-30", Plot == "14") # Cool

# Figure out which version is correct
count(filter(monitor.sub, Site == "AVRCD", Date_Monitored == "2020-04-30", Treatment == "Seed"), PlotMix)
count(filter(monitor.2x2, Site == "AVRCD", Date_Monitored == "2020-04-30", Treatment == "Seed"), PlotMix)
#   should be 4 Cool and 4 Warm, info from 2x2 data is correct

# Create correct row
fix.AVRCD1 <- monitor.2x2 %>% 
  filter(Site == "AVRCD", Date_Monitored == "2020-04-30", Plot == "14")


# 2. Date_Seeded conflict  
# Extract differing rows
filter(monitor.diff, Site == "AVRCD", Date_Monitored == "2021-04-06") # 2020
filter(monitor.sub, Site == "AVRCD", Date_Monitored == "2021-04-06") # 2021
filter(monitor.2x2, Site == "AVRCD", Date_Monitored == "2021-04-06") # 2020

# Figure out which version is correct
count(filter(monitor.sub, Site == "AVRCD"), Date_Seeded)
count(filter(monitor.2x2, Site == "AVRCD"), Date_Seeded)
#   impossible that it was seeded in 2021, because monitoring occurred in 2020, 2x2 is correct

# Create correct row
fix.AVRCD2 <- monitor.2x2 %>% 
  filter(Site == "AVRCD", Date_Monitored == "2021-04-06")

# Combine AVRCD conflicts
fix.AVRCD <- bind_rows(fix.AVRCD1, fix.AVRCD2)




# FlyingM -----------------------------------------------------------------




# FlyingM
monitor.sub.FlyingM <- monitor.sub %>% 
  filter(Site == "FlyingM") %>% 
  filter(Treatment == "Pits") %>% 
  filter(PlotMix == "Med-Warm") %>% 
  filter(Plot == "36") %>% 
  filter(Date_Monitored == "2019-06-12") # Date_Seeded and Date_Monitored have conflicts

count(filter(monitor.sub, Site == "FlyingM"), Date_Seeded)
count(filter(monitor.2x2, Site == "FlyingM"), Date_Seeded) # 7/25 date is wrong
#   Even though this isn't a conflict between subplot and 2x2 data, It does not make sense 
#     that there would be two seeding dates, and it's not as though some plots 
#     were seeded on 7/17 and others were seeded on 7/18. There are monitoring days 
#     that have all plots marked as 7/17, and other monitoring days that have all plots 
#     marked as 7/18. The majority say 7/18, so I will change all of the subplot data  
#     and 2x2 data Date_Seeded values in a separate line of code.

count(filter(monitor.2x2, Site == "FlyingM"), Date_Monitored) # 6/13 is wrong, all others are 6/12


# Mesquite: 2 issues
monitor.sub.Mesquite <- monitor.sub %>% 
  filter(Site == "Mesquite") %>% 
  filter(Date_Monitored == "2020-12-12") %>% # Date_Monitored conflicting
  bind_rows(filter(monitor.sub,
                   Site == "Mesquite",
                   Date_Monitored == "2021-10-10",
                   Plot %in% c("233", "234", "235"))) # PlotMix and/or Treatment conflicting for other rows

count(filter(monitor.sub, Site == "Mesquite"), Date_Monitored)
count(filter(monitor.2x2, Site == "Mesquite"), Date_Monitored) 
#   no way to tell if date should be 12/12 or 12/13,
#     but will change it all to 12/13 to be standardized across 2x2 and subplot data

filter(monitor.sub, Site == "Mesquite", Plot == "233") # PlotMix should be None
filter(monitor.sub, Site == "Mesquite", Plot == "234") # Should be Seed Medium
filter(monitor.sub, Site == "Mesquite", Plot == "235") # Treatment should be Mulch


# Patagonia
monitor.sub.Patagonia <- monitor.sub %>% 
  filter(Site == "Patagonia") %>% 
  filter(Date_Monitored == "2021-03-12") %>% 
  filter(Plot == "33") # Treatment conflicting

monitor.2x2 %>% 
  filter(Site == "Patagonia") %>% 
  filter(Date_Monitored == "2021-03-12") %>% 
  filter(Plot == "33")
monitor.sub.Patagonia # Typo; should be ConMod, not `ConMod


# Pleasant  
monitor.sub.Pleasant <- monitor.sub %>% 
  filter(Site == "Pleasant") %>% 
  filter(Date_Monitored %in% c("2021-04-02", "2021-10-04")) %>% 
  filter(Plot %in% c("4", "8", "12", "15", "22", "23", "30", "32")) # Treatment conflicting

count(filter(monitor.sub, Site == "Pleasant"), Treatment)
count(filter(monitor.2x2, Site == "Pleasant"), Treatment) # should be "Seed" not "Seed only"


# Preserve: 2 issues
monitor.sub.Preserve <- monitor.sub %>% 
  filter(Site == "Preserve") %>% 
  filter(Date_Monitored == "2020-03-26") %>%
  bind_rows(filter(monitor.sub,
                   Site == "Preserve",
                   Date_Monitored == "2021-03-30",
                   Plot %in% c("2", "6", "10", "11", "15", "21", "23", "32"))) %>% 
  bind_rows(filter(monitor.sub,
                   Site == "Preserve",
                   Date_Monitored == "2021-10-06",
                   Plot == "11")) # Date_Seeded conflicting, Treatment conflicting

count(filter(monitor.sub, Site == "Preserve"), Date_Seeded)
count(filter(monitor.2x2, Site == "Preserve"), Date_Seeded) # should be 11/25

count(filter(monitor.sub, Site == "Preserve"), Treatment)
count(filter(monitor.2x2, Site == "Preserve"), Treatment) # should be "Seed" not "Seed only"


# Roosevelt: 2 issues
monitor.sub.Roosevelt <- monitor.sub %>% 
  filter(Site == "Roosevelt") %>% 
  filter(Date_Monitored %in% c("2020-03-25")) %>% 
  bind_rows(filter(monitor.sub,
                   Site == "Roosevelt",
                   Date_Monitored == "2021-04-01",
                   Plot %in% c("2", "7", "11", "16", "20", "30", "34", "36"))) %>% 
  bind_rows(filter(monitor.sub,
                   Site == "Roosevelt",
                   Date_Monitored == "2021-10-08",
                   Plot %in% c("2", "16", "30", "34"))) # Date_Seeded conflicting, Treatment conflicting

count(filter(monitor.sub, Site == "Roosevelt"), Date_Seeded)
count(filter(monitor.2x2, Site == "Roosevelt"), Date_Seeded) # should be 11/22

count(filter(monitor.sub, Site == "Roosevelt"), Treatment)
count(filter(monitor.2x2, Site == "Roosevelt"), Treatment) # should be "Seed" not "Seed only"


# Salt Desert
monitor.sub.SaltDesert <- monitor.sub %>% 
  filter(Site == "Salt_Desert") %>% 
  filter(Date_Monitored == "2019-03-29") %>% 
  filter(Plot == "32") %>% 
  filter(Treatment == "Pits") # Plot conflicting

filter(monitor.sub, Site == "Salt_Desert", Plot == "32")
filter(monitor.sub, Site == "Salt_Desert", Plot == "33") # should be 33


# SCC
monitor.sub.SCC <- monitor.sub %>% 
  filter(Site == "SCC") %>% 
  filter(Date_Monitored == "2020-03-27") %>% 
  bind_rows(filter(monitor.sub,
                   Site == "SCC",
                   Date_Monitored == "2021-03-31",
                   Plot %in% c("4", "8", "13", "16", "22", "25", "32", "35"))) %>% 
  bind_rows(filter(monitor.sub,
                   Site == "SCC",
                   Date_Monitored == "2021-10-13",
                   Plot %in% c("16", "25", "35"))) # Date_Seeded conflicting, Treatment conflicting

count(filter(monitor.sub, Site == "SCC"), Date_Seeded)
count(filter(monitor.2x2, Site == "SCC"), Date_Seeded) # should be 11/21

count(filter(monitor.sub, Site == "SCC"), Treatment)
count(filter(monitor.2x2, Site == "SCC"), Treatment) # should be "Seed" not "Seed only"



# Combing monitoring info for all sites
monitor.sub.all <- bind_rows(monitor.sub.AVRCD,
                             monitor.sub.FlyingM,
                             monitor.sub.Mesquite,
                             monitor.sub.Patagonia,
                             monitor.sub.Pleasant,
                             monitor.sub.Preserve,
                             monitor.sub.Roosevelt,
                             monitor.sub.SaltDesert,
                             monitor.sub.SCC)

nrow(monitor.sub.all) == nrow(monitor.diff)

# Combine subplot codes and 2x2 codes for comparison
monitor.fix <- monitor.diff %>% 
  select(-Site, -MonitorID) %>% 
  rename(Date_Seeded_2x2 = Date_Seeded,
         Date_Monitored_2x2 = Date_Monitored,
         Plot_2x2 = Plot,
         Treatment_2x2 = Treatment,
         PlotMix_2x2 = PlotMix)
monitor.fix <- bind_cols(monitor.sub.all, monitor.fix)
monitor.fix <- monitor.fix %>% 
  select(Site, Date_Seeded, Date_Seeded_2x2, Date_Monitored, Date_Monitored_2x2,
         Plot, Plot_2x2, Treatment, Treatment_2x2, PlotMix, PlotMix_2x2, MonitorID)

# OUTPUT: write subplot and 2x2 comparison to csv
write_csv(monitor.fix,
          file = "data/raw/02a_output-monitor_subplot-2x2-conflicting-monitoring-info.csv")


# EDITED: manually edited to include correct monitoring info only
monitor.fix <- read_xlsx("data/raw/02b_edited-monitor_conflicting-monitoring-info-resolved.xlsx",
                         sheet = "corrected")


# Compile a complete corrected list of monitoring info
monitor.info <- monitor.sub %>% 
  mutate(Date_Seeded = as.Date(monitor.sub$Date_Seeded),
         Date_Monitored = as.Date(monitor.sub$Date_Monitored),
         Plot = as.numeric(monitor.sub$Plot)) %>% 
  filter(!MonitorID %in% monitor.fix$MonitorID) %>% 
  bind_rows(monitor.fix) %>% 
  arrange(MonitorID)

# Change Date_Seeded to 7/18 for all of FlyingM
monitor.info <- monitor.info %>% 
  mutate(Date_Seeded = as.Date(Date_Seeded)) %>% 
  mutate(Date_Seeded = if_else(Site == "FlyingM", as.Date("2018-07-18"), Date_Seeded))

# Write to CSV
write_csv(monitor.info,
          file = "data/cleaned/corrected-monitoring-info_clean.csv")



save.image("RData/02_correct-monitoring-info.RData")
