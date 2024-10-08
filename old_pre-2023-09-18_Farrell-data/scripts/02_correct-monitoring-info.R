# Created: 2022-12-09
# Last updated: 2023-09-18

# Purpose: In comparing the monitoring information from the subplot vs. 2x2 plot data, 
#   there were discrepancies, but there should be only one correct version. 
#   This script shows what corrections were made and why, to standardize information 
#   for all the monitoring events. 

# Monitoring events were assigned a MonitorID, where the ID is unique for each plot monitored 
#   at each time point (Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix cols),
#   without taking into account any actual data collection (species present or species measurements).

# Wrote out separate tables of incorrect events and correct events (with correct MonitorID) that must be
#   fixed in subplot and 2x2 data during data wrangling.


library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

p2x2.raw <- read_xlsx("old_pre-2023-09-18_Farrell-data/data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")
subplot.raw <- read_xlsx("old_pre-2023-09-18_Farrell-data/data/raw/Master Germination Data 2022.xlsx", sheet = "AllSubplotData")


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
  select(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix) %>% 
  mutate(across(everything(), as.character))


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
  select(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix) %>% 
  mutate(across(everything(), as.character))


# Assign MonitorID --------------------------------------------------------

# Assign monitoring events a (preliminary) ID based on subplot monitoring info
#   Use subplot monitoring info because some monitoring events were not recorded in 2x2 data
monitor.sub <- subplot %>% 
  mutate(across(everything(), as.character)) %>% 
  mutate(MonitorID = 1:nrow(subplot))

# Add monitoring IDs to 2x2 plot monitoring information
monitor.2x2 <- p2x2 %>% 
  mutate(across(everything(), as.character)) %>% 
  left_join(monitor.sub)



# Examine conflicts between subplot & 2x2 monitoring info -----------------

# Examine differences by looking at NAs formed from left_join()
monitor.diff <- monitor.2x2 %>% 
  filter(is.na(MonitorID)) %>% 
  arrange(Site) 


# Need to manually inspect monitor.diff for differences by site, then write df of 
#   corresponding rows using subplot monitoring info
# Compare monitor.diff (2x2 monitoring info) values with relevant obs from 
#   subplot monitoring info to determine correct value
# Correct subplot info when needed; use subplot data as base for a complete list of 
#   monitoring events because it has the monitoring ID attached

# Make dfs of wrong and correct subplot monitoring data for later data wrangling
# Make dfs of wrong and correct 2x2 monitoring data for later data wrangling



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

# Extract incorrect row for subplot wrangling
wrong.sub.AVRCD1 <- filter(monitor.sub, Site == "AVRCD", Date_Monitored == "2020-04-30", Plot == "14")

# Create correct row for subplot
fix.sub.AVRCD1 <- monitor.sub %>% 
  filter(Site == "AVRCD", Date_Monitored == "2020-04-30", Plot == "14")
fix.sub.AVRCD1$PlotMix <- "Cool"



# 2. Date_Seeded conflict  
# Extract differing rows
filter(monitor.diff, Site == "AVRCD", Date_Monitored == "2021-04-06") # 2020
filter(monitor.sub, Site == "AVRCD", Date_Monitored == "2021-04-06") # 2021
filter(monitor.2x2, Site == "AVRCD", Date_Monitored == "2021-04-06") # 2020

# Figure out which version is correct
count(filter(monitor.sub, Site == "AVRCD"), Date_Seeded)
count(filter(monitor.2x2, Site == "AVRCD"), Date_Seeded)
#   impossible that it was seeded in 2021, because monitoring occurred in 2020, 2x2 is correct

# Extract incorrect subplot row
wrong.sub.AVRCD2 <- filter(monitor.sub, Site == "AVRCD", Date_Monitored == "2021-04-06") 

# Create correct subplot row 
fix.sub.AVRCD2 <- monitor.sub %>% 
  filter(Site == "AVRCD", Date_Monitored == "2021-04-06")
fix.sub.AVRCD2$Date_Seeded <- rep("2020-03-17", nrow(fix.sub.AVRCD2))


# Combine AVRCD conflicts
wrong.sub.AVRCD <- bind_rows(wrong.sub.AVRCD1, wrong.sub.AVRCD2)
fix.sub.AVRCD <- bind_rows(fix.sub.AVRCD1, fix.sub.AVRCD2)




# FlyingM -----------------------------------------------------------------

# FlyingM (CO Plateau): 2 issues
# 1-2. Date_Seeded and Date_Monitored conflict
# Extract differing rows
filter(monitor.diff, Site == "FlyingM") # seeded 2018-07-25, monitored 2019-06-13
monitor.sub %>% 
  filter(Site == "FlyingM") %>% 
  filter(Treatment == "Pits") %>% 
  filter(PlotMix == "Med-Warm") %>% 
  filter(Plot == "36") # seeded 2018-07-18, monitored 2019-06-12 
monitor.2x2 %>% 
  filter(Site == "FlyingM") %>% 
  filter(Treatment == "Pits") %>% 
  filter(PlotMix == "Med-Warm") %>% 
  filter(Plot == "36") # seeded 2018-07-25, monitored 2019-06-13 

# Figure out which version is correct
count(filter(monitor.sub, Site == "FlyingM"), Date_Seeded)
count(filter(monitor.2x2, Site == "FlyingM"), Date_Seeded) # 7/25 date is wrong
#   They re-seeded this site, hence two different seeding dates

count(filter(monitor.2x2, Site == "FlyingM"), Date_Monitored) # 6/13 is wrong, all others are 6/12

# Create correct row
#   Subplot data already correct; no fix needed

# Create wrong row that needs to be corrected for 2x2
wrong.2x2.FlyingM <- filter(monitor.diff, Site == "FlyingM")

# Create right row for 2x2
fix.2x2.FlyingM <- monitor.sub %>% 
  filter(Site == "FlyingM") %>% 
  filter(Treatment == "Pits") %>% 
  filter(PlotMix == "Med-Warm") %>% 
  filter(Plot == "36") |> 
  filter(Date_Seeded == "2018-07-18") |> 
  filter(Date_Monitored == "2019-06-12")



# Mesquite ----------------------------------------------------------------

# Mesquite (Chihuahuan): 4 issues

# 1. Date_Monitored conflict
# Extract conflicting rows
filter(monitor.diff, Site == "Mesquite") # Date_Monitored 2020-12-13
monitor.sub |> 
  filter(Site == "Mesquite") |> 
  filter(str_detect(Date_Monitored, "2020-12")) |> 
  print(n = 50) # Date_Monitored 2020-12-12
monitor.2x2 |> 
  filter(Site == "Mesquite") |> 
  filter(str_detect(Date_Monitored, "2020-12")) |> 
  print(n = 50) # Date_Monitored 2020-12-13

# Figure out which version is correct
count(filter(monitor.sub, Site == "Mesquite"), Date_Monitored)
count(filter(monitor.2x2, Site == "Mesquite"), Date_Monitored) 
#   no way to tell if date should be 12/12 or 12/13,
#     but will change it all to 12/12 to be standardized across 2x2 and subplot data

# Create correct row
#   Subplot data already correct; no fix needed

# Create wrong row that needs to be corrected for 2x2
wrong.2x2.Mesquite <- filter(monitor.diff, Site == "Mesquite",
                             Date_Monitored == "2020-12-13")

# Create right row for 2x2
fix.2x2.Mesquite <- monitor.sub |> 
  filter(Site == "Mesquite") |> 
  filter(Date_Monitored == "2020-12-12")


# 2. PlotMix conflict for Plot 233
# Extract differing rows
filter(monitor.diff, Site == "Mesquite", Date_Monitored == "2021-10-10", Plot == "233") # PlotMix is None
filter(monitor.sub, Site == "Mesquite", Date_Monitored == "2021-10-10", Plot == "233") # PlotMix is Medium
filter(monitor.2x2, Site == "Mesquite", Date_Monitored == "2021-10-10", Plot == "233") # PlotMix is None

# Figure out which version is correct
filter(monitor.sub, Site == "Mesquite", Plot == "233") # PlotMix should be None
filter(monitor.2x2, Site == "Mesquite", Plot == "233") # 2x2 is right

# Extract incorrect subplot row
wrong.sub.Mesquite2 <- filter(monitor.sub, Site == "Mesquite", Date_Monitored == "2021-10-10", Plot == "233")

# Create correct subplot row
fix.sub.Mesquite2 <- filter(monitor.sub, Site == "Mesquite", Date_Monitored == "2021-10-10", Plot == "233")
fix.sub.Mesquite2$PlotMix <- "None"


# 3. Treatment and PlotMix conflict for Plot 234
filter(monitor.diff, Site == "Mesquite", Date_Monitored == "2021-10-10", Plot == "234") # PlotMix is Medium, Treatment is Seed
filter(monitor.sub, Site == "Mesquite", Date_Monitored == "2021-10-10", Plot == "234") # PlotMix is Warm, Treatment is Mulch
filter(monitor.2x2, Site == "Mesquite", Date_Monitored == "2021-10-10", Plot == "234") # PlotMix is Medium, Treatment is Seed

# Figure out which version is correct
filter(monitor.sub, Site == "Mesquite", Plot == "234") # PlotMix should be Medium, Treatment should be Seed

# Extract incorrect subplot row
wrong.sub.Mesquite3 <- filter(monitor.sub, Site == "Mesquite", Date_Monitored == "2021-10-10", Plot == "234")

# Create correct subplot row
fix.sub.Mesquite3 <- filter(monitor.sub, Site == "Mesquite", Date_Monitored == "2021-10-10", Plot == "234")
fix.sub.Mesquite3$PlotMix <- "Medium"
fix.sub.Mesquite3$Treatment <- "Seed"


# 4. Treatment conflict for Plot 235
filter(monitor.diff, Site == "Mesquite", Date_Monitored == "2021-10-10", Plot == "235") # Treatment is Mulch
filter(monitor.sub, Site == "Mesquite", Date_Monitored == "2021-10-10", Plot == "235") # Treatment is Seed
filter(monitor.2x2, Site == "Mesquite", Date_Monitored == "2021-10-10", Plot == "235") # Treatment is Mulch

# Figure out which version is correct
filter(monitor.sub, Site == "Mesquite", Plot == "235") # Treatment should be Mulch

# Extract incorrect subplot row
wrong.sub.Mesquite4 <- filter(monitor.sub, Site == "Mesquite", Date_Monitored == "2021-10-10", Plot == "235")

# Create correct subplot row
fix.sub.Mesquite4 <- filter(monitor.sub, Site == "Mesquite", Date_Monitored == "2021-10-10", Plot == "235")
fix.sub.Mesquite4$Treatment <- "Mulch"


# Combine fixes
wrong.sub.Mesquite <- bind_rows(wrong.sub.Mesquite2, wrong.sub.Mesquite3, wrong.sub.Mesquite4)
fix.sub.Mesquite <- bind_rows(fix.sub.Mesquite2, fix.sub.Mesquite3, fix.sub.Mesquite4)




# Patagonia ---------------------------------------------------------------

# Patagonia (SRER): 1 issue

# 1. Treatment conflicting
# Extract differing rows
filter(monitor.diff, Site == "Patagonia")
filter(monitor.sub, Site == "Patagonia", Date_Monitored == "2021-03-12", Plot == "33")
filter(monitor.2x2, Site == "Patagonia", Date_Monitored == "2021-03-12", Plot == "33")

# Figure out which version is correct
#   Typo in subplot data; should be ConMod, not `ConMod

# Extract incorrect subplot row
wrong.sub.Patagonia <- filter(monitor.sub, Site == "Patagonia", Date_Monitored == "2021-03-12", Plot == "33")

# Create correct subplot row
fix.sub.Patagonia <- filter(monitor.sub, Site == "Patagonia", Date_Monitored == "2021-03-12", Plot == "33")
fix.sub.Patagonia$Treatment <- "ConMod"



# Pleasant ----------------------------------------------------------------

# Pleasant (Sonoran Central): 1 issue

# 1. Treatment conflicting
# Determine differences
filter(monitor.diff, Site == "Pleasant")
count(filter(monitor.sub, Site == "Pleasant"), Treatment)
count(filter(monitor.2x2, Site == "Pleasant"), Treatment) # includes "Seed only"

# Figure out which version is correct
#   Treatment should be "Seed" and not "Seed Only"

# Create correct rows
#   Subplot data already correct; no fix needed

# Create wrong row that needs to be corrected for 2x2
wrong.2x2.Pleasant <- filter(monitor.diff, Site == "Pleasant")

# Create right row for 2x2
fix.2x2.Pleasant <- monitor.sub |> 
  filter(Date_Monitored %in% wrong.2x2.Pleasant$Date_Monitored,
         Plot %in% wrong.2x2.Pleasant$Plot)



# Preserve ----------------------------------------------------------------

# Preserve (Sonoran Central): 2 issues

# 1. Date_Seeded conflicting
# Determine differences
count(filter(monitor.sub, Site == "Preserve"), Date_Seeded) # 2019-11-25 
count(filter(monitor.2x2, Site == "Preserve"), Date_Seeded) # 2019-09-25, 2019-11-25

# Figure out correct version
#   Should be just 2019-11-25; subplot is correct

# Create correct rows
#   Subplot data already correct; no fix needed

# Create wrong row that needs to be corrected for 2x2
wrong.2x2.Preserve1 <- monitor.diff |> 
  filter(Site == "Preserve",
         Date_Seeded == "2019-09-25")

# Create right row for 2x2
fix.2x2.Preserve1 <- monitor.sub |> 
  filter(Date_Monitored %in% wrong.2x2.Preserve1$Date_Monitored,
         Plot %in% wrong.2x2.Preserve1$Plot)



# 2. Treatment conflicting
# Extract differing rows
count(filter(monitor.sub, Site == "Preserve"), Treatment)
count(filter(monitor.2x2, Site == "Preserve"), Treatment)

# Figure out correct version
#   All should be "Seed", not "Seed only"

# Extract incorrect subplot rows
wrong.sub.Preserve <- filter(monitor.sub, Site == "Preserve", Treatment == "Seed only")

# Create correct rows
fix.sub.Preserve <- filter(monitor.sub, Site == "Preserve", Treatment == "Seed only")
fix.sub.Preserve$Treatment <- "Seed"

# Create wrong row that needs to be corrected for 2x2
wrong.2x2.Preserve2 <- filter(monitor.diff, Site == "Preserve", Treatment == "Seed only")

# Create right row for 2x2
fix.2x2.Preserve2 <- monitor.sub |> 
  filter(Site == "Preserve") |> 
  filter(Date_Monitored %in% wrong.2x2.Preserve2$Date_Monitored,
         Plot %in% wrong.2x2.Preserve2$Plot) |> 
  filter(Treatment == "Seed")




# Roosevelt ---------------------------------------------------------------

# Roosevelt (Sonoran Central): 2 issues

# 1. Date_Seeded conflicting
# Determine differences
count(filter(monitor.sub, Site == "Roosevelt"), Date_Seeded) # 2019-11-22 
count(filter(monitor.2x2, Site == "Roosevelt"), Date_Seeded) # 2019-09-22, 2019-11-22

# Figure out correct version
#   2019-11-22 is correct

# Create correct rows
#   Subplot data already correct; no fix needed

# Create wrong row that needs to be corrected for 2x2
wrong.2x2.Roosevelt1 <- monitor.diff |> 
  filter(Site == "Roosevelt",
         Date_Seeded == "2019-09-22")

# Create right row for 2x2
fix.2x2.Roosevelt1 <- monitor.sub |> 
  filter(Date_Monitored %in% wrong.2x2.Roosevelt1$Date_Monitored,
         Plot %in% wrong.2x2.Roosevelt1$Plot)



# 2. Treatment conflicting
# Determine differences
count(filter(monitor.sub, Site == "Roosevelt"), Treatment)
count(filter(monitor.2x2, Site == "Roosevelt"), Treatment) 

# Figure out correct version
#   Should be "Seed" not "Seed only"

# Extract incorrect subplot rows
wrong.sub.Roosevelt <- filter(monitor.sub, Site == "Roosevelt", Treatment == "Seed only")

# Create correct rows
fix.sub.Roosevelt <- filter(monitor.sub, Site == "Roosevelt", Treatment == "Seed only")
fix.sub.Roosevelt$Treatment <- "Seed"

# Create wrong row that needs to be corrected for 2x2
wrong.2x2.Roosevelt2 <- filter(monitor.diff, Site == "Roosevelt", Treatment == "Seed only")

# Create right row for 2x2
fix.2x2.Roosevelt2 <- monitor.sub |> 
  filter(Site == "Roosevelt") |> 
  filter(Date_Monitored %in% wrong.2x2.Roosevelt2$Date_Monitored,
         Plot %in% wrong.2x2.Roosevelt2$Plot) |> 
  filter(Treatment == "Seed")




# SCC ---------------------------------------------------------------------

# SCC (Sonoran Central): 2 issues

# 1. Date_Seeded conflict
# Determine differences
count(filter(monitor.sub, Site == "SCC"), Date_Seeded) # 2019-11-21
count(filter(monitor.2x2, Site == "SCC"), Date_Seeded) # 2019-09-21, 2019-11-21

# Figure out correct version
#   Subplot data is correct, should be 2019-11-21)

# Create correct rows
#   Subplot data already correct; no fix needed

# Create wrong row that needs to be corrected for 2x2
wrong.2x2.SCC1 <- monitor.diff |> 
  filter(Site == "SCC",
         Date_Seeded == "2019-09-21")

# Create right row for 2x2
fix.2x2.SCC1 <- monitor.sub |> 
  filter(Date_Monitored %in% wrong.2x2.SCC1$Date_Monitored,
         Plot %in% wrong.2x2.SCC1$Plot)



# 2. Treatment conflicting
# Determine differences
count(filter(monitor.sub, Site == "SCC"), Treatment)
count(filter(monitor.2x2, Site == "SCC"), Treatment) 

# Figure out correct version
#   Should be "Seed"

# Extract incorrect subplot row
wrong.sub.SCC <- filter(monitor.sub, Site == "SCC", Treatment == "Seed only")

# Create correct rows
fix.sub.SCC <- filter(monitor.sub, Site == "SCC", Treatment == "Seed only")
fix.sub.SCC$Treatment <- "Seed"

# Create wrong row that needs to be corrected for 2x2
wrong.2x2.SCC2 <- filter(monitor.diff, Site == "SCC", Treatment == "Seed only")

# Create right row for 2x2
fix.2x2.SCC2 <- monitor.sub |> 
  filter(Site == "SCC") |> 
  filter(Date_Monitored %in% wrong.2x2.SCC2$Date_Monitored,
         Plot %in% wrong.2x2.SCC2$Plot) |> 
  filter(Treatment == "Seed")




# Salt Desert -------------------------------------------------------------

# Salt Desert (Utah): 1 issue

# 1. Plot number conflicting
monitor.sub %>% 
  filter(Site == "Salt_Desert") %>% 
  filter(Date_Monitored == "2019-03-29") %>% 
  filter(Plot == "32") 
monitor.sub %>% 
  filter(Site == "Salt_Desert") %>% 
  filter(Date_Monitored == "2019-03-29") %>% 
  filter(Plot == "33") # Plot 33 does not exist

monitor.2x2 %>% 
  filter(Site == "Salt_Desert") %>% 
  filter(Date_Monitored == "2019-03-29") %>% 
  filter(Plot == "32") # Treatment is Mulch, PlotMix is Cool-Med
monitor.2x2 %>% 
  filter(Site == "Salt_Desert") %>% 
  filter(Date_Monitored == "2019-03-29") %>% 
  filter(Plot == "33") # Treatment is Pits, PlotMix is Med-Warm

# Figure out correct version
#   Plot number needs to be corrected

# Extract incorrect subplot row
wrong.sub.Salt_Desert <- monitor.sub %>% 
  filter(Site == "Salt_Desert") %>% 
  filter(Date_Monitored == "2019-03-29") %>% 
  filter(Plot == "32") |> 
  filter(Treatment == "Pits")

# Create correct subplot row
fix.sub.Salt_Desert <- wrong.sub.Salt_Desert
fix.sub.Salt_Desert$Plot <- "33"



# Look for any last instances of "Seed only" ------------------------------

#    These may not have shown up in monitor.diff because both suplot and 2x2
#     monitoring info may be matching but both wrong

dim(filter(monitor.diff, Treatment == "Seed only")) # 48 conflicts where subplot was right and 2x2 was wrong
count(subplot, Treatment) # 16 instances to be fixed in subplot data
count(p2x2, Treatment) # 64 instances to be fixed in 2x2 data

# Find which 2x2 events need fixing
filter(p2x2, Treatment == "Seed only") |> 
  count(Site) # 16 instances to be fixed at Pleasant, Preserve, Roosevelt, and SCC

# 2x2 events already been accounted for (48 total, missing 16)
dim(wrong.2x2.Pleasant) # 16, missing none
dim(wrong.2x2.Preserve2) # 9, missing 7
dim(wrong.2x2.Roosevelt2) # 12, missing 4
dim(wrong.2x2.SCC2) # 11, missing 5

# See which are missing by Site and Date_Monitored
count(wrong.2x2.Preserve2, Date_Monitored) # 2021-10-06 missing ones
count(wrong.2x2.Roosevelt2, Date_Monitored) # 2021-10-08 missing ones
count(wrong.2x2.SCC2, Date_Monitored) # 2021-10-13  missing ones

# Compare with subplot events that needed to be fixed
bind_rows(fix.sub.Roosevelt, fix.sub.Preserve, fix.sub.SCC) |> 
  filter(Treatment == "Seed") # these are the same events missing from 2x2 fixes


# Create wrong row for 2x2 to be corrected
wrong.2x2.seedonly <- bind_rows(
  filter(p2x2, Site == "Roosevelt", 
         Date_Monitored %in% fix.sub.Roosevelt$Date_Monitored, 
         Plot %in% fix.sub.Roosevelt$Plot),
  filter(p2x2, Site == "Preserve", 
         Date_Monitored %in% fix.sub.Preserve$Date_Monitored,
         Plot %in% fix.sub.Preserve$Plot),
  filter(p2x2, Site == "SCC", 
         Date_Monitored %in% fix.sub.SCC$Date_Monitored,
         Plot %in% fix.sub.SCC$Plot))

# Create right row for 2x2
fix.2x2.seedonly <- bind_rows(fix.sub.Roosevelt, fix.sub.Preserve, fix.sub.SCC)
  



# Begin to compile list of correct monitoring info ------------------------

# Combine corrected conflicting monitoring info
fix.sub.conflict <- bind_rows(fix.sub.AVRCD,
                     fix.sub.Mesquite,
                     fix.sub.Patagonia,
                     fix.sub.Preserve,
                     fix.sub.Roosevelt,
                     fix.sub.SCC,
                     fix.sub.Salt_Desert)

# Subplot data
#   Replace monitor info from subplot data with correct info
monitor.correct <- monitor.sub |> 
  filter(!MonitorID %in% fix.sub.conflict$MonitorID) |> 
  bind_rows(fix.sub.conflict) |> 
  arrange(MonitorID)

nrow(monitor.correct) == nrow(monitor.sub)



# Look for mistakes in subplot data ---------------------------------------

#   Before, we had assumed that all the subplot monitoring information that matched the 2x2 data
#     was correct, but it actually has mistakes

# Look for mistakes in subplot data by examining each site 

unique(monitor.correct$Site)


# Number of plots
#   CO Plateau all have normal 36 plots
unique(filter(monitor.correct, Site == "AguaFria")$Plot)
unique(filter(monitor.correct, Site == "BabbittPJ")$Plot)
unique(filter(monitor.correct, Site == "BarTBar")$Plot)
unique(filter(monitor.correct, Site == "FlyingM")$Plot)
unique(filter(monitor.correct, Site == "MOWE")$Plot)
unique(filter(monitor.correct, Site == "PEFO")$Plot)
unique(filter(monitor.correct, Site == "Spiderweb")$Plot)
unique(filter(monitor.correct, Site == "TLE")$Plot)

#   Mojave have 8 extra plots, 37-44
unique(filter(monitor.correct, Site == "29_Palms")$Plot)
unique(filter(monitor.correct, Site == "AVRCD")$Plot)

monitor.correct |> 
  filter(Site == "29_Palms",
         Plot %in% c("37", "38", "39", "40", "41", "42", "43", "44"))

#   Utah all have normal 36
unique(filter(monitor.correct, Site == "CRC")$Plot)
unique(filter(monitor.correct, Site == "UtahPJ")$Plot)
unique(filter(monitor.correct, Site == "Salt_Desert")$Plot)

#   Sonoran SE all have normal 36
unique(filter(monitor.correct, Site == "SRER")$Plot)
unique(filter(monitor.correct, Site == "Patagonia")$Plot)

#   Sonoran Central all have normal 36
unique(filter(monitor.correct, Site == "Roosevelt")$Plot)
unique(filter(monitor.correct, Site == "Preserve")$Plot)
unique(filter(monitor.correct, Site == "Pleasant")$Plot)
unique(filter(monitor.correct, Site == "SCC")$Plot)

#   Chihuahuan have normal 36, but are named starting with 2
unique(filter(monitor.correct, Site == "Creosote")$Plot)
unique(filter(monitor.correct, Site == "Mesquite")$Plot)


# Number of times plots were measured
plots <- count(monitor.correct, Plot) 
#   all seem to have the same number of events as many others except Plots 33, 29, 34


# Number of monitoring events

# CO Plateau
#   Some sites were seeded twice, once in July/Aug of 2018 and again in July/Aug of 2020
#     BabbittPJ, BarTBar, FlyingM, MOWE, PEFO, Spiderweb
# AguaFria 
#   7 monitoring dates, fall 2018 to summer 2019
monitor.correct |> 
  filter(Site == "AguaFria") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)

# BabbittPJ
#   15 monitoring dates, summer/fall 2018 to fall/winter 2021
#   2 seeding dates
monitor.correct |> 
  filter(Site == "BabbittPJ") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)
monitor.correct |> 
  filter(Site == "BabbittPJ") |> 
  count(Date_Seeded) 

# BarTBar
#   14 monitoring dates, fall 2018 to summer 2021
#   2 seeding dates 
monitor.correct |> 
  filter(Site == "BarTBar") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)
monitor.correct |> 
  filter(Site == "BarTBar") |> 
  count(Date_Seeded) 
monitor.correct |> 
  filter(Site == "BarTBar") |> 
  filter(Date_Monitored == "2019-04-16") |> 
  print(n = 37) # Plot 33 is listed twice

# FlyingM
#   14 monitoring dates, fall 2018 to fall/winter 2021
#   2 seeding dates
monitor.correct |> 
  filter(Site == "FlyingM") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)
monitor.correct |> 
  filter(Site == "FlyingM") |> 
  count(Date_Seeded) 

# MOWE
#   12 monitoring dates, fall 2018 to summer 2021
#   2 seeding dates
monitor.correct |> 
  filter(Site == "MOWE") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)
monitor.correct |> 
  filter(Site == "MOWE") |> 
  count(Date_Seeded) 

# PEFO
#   13 monitoring dates, fall 2018 to summer 2021
#   2 seeding dates
monitor.correct |> 
  filter(Site == "PEFO") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)
monitor.correct |> 
  filter(Site == "PEFO") |> 
  count(Date_Seeded) 

# Spiderweb
#   13 monitoring dates, fall 2018 to fall 2021
#   2 seeding dates
monitor.correct |> 
  filter(Site == "Spiderweb") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)
monitor.correct |> 
  filter(Site == "Spiderweb") |> 
  count(Date_Seeded) 
monitor.correct |> 
  filter(Site == "Spiderweb") |> 
  filter(Date_Monitored == "2021-09-29") |> 
  print(n = 37) # Plot 29 is listed twice


# TLE
#   4 monitoring dates, fall 2019 to summer 2021
monitor.correct |> 
  filter(Site == "TLE") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)

# Mojave
# 29_Palms
#   2 monitoring dates, spring 2020, spring 2021
monitor.correct |> 
  filter(Site == "29_Palms") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)

# AVRCD
#   2 monitoring dates, spring 2020, spring 2021
monitor.correct |> 
  filter(Site == "AVRCD") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)


# Utah
# CRC
#   10 monitoring dates, fall/winter 2018 to summer 2021
monitor.correct |> 
  filter(Site == "CRC") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)

# UtahPJ
#   8 monitoring events, fall/winter 2018 to summer 2021
monitor.correct |> 
  filter(Site == "UtahPJ") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)
monitor.correct |> 
  filter(Site == "UtahPJ") |> 
  filter(Date_Monitored == "2019-04-26") |> 
  print(n = 38) # Plots 29 and 34 is listed twice
monitor.correct |> 
  filter(Site == "UtahPJ") |> 
  filter(Date_Monitored == "2019-05-16") |> 
  print(n = 38) # Plots 29 and 34 is listed twice
monitor.correct |> 
  filter(Site == "UtahPJ") |> 
  filter(Date_Monitored == "2019-05-29") |> 
  print(n = 37) # Plot 34 is listed twice
monitor.correct |> 
  filter(Site == "UtahPJ") |> 
  filter(Date_Monitored == "2019-07-02") |> 
  print(n = 38) # Plots 29 and 34 is listed twice

# Salt_Desert
#   9 monitoring events, fall/winter 2018 to summer 2021
monitor.correct |> 
  filter(Site == "Salt_Desert") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)


# Sonoran SE
# SRER
#   5 monitoring dates, fall/winter 2019 to fall 2021
monitor.correct |> 
  filter(Site == "SRER") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)

# Patagonia
#   5 monitoring dates, fall/winter 2019 to fall 2021
monitor.correct |> 
  filter(Site == "Patagonia") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)



# Sonoran Central
# Roosevelt
#   4 monitoring dates, spring 2020 to fall 2021
monitor.correct |> 
  filter(Site == "Roosevelt") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)

# SCC
#   4 monitoring dates, spring 2020 to fall 2021
monitor.correct |> 
  filter(Site == "SCC") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)

# Pleasant
#   4 monitoring dates, spring 2020 to fall 2021
monitor.correct |> 
  filter(Site == "Pleasant") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)

# Preserve
#   4 monitoring dates, spring 2020 to fall 2021
 monitor.correct |> 
  filter(Site == "Preserve") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)


# Chihuahuan
# Creosote
#   7 monitoring dates, fall 2020 to fall 2021
monitor.correct |> 
  filter(Site == "Creosote") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)

# Mesquite
#   6 monitoring dates, fall 2020 to fall 2021
monitor.correct |> 
  filter(Site == "Mesquite") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)



# Resolve plot conflicts --------------------------------------------------


# Create wrong and correct rows by site

# BarTBar
monitor.correct |> 
  filter(Site == "BarTBar",
         Plot == "33") |> 
  count(Treatment)

#   Create wrong and correct rows
wrong.sub.BarTBar <- monitor.correct |> 
  filter(Site == "BarTBar",
         Plot == "33",
         Treatment == "Mulch")
wrong.sub.BarTBar
monitor.correct |> 
  filter(Site == "BarTBar",
         Plot == "33",
         Treatment == "ConMod",
         Date_Monitored == "2019-04-16")
fix.sub.BarTBar <- wrong.sub.BarTBar |> 
  mutate(Treatment = "ConMod",
         MonitorID = 1005)


# Spiderweb
monitor.correct |> 
  filter(Site == "Spiderweb",
         Plot == "29") |> 
  count(PlotMix)

#   Create wrong and correct rows
wrong.sub.Spiderweb <- monitor.correct |> 
  filter(Site == "Spiderweb",
         Plot == "29",
         PlotMix == "Med-Warm")
wrong.sub.Spiderweb
monitor.correct |> 
  filter(Site == "Spiderweb",
         Plot == "29",
         PlotMix == "Warm",
         Date_Monitored == "2021-09-29")
fix.sub.Spiderweb <- wrong.sub.Spiderweb |> 
  mutate(PlotMix = "Warm",
         MonitorID = 3234)
fix.sub.Spiderweb


# UtahPJ
#   Plot 29
monitor.correct |> 
  filter(Site == "UtahPJ",
         Plot == "29") |> 
  count(Treatment)

#   Create wrong and correct rows
wrong.sub.UtahPJ1 <- monitor.correct |> 
  filter(Site == "UtahPJ",
         Plot == "29",
         Treatment == "Pits")
wrong.sub.UtahPJ1
monitor.correct |> 
  filter(Site == "UtahPJ",
         Plot == "29",
         Treatment == "Mulch",
         Date_Monitored %in% wrong.sub.UtahPJ1$Date_Monitored)
fix.sub.UtahPJ1 <- wrong.sub.UtahPJ1 |> 
  mutate(Treatment = "Mulch",
         MonitorID = c(3951, 4025, 4244))
fix.sub.UtahPJ1

#   Plot 34
monitor.correct |> 
  filter(Site == "UtahPJ",
         Plot == "34") |> 
  count(Treatment) # Probably supposed to be Control?

monitor.correct |> 
  filter(Site == "UtahPJ",
         Plot == "34") |> 
  count(PlotMix) # probably should be None

monitor.correct |> 
  filter(Site == "UtahPJ") |> 
  count(Treatment)
monitor.correct |> 
  filter(Site == "Salt_Desert") |> 
  count(Treatment) # compare with correct version
#   There should be 36 control in total; Plot 34 should be Control (and PlotMix should be None)

#   Create wrong and correct rows
wrong.sub.UtahPJ2 <- monitor.correct |> 
  filter(Site == "UtahPJ",
         Plot == "34",
         Treatment == "ConMod")
wrong.sub.UtahPJ2
monitor.correct |> 
  filter(Site == "UtahPJ",
         Plot == "34",
         Treatment == "Control",
         Date_Monitored %in% wrong.sub.UtahPJ2$Date_Monitored)
fix.sub.UtahPJ2 <- wrong.sub.UtahPJ2 |> 
  mutate(Treatment = "Control",
         PlotMix = "None",
         MonitorID = c(3957, 4031, 4140, 4250))
fix.sub.UtahPJ2



# Standardize PlotMix and Treatment spelling ------------------------------

# Treatment
unique(monitor.correct$Treatment)

wrong.sub.conmod <- monitor.correct |> 
  filter(Treatment == "Con/Mod")

fix.sub.conmod <- wrong.sub.conmod |> 
  mutate(Treatment = "ComMod")


# PlotMix
unique(monitor.correct$PlotMix)
#   no fix needed



# All correct monitoring info ---------------------------------------------

# Remove MonitorIDs with wrong plot information
#   The plot ones have already existing correct rows with MonitorID and can be removed
#   The ConMod ones need to be removed and replaced
wrong.leftover <- bind_rows(wrong.sub.BarTBar, 
                            wrong.sub.Spiderweb, 
                            wrong.sub.UtahPJ1, 
                            wrong.sub.UtahPJ2, 
                            wrong.sub.conmod)

monitor.correct <- monitor.correct |> 
  filter(!MonitorID %in% wrong.leftover$MonitorID) |> 
  bind_rows(fix.sub.conmod) |>  # add back corrected ConMod rows; Plot conflicts were duplicates, 
  #                                 but ConMod rows should be replaced
  arrange(MonitorID)


# Check for whole number
(nrow(monitor.correct) - (8 * 4))/ 36
#   subtract out 8 extra plots at Mojave sites * 4 sampling events at Mojave sites

# Write to csv
write_csv(monitor.correct,
          file = "old_pre-2023-09-18_Farrell-data/data/cleaned/corrected-monitoring-info_clean.csv")



# Make subplot tables -----------------------------------------------------

# Compile
wrong.sub <- bind_rows(wrong.sub.AVRCD,
                       wrong.sub.BarTBar,
                       wrong.sub.conmod,
                       wrong.sub.Mesquite,
                       wrong.sub.Patagonia,
                       wrong.sub.Preserve,
                       wrong.sub.Roosevelt,
                       wrong.sub.Salt_Desert,
                       wrong.sub.SCC,
                       wrong.sub.Spiderweb,
                       wrong.sub.UtahPJ1,
                       wrong.sub.UtahPJ2)

fix.sub <- bind_rows(fix.sub.AVRCD,
                       fix.sub.BarTBar,
                       fix.sub.conmod,
                       fix.sub.Mesquite,
                       fix.sub.Patagonia,
                       fix.sub.Preserve,
                       fix.sub.Roosevelt,
                       fix.sub.Salt_Desert,
                       fix.sub.SCC,
                       fix.sub.Spiderweb,
                       fix.sub.UtahPJ1,
                       fix.sub.UtahPJ2)

nrow(wrong.sub) == nrow(fix.sub)


# Write csv of wrong subplot monitor data for later subplot data wrangling
write_csv(wrong.sub,
          file = "old_pre-2023-09-18_Farrell-data/data/data-wrangling-intermediate/02_subplot-wrong-monitor-events.csv")


# Write csv of corrected subplot monitor data
write_csv(fix.sub,
          file = "old_pre-2023-09-18_Farrell-data/data/data-wrangling-intermediate/02_subplot-wrong-monitor-events-corrected.csv")



# Make 2x2 tables ---------------------------------------------------------

# 2x2 plot data
wrong.2x2 <- bind_rows(wrong.2x2.FlyingM,
                       wrong.2x2.Mesquite,
                       wrong.2x2.Pleasant,
                       wrong.2x2.Preserve1,
                       wrong.2x2.Preserve2,
                       wrong.2x2.Roosevelt1,
                       wrong.2x2.Roosevelt2,
                       wrong.2x2.SCC1,
                       wrong.2x2.SCC2,
                       wrong.2x2.seedonly)

fix.2x2 <- bind_rows(fix.2x2.FlyingM,
                     fix.2x2.Mesquite1,
                     fix.2x2.Pleasant,
                     fix.2x2.Preserve1,
                     fix.2x2.Preserve2,
                     fix.2x2.Roosevelt1,
                     fix.2x2.Roosevelt2,
                     fix.2x2.SCC1,
                     fix.2x2.SCC2,
                     fix.2x2.seedonly)

nrow(wrong.2x2) == nrow(fix.2x2)


# Write csv of wrong 2x2 monitor data for later 2x2 data wrangling
write_csv(wrong.2x2,
          file = "old_pre-2023-09-18_Farrell-data/data/data-wrangling-intermediate/02_2x2-wrong-monitor-events.csv")


# Write csv of corrected 2x2 monitor data
write_csv(fix.2x2,
          file = "old_pre-2023-09-18_Farrell-data/data/data-wrangling-intermediate/02_2x2-wrong-monitor-events-corrected.csv")





  

save.image("old_pre-2023-09-18_Farrell-data/RData/02_correct-monitoring-info.RData")
