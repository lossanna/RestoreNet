# Created: 2023-09-18
# Last updated: 2023-09-19

# Purpose: In comparing the monitoring information from the subplot vs. 2x2 plot data, 
#   there were discrepancies, but there should be only one correct version. 
#   This script shows what corrections were made and why, to standardize information 
#   for all the monitoring events. 

# Monitoring events were assigned a MonitorID, where the ID is unique for each plot monitored 
#   at each time point (Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix cols),
#   without taking into account any actual data collection (species present or species measurements).

# Workflow:
#   Create initial MonitorID values based on subplot data (sometimes 2x2 data wasn't always recorded if nothing
#     was growing).
#   Examine conflicts between subplot and 2x2 monitoring info by site:
#     1. Extract differing rows
#     2. Figure out which version is correct
#     3. Extract incorrect rows as a dataframe (will be either subplot or 2x2)
#     4. Extract correct rows as a dataframe (will be either subplot or 2x2)
#   Correct any instances of "Seed only" as a Treatment
#   Look for mistakes in both subplot and 2x2 data (and therefore wasn't previously examined)
#   Resolve Plot conflicts, and standardize spelling for PlotMix and Treatment
#   Write out separate tables of incorrect events and correct events (with correct MonitorID) that must be
#     fixed in subplot and 2x2 data during data wrangling.


library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

p2x2.raw <- read_xlsx("data/raw/2023-09-15_Master 1.0 Germination Data_raw.xlsx", sheet = "AllPlotData")
subplot.raw <- read_xlsx("data/raw/2023-09-15_Master 1.0 Germination Data_raw.xlsx", sheet = "AllSubplotData")


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



# 29_Palms ----------------------------------------------------------------

# 29_Palms (Mojave): 1 issue

# 1. Subplot data not collected in 2022-04-15
# Extract differing rows
filter(monitor.diff, Site == "29_Palms")
filter(monitor.sub, Site == "29_Palms", Date_Monitored == "2022-04-15") # nothing monitored during this date
filter(monitor.2x2, Site == "29_Palms", Date_Monitored == "2022-04-15") # data collected on 2022-04-15

# Figure out which version is correct
#   Even if subplot data wasn't collected, 2x2 should still get MonitorID

# Extract incorrect row for 2x2 wrangling (does not have a MonitorID)
wrong.2x2.29palms <- filter(monitor.2x2, Site == "29_Palms", Date_Monitored == "2022-04-15")

# Create correct row for subplot and 2x2
#   Pick up MonitorID where monitor.sub left off
add.29palms <- monitor.diff |> 
  filter(Site == "29_Palms") |> 
  mutate(MonitorID = 6348:6391)



# AVRCD -------------------------------------------------------------------

# AVRCD (Mojave): 4 issues

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
#   sites were reseeded only in 2020, 2x2 is correct

# Extract incorrect subplot row
wrong.sub.AVRCD2 <- filter(monitor.sub, Site == "AVRCD", Date_Monitored == "2021-04-06") 

# Create correct subplot row 
fix.sub.AVRCD2 <- monitor.sub %>% 
  filter(Site == "AVRCD", Date_Monitored == "2021-04-06")
fix.sub.AVRCD2$Date_Seeded <- rep("2020-03-17", nrow(fix.sub.AVRCD2))


# 3. Plot 2 conflict
# Extract differing rows
filter(monitor.diff, Site == "AVRCD", Plot == "2", Date_Monitored == "2022-04-13")
filter(monitor.sub, Site == "AVRCD", Plot == "2") # Plot 2 monitored twice
filter(monitor.2x2, Site == "AVRCD", Plot == "2") # Plot 2 monitored 3 times, also on 2022-04-13

# Figure out which version is correct
#   2x2 is correct, even if subplots weren't measured there should still be a MonitorID

# Extract incorrect row for 2x2 wrangling (doesn't have a MonitorID)
wrong.2x2.AVRCD1 <- filter(monitor.2x2, Site == "AVRCD", Plot == "2", Date_Monitored == "2022-04-13")

# Create correct subplot and 2x2 row (assign MonitorID)
add.AVRCD1 <- wrong.2x2.AVRCD1 |> 
  mutate(MonitorID = 6392)


# 4. Plot 44 conflict
# Extract differing rows
filter(monitor.diff, Site == "AVRCD", Plot == "44", Date_Monitored == "2022-04-13")
filter(monitor.sub, Site == "AVRCD", Plot == "44") # Plot 44 monitored twice
filter(monitor.2x2, Site == "AVRCD", Plot == "44") # Plot 44 monitored 3 times, also on 2022-04-13

# Figure out which version is correct
#   2x2 is correct, even if subplots weren't measured there should still be a MonitorID

# Extract incorrect row for 2x2 wrangling (doesn't have a MonitorID)
wrong.2x2.AVRCD2 <- filter(monitor.2x2, Site == "AVRCD", Plot == "44", Date_Monitored == "2022-04-13")

# Create correct subplot and 2x2 row (assign MonitorID)
add.AVRCD2 <- wrong.2x2.AVRCD2 |> 
  mutate(MonitorID = 6393)



# Combine AVRCD conflicts
wrong.sub.AVRCD <- bind_rows(wrong.sub.AVRCD1, wrong.sub.AVRCD2)
fix.sub.AVRCD <- bind_rows(fix.sub.AVRCD1, fix.sub.AVRCD2)

wrong.2x2.AVRCD <- bind_rows(wrong.2x2.AVRCD1, wrong.2x2.AVRCD2)
add.AVRCD <- bind_rows(add.AVRCD1, add.AVRCD2)




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

# Extract incorrect row for 2x2 wrangling
wrong.2x2.FlyingM <- filter(monitor.diff, Site == "FlyingM")

# Create correct 2x2 row
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

# Extract incorrect row for 2x2 wrangling
wrong.2x2.Mesquite <- filter(monitor.diff, Site == "Mesquite",
                             Date_Monitored == "2020-12-13")

# Create correct 2x2 row
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



# PEFO --------------------------------------------------------------------

# PEFO (Colorado Plateau): 1 issue

# 1. Date_Seeded conflict
filter(monitor.diff, Site == "PEFO")
filter(monitor.sub, Site == "PEFO", Date_Monitored == "2022-04-12") # Date_Seeded 2020-07-23
filter(monitor.2x2, Site == "PEFO", Date_Monitored == "2022-04-12") # Date_Seeded 2018-08-15

# Figure out which version is correct
#   Entire site was reseeded in 2020, so observations taken in 2022 reflect 2020 seeding
#   Subplot is right

# Create correct row
#   Subplot data already correct; no fix needed

# Extract incorrect row for 2x2 data wrangling
wrong.2x2.PEFO <- filter(monitor.2x2, Site == "PEFO", Date_Monitored == "2022-04-12") 

# Create correct row for 2x2
fix.2x2.PEFO <- filter(monitor.sub, Site == "PEFO", Date_Monitored == "2022-04-12")




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




# Preserve ----------------------------------------------------------------

# Preserve (Sonoran Central): 1 issue

# 1. Date_Seeded conflicting
# Determine differences
count(filter(monitor.sub, Site == "Preserve"), Date_Seeded) # 2019-11-25 
count(filter(monitor.2x2, Site == "Preserve"), Date_Seeded) # 2019-09-25, 2019-11-25

# Figure out correct version
#   Should be just 2019-11-25; subplot is correct

# Create correct rows
#   Subplot data already correct; no fix needed

# Extract incorrect row for 2x2 wrangling
wrong.2x2.Preserve <- monitor.diff |> 
  filter(Site == "Preserve",
         Date_Seeded == "2019-09-25")

# Create correct 2x2 row
fix.2x2.Preserve <- monitor.sub |> 
  filter(Date_Monitored %in% wrong.2x2.Preserve$Date_Monitored,
         Plot %in% wrong.2x2.Preserve$Plot)




# Roosevelt ---------------------------------------------------------------

# Roosevelt (Sonoran Central): 1 issue

# 1. Date_Seeded conflicting
# Determine differences
count(filter(monitor.sub, Site == "Roosevelt"), Date_Seeded) # 2019-11-22 
count(filter(monitor.2x2, Site == "Roosevelt"), Date_Seeded) # 2019-09-22, 2019-11-22

# Figure out correct version
#   2019-11-22 is correct

# Create correct rows
#   Subplot data already correct; no fix needed

# Extract incorrect row for 2x2 wrangling
wrong.2x2.Roosevelt <- monitor.diff |> 
  filter(Site == "Roosevelt",
         Date_Seeded == "2019-09-22")

# Create correct 2x2 row
fix.2x2.Roosevelt <- monitor.sub |> 
  filter(Date_Monitored %in% wrong.2x2.Roosevelt$Date_Monitored,
         Plot %in% wrong.2x2.Roosevelt$Plot)





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

# Extract incorrect row for 2x2 wrangling
wrong.2x2.SCC <- monitor.diff |> 
  filter(Site == "SCC",
         Date_Seeded == "2019-09-21")

# Create correct 2x2 row
fix.2x2.SCC <- monitor.sub |> 
  filter(Date_Monitored %in% wrong.2x2.SCC$Date_Monitored,
         Plot %in% wrong.2x2.SCC$Plot)


# SRER --------------------------------------------------------------------

# SRER (Sonoran SE):

# Extract differing rows
filter(monitor.diff, Site == "SRER")
filter(monitor.sub, Site == "SRER", Date_Monitored == "2022-03-23") # does not include Plot 12
filter(monitor.2x2, Site == "SRER", Date_Monitored == "2022-03-23") # includes Plot 12

# Figure out which version is correct
monitor.sub |> 
  filter(Site == "SRER") |> 
  count(Date_Monitored)

monitor.2x2 |> 
  filter(Site == "SRER") |> 
  count(Date_Monitored)
#   probably split up monitoring equally (12 over 3 days), rather than 11 and 13;
#     2x2 data is correct

# Extract incorrect subplot row
wrong.sub.SRER <- filter(monitor.sub, Site == "SRER", Date_Monitored == "2022-03-24", Plot == "12")

# Create correct row
fix.sub.SRER <- wrong.sub.SRER |> 
  mutate(Date_Monitored = "2022-03-23")




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




# Begin to compile list of correct monitoring info ------------------------

# Combine corrected conflicting monitoring info
fix.sub.conflict <- bind_rows(fix.sub.AVRCD,
                     fix.sub.Mesquite,
                     fix.sub.Patagonia,
                     fix.sub.SRER,
                     fix.sub.Salt_Desert)

# Subplot data
#   Replace monitor info from subplot data with correct info
monitor.correct <- monitor.sub |> 
  filter(!MonitorID %in% fix.sub.conflict$MonitorID) |> 
  bind_rows(fix.sub.conflict) |> 
  arrange(MonitorID)

nrow(monitor.correct) == nrow(monitor.sub)


# Add extra 2x2 monitoring events at 29_Palms and AVRCD
monitor.correct <- bind_rows(monitor.correct, add.29palms, add.AVRCD)


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
         Plot %in% c("37", "38", "39", "40", "41", "42", "43", "44")) # Treatment includes Pellets

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
plots <- count(monitor.correct, Plot) |> 
  arrange(desc(n))
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
#   16 monitoring dates, summer/fall 2018 to spring 2022
#   2 seeding dates
monitor.correct |> 
  filter(Site == "BabbittPJ") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)
monitor.correct |> 
  filter(Site == "BabbittPJ") |> 
  count(Date_Seeded) 

# BarTBar
#   15 monitoring dates, fall 2018 to spring 2022
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
#   15 monitoring dates, fall 2018 to spring 2022
#   2 seeding dates
monitor.correct |> 
  filter(Site == "FlyingM") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)
monitor.correct |> 
  filter(Site == "FlyingM") |> 
  count(Date_Seeded) 

# MOWE
#   13 monitoring dates, fall 2018 to spring 2022
#   2 seeding dates
monitor.correct |> 
  filter(Site == "MOWE") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)
monitor.correct |> 
  filter(Site == "MOWE") |> 
  count(Date_Seeded) 

# PEFO
#   14 monitoring dates, fall 2018 to spring 2022
#   2 seeding dates
monitor.correct |> 
  filter(Site == "PEFO") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)
monitor.correct |> 
  filter(Site == "PEFO") |> 
  count(Date_Seeded) 

# Spiderweb
#   14 monitoring dates, fall 2018 to spring 2022
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
#   3 monitoring dates, spring 2020, spring 2021, spring 2022
monitor.correct |> 
  filter(Site == "29_Palms") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)

# AVRCD
#   3 monitoring dates, spring 2020, spring 2021, spring 2022
monitor.correct |> 
  filter(Site == "AVRCD") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)
monitor.correct |> 
  filter(Site == "AVRCD") |> 
  filter(Date_Monitored == "2022-04-13") |> 
  print(n = 44)


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
#   6 monitoring events, fall/winter 2019 to spring 2022
#     some monitoring events took more than 1 day
monitor.correct |> 
  filter(Site == "SRER") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)

# Patagonia
#   6 monitoring events, fall/winter 2019 to spring 2022
#     some monitoring events took more than 1 day
monitor.correct |> 
  filter(Site == "Patagonia") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)



# Sonoran Central
# Roosevelt
#   5 monitoring dates, spring 2020 to spring 2022
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
#   6 monitoring dates, spring 2020 to spring 2023
monitor.correct |> 
  filter(Site == "Pleasant") |> 
  count(Date_Monitored) |> 
  rename(Plots_Monitored = n)

# Preserve
#   5 monitoring dates, spring 2020 to spring 2022
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
#   These are plots listed twice, and there is already a correct version with a MonitorID
#   The rows with wrong monitoring information have MonitorIDs that will become null
#   But must keep original MonitorID to connect wrong and fix dfs
#     fix dfs will have correct monitoring info but a MonitorID that needs to be replaced
#   Fixing the MonitorID will have to happen after left_joins to fix the monitoring info

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
  mutate(Treatment = "ConMod")
fix.sub.BarTBar # has old MonitorID to be able to connect to incorrect rows
replaceID.BarTBar <- data.frame(MonitorID_old = 1006,
                                MonitorID_replace = 1005)


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
  mutate(PlotMix = "Warm")
fix.sub.Spiderweb
replaceID.Spiderweb <- data.frame(MonitorID_old = 3235,
                                MonitorID_replace = 3234)


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
  mutate(Treatment = "Mulch")
fix.sub.UtahPJ1
replaceID.UtahPJ1 <- data.frame(MonitorID_old = wrong.sub.UtahPJ1$MonitorID,
                                  MonitorID_replace = c(4167, 4241, 4460))


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
         PlotMix = "None")
fix.sub.UtahPJ2
replaceID.UtahPJ2 <- data.frame(MonitorID_old = wrong.sub.UtahPJ2$MonitorID,
                                MonitorID_replace = c(4173, 4247, 4356, 4466))

# Combine replaceID
replaceID <- bind_rows(replaceID.BarTBar, replaceID.Spiderweb, 
                       replaceID.UtahPJ1, replaceID.UtahPJ2)




# Standardize PlotMix and Treatment spelling ------------------------------

# Treatment
unique(monitor.correct$Treatment)
unique(monitor.sub$Treatment) # already fixed `ConMod typo
unique(monitor.2x2$Treatment)

# ConMod
filter(monitor.sub, Treatment == "Con/Mod") == filter(monitor.correct, Treatment == "Con/Mod")
filter(monitor.2x2, Treatment == "Con/Mod") == filter(monitor.correct, Treatment == "Con/Mod")
#   monitor.correct, monitor.sub, and monitor.2x2 are all wrong in the same places
#     same fix needs to applied to all

wrong.conmod <- monitor.correct |> 
  filter(Treatment == "Con/Mod")

fix.conmod <- wrong.conmod |> 
  mutate(Treatment = "ComMod")


# Seed only
filter(monitor.sub, Treatment == "Seed only") == filter(monitor.correct, Treatment == "Seed only")
filter(monitor.2x2, Treatment == "Seed only") == filter(monitor.correct, Treatment == "Seed only")
#   monitor.correct, monitor.sub, and monitor.2x2 are all wrong in the same places
#     same fix needs to applied to all

wrong.seedonly <- monitor.correct |> 
  filter(Treatment == "Seed only")

fix.seedonly <- wrong.seedonly |> 
  mutate(Treatment = "Seed")


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
                            wrong.conmod,
                            wrong.seedonly)

monitor.correct <- monitor.correct |> 
  filter(!MonitorID %in% wrong.leftover$MonitorID) |> 
  bind_rows(fix.conmod, fix.seedonly) |>  # add back corrected ConMod & Seed rows; Plot conflicts were duplicates, 
  #                                 but ConMod & Seed rows should be replaced
  arrange(MonitorID)


# Check for whole number
(nrow(monitor.correct) - (8 * 6))/ 36
#   subtract out 8 extra plots at Mojave sites * 6 sampling events at Mojave sites

# Write to csv
write_csv(monitor.correct,
          file = "data/cleaned/corrected-monitoring-info_clean.csv")



# Make subplot tables -----------------------------------------------------

# Compile
wrong.sub <- bind_rows(wrong.sub.AVRCD,
                       wrong.sub.BarTBar,
                       wrong.sub.Mesquite,
                       wrong.sub.Patagonia,
                       wrong.sub.Salt_Desert,
                       wrong.sub.Spiderweb,
                       wrong.sub.UtahPJ1,
                       wrong.sub.UtahPJ2,
                       wrong.conmod,
                       wrong.seedonly)

fix.sub <- bind_rows(fix.sub.AVRCD,
                     fix.sub.BarTBar,
                     fix.sub.Mesquite,
                     fix.sub.Patagonia,
                     fix.sub.Salt_Desert,
                     fix.sub.Spiderweb,
                     fix.sub.UtahPJ1,
                     fix.sub.UtahPJ2,
                     fix.conmod,
                     fix.seedonly)

nrow(wrong.sub) == nrow(fix.sub)


# Write csv of wrong subplot monitor data for later subplot data wrangling
write_csv(wrong.sub,
          file = "data/data-wrangling-intermediate/02_subplot-wrong-monitor-events.csv")


# Write csv of corrected subplot monitor data
write_csv(fix.sub,
          file = "data/data-wrangling-intermediate/02_subplot-wrong-monitor-events-corrected.csv")



# Make 2x2 tables ---------------------------------------------------------

# 2x2 plot data
wrong.2x2 <- bind_rows(wrong.2x2.29palms,
                       wrong.2x2.AVRCD,
                       wrong.2x2.FlyingM,
                       wrong.2x2.Mesquite,
                       wrong.2x2.PEFO,
                       wrong.2x2.Preserve,
                       wrong.2x2.Roosevelt,
                       wrong.2x2.SCC,
                       wrong.conmod,
                       wrong.seedonly)

fix.2x2 <- bind_rows(add.29palms,
                     add.AVRCD,
                     fix.2x2.FlyingM,
                     fix.2x2.Mesquite,
                     fix.2x2.PEFO,
                     fix.2x2.Preserve,
                     fix.2x2.Roosevelt,
                     fix.2x2.SCC,
                     fix.conmod,
                     fix.seedonly)

nrow(wrong.2x2) == nrow(fix.2x2)


# Write csv of wrong 2x2 monitor data for later 2x2 data wrangling
write_csv(wrong.2x2,
          file = "data/data-wrangling-intermediate/02_2x2-wrong-monitor-events.csv")


# Write csv of corrected 2x2 monitor data
write_csv(fix.2x2,
          file = "data/data-wrangling-intermediate/02_2x2-wrong-monitor-events-corrected.csv")



# Write csv of MonitorIDs that need to be replaced
write_csv(replaceID,
          file = "data/data-wrangling-intermediate/02_MonitorID-replacements.csv")

  

save.image("RData/02_correct-monitoring-info.RData")
