# Created: 2025-01-21
# Last updated: 2025-10-10

# Purpose: In comparing the monitoring information from the subplot vs. 2x2 plot data,
#   there were discrepancies, but there should be only one correct version.
#   This script shows what corrections were made and why, to standardize information
#   for all the monitoring events. I also assigned two monitoring IDs to events based on
#   unique combinations of different columns grouped together. Monitoring IDs do not
#   take into account any of the actual data collection (species present or species measurements).
# Ultimately, there weren't a lot of problems with the monitoring info from the Sonoran sites.

# Workflow:
# 1. Create initial SiteDatePlotID based on subplot data.
# 2. Examine conflicts between subplot and 2x2 monitoring date info by site:
#   a. Extract differing rows.
#   b. Figure out which version is correct.
#   c. Extract incorrect rows as a dataframe, separate for subplot or 2x2 (mistakes are unique to datasets).
#   d. Extract correct rows as a dataframe, separate for subplot or 2x2.
# 3. Examine site characteristics to find other mistakes in both subplot and 2x2 data (and
#     therefore not yet corrected):
#     - Find number of plots by site. [no problems for Sonoran sites]
#     - Find number of sampling events and number of seeding events. [no problems for Sonoran sites]
# 4. Standardize spelling for PlotMix and Treatment.
# 5. Write out complete corrected monitoring info.
# 6. Write out separate tables of incorrect events and correct events (with correct SiteDatePlotID) to be
#     fixed in subplot and 2x2 data during data wrangling.
# 7. Write separate list of SiteDateID.
# 8. Write separate list of SitePlotID.

# Types of monitoring IDs
#   SiteDatePlotID: 1152 total, values of 1-1152
#     Unique combinations of Site, Date_Seeded, Date_Monitored, Plot, Treatment, and Plotmix columns.
#     This is the most specific monitoring IDs get. Observations have the same ID if they were
#     plants growing in the same plot, observed on the same day.
#   SiteDateID: 41 total, values of 1-41
#     Unique combinations of Site, Date_Seeded, Date_Monitored columns.
#     This is used for connecting precipitation data to monitoring events, because precipitation
#     is measured as  precip since the last monitoring event. Sometimes the
#     same SiteDateID does not contain all 36 plots at the site, because sometimes sites took
#     more than one day to monitor. This specificity is retained in determining precipitation.
#     Values range from 1 to 187.
#   SitePlotID: 216 total, values of 1-216
#     Unique combinations of Site, Date_Seeded, Plot columns.
#     This the number of unique plots, and will be used to account for repeat measurements taken from 
#     the same plots by having SitePlotID as a random variable.



library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.se.raw <- read_xlsx("Sonoran-data/raw/Sonoran_2023-09-15_Master 1.0 Germination Data_LO.xlsx", 
                            sheet = "SonoranSE_Subplots_LO")
subplot.cen.raw <- read_xlsx("Sonoran-data/raw/Sonoran_2023-09-15_Master 1.0 Germination Data_LO.xlsx", 
                             sheet = "SonoranCentral_Subplots_LO")
plot.2x2.se.raw <- read_xlsx("Sonoran-data/raw/Sonoran_2023-09-15_Master 1.0 Germination Data_LO.xlsx",
                             sheet = "SonoranSE_Plots_LO")
plot.2x2.cen.raw <- read_xlsx("Sonoran-data/raw/Sonoran_2023-09-15_Master 1.0 Germination Data_LO.xlsx",
                              sheet = "SonoranCentral_Plots_LO")


# Set up subplot data -----------------------------------------------------

# Convert cols to character and combine all Sonoran Desert sites 
subplot.se <- subplot.se.raw %>% 
  mutate(across(everything(), as.character))
subplot.cen <- subplot.cen.raw %>% 
  mutate(across(everything(), as.character))
subplot <- bind_rows(subplot.se, subplot.cen)

# Narrow down subplot columns
subplot <- subplot %>%
  rename(PlotMix = Seed_Mix) %>%
  select(Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix) %>%
  distinct(.keep_all = TRUE) 

# Add Region col
subplot <- subplot %>%
  mutate(Region = case_when(
    str_detect(subplot$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(subplot$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central")) %>%
  select(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix)


# Set up 2x2 plot data ----------------------------------------------------

# Convert cols to character and combine all Sonoran Desert sites 
p2x2.se <- plot.2x2.se.raw %>% 
  mutate(across(everything(), as.character))
p2x2.cen <- plot.2x2.cen.raw %>% 
  mutate(across(everything(), as.character))
p2x2 <- bind_rows(p2x2.cen, p2x2.se) 

# Narrow down cols
p2x2 <- p2x2 %>%
  rename(PlotMix = Seed_Mix) %>%
  select(Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix) %>%
  distinct(.keep_all = TRUE) %>% 
  filter(!is.na(Site))

# Add Region col
p2x2 <- p2x2 %>%
  mutate(Region = case_when(
    str_detect(p2x2$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(p2x2$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central")) %>%
  select(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix)



# Remove incomplete monitoring events -------------------------------------

# For two monitoring events, not all subplots were sampled, and presence of seeded species
#   only was recorded for 2x2 plots. 

# 2022-10-06 at Pleasant and 2022-10-05 at Roosevelt
subplot <- subplot %>% 
  filter(!Date_Monitored %in% c("2022-10-06", "2022-10-05"))

p2x2 <- p2x2 %>% 
  filter(!Date_Monitored %in% c("2022-10-06", "2022-10-05"))


# Assign SiteDatePlotID ---------------------------------------------------

# Assign monitoring events a (preliminary) ID based on subplot monitoring info
#   Use subplot monitoring info because some monitoring events were not recorded in 2x2 data
monitor.sub <- subplot %>%
  mutate(across(everything(), as.character)) %>%
  mutate(SiteDatePlotID = 1:nrow(subplot))

# Add monitoring IDs to 2x2 plot monitoring information
monitor.2x2 <- p2x2 %>%
  left_join(monitor.sub)


# Fix monitoring dates ----------------------------------------------------

# Fix any wrong Date_Seeded, Date_Monitored, Plot, Treatment, or PlotMix
#   Create "wrong" dataframes where the wrong info will be matched to the correct
#     info via the correct SiteDatePlotID during data wrangling.
#   Create "fixed" dataframes that connect rows to the correct info via the SiteDatePlotID.


## Examine conflicts between subplot & 2x2 monitoring info -----------------

# Examine differences by looking at NAs formed from left_join()
monitor.diff <- monitor.2x2 %>%
  filter(is.na(SiteDatePlotID)) %>%
  arrange(Site)

# Need to manually inspect monitor.diff for date differences by site, then write df of
#   corresponding rows using subplot monitoring info. Differences are identified
#   by NA for SitePlotDateID in monitor.diff.
# Compare monitor.diff (2x2 monitoring info) values with relevant obs from
#   subplot monitoring info to determine correct value.
# Correct subplot monitoring info when needed; use subplot data as base for a complete list of
#   monitoring events because it has SiteDatePlotID attached.
# Create wrong and fixed dfs for subplot data when 2x2 data is correct.

# Standardization for spelling of Treatment and PloxMix will happen later.



## Patagonia ---------------------------------------------------------------

# Patagonia (Sonoran SE): 0 date issues


## SRER --------------------------------------------------------------------

# SRER (Sonoran SE): 1 date issue

# 1. Date_Monitored conflicting
# Extract differing rows
filter(monitor.diff, Site == "SRER")
filter(monitor.sub, Site == "SRER", Date_Monitored == "2022-03-23") # does not include Plot 12
filter(monitor.2x2, Site == "SRER", Date_Monitored == "2022-03-23") # includes Plot 12

# Figure out which version is correct
monitor.sub %>%
  filter(Site == "SRER") %>%
  count(Date_Monitored)

monitor.2x2 %>%
  filter(Site == "SRER") %>%
  count(Date_Monitored)
#   probably split up monitoring equally (12 over 3 days), rather than 11 and 13 (and even if
#     they did not, it's easiest to have the same monitoring event info for both subplot and 2x2 for 
#     the same plot, and one day really does not make any difference);
#   2x2 data is correct

# Extract incorrect subplot row
wrong.sub.SRER <- filter(monitor.sub, Site == "SRER", Date_Monitored == "2022-03-24", Plot == "12")

# Create correct row
fix.sub.SRER <- wrong.sub.SRER %>%
  mutate(Date_Monitored = "2022-03-23")


## Pleasant ---------------------------------------------------------------

# Pleasant (Sonoran Central): 0 date issues


## Preserve ----------------------------------------------------------------

# Preserve (Sonoran Central): 1 date issue 

# 1. Date_Seeded conflicting
# Determine differences
count(filter(monitor.sub, Site == "Preserve"), Date_Seeded) # 2019-11-25
count(filter(monitor.2x2, Site == "Preserve"), Date_Seeded) # 2019-09-25, 2019-11-25

# Figure out correct version
#   Should be just 2019-11-25; subplot is correct

# Create correct rows
#   Subplot data already correct; no fix needed

# Extract incorrect row for 2x2 wrangling
wrong.2x2.Preserve <- monitor.diff %>%
  filter(Site == "Preserve",
         Date_Seeded == "2019-09-25")

# Create correct 2x2 row
fix.2x2.Preserve <- monitor.sub %>%
  filter(Date_Monitored %in% wrong.2x2.Preserve$Date_Monitored,
         Plot %in% wrong.2x2.Preserve$Plot)

# Add SiteDatePlotID to wrong row so it can connect to fixed df
wrong.2x2.Preserve$SiteDatePlotID <- fix.2x2.Preserve$SiteDatePlotID


## Roosevelt ---------------------------------------------------------------

# Roosevelt (Sonoran Central): 1 date issue

# 1. Date_Seeded conflicting
# Determine differences
count(filter(monitor.sub, Site == "Roosevelt"), Date_Seeded) # 2019-11-22
count(filter(monitor.2x2, Site == "Roosevelt"), Date_Seeded) # 2019-09-22, 2019-11-22

# Figure out correct version
#   2019-11-22 is correct

# Create correct rows
#   Subplot data already correct; no fix needed

# Extract incorrect row for 2x2 wrangling
wrong.2x2.Roosevelt <- monitor.diff %>%
  filter(Site == "Roosevelt",
         Date_Seeded == "2019-09-22")

# Create correct 2x2 row
fix.2x2.Roosevelt <- monitor.sub %>%
  filter(Date_Monitored %in% wrong.2x2.Roosevelt$Date_Monitored,
         Plot %in% wrong.2x2.Roosevelt$Plot)

# Add SiteDatePlotID to wrong row so it can connect to fixed df
wrong.2x2.Roosevelt$SiteDatePlotID <- fix.2x2.Roosevelt$SiteDatePlotID


## SCC ---------------------------------------------------------------------

# SCC (Sonoran Central): 1 date issue

# 1. Date_Seeded conflicting
# Determine differences
count(filter(monitor.sub, Site == "SCC"), Date_Seeded) # 2019-11-21
count(filter(monitor.2x2, Site == "SCC"), Date_Seeded) # 2019-09-21, 2019-11-21

# Figure out correct version
#   Subplot data is correct, should be 2019-11-21

# Create correct rows
#   Subplot data already correct; no fix needed

# Extract incorrect row for 2x2 wrangling
wrong.2x2.SCC <- monitor.diff %>%
  filter(Site == "SCC", 
         Date_Seeded == "2019-09-21")

# Create correct 2x2 row
fix.2x2.SCC <- monitor.sub %>%
  filter(Date_Monitored %in% wrong.2x2.SCC$Date_Monitored,
         Plot %in% wrong.2x2.SCC$Plot)

# Add SiteDatePlotID to wrong row so it can connect to fixed df
wrong.2x2.SCC$SiteDatePlotID <- fix.2x2.SCC$SiteDatePlotID



# Begin to compile list of correct monitoring info ------------------------

# Create monitor.correct based on subplot data
monitor.correct <- monitor.sub %>%
  filter(!SiteDatePlotID %in% fix.sub.SRER$SiteDatePlotID) %>%
  bind_rows(fix.sub.SRER) %>%
  arrange(SiteDatePlotID)

nrow(monitor.correct) == nrow(monitor.sub) # check for matching lengths



# Look for mistakes in compiled monitoring data ---------------------------

# Before, we had assumed that all the subplot monitoring information that matched the 2x2 data
#   was correct, but it actually has mistakes (it was wrong in both subplot and 2x2 data,
#   so no conflict was created, which is how we determined mistakes in the previous section).
# Ultimately found that there were a few instances where the Plot number was wrong,
#   but a correct row already existed (with its own correct SiteDatePlotID).
# (no objects were modified or created in this section)

# Look for mistakes in currently compiled monitoring info by examining each site
unique(monitor.correct$Site)


## Number of plots by site -------------------------------------------------

# Sonoran SE all have normal 36
unique(filter(monitor.correct, Site == "SRER")$Plot)
unique(filter(monitor.correct, Site == "Patagonia")$Plot)

# Sonoran Central all have normal 36
unique(filter(monitor.correct, Site == "Roosevelt")$Plot)
unique(filter(monitor.correct, Site == "Preserve")$Plot)
unique(filter(monitor.correct, Site == "Pleasant")$Plot)
unique(filter(monitor.correct, Site == "SCC")$Plot)


# Number of times plots were measured
count(monitor.correct, Plot) %>%
  arrange(desc(n)) %>%
  print(n = 36)
#   all seem to have the same number of events as many others 



## Number of monitoring events and seeding by site -------------------------

# Sonoran SE
# SRER
#   6 monitoring events, fall/winter 2019 to spring 2022
#     some monitoring events took more than 1 day
monitor.correct %>%
  filter(Site == "SRER") %>%
  count(Date_Monitored) %>%
  rename(Plots_Monitored = n)
monitor.correct %>%
  filter(Site == "SRER") %>%
  count(Date_Seeded)

# Patagonia
#   6 monitoring events, fall/winter 2019 to spring 2022
#     some monitoring events took more than 1 day
monitor.correct %>%
  filter(Site == "Patagonia") %>%
  count(Date_Monitored) %>%
  rename(Plots_Monitored = n)
monitor.correct %>%
  filter(Site == "Patagonia") %>%
  count(Date_Seeded)


# Sonoran Central
# Roosevelt
#   5 monitoring dates, spring 2020 to spring 2022
monitor.correct %>%
  filter(Site == "Roosevelt") %>%
  count(Date_Monitored) %>%
  rename(Plots_Monitored = n)
monitor.correct %>%
  filter(Site == "Roosevelt") %>%
  count(Date_Seeded)

# SCC
#   4 monitoring dates, spring 2020 to fall 2021
monitor.correct %>%
  filter(Site == "SCC") %>%
  count(Date_Monitored) %>%
  rename(Plots_Monitored = n)
monitor.correct %>%
  filter(Site == "SCC") %>%
  count(Date_Seeded)

# Pleasant
#   6 monitoring dates, spring 2020 to spring 2023
monitor.correct %>%
  filter(Site == "Pleasant") %>%
  count(Date_Monitored) %>%
  rename(Plots_Monitored = n)
monitor.correct %>%
  filter(Site == "Pleasant") %>%
  count(Date_Seeded)

# Preserve
#   5 monitoring dates, spring 2020 to spring 2022
monitor.correct %>%
  filter(Site == "Preserve") %>%
  count(Date_Monitored) %>%
  rename(Plots_Monitored = n)
monitor.correct %>%
  filter(Site == "Preserve") %>%
  count(Date_Seeded)



# Standardize PlotMix and Treatment spelling ------------------------------

# Unlike Plot number, correct duplicates do not exist, so there is no correct SiteDatePlotID
#   to reassign. Instead, must change info of the current SiteDatePlotID.

# PlotMix
unique(monitor.correct$PlotMix)
#   no fix needed

# Treatment
unique(monitor.correct$Treatment)
unique(monitor.sub$Treatment)
unique(monitor.2x2$Treatment)

# Problems
#   Should be "Seed", not "Seed only"


## Seed only ---------------------------------------------------------------

# Look to see where issues exist
filter(monitor.sub, Treatment == "Seed only") == 
  filter(monitor.correct, Treatment == "Seed only") # same issues between monitor.sub and monitor.correct
nrow(filter(monitor.2x2, Treatment == "Seed only")) == 
  nrow(filter(monitor.sub, Treatment == "Seed only")) # different issues between monitor.2x2 and monitor.sub

# Create wrong row for subplot & monitor.correct
wrong.seedonly.sub <- monitor.sub %>% 
  filter(Treatment == "Seed only")
#   monitor.correct and monitor.sub are wrong in the same places (everything is TRUE);
#     same fix needs to be applied to both in 48 instances

# Create wrong row for 2x2
wrong.seedonly.2x2 <- monitor.2x2 %>% 
  filter(Treatment == "Seed only")
setdiff(wrong.seedonly.2x2$SiteDatePlotID, wrong.seedonly.sub$SiteDatePlotID)
setdiff(wrong.seedonly.sub$SiteDatePlotID, wrong.seedonly.2x2$SiteDatePlotID)
#   There are an additional 48 rows in monitor.2x2 that need to be corrected (have NA for SiteDatePlotID)


# Create correct row for subplot & monitor.correct
fix.seedonly.sub <- wrong.seedonly.sub %>%
  mutate(Treatment = "Seed")

# Create correct row for 2x2
fix.seedonly.2x2 <- wrong.seedonly.2x2 %>% 
  mutate(Treatment = "Seed")


# Check to make sure there isn't already a correct version in monitor.correct
#   Find what already exists in monitor.correct that is wrong
a <- monitor.correct %>%
  filter(Site %in% fix.seedonly.sub$Site,
         Date_Monitored %in% fix.seedonly.sub$Date_Monitored,
         Treatment == "Seed only")

#   Find what already exists in monitor.correct that is correct
b <- monitor.correct %>%
  filter(Site %in% fix.seedonly.sub$Site,
         Date_Monitored %in% fix.seedonly.sub$Date_Monitored,
         Treatment == "Seed") 

#   Look for overlap
intersect(a, b) # no duplicates, correct version does not already exist


## Make correction ---------------------------------------------------------

# Replace rows in monitor.correct with corrected info (all must be replaced)
wrong.spelling.id <- wrong.seedonly.sub$SiteDatePlotID
monitor.correct <- monitor.correct %>%
  filter(!SiteDatePlotID %in% wrong.spelling.id) %>%
  bind_rows(fix.seedonly.sub) %>%
  arrange(SiteDatePlotID)


# Remove temporary objects
rm(a, b)



# All correct monitoring info ---------------------------------------------

# Check for whole number
nrow(monitor.sub) / 36

# Write to csv
write_csv(monitor.correct,
          file = "Sonoran-data/cleaned/02_corrected-monitoring-info_clean.csv")


# Make subplot tables -----------------------------------------------------

# Compile
wrong.sub <- bind_rows(wrong.sub.SRER,
                       wrong.seedonly.sub)

fix.sub <- bind_rows(fix.sub.SRER,
                     fix.seedonly.sub)

nrow(wrong.sub) == nrow(fix.sub) # check for matching lengths


# Write csv of wrong subplot monitor data for later subplot data wrangling
write_csv(wrong.sub,
  file = "Sonoran-data/data-wrangling-intermediate/02_subplot-wrong-monitor-events.csv")


# Write csv of corrected subplot monitor data
write_csv(fix.sub,
  file = "Sonoran-data/data-wrangling-intermediate/02_subplot-wrong-monitor-events-corrected.csv")



# Make 2x2 tables ---------------------------------------------------------

# 2x2 plot data
wrong.2x2 <- bind_rows(wrong.2x2.Preserve,
                       wrong.2x2.Roosevelt,
                       wrong.2x2.SCC,  
                       wrong.seedonly.2x2)

fix.2x2 <- bind_rows(fix.2x2.Preserve,
                     fix.2x2.Roosevelt,
                     fix.2x2.SCC,
                     fix.seedonly.2x2)

nrow(wrong.2x2) == nrow(fix.2x2)


# Write csv of wrong 2x2 monitor data for later 2x2 data wrangling
write_csv(wrong.2x2,
          file = "Sonoran-data/data-wrangling-intermediate/02_2x2-wrong-monitor-events.csv")


# Write csv of corrected 2x2 monitor data
write_csv(fix.2x2,
          file = "Sonoran-data/data-wrangling-intermediate/02_2x2-wrong-monitor-events-corrected.csv")


# Create list of SiteDateID -----------------------------------------------

# Narrow down columns
monitor.site <- monitor.correct %>%
  select(Region, Site, Date_Seeded, Date_Monitored) %>%
  distinct(.keep_all = TRUE)

# Add SiteDateID
monitor.site <- monitor.site %>%
  arrange(Date_Monitored) %>%
  arrange(Site) %>%
  arrange(Region) %>%
  mutate(SiteDateID = 1:nrow(monitor.site))

# Write csv
write_csv(monitor.site,
  file = "Sonoran-data/cleaned/02_SiteDateID_clean.csv")



# Create list of SitePlotID -----------------------------------------------

# Might be used as a random effect to account for repeat measures on the same plots.

siteplot.id <- monitor.correct %>% 
  select(Region, Site, Date_Seeded, Plot) %>% 
  distinct(.keep_all = TRUE)
siteplot.id <- siteplot.id %>% 
  mutate(SitePlotID = 1:nrow(siteplot.id))

write_csv(siteplot.id,
          file = "Sonoran-data/cleaned/02_SitePlotID_clean.csv")



save.image("Sonoran-RData/02_correct-monitoring-info.RData")
