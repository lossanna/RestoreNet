# Created: 2025-01-21
# Last updated: 2025-01-21

# Purpose: In comparing the monitoring information from the subplot vs. 2x2 plot data,
#   there were discrepancies, but there should be only one correct version.
#   This script shows what corrections were made and why, to standardize information
#   for all the monitoring events. I also assigned two monitoring IDs to events based on
#   unique combinations of different columns grouped together. Monitoring IDs do not
#   take into account any of the actual data collection (species present or species measurements).
# Ultimately, there weren't a lot of problems with the monitoring info from the Sonoran sites.

# Workflow:
# 1. Create initial SiteDatePlotID based on subplot data (sometimes 2x2 data wasn't always recorded if nothing
#     was growing).
# 2. Examine conflicts between subplot and 2x2 monitoring info by site:
#   a. Extract differing rows.
#   b. Figure out which version is correct.
#   c. Extract incorrect subplot rows as a dataframe.
#   d. Extract correct subplot rows as a dataframe.
# 3. Examine site characteristics to find other mistakes in both subplot and 2x2 data (and
#     therefore not yet corrected):
#     - Find number of plots by site. [no problems for Sonoran sites]
#     - Find number of sampling events and number of seeding events. [no problems for Sonoran sites]
# 4. Fix problems found in section 3. [not needed for Sonoran sites]
# 5. Standardize spelling for PlotMix and Treatment:
#     In this case, there wasn't a correct version that already existed, so no null SiteDatePlotIDs
#     were created.
# 6. Write out complete corrected monitoring info.
# 7. Write out separate tables of incorrect events and correct events (with correct SiteDatePlotID) to be
#     fixed in subplot data during data wrangling.
# 8. Write separate list of SiteDateID.
# 9. Write separate list of SitePlotID.

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

p2x2.raw <- read_xlsx("data/raw/2023-09-15_Master 1.0 Germination Data_raw.xlsx", sheet = "AllPlotData")
subplot.raw <- read_xlsx("data/raw/2023-09-15_Master 1.0 Germination Data_raw.xlsx", sheet = "AllSubplotData")


# Set up subplot data -----------------------------------------------------

# Narrow down subplot columns
subplot <- subplot.raw %>%
  rename(PlotMix = Seed_Mix) %>%
  select(Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix) %>%
  distinct(.keep_all = TRUE) |> 
  filter(Site %in% c("SRER", "Patagonia", "Roosevelt", "SCC", "Pleasant", "Preserve"))

# Add Region col
subplot <- subplot %>%
  mutate(Region = case_when(
    str_detect(subplot$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(subplot$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central")) |>
  select(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix) %>%
  mutate(across(everything(), as.character))


# Set up 2x2 plot data ----------------------------------------------------

# Narrow down cols
p2x2 <- p2x2.raw %>%
  rename(PlotMix = Seed_Mix) %>%
  select(Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix) %>%
  distinct(.keep_all = TRUE) |> 
  filter(Site %in% c("SRER", "Patagonia", "Roosevelt", "SCC", "Pleasant", "Preserve"))

# Add Region
p2x2 <- p2x2 |>
  mutate(Region = case_when(
    str_detect(p2x2$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(p2x2$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central")) |>
  select(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix) %>%
  mutate(across(everything(), as.character))


# Assign SiteDatePlotID ---------------------------------------------------

# Assign monitoring events a (preliminary) ID based on subplot monitoring info
#   Use subplot monitoring info because some monitoring events were not recorded in 2x2 data
monitor.sub <- subplot %>%
  mutate(across(everything(), as.character)) %>%
  mutate(SiteDatePlotID = 1:nrow(subplot))

# Add monitoring IDs to 2x2 plot monitoring information
monitor.2x2 <- p2x2 %>%
  mutate(across(everything(), as.character)) %>%
  left_join(monitor.sub)



# Fix monitoring info -----------------------------------------------------

# Fix any wrong Date_Seeded, Date_Monitored, Plot, Treatment, or PlotMix
#   Create "wrong" dataframes where the wrong info will be matched to the correct
#     info via the correct SiteDatePlotID during data wrangling.
#   Create "fixed" dataframes that connect rows to the correct info via the SiteDatePlotID.


## Examine conflicts between subplot & 2x2 monitoring info -----------------

# Examine differences by looking at NAs formed from left_join()
monitor.diff <- monitor.2x2 %>%
  filter(is.na(SiteDatePlotID)) %>%
  arrange(Site)

# Need to manually inspect monitor.diff for differences by site, then write df of
#   corresponding rows using subplot monitoring info. Differences are identified
#   by NA for SitePlotDateID in monitor.diff.
# Compare monitor.diff (2x2 monitoring info) values with relevant obs from
#   subplot monitoring info to determine correct value.
# Correct subplot monitoring info when needed; use subplot data as base for a complete list of
#   monitoring events because it has SiteDatePlotID attached.
# Create wrong and fixed dfs for subplot data when 2x2 data is correct.





## Patagonia ---------------------------------------------------------------

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



## Preserve ----------------------------------------------------------------

# Preserve (Sonoran Central): 1 issue

# 1. Date_Seeded conflicting
# Determine differences
count(filter(monitor.sub, Site == "Preserve"), Date_Seeded) # 2019-11-25
count(filter(monitor.2x2, Site == "Preserve"), Date_Seeded) # 2019-09-25, 2019-11-25

# Figure out correct version
#   Should be just 2019-11-25; subplot is correct

# Create correct rows
#   Subplot data already correct; no fix needed



## Roosevelt ---------------------------------------------------------------

# Roosevelt (Sonoran Central): 1 issue

# 1. Date_Seeded conflicting
# Determine differences
count(filter(monitor.sub, Site == "Roosevelt"), Date_Seeded) # 2019-11-22
count(filter(monitor.2x2, Site == "Roosevelt"), Date_Seeded) # 2019-09-22, 2019-11-22

# Figure out correct version
#   2019-11-22 is correct

# Create correct rows
#   Subplot data already correct; no fix needed



## SCC ---------------------------------------------------------------------

# SCC (Sonoran Central): 2 issues

# 1. Date_Seeded conflict
# Determine differences
count(filter(monitor.sub, Site == "SCC"), Date_Seeded) # 2019-11-21
count(filter(monitor.2x2, Site == "SCC"), Date_Seeded) # 2019-09-21, 2019-11-21

# Figure out correct version
#   Subplot data is correct, should be 2019-11-21)

# Create correct rows
#   Subplot data already correct; no fix needed



## SRER --------------------------------------------------------------------

# SRER (Sonoran SE): 1 issue

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




# Begin to compile list of correct monitoring info ------------------------

# Combine corrected conflicting monitoring info
fix.sub.conflict <- bind_rows(
  fix.sub.Patagonia,
  fix.sub.SRER)

# Subplot data
#   Replace monitor info from subplot data with correct info
monitor.correct <- monitor.sub |>
  filter(!SiteDatePlotID %in% fix.sub.conflict$SiteDatePlotID) |>
  bind_rows(fix.sub.conflict) |>
  arrange(SiteDatePlotID)

nrow(monitor.correct) == nrow(monitor.sub)



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
count(monitor.correct, Plot) |>
  arrange(desc(n)) |>
  print(n = 36)
#   all seem to have the same number of events as many others 



## Number of monitoring events and seeding by site -------------------------

# Sonoran SE
# SRER
#   6 monitoring events, fall/winter 2019 to spring 2022
#     some monitoring events took more than 1 day
monitor.correct |>
  filter(Site == "SRER") |>
  count(Date_Monitored) |>
  rename(Plots_Monitored = n)
monitor.correct |>
  filter(Site == "SRER") |>
  count(Date_Seeded)

# Patagonia
#   6 monitoring events, fall/winter 2019 to spring 2022
#     some monitoring events took more than 1 day
monitor.correct |>
  filter(Site == "Patagonia") |>
  count(Date_Monitored) |>
  rename(Plots_Monitored = n)
monitor.correct |>
  filter(Site == "Patagonia") |>
  count(Date_Seeded)


# Sonoran Central
# Roosevelt
#   5 monitoring dates, spring 2020 to spring 2022
monitor.correct |>
  filter(Site == "Roosevelt") |>
  count(Date_Monitored) |>
  rename(Plots_Monitored = n)
monitor.correct |>
  filter(Site == "Roosevelt") |>
  count(Date_Seeded)

# SCC
#   4 monitoring dates, spring 2020 to fall 2021
monitor.correct |>
  filter(Site == "SCC") |>
  count(Date_Monitored) |>
  rename(Plots_Monitored = n)
monitor.correct |>
  filter(Site == "SCC") |>
  count(Date_Seeded)

# Pleasant
#   6 monitoring dates, spring 2020 to spring 2023
monitor.correct |>
  filter(Site == "Pleasant") |>
  count(Date_Monitored) |>
  rename(Plots_Monitored = n)
monitor.correct |>
  filter(Site == "Pleasant") |>
  count(Date_Seeded)

# Preserve
#   5 monitoring dates, spring 2020 to spring 2022
monitor.correct |>
  filter(Site == "Preserve") |>
  count(Date_Monitored) |>
  rename(Plots_Monitored = n)
monitor.correct |>
  filter(Site == "Preserve") |>
  count(Date_Seeded)





# Standardize PlotMix and Treatment spelling ------------------------------

# Unlike Plot number, correct duplicates do not exist, so there is no correct SiteDatePlotID
#   to reassign. Instead, must change info of the current SiteDatePlotID.

# PlotMix
unique(monitor.correct$PlotMix)
#   no fix needed

# Treatment
unique(monitor.correct$Treatment)
unique(monitor.sub$Treatment) # already fixed `ConMod typo
unique(monitor.2x2$Treatment)

# Problems
# Should be "Seed", not "Seed only"
# (Already fixed `ConMod typo, it conflicted between subplot and 2x2 data and was
#   fixed in Patagonia section.)


## Seed only ---------------------------------------------------------------

# Look to see where issues exist
filter(monitor.sub, Treatment == "Seed only") == filter(monitor.correct, Treatment == "Seed only")
filter(monitor.2x2, Treatment == "Seed only") == filter(monitor.correct, Treatment == "Seed only")
#   monitor.correct, monitor.sub, and monitor.2x2 are all wrong in the same places
#     (everything is TRUE); same fix needs to applied to all

# Create wrong row
wrong.seedonly <- monitor.correct |>
  filter(Treatment == "Seed only")

# Create correct row
fix.seedonly <- wrong.seedonly |>
  mutate(Treatment = "Seed")

# Check to make sure there isn't already a correct version in monitor.correct
#   Find what already exists in monitor.correct that is correct
a <- monitor.correct |>
  filter(
    Site %in% fix.seedonly$Site,
    Date_Monitored %in% fix.seedonly$Date_Monitored,
    Treatment == "Seed only"
  )

#   Find what already exists in monitor.correct that is wrong
b <- monitor.correct |>
  filter(
    Site %in% fix.seedonly$Site,
    Date_Monitored %in% fix.seedonly$Date_Monitored,
    Treatment == "Seed"
  ) # no duplicates

#   Look for overlap
intersect(a, b) # no duplicates, correct version does not already exist


## Make correction ---------------------------------------------------------

# Replace rows in monitor.correct with corrected info (all must be replaced)
wrong.spelling.id <- wrong.seedonly$SiteDatePlotID
monitor.correct <- monitor.correct |>
  filter(!SiteDatePlotID %in% wrong.spelling.id) |>
  bind_rows(fix.seedonly) |>
  arrange(SiteDatePlotID)


# Remove temporary objects
rm(a, b)



# All correct monitoring info ---------------------------------------------

# Check for whole number
nrow(monitor.correct) / 36

# Write to csv
write_csv(monitor.correct,
          file = "Sonoran-data/cleaned/02_corrected-monitoring-info_clean.csv")


# Make subplot tables -----------------------------------------------------

# Compile
wrong.sub <- bind_rows(
  wrong.sub.Patagonia,
  wrong.sub.SRER,
  wrong.seedonly)

fix.sub <- bind_rows(
  fix.sub.Patagonia,
  fix.sub.SRER,
  fix.seedonly)

nrow(wrong.sub) == nrow(fix.sub)


# Write csv of wrong subplot monitor data for later subplot data wrangling
write_csv(wrong.sub,
  file = "Sonoran-data/data-wrangling-intermediate/02_subplot-wrong-monitor-events.csv")


# Write csv of corrected subplot monitor data
write_csv(fix.sub,
  file = "Sonoran-data/data-wrangling-intermediate/02_subplot-wrong-monitor-events-corrected.csv")




# Create list of SiteDateID -----------------------------------------------

# Narrow down columns
monitor.site <- monitor.correct |>
  select(Region, Site, Date_Seeded, Date_Monitored) |>
  distinct(.keep_all = TRUE)

# Add SiteDateID
monitor.site <- monitor.site |>
  arrange(Date_Monitored) |>
  arrange(Site) |>
  arrange(Region) |>
  mutate(SiteDateID = 1:nrow(monitor.site))

# Write csv
write_csv(monitor.site,
  file = "Sonoran-data/cleaned/02_SiteDateID_clean.csv")



# Create list of SitePlotID -----------------------------------------------

# Will be used as a random effect to account for repeat measures on the same plots.

siteplot.id <- monitor.correct |> 
  select(Region, Site, Date_Seeded, Plot) |> 
  distinct(.keep_all = TRUE)
siteplot.id <- siteplot.id |> 
  mutate(SitePlotID = 1:nrow(siteplot.id))

write_csv(siteplot.id,
          file = "Sonoran-data/cleaned/02_SitePlotID_clean.csv")



save.image("Sonoran-RData/02_correct-monitoring-info.RData")
