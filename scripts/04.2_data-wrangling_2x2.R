# Created: 2023-09-18
# Last updated: 2024-03-04

# Purpose: Create 2 clean data tables for 2x2 plot data: one with cover, species richness, 
#   and PlantSource data (one row for each monitoring event/SiteDatePlotID), and one with 
#  the list of species present for each monitoring event (can have multiple rows for SiteDatePlotID).

#  Ensure corrected and standardized species information, and monitoring and plot information,
#   and SpeciesSeeded column based on site-specific seed mixes and plot.
# Note that a lot of the Utah plots did not have Additional Species in Plot recorded, but did
#   have cover data. These plots were removed from dataset since they weren't full observations.


library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

p2x2.raw <- read_xlsx("data/raw/2023-09-15_Master 1.0 Germination Data_raw.xlsx", sheet = "AllPlotData")
species.in <- read_csv("data/cleaned/01_p2x2_species-list_location-independent_clean.csv")
species.de <- read_csv("data/cleaned/01_p2x2_species-list_location-dependent_clean.csv")
mix <- read_xlsx("data/raw/from-Master_seed-mix_LO.xlsx", sheet = "with-site_R")
monitor.info <- read_csv("data/cleaned/02_corrected-monitoring-info_clean.csv")
monitor.wrong <- read_csv("data/data-wrangling-intermediate/02_p2x2-wrong-monitor-events.csv")
monitor.fixed <- read_csv("data/data-wrangling-intermediate/02_p2x2-wrong-monitor-events-corrected.csv")
monitor.site <- read_csv("data/cleaned/02_corrected-monitoring-info-by-date-and-site_clean.csv")
subplot <- read_csv("data/cleaned/04.1_subplot-data_clean.csv")



# Organize columns --------------------------------------------------------

# Add raw.row and Region cols
p2x2.wide <- p2x2.raw %>% 
  select(-Recorder_Initials, -`%_mulch_remaining_in_plot`, -`%_mulch_remaining_in_subplot`, -Notes) %>% 
  mutate(raw.row = 1:nrow(p2x2.raw)) %>% # row number is to be able to easily refer back to the raw data and excluded columns if needed
  rename(PlotMix = Seed_Mix) %>% 
  mutate(Region = case_when(
    str_detect(p2x2.raw$Site, c("AguaFria|BabbittPJ|MOWE|Spiderweb|BarTBar|FlyingM|PEFO|TLE")) ~ "Colorado Plateau",
    str_detect(p2x2.raw$Site, c("CRC|UtahPJ|Salt_Desert")) ~ "Utah",
    str_detect(p2x2.raw$Site, c("29_Palms|AVRCD")) ~ "Mojave",
    str_detect(p2x2.raw$Site, c("Creosote|Mesquite")) ~ "Chihuahuan",
    str_detect(p2x2.raw$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(p2x2.raw$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central"))


# Correct monitoring info -------------------------------------------------

## Fix incorrect monitoring info -------------------------------------------

# Separate out monitoring info
p2x2.monitor <- p2x2.wide |> 
  select(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix, raw.row) 

# Find raw.row number of events that need to be fixed
wrong.raw.row <- left_join(monitor.wrong, p2x2.monitor) 

# Attach raw.row to corrected monitoring info
#   some events that need to be fixed have multiple rows in subplot data
monitor.assign <- data.frame(SiteDatePlotID = wrong.raw.row$SiteDatePlotID)
monitor.assign <- left_join(monitor.assign, monitor.fixed)
monitor.assign$raw.row <- wrong.raw.row$raw.row


# Separate monitor info that doesn't need to be fixed
p2x2.monitor <- p2x2.monitor |> 
  filter(!raw.row %in% monitor.assign$raw.row) |> 
  left_join(monitor.info)
p2x2.monitor |> 
  filter(is.na(SiteDatePlotID)) # all assigned SiteDatePlotID

# Add corrected monitor info for complete list
p2x2.monitor <- bind_rows(p2x2.monitor, monitor.assign) |> 
  arrange(SiteDatePlotID)

# Check for matching lengths
nrow(p2x2.monitor) == nrow(p2x2.raw)

# Attach correct monitoring info to p2x2 data
p2x2.wide <- p2x2.wide[ , -c(1:6)]
p2x2.wide <- left_join(p2x2.monitor, p2x2.wide)

# Add SiteDateID
p2x2.wide <- left_join(p2x2.wide, monitor.site)


## Check for missing SiteDatePlotID ----------------------------------------

# Find missing SiteDatePlotIDs
setdiff(monitor.info$SiteDatePlotID, p2x2.wide$SiteDatePlotID)

id.missing <- monitor.info |> 
  filter(SiteDatePlotID %in% setdiff(monitor.info$SiteDatePlotID, p2x2.wide$SiteDatePlotID))

id.missing.subplot <- subplot |> 
  filter(SiteDatePlotID %in% id.missing$SiteDatePlotID)
# I looked at the subplot data and the Note column says "Only subplots checked due to time, 
#   Full plot 10/8/2020 data not available. No major changes observed" which refers to SiteDateID 2; 
#   and says "Only subplots checked due to time, Full plot 11/14/2020 data not available. 
#   No major changes observed" which refers to SiteDateID 4.   
# No fix needed since data was not collected.

# Check for matching lengths 
#   6384 IDs, but 72 are missing from 2x2 data
nrow(monitor.info) == (nrow(p2x2.wide) + nrow(id.missing))



# Create present_species table --------------------------------------------

# Goal: create table and pivot to long so each Code is a row.

# Inspect Additional_species_in_plot cols
unique(p2x2.wide$Additional_Species_In_Plot...28)
unique(p2x2.wide$Additional_Species_In_Plot...27)
unique(p2x2.wide$Additional_Species_In_Plot...26)
unique(p2x2.wide$Additional_Species_In_Plot...25)
unique(p2x2.wide$Additional_Species_In_Plot...24)
unique(p2x2.wide$Additional_Species_In_Plot...23)
unique(p2x2.wide$Additional_Species_In_Plot...22)
unique(p2x2.wide$Additional_Species_In_Plot...21)
unique(p2x2.wide$Additional_Species_In_Plot...20)
unique(p2x2.wide$Additional_Species_In_Plot...19) # observations in 19 and smaller

# Drop empty Additional_Species_In_Plot cols
p2x2.wide <- p2x2.wide %>% 
  select(-Additional_Species_In_Plot...28,
         -Additional_Species_In_Plot...27,
         -Additional_Species_In_Plot...26,
         -Additional_Species_In_Plot...25,
         -Additional_Species_In_Plot...24,
         -Additional_Species_In_Plot...23,
         -Additional_Species_In_Plot...22, 
         -Additional_Species_In_Plot...21,
         -Additional_Species_In_Plot...20)


# Remove cover cols to create present_species table
present_species <- p2x2.wide |> 
  select(-Total_Veg_Cover, -Seeded_Cover)

# Check all cols for NAs
apply(present_species, 2, anyNA) # NAs in Additional_Species13-19

# Inspect instances of NAs in Additional_Species cols
add.spec.na <- present_species |> 
  filter(is.na(Additional_Species_In_Plot...12) |
         is.na(Additional_Species_In_Plot...13) |
         is.na(Additional_Species_In_Plot...14) |
         is.na(Additional_Species_In_Plot...15) |
         is.na(Additional_Species_In_Plot...16) |
         is.na(Additional_Species_In_Plot...17) |
         is.na(Additional_Species_In_Plot...18) |
         is.na(Additional_Species_In_Plot...19))
#   Manually check and see that any NAs in Addition_Species_In_Plot cols should be 0s

# Change NAs to 0
present_species$Additional_Species_In_Plot...13[is.na(present_species$Additional_Species_In_Plot...13)] <- "0"
present_species$Additional_Species_In_Plot...14[is.na(present_species$Additional_Species_In_Plot...14)] <- "0"
present_species$Additional_Species_In_Plot...15[is.na(present_species$Additional_Species_In_Plot...15)] <- "0"
present_species$Additional_Species_In_Plot...16[is.na(present_species$Additional_Species_In_Plot...16)] <- "0"
present_species$Additional_Species_In_Plot...17[is.na(present_species$Additional_Species_In_Plot...17)] <- "0"
present_species$Additional_Species_In_Plot...18[is.na(present_species$Additional_Species_In_Plot...18)] <- "0"
present_species$Additional_Species_In_Plot...19[is.na(present_species$Additional_Species_In_Plot...19)] <- "0"

# Check all cols for NAs
apply(present_species, 2, anyNA) 


# Pivot species columns to long and remove 0 Codes
present_species <- present_species %>% 
  pivot_longer(c(starts_with("Additional")), names_to = "source", values_to = "CodeOriginal") %>%
  mutate(source = case_when(
    source == "Additional_Species_In_Plot...12" ~ "add1",
    source == "Additional_Species_In_Plot...13" ~ "add2",
    source == "Additional_Species_In_Plot...14" ~ "add3",
    source == "Additional_Species_In_Plot...15" ~ "add4",
    source == "Additional_Species_In_Plot...16" ~ "add5",
    source == "Additional_Species_In_Plot...17" ~ "add6",
    source == "Additional_Species_In_Plot...18" ~ "add7",
    source == "Additional_Species_In_Plot...19" ~ "add8",
    TRUE ~ source)) %>% 
  filter(CodeOriginal != "0")


# Save intermediate
#   Pivoted to long form with CodeOriginal only, no other species info added yet
ps0 <- present_species



# Attach species info to present_species table ----------------------------

# Goal: connect CodeOriginal to species info from species lists.

# Compile lists, handling location independent/location dependent separately, and
#   with duplicate row/no duplicate row separately, because each needs to left_join()
#   with a different species list dataframe.
# Hence, four categories: 
#   (1) location independent, no duplicate row; 
#   (2) location dependent, no duplicate row;
#   (3) location independent, with duplicate row;
#   (4) location dependent, with duplicate row.

# Unique CodeOriginal
#   (1) Location independent, no duplicate
species.in.unique <- species.in |> 
  filter(NeedsItsDuplicate == "No")
ps.in.unq <- ps0 |> 
  filter(CodeOriginal %in% species.in.unique$CodeOriginal)
ps.in.unq <- left_join(ps.in.unq, species.in.unique) 

#   (2) Location dependent, no duplicate
species.de.unique <- species.de |> 
  filter(NeedsItsDuplicate == "No")
ps.de.unq <- ps0 |> 
  filter(CodeOriginal %in% species.de.unique$CodeOriginal)
ps.de.unq <- left_join(ps.de.unq, species.de.unique)


# Non-unique CodeOriginal
#   (3) Location independent with duplicate
species.in.dup <- species.in |> 
  filter(NeedsItsDuplicate == "Yes")
ps.in.dup <- ps0 |> 
  filter(CodeOriginal %in% species.in.dup$CodeOriginal)
ps.in.dup <- left_join(ps.in.dup, species.in.dup) 

#   (4) Location dependent with duplicate
species.de.dup <- species.de |> 
  filter(NeedsItsDuplicate == "Yes")
ps.de.dup <- ps0 |> 
  filter(CodeOriginal %in% species.de.dup$CodeOriginal)
ps.de.dup <- left_join(ps.de.dup, species.de.dup) 


# Combine all four categories
present_species <- bind_rows(ps.in.unq, ps.de.unq, ps.in.dup, ps.de.dup) |> 
  arrange(SiteDatePlotID)

# Check all cols for NAs
apply(present_species, 2, anyNA) 


# Save intermediate
#   Species info added, NeedsItsDuplicate column dealt with, includes p2x2 additional species only
ps1 <- present_species 



# Create SpeciesSeeded column ---------------------------------------------

# Goal: create corrected SpeciesSeeded column based on site-specific and 
#   plot-specific (cool/warm/none) seed mixes.

## Subplot SpeciesSeeded info ----------------------------------------------

# Extract info from subplot data
seeded.subplot <- subplot |> 
  select(Region, Site, PlotMix, CodeOriginal, Code, SpeciesSeeded) |> 
  distinct(.keep_all = TRUE)

# Attach subplot info to present_species
present_species <- left_join(ps1, seeded.subplot)
unique(present_species$SpeciesSeeded) # some not yet assigned SpeciesSeeded (are NA)

# Inspect those without SpeciesSeeded assignment
ps.ss.na <- present_species |> 
  filter(is.na(SpeciesSeeded)) |> 
  select(Region, Site, PlotMix, CodeOriginal, Code, Name, SpeciesSeeded) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Code) |> 
  arrange(Site) |> 
  arrange(Region)


## Species not in a seed mix -----------------------------------------------

# Species not included in any seed mix were not seeded regardless of site or plot
#   Separate out species not in any mix by Code
ps.ss.not.in.mix <- ps.ss.na |> 
  filter(!Code %in% mix$CodeOriginal) 

# Three SRER codes should be inspected manually because they might have seeded species
#   CodeOriginal does not match exactly, but it mentions something to species level
srer.ss <- ps.ss.not.in.mix |> 
  filter(str_detect(Code, "BOCU|PLJA"))
srer.ss
#   BOCU and PLJA were only in Warm mix, so "BOsp. a lot of possibly BOCU..." is the
#     only one that could have been seeded. Since plant identification isn't sure, we will 
#     mark these as not seeded (no fix needed).

# Label those not in seed mix as No for SpeciesSeeded
ps.ss.not.in.mix <- ps.ss.not.in.mix |> 
  mutate(SpeciesSeeded = "No")


## Species in any seed mix -------------------------------------------------

# Manually inspect species that exist in (a) seed mix
ps.ss.mix <- ps.ss.na |> 
  filter(!Code %in% ps.ss.not.in.mix$Code)

# OUTPUT: list of species that need SpeciesSeeded assignment
write_csv(ps.ss.mix,
          file = "data/data-wrangling-intermediate/04.2a_output-species-seeded1_in-mix-need-assignment.csv")

# EDITED: manually check if the species was seeded based on site-specific plot mix
ps.ss.mix <- read_xlsx("data/data-wrangling-intermediate/04.2b_edited-species-seeded1_SpeciesSeeded-in-mix-assigned.xlsx")


## Compile -----------------------------------------------------------------

# Combine those not in mix with ones in mix
ps.ss.assigned <- bind_rows(ps.ss.not.in.mix, ps.ss.mix)
nrow(ps.ss.assigned) == nrow(ps.ss.na)
unique(ps.ss.assigned$SpeciesSeeded) # all assigned

# Assign SpeciesSeeded to present_species
ps.ss <- present_species |> 
  filter(is.na(SpeciesSeeded)) |> 
  select(-SpeciesSeeded) 
ps.ss <- left_join(ps.ss, ps.ss.assigned)
unique(ps.ss$SpeciesSeeded)

# Compile all with correct SeededSpecies
present_species <- present_species |> 
  filter(!is.na(SpeciesSeeded)) |> 
  bind_rows(ps.ss) |> 
  arrange(raw.row)
unique(present_species$SpeciesSeeded) # all assigned


# Add column to denote where observation is coming from
present_species$ObsSource <- "2x2"

# Check all cols for NAs
apply(present_species, 2, anyNA) 

# Save intermediate
#   Species info and correct SpeciesSeeded for additional p2x2 species only
ps2 <- present_species 



# Add PlantSource column --------------------------------------------------

# Because plants couldn't always be identified to the species level, H. Farrell
#   grouped them by Native_Recruited, Invasive, and Seeded. 

unique(present_species$Native)
unique(present_species$SpeciesSeeded)

present_species <- present_species |> 
  mutate(PlantSource = paste0(present_species$Native, "_", present_species$SpeciesSeeded))
unique(present_species$PlantSource)

# Create Source column
present_species <- present_species |> 
  mutate(PlantSource = case_when(
    PlantSource == "0_No" ~ "0",
    PlantSource == "Unknown_No" ~ "Unknown_recruit",
    PlantSource == "Native_No" ~ "Native_recruit",
    PlantSource == "Introduced_No" ~ "Introduced/Invasive",
    PlantSource == "Native_Yes" ~ "Seeded",
    PlantSource == "Native/Unknown_No" ~ "Likely native_recruit",
    PlantSource == "Unknown_Yes" ~ "Seeded"))
unique(present_species$PlantSource)

# Save intermediate
#   Species info correct for additional p2x2 species only, "0" still included
ps3 <- present_species 



# Examine NA and 0 codes --------------------------------------------------

# Look for NA codes
present_species |> 
  filter(is.na(Code)) # should be none


# Look for 0 codes
#   Should not have any 0 CodeOriginal because empty 2x2 plots marked 0 were dropped after pivot_longer().
present_species |> 
  filter(CodeOriginal == "0")

#   But there are some that have a Code of 0 but not a CodeOriginal of 0.
code0 <- present_species |> 
  filter(Code == "0")
unique(code0$CodeOriginal) 
#   These rows should be removed because for most of them, no actual plant was observed (CodeOriginal describes
#     plant nearby or other plot conditions), and for "Not recorded" plots weren't measured 
#     so species richness count won't be correct/comparable for 2x2 plots.

present_species <- present_species |> 
  filter(Code != "0")


# Save intermediate
#   Species info correct for additional p2x2 species only, "0" removed
ps4 <- present_species 



# Combine with subplot to get all species present -------------------------

# Currently present_species is only the additional species in 2x2 plot but not subplot;
#   add subplot observations, which already have correct plant species info.

# Add subplot species
#   Separate out subplot species
subplot.species <- subplot |> 
  select(-Count, -Height, -raw.row)
subplot.species$ObsSource <- "subplot"

#   Remove cols so bind_rows() will work
present_species <- ps4 |> 
  select(-raw.row, -source, -NeedsItsDuplicate, -DuplicateNum)

#   Add subplot species to present_species
present_species <- bind_rows(present_species, subplot.species) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Code) |> 
  arrange(SiteDatePlotID) |> 
  arrange(SiteDateID)


# Remove rows where subplots were observed but 2x2 plots were not observed
present_species <- present_species |> 
  filter(!SiteDatePlotID %in% c(id.missing$SiteDatePlotID, code0$SiteDatePlotID))

# Check for SiteDatePlotIDs present
#   2x2 plots that weren't observed have been removed
length(unique(present_species$SiteDatePlotID)) == (nrow(monitor.info) - nrow(id.missing) - nrow(code0))

# Save intermediate
#   All species present in plot with correct species info; 2x2 plots not recorded have been removed
ps5 <- present_species 




# Create table of species richness ----------------------------------------

#   This is the number of species present at each plot during each monitoring event, 
#     without taking the species themselves or their abundance into account.


## Separate out completely empty plots -------------------------------------

#  Empty subplots have Code 0 and only one row (one SiteDatePlotID observation). 
# Empty 2x2 plots do not have a row in present_species,
#   because it would have been a "0" that was dropped earlier.
# Therefore, completely empty plots are those with only one row per SiteDatePlotID
#   in present_species with subplot Code of 0.


# Find list of SiteDatePlotIDs with only 1 row in present_species
SiteDatePlotID.count <- present_species |> 
  count(SiteDatePlotID)
SiteDatePlotID.single <- SiteDatePlotID.count |> 
  filter(n == 1)

# The row should come from subplot obs, with code of 0
empty.plots <- present_species |> 
  filter(SiteDatePlotID %in% SiteDatePlotID.single$SiteDatePlotID) |> 
  filter(ObsSource == "subplot") |> 
  filter(Code == "0")

  

# Create table of species richness ----------------------------------------

#   This is the number of species present at each plot during each monitoring event, 
#     without taking the species themselves or their abundance into account.

# Separate non-empty plots
nonempty.plots <- present_species |> 
  filter(!SiteDatePlotID %in% empty.plots$SiteDatePlotID)

# See what 0 Codes remain in non-empty plots
nonempty.code0 <- nonempty.plots |> 
  filter(Code == "0")
unique(nonempty.code0$ObsSource) # this occurs when subplot is empty but 2x2 is not (only "subplot" shows up)
#                                   no fix needed

# Calculate species richness of 2x2 plots for plots that had plants
nonempty.plots.richness <- nonempty.plots |> 
  filter(Code != "0") |>
  group_by(Region, Site, Date_Seeded, Date_Monitored, SiteDateID, Plot, Treatment, PlotMix, SiteDatePlotID) |> 
  summarise(Richness = n_distinct(Code),
            .groups = "keep")

# Assign richness to empty plots
empty.plots.richness <- empty.plots |> 
  select(Region, Site, Date_Seeded, Date_Monitored, SiteDateID, Plot, Treatment, PlotMix, SiteDatePlotID) |> 
  mutate(Richness = 0)

# Combine empty and non-empty plots
richness <- bind_rows(nonempty.plots.richness, empty.plots.richness)

# Check that all SiteDatePlotIDs are present
#   2x2 plots that weren't observed have been removed
length(unique(richness$SiteDatePlotID)) == (nrow(monitor.info) - nrow(id.missing) - nrow(code0))



# Create table of PlantSource counts --------------------------------------

# Calculate number of species in each PlantSource category per plot
nonempty.plots.plantsource <- nonempty.plots |> 
  filter(Code != "0") |>
  group_by(Region, Site, Date_Seeded, Date_Monitored, SiteDateID, Plot, Treatment, PlotMix, SiteDatePlotID) |> 
  count(PlantSource)
#   rows missing indicate no species of that PlantSource was present

# pivot_wider so each PlantSource category is its own column
#   replace NAs (equivalent of missing rows) with 0 using values_fill
unique(nonempty.plots.plantsource$PlantSource)
nonempty.plots.plantsource <- nonempty.plots.plantsource |> 
  pivot_wider(names_from = PlantSource,
              values_from = n,
              values_fill = 0)
nonempty.plots.plantsource <- nonempty.plots.plantsource |> 
  rename(LikelyNative_recruit = `Likely native_recruit`,
         Invasive = `Introduced/Invasive`)

# Add Weedy and Desirable_recruit cols
#   Weedy = Unknown_recruit + Invasive
nonempty.plots.plantsource <- nonempty.plots.plantsource |> 
  mutate(Weedy = Unknown_recruit + Invasive,
         Desirable_recruit = Native_recruit + LikelyNative_recruit)

# Create equivalent table for empty plots (all 0s)
empty.plots.plantsource <- empty.plots |> 
  select(Region, Site, Date_Seeded, Date_Monitored, SiteDateID, Plot, Treatment, PlotMix, SiteDatePlotID) |> 
  mutate(Native_recruit = 0,
         Unknown_recruit = 0,
         LikelyNative_recruit = 0,
         Seeded = 0,
         Invasive = 0,
         Weedy = 0,
         Desirable_recruit = 0)

# Combine empty and non-empty plots
plantsource <- bind_rows(nonempty.plots.plantsource, empty.plots.plantsource)

# Check that all SiteDatePlotIDs are present
#   2x2 plots that weren't observed have been removed
length(unique(plantsource$SiteDatePlotID)) == (nrow(monitor.info) - nrow(id.missing) - nrow(code0))



# Create cover table ------------------------------------------------------

# Remove species columns and 2x2 plots that weren't fully observed
cover <- p2x2.wide |> 
  select(-starts_with("Additional")) |> 
  filter(SiteDatePlotID %in% richness$SiteDatePlotID)


## Change Seeded_Cover to numeric ------------------------------------------

# See possible values of Seeded_Cover
unique(cover$Seeded_Cover)

# Replace TR and 0/TR with 0.5 and remove asterisks
cover$Seeded_Cover[cover$Seeded_Cover == "TR"] <- 0.5
cover$Seeded_Cover[cover$Seeded_Cover == "0/TR"] <- 0.5
cover$Seeded_Cover[cover$Seeded_Cover == "TR*"] <- 0.5
cover$Seeded_Cover[cover$Seeded_Cover == "1*"] <- 1
cover$Seeded_Cover[cover$Seeded_Cover == "2*"] <- 2

# Replace "NA" with logical NA
cover$Seeded_Cover[cover$Seeded_Cover == "NA"] <- NA

# Convert Seeded_Cover to numeric
cover$Seeded_Cover <- as.numeric(cover$Seeded_Cover)


## Change Total_Veg_Cover to numeric ---------------------------------------

# See possible values of Total_Veg_Cover
unique(cover$Total_Veg_Cover)

# Replace TR with 0.5
cover$Total_Veg_Cover[cover$Total_Veg_Cover == "TR"] <- 0.5

# Replace "NA" with logical NA
cover$Total_Veg_Cover[cover$Total_Veg_Cover == "NA"] <- NA

# Convert Seeded_Cover to numeric
cover$Total_Veg_Cover <- as.numeric(cover$Total_Veg_Cover)


## Add "Not_Seeded_Cover" column -------------------------------------------

cover$Not_Seeded_Cover <- cover$Total_Veg_Cover - cover$Seeded_Cover
summary(cover$Not_Seeded_Cover) # creates negative values

# Examine negative values
cover.neg.notseed <- cover |> 
  filter(Not_Seeded_Cover < 0)
cover.neg.notseed |> 
  select(Site, SiteDatePlotID, Seeded_Cover, Total_Veg_Cover, Not_Seeded_Cover)
#   Mostly it is small discrepancy, except for one which is -44;
#     SiteDatePlotID of 6241, at Roosevelt


# Create small fixes for all but 6241
#   Add Seeded_cover to Total_Veg_Cover
cover.neg.notseed.fix <- cover.neg.notseed |> 
  filter(SiteDatePlotID != 6241) |> 
  mutate(Total_Veg_Cover = Seeded_Cover + Total_Veg_Cover) |> 
  select(-Not_Seeded_Cover)
cover.neg.notseed.fix <- cover.neg.notseed.fix |> 
  mutate(Not_Seeded_Cover = Total_Veg_Cover - Seeded_Cover)


# Figure out what is going on with 6241
#   Plot 8 from Roosevelt, Seed treatment with Med-Warm mix
cover |> 
  filter(Site == "Roosevelt",
         Plot == 8) |> 
  select(Site, Date_Seeded, Date_Monitored, Seeded_Cover, Total_Veg_Cover)
#   99 for Seeded_Cover is definitely a typo; am going to assume it was supposed to be 9

cover.neg.notseed.fix6241 <- cover.neg.notseed |> 
  filter(SiteDatePlotID == 6241) |> 
  mutate(Seeded_Cover = 9) |> 
  select(-Not_Seeded_Cover)
cover.neg.notseed.fix6241 <- cover.neg.notseed.fix6241 |> 
  mutate(Not_Seeded_Cover = Total_Veg_Cover - Seeded_Cover)


# Combine all fixes
cover.neg.notseed.fix <- bind_rows(cover.neg.notseed.fix, cover.neg.notseed.fix6241)

# Replace fixes in cover table
cover <- cover |> 
  filter(!SiteDatePlotID %in% cover.neg.notseed.fix$SiteDatePlotID) |> 
  bind_rows(cover.neg.notseed.fix)



## Examine cover of empty plots --------------------------------------------

empty.plots.cover <- left_join(empty.plots, cover)
unique(empty.plots.cover$Total_Veg_Cover)
unique(empty.plots.cover$Seeded_Cover)

empty.plots.not0total <- empty.plots.cover |> 
  filter(Total_Veg_Cover > 0)

empty.plots.not0seeded <- empty.plots.cover |> 
  filter(Seeded_Cover > 0)

# not sure what to do about this tbh



## Handle cover NAs --------------------------------------------------------

# Check for NAs
apply(cover, 2, anyNA)

# Seeded cover
cover.seeded.na <- cover |> 
  filter(is.na(Seeded_Cover))

#   SRER and Patagonia are probably 0 because they are all Control plots
cover.seeded.na.fix <- cover.seeded.na |> 
  filter(Region != "Colorado Plateau") |> 
  mutate(Seeded_Cover = 0)
cover.seeded.na.fix <- cover.seeded.na.fix |> 
  mutate(Not_Seeded_Cover = Total_Veg_Cover - Seeded_Cover)

#   Add back in fix to cover data
cover <- cover |> 
  filter(!SiteDatePlotID %in% cover.seeded.na.fix$SiteDatePlotID) |> 
  bind_rows(cover.seeded.na.fix) |> 
  arrange(SiteDatePlotID)


# Total veg cover
cover.total.na <- cover |> 
  filter(is.na(Total_Veg_Cover)) # not sure how to fix any of these yet, need raw data sheets



# Combine 2x2 richness, plantsource, and cover ----------------------------

# Check for matching row number
nrow(richness) == nrow(plantsource)
nrow(plantsource) == nrow(cover)

# Combine
p2x2.richness.cover <- left_join(richness, plantsource)
p2x2.richness.cover <- left_join(p2x2.richness.cover, cover) |> 
  select(Region, Site, Date_Seeded, Date_Monitored, SiteDateID, Plot, Treatment, PlotMix,
         SiteDatePlotID, raw.row, Richness, Seeded, Native_recruit, LikelyNative_recruit,
         Unknown_recruit, Invasive, Desirable_recruit, Weedy,
         Seeded_Cover, Total_Veg_Cover, Not_Seeded_Cover) |> 
  arrange(SiteDatePlotID)





# Write clean tables ------------------------------------------------------

# List of species present
write_csv(present_species,
          file = "data/cleaned/04.2_2x2-species-present_clean.csv")

# Richness and cover
write_csv(p2x2.richness.cover,
          file = "data/cleaned/04.2_2x2-richness-cover_clean.csv")


save.image("RData/04.2_data-wrangling_2x2.RData")
