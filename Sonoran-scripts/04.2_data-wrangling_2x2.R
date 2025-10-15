# Created: 2025-10-08
# Last updated: 2025-10-13

# Purpose: Create 2 clean data tables for 2x2 plot data: one with cover, species richness,
#   and grouping cols (one row for each monitoring event/SiteDatePlotID), and one with
#  the list of species present for each monitoring event (can have multiple rows for SiteDatePlotID).

#  Ensure corrected and standardized species information, and monitoring and plot information,
#   and SpeciesSeeded column based on site-specific seed mixes and plot.

# Note: I had to sort of jump around with things, because first I had to make the present_species table so 
#   I could determine which plots were empty, which was needed to construct the cover table. 
# But then in looking at cover data, there were some plots that had no species but had a non-0
#   value for cover, so I had to end up adding those back in as unknown species to present_species.  


library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

plot.2x2.se.raw <- read_xlsx("Sonoran-data/raw/Sonoran_2023-09-15_Master 1.0 Germination Data_LO.xlsx",
                             sheet = "SonoranSE_Plots_LO")
plot.2x2.cen.raw <- read_xlsx("Sonoran-data/raw/Sonoran_2023-09-15_Master 1.0 Germination Data_LO.xlsx",
                              sheet = "SonoranCentral_Plots_LO")
species.in <- read_csv("Sonoran-data/cleaned/01_2x2_species-list-with-duplicates_location-independent_clean.csv")
species.de <- read_csv("Sonoran-data/cleaned/01_2x2_species-list-with-duplicates_location-dependent_clean.csv")
mix <- read_xlsx("Sonoran-data/raw/from-Master_seed-mix_LO_Sonoran.xlsx", sheet = "with-site_R") %>% 
  filter(Site %in% c("SRER", "Patagonia", "Preserve", "Pleasant", "SCC", "Roosevelt"))
monitor.info <- read_csv("Sonoran-data/cleaned/02_corrected-monitoring-info_clean.csv")
monitor.wrong <- read_csv("Sonoran-data/data-wrangling-intermediate/02_2x2-wrong-monitor-events.csv")
monitor.fixed <- read_csv("Sonoran-data/data-wrangling-intermediate/02_2x2-wrong-monitor-events-corrected.csv")
monitor.site <- read_csv("Sonoran-data/cleaned/02_SiteDateID_clean.csv")
monitor.siteplot <- read_csv("Sonoran-data/cleaned/02_SitePlotID_clean.csv")
subplot <- read_csv("Sonoran-data/cleaned/04.1_subplot-data_clean.csv")


# Organize columns --------------------------------------------------------

# Convert cols to character and combine all Sonoran Desert sites 
p2x2.se <- plot.2x2.se.raw %>% 
  mutate(across(everything(), as.character))
p2x2.cen <- plot.2x2.cen.raw %>% 
  filter(!is.na(Site)) %>% 
  select(-...31, -...32, -...33) %>% 
  mutate(across(everything(), as.character))
p2x2 <- bind_rows(p2x2.cen, p2x2.se)

# Add raw.row and Region cols
p2x2.wide <- p2x2 %>%
  select(-Recorder_Initials, -`%_mulch_remaining_in_plot`, -`%_mulch_remaining_in_subplot`, -Notes) %>%
  mutate(raw.row = 1:nrow(p2x2)) %>% # row number is to be able to easily refer back to the raw data and excluded columns if needed
  rename(PlotMix = Seed_Mix) %>%
  mutate(Region = case_when(
    str_detect(p2x2$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(p2x2$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central"))

# Remove incomplete monitoring events 
#   2022-10-06 at Pleasant and 2022-10-05 at Roosevelt (not all subplots were sampled)
p2x2.wide <- p2x2.wide %>% 
  filter(!Date_Monitored %in% c("2022-10-06", "2022-10-05"))

# Convert Date_Seeded and Date_Monitored to date cols, and Plot to numeric
p2x2.wide <- p2x2.wide %>%
  mutate(Date_Seeded = as.Date(Date_Seeded),
         Date_Monitored = as.Date(Date_Monitored),
         Plot = as.numeric(Plot)) 



# Correct monitoring info -------------------------------------------------

## Fix incorrect monitoring info -------------------------------------------

# Separate out monitoring info
p2x2.monitor <- p2x2.wide %>%
  select(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix, raw.row) 

# Find raw.row number of events that need to be fixed
wrong.raw.row <- left_join(monitor.wrong, p2x2.monitor)

# Attach raw.row to corrected monitoring info
#   some events that need to be fixed have multiple rows in subplot data
monitor.assign <- data.frame(SiteDatePlotID = wrong.raw.row$SiteDatePlotID)
monitor.assign <- left_join(monitor.assign, monitor.fixed)
monitor.assign$raw.row <- wrong.raw.row$raw.row


# Separate monitor info that doesn't need to be fixed
p2x2.monitor <- p2x2.monitor %>%
  filter(!raw.row %in% monitor.assign$raw.row) %>%
  left_join(monitor.info)
p2x2.monitor %>%
  filter(is.na(SiteDatePlotID)) # all assigned SiteDatePlotID

# Add corrected monitor info for complete list
p2x2.monitor <- bind_rows(p2x2.monitor, monitor.assign) %>%
  arrange(SiteDatePlotID)

# Check for matching lengths
nrow(p2x2.monitor) == nrow(p2x2.wide)

# Attach correct monitoring info to p2x2 data
p2x2.wide <- p2x2.wide[, -c(1:6)]
p2x2.wide <- left_join(p2x2.monitor, p2x2.wide)

# Add SiteDateID
p2x2.wide <- left_join(p2x2.wide, monitor.site)

# Add SitePlotID
p2x2.wide <- left_join(p2x2.wide, monitor.siteplot)


## Check for missing SiteDatePlotID ----------------------------------------

# Find missing SiteDatePlotIDs
setdiff(monitor.info$SiteDatePlotID, p2x2.wide$SiteDatePlotID)

# Check for matching lengths
#   1152 IDs
nrow(monitor.info) == nrow(p2x2.wide)



# Create present_species table --------------------------------------------

# Goal: create table and pivot to long so each Code is a row.

# Remove cover cols to create present_species table
present_species <- p2x2.wide %>%
  select(-Total_Veg_Cover, -Seeded_Cover)

# Check all cols for NAs
apply(present_species, 2, anyNA) # NAs in Additional_Species 13, 16, 17, 18, 21

# Inspect instances of NAs in Additional_Species cols
add.spec.na <- present_species %>%
  filter(is.na(Additional_Species_In_Plot...13) |
           is.na(Additional_Species_In_Plot...16) |
           is.na(Additional_Species_In_Plot...17) |
           is.na(Additional_Species_In_Plot...18) |
           is.na(Additional_Species_In_Plot...21))
#   Manually check and see that any NAs in Addition_Species_In_Plot cols should be 0s

# Change NAs to 0
present_species$Additional_Species_In_Plot...13[is.na(present_species$Additional_Species_In_Plot...13)] <- "0"
present_species$Additional_Species_In_Plot...16[is.na(present_species$Additional_Species_In_Plot...16)] <- "0"
present_species$Additional_Species_In_Plot...17[is.na(present_species$Additional_Species_In_Plot...17)] <- "0"
present_species$Additional_Species_In_Plot...18[is.na(present_species$Additional_Species_In_Plot...18)] <- "0"
present_species$Additional_Species_In_Plot...21[is.na(present_species$Additional_Species_In_Plot...21)] <- "0"

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



## Attach species info to present_species table ---------------------------

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
species.in.unique <- species.in %>%
  filter(NeedsItsDuplicate == "No")
ps.in.unq <- ps0 %>%
  filter(CodeOriginal %in% species.in.unique$CodeOriginal)
ps.in.unq <- left_join(ps.in.unq, species.in.unique)

#   (2) Location dependent, no duplicate
species.de.unique <- species.de %>%
  filter(NeedsItsDuplicate == "No")
ps.de.unq <- ps0 %>%
  filter(CodeOriginal %in% species.de.unique$CodeOriginal)
ps.de.unq <- left_join(ps.de.unq, species.de.unique)


# Non-unique CodeOriginal
#   (3) Location independent with duplicate
species.in.dup <- species.in %>%
  filter(NeedsItsDuplicate == "Yes")
ps.in.dup <- ps0 %>%
  filter(CodeOriginal %in% species.in.dup$CodeOriginal)
ps.in.dup <- left_join(ps.in.dup, species.in.dup)

#   (4) Location dependent with duplicate
species.de.dup <- species.de %>%
  filter(NeedsItsDuplicate == "Yes")
ps.de.dup <- ps0 %>%
  filter(CodeOriginal %in% species.de.dup$CodeOriginal)
ps.de.dup <- left_join(ps.de.dup, species.de.dup)


# Combine all four categories
present_species <- bind_rows(ps.in.unq, ps.de.unq, ps.in.dup, ps.de.dup) %>%
  arrange(SiteDatePlotID)

# Check all cols for NAs
apply(present_species, 2, anyNA)

# Save intermediate
#   Species info added, NeedsItsDuplicate column dealt with, includes p2x2 additional species only
ps1 <- present_species



## Create SpeciesSeeded column --------------------------------------------

# Goal: create corrected SpeciesSeeded column based on site-specific and
#   plot-specific (cool/warm/none) seed mixes.

### Subplot SpeciesSeeded info ---------------------------------------------

# Extract info from subplot data
seeded.subplot <- subplot %>%
  select(Region, Site, PlotMix, CodeOriginal, Code, SpeciesSeeded) %>%
  distinct(.keep_all = TRUE)

# Attach subplot info to present_species
present_species <- left_join(ps1, seeded.subplot)
unique(present_species$SpeciesSeeded) # some not yet assigned SpeciesSeeded (are NA)

# Inspect those without SpeciesSeeded assignment
ps.ss.na <- present_species %>%
  filter(is.na(SpeciesSeeded)) %>%
  select(Region, Site, PlotMix, CodeOriginal, Code, Name, SpeciesSeeded) %>%
  distinct(.keep_all = TRUE) %>%
  arrange(Code) %>%
  arrange(Site) %>%
  arrange(Region)


### Species not in a seed mix ----------------------------------------------

# Species not included in any seed mix were not seeded regardless of site or plot
#   Separate out species not in any mix by Code
ps.ss.not.in.mix <- ps.ss.na %>%
  filter(!Code %in% mix$CodeOriginal)

# Three SRER codes should be inspected manually because they might have seeded species
#   CodeOriginal does not match exactly, but it mentions something to species level
srer.ss <- ps.ss.not.in.mix %>%
  filter(str_detect(Code, "BOCU|PLJA"))
srer.ss
#   BOCU and PLJA were only in Warm mix, so "BOsp. a lot of possibly BOCU..." is the
#     only one that could have been seeded. Since plant identification isn't sure, we will
#     mark these as not seeded (no fix needed).

# Label those not in seed mix as No for SpeciesSeeded
ps.ss.not.in.mix <- ps.ss.not.in.mix %>%
  mutate(SpeciesSeeded = "No")


### Species in any seed mix ------------------------------------------------

# Manually inspect species that exist in (a) seed mix
ps.ss.mix <- ps.ss.na %>%
  filter(!Code %in% ps.ss.not.in.mix$Code)

# OUTPUT: list of species that need SpeciesSeeded assignment
write_csv(ps.ss.mix,
          file = "Sonoran-data/data-wrangling-intermediate/04.2a_output1_SpeciesSeeded-in-mix-need-assignment.csv")

# EDITED: manually check if the species was seeded based on site-specific plot mix
ps.ss.mix <- read_xlsx("Sonoran-data/data-wrangling-intermediate/04.2b_edited1_SpeciesSeeded-in-mix-assigned.xlsx")



### Compile ----------------------------------------------------------------

# Combine those not in mix with ones in mix
ps.ss.assigned <- bind_rows(ps.ss.not.in.mix, ps.ss.mix)
nrow(ps.ss.assigned) == nrow(ps.ss.na)
unique(ps.ss.assigned$SpeciesSeeded) # all assigned (no NAs)

# Assign SpeciesSeeded to present_species
ps.ss <- present_species %>%
  filter(is.na(SpeciesSeeded)) %>%
  select(-SpeciesSeeded)
ps.ss <- left_join(ps.ss, ps.ss.assigned)
unique(ps.ss$SpeciesSeeded) # all assigned (no NAs)

# Compile all with correct SeededSpecies
present_species <- present_species %>%
  filter(!is.na(SpeciesSeeded)) %>%
  bind_rows(ps.ss) %>%
  arrange(raw.row)
unique(present_species$SpeciesSeeded) # all assigned (no NAs)


# Add column to denote where observation is coming from
present_species$ObsSource <- "2x2"

# Check all cols for NAs
apply(present_species, 2, anyNA)

# Save intermediate
#   Species info and correct SpeciesSeeded for additional p2x2 species only
ps2 <- present_species



## Add additional grouping columns ----------------------------------------

# PlantSource: Unknown_recruit, Native_recruit, Introduced/Invasive, Seeded, Likely native_recruit, 0
# Weedy: Weedy, Desirable, 0
# PlantSource2: Recruit, Native_recruit, Introduced/Invasive, Seeded, 0
# PlotMix_Climate: None, Current, Projected, 0


### Add PlantSource & PlantSource2 columns --------------------------------

# Because plants couldn't always be identified to the species level, H. Farrell
#   grouped them by Native_Recruited, Invasive, and Seeded.

unique(present_species$Native)
unique(present_species$SpeciesSeeded)

present_species <- present_species %>%
  mutate(PlantSource = paste0(present_species$Native, "_", present_species$SpeciesSeeded))
unique(present_species$PlantSource)

# Create PlantSource column
present_species <- present_species %>%
  mutate(PlantSource = case_when(
    PlantSource == "0_No" ~ "0",
    PlantSource == "Unknown_No" ~ "Unknown_recruit",
    PlantSource == "Native_No" ~ "Native_recruit",
    PlantSource == "Introduced_No" ~ "Introduced/Invasive",
    PlantSource == "Native_Yes" ~ "Seeded",
    PlantSource == "Native/Unknown_No" ~ "Likely native_recruit",
    PlantSource == "Unknown_Yes" ~ "Seeded"))
unique(present_species$PlantSource)

# Create PlantSource2 column
unique(present_species$PlantSource)
present_species <- present_species %>%
  mutate(PlantSource2 = case_when(
    present_species$PlantSource == "Unknown_recruit" ~ "Recruit",
    str_detect(present_species$PlantSource, "Native_recruit|Likely native_recruit") ~ "Native recruit",
    TRUE ~ present_species$PlantSource))
unique(present_species$PlantSource2)


### Add Weedy column ------------------------------------------------------

# To further simplify weedy vs. desirable species

# Add Weedy column
unique(present_species$PlantSource)
present_species <- present_species %>%
  mutate(Weedy = case_when(
    str_detect(present_species$PlantSource, "Unknown_recruit|Introduced/Invasive") ~ "Weedy",
    str_detect(present_species$PlantSource, "Native_recruit|Likely native_recruit|Seeded") ~ "Desirable",
    TRUE ~ present_species$PlantSource))
unique(present_species$Weedy)


### Add PlotMix_Climate column --------------------------------------------

# PlotMix names alone cannot be used to group what is climate-adapted and what is current-adapted
#   because mixes are site-specific, and some mixes that are current-adapted are climate-adapted at other sites
#   and vise versa.
# Need ot manually check each site and its seedmix to figure out which is which.

# Add PlotMix_Climate col
present_species <- present_species %>%
  mutate(PlotMix_Climate = case_when(
    str_detect(present_species$Site, "Patagonia|SRER") &
      present_species$PlotMix == "Medium" ~ "Current",
    str_detect(present_species$Site, "Patagonia|SRER") &
      present_species$PlotMix == "Warm" ~ "Projected",
    str_detect(present_species$Site, "Preserve|SCC|Roosevelt|Pleasant") &
      present_species$PlotMix == "Cool" ~ "Current",
    str_detect(present_species$Site, "Preserve|SCC|Roosevelt|Pleasant") &
      present_species$PlotMix == "Warm" ~ "Projected",
    TRUE ~ present_species$PlotMix))
present_species$PlotMix_Climate <- factor(present_species$PlotMix_Climate,
                                          levels = c("None", "Current", "Projected"))


# Save intermediate
#   Species info correct for additional p2x2 species only, "0" still included
ps3 <- present_species



## Examine NA and 0 codes -------------------------------------------------

# Look for NA codes
present_species %>%
  filter(is.na(Code)) # should be none

# Look for 0 codes
#   Should not have any 0 CodeOriginal because empty 2x2 plots marked 0 were dropped after pivot_longer().
present_species %>%
  filter(CodeOriginal == "0")

# Check when Code is 0, but CodeOriginal is non-0 
non0.codeoriginal <- present_species %>% 
  filter(CodeOriginal != "0" & Code == "0")
non0.codeoriginal$CodeOriginal

#   These rows just describe empty plots and no species, so they should be removed
present_species <- present_species %>% 
  filter(!CodeOriginal %in% non0.codeoriginal$CodeOriginal)



## Combine with subplot to get all species present ------------------------

# Currently present_species is only the additional species in 2x2 plot but not subplot;
#   add subplot observations, which already have correct plant species info.

# Add subplot species
#   Separate out subplot species
subplot.species <- subplot %>%
  select(-Count, -Height, -raw.row)
subplot.species$ObsSource <- "subplot"

#   Remove cols so bind_rows() will work
present_species <- present_species %>%
  select(-raw.row, -source, -NeedsItsDuplicate, -DuplicateNum)

#   Add subplot species to present_species
present_species <- bind_rows(present_species, subplot.species) %>%
  distinct(.keep_all = TRUE) %>%
  arrange(Code) %>%
  arrange(SiteDatePlotID) %>%
  arrange(SiteDateID)


# Check for SiteDatePlotIDs present
length(unique(present_species$SiteDatePlotID)) == (nrow(monitor.info))
setdiff(monitor.info$SiteDatePlotID, present_species$SiteDatePlotID)

# Check for duplicate 2x2 codes (species should only be listed once)
ps.dup.test <- present_species %>% 
  select(-ObsSource)
ps.dup <- ps.dup.test %>% 
  group_by(across(everything())) %>%  
  filter(n() > 1) %>%
  ungroup() %>% 
  mutate(Code_SDPID = paste0(Code, ", ", SiteDatePlotID)) # Create Code_SDPID to check duplicates specific to SiteDatePlotID

# Inspect duplicate rows
ps.dup.inspect <- present_species %>% 
  mutate(Code_SDPID = paste0(Code, ", ", SiteDatePlotID)) %>% 
  filter(Code_SDPID %in% ps.dup$Code_SDPID)
#   see that they are all duplicates with one from subplot and one from 2x2

# Create df of duplicate rows to remove with new Code_SDPID_Obs col to identify rows to remove
ps.dup.rm <- ps.dup.inspect %>% 
  filter(ObsSource == "2x2") %>% 
  mutate(Code_SDPID_Obs = paste0(Code_SDPID, ", ", ObsSource))

# Remove from present_species
present_species <- present_species %>% 
  mutate(Code_SDPID_Obs = paste0(Code, ", ", SiteDatePlotID, ", ", ObsSource)) %>% 
  filter(!Code_SDPID_Obs %in% ps.dup.rm$Code_SDPID_Obs) %>% 
  select(-Code_SDPID_Obs)


# Save intermediate
#   All species present in plot with correct species info
ps4 <- present_species



# Separate out completely empty plots --------------------------------------

# Empty subplots have Code 0 and only one row (one SiteDatePlotID observation).
# Empty 2x2 plots do not have a row in present_species,
#   because it would have been a "0" that was dropped earlier.
# Therefore, completely empty plots are those with only one row per SiteDatePlotID
#   in present_species with subplot Code of 0.


# Find list of SiteDatePlotIDs with only 1 row in present_species
SiteDatePlotID.count <- present_species %>%
  count(SiteDatePlotID)
SiteDatePlotID.single <- SiteDatePlotID.count %>%
  filter(n == 1)

# The row should come from subplot obs, with code of 0
empty.plots <- present_species %>%
  filter(SiteDatePlotID %in% SiteDatePlotID.single$SiteDatePlotID) %>%
  filter(ObsSource == "subplot") %>%
  filter(Code == "0")


## Examine non-empty plots ------------------------------------------------

# Separate non-empty plots
nonempty.plots <- present_species %>%
  filter(!SiteDatePlotID %in% empty.plots$SiteDatePlotID)

# See what 0 Codes remain in non-empty plots
nonempty.code0 <- nonempty.plots %>%
  filter(Code == "0")
unique(nonempty.code0$ObsSource) # this occurs when subplot is empty but 2x2 is not (only "subplot" shows up)



# Create cover table ------------------------------------------------------

# Remove species columns
cover <- p2x2.wide %>%
  select(-starts_with("Additional")) 


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



## Examine cover of empty plots --------------------------------------------

empty.plots.cover <- left_join(empty.plots, cover)
unique(empty.plots.cover$Total_Veg_Cover)
unique(empty.plots.cover$Seeded_Cover)

# Non-0 Total_Veg_Cover
empty.plots.non0total <- empty.plots.cover %>%
  filter(Total_Veg_Cover > 0)

# Non-0 Seeded_Cover
empty.plots.non0seeded <- empty.plots.cover %>%
  filter(Seeded_Cover > 0) # same as one of the non-0 Total_Veg_Cover plots

# OUTPUT: plots with no species recorded but >0 Total_Veg_Cover or Seeded_Cover
write_csv(empty.plots.non0total,
          file = "Sonoran-data/data-wrangling-intermediate/04.2a_output2_empty-plots_non-0-total-veg-cover.csv")

# EDITED: add "Unknown species" to note there was at least one species present 
#   (will adjust richness table later)
empty.plots.non0 <- read_xlsx("Sonoran-data/data-wrangling-intermediate/04.2b_edited2_empty-plots_non-0-cover_species-added.xlsx")

# No fix needed for cover tables, as cover values did not change;
#   will fix present_species later



## Handle cover NAs --------------------------------------------------------

# Check for NAs
apply(cover, 2, anyNA)

# Seeded cover
cover.seeded.na <- cover %>%
  filter(is.na(Seeded_Cover))

#   SRER and Patagonia are probably 0 because they are all Control plots
cover.seeded.na.fix <- cover.seeded.na %>%
  mutate(Seeded_Cover = 0)

#   Add back in fix to cover data
cover <- cover %>%
  filter(!SiteDatePlotID %in% cover.seeded.na.fix$SiteDatePlotID) %>%
  bind_rows(cover.seeded.na.fix) %>%
  arrange(SiteDatePlotID)

# Total veg cover
cover.total.na <- cover %>%
  filter(is.na(Total_Veg_Cover)) 

# Create fixes
#   1. For Patagonia, total veg cover = seeded, because no other species were listed in 2x2 plot than seeded ones
#   2. For SRER, total veg cover = seeded = 0.5 (TR) because no other species were listed in 2x2 plot
#   3 & 4. Not sure because non-seeded species were also found in plot; will just do total veg = seeded
cover.total.na.fix <- cover.total.na %>% 
  mutate(Total_Veg_Cover = Seeded_Cover)

#   Add back in fix to cover data
cover <- cover %>%
  filter(!SiteDatePlotID %in% cover.total.na.fix$SiteDatePlotID) %>%
  bind_rows(cover.total.na.fix) %>%
  arrange(SiteDatePlotID)



# Create table of species richness ----------------------------------------

#   This is the number of species present at each plot during each monitoring event,
#     without taking the species themselves or their abundance into account.


## Add in species from non-0 cover to present_species ---------------------

# Create non-0 cover species fix
empty.plots.non0.sp.fix <- empty.plots.non0 %>% 
  select(-Total_Veg_Cover, -Seeded_Cover, -raw.row) %>% 
  mutate(CodeOriginal = as.character(CodeOriginal))

# Check for matching columns
setdiff(colnames(empty.plots.non0.sp.fix), colnames(present_species))

# Add in fix
present_species <- present_species %>% 
  filter(!SiteDatePlotID %in% empty.plots.non0.sp.fix$SiteDatePlotID) %>% 
  bind_rows(empty.plots.non0.sp.fix) %>% 
  select(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix, PlotMix_Climate,
         SiteDateID, SitePlotID, SiteDatePlotID, CodeOriginal, Code, Name, Native, Duration, Lifeform,
         SpeciesSeeded, ObsSource, PlantSource, PlantSource2, Weedy) %>%
  arrange(SiteDatePlotID)

# Edit empty plot list
empty.plots <- empty.plots %>% 
  filter(!SiteDatePlotID %in% empty.plots.non0.sp.fix$SiteDatePlotID)

# Edit non-empty plot list
nonempty.plots <- nonempty.plots %>% 
  bind_rows(empty.plots.non0.sp.fix) %>% 
  arrange(SiteDatePlotID)



## Calculate richness ----------------------------------------------------

# Calculate species richness of 2x2 plots for plots that had plants
nonempty.plots.richness <- nonempty.plots %>%
  filter(Code != "0") %>%
  group_by(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix, PlotMix_Climate,
           SiteDateID, SitePlotID, SiteDatePlotID) %>%
  summarise(Richness = n_distinct(Code),
            .groups = "keep")

# Assign richness to empty plots
empty.plots.richness <- empty.plots %>%
  select(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix, PlotMix_Climate,
         SiteDateID, SitePlotID, SiteDatePlotID) %>%
  mutate(Richness = 0)

# Combine empty and non-empty plots
richness <- bind_rows(nonempty.plots.richness, empty.plots.richness)

# Check that all SiteDatePlotIDs are present
length(unique(richness$SiteDatePlotID)) == nrow(monitor.info)
setdiff(monitor.info$SiteDatePlotID, richness$SiteDatePlotID) 



## Create table of PlantSource counts -------------------------------------

# Calculate number of species in each PlantSource category per plot
nonempty.plots.plantsource <- nonempty.plots %>%
  filter(Code != "0") %>%
  group_by(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix, PlotMix_Climate,
           SiteDateID, SitePlotID, SiteDatePlotID) %>%
  count(PlantSource)
#   rows missing indicate no species of that PlantSource was present

# pivot_wider so each plantsource category is its own column
#   replace NAs (equivalent of missing rows) with 0 using values_fill
unique(nonempty.plots.plantsource$PlantSource)
nonempty.plots.plantsource <- nonempty.plots.plantsource %>%
  pivot_wider(names_from = PlantSource,
              values_from = n,
              values_fill = 0) %>% 
  rename(Invasive = `Introduced/Invasive`,
         LikelyNative_recruit = `Likely native_recruit`)

# Add Weedy, Desirable_recruit, and Desirable cols
nonempty.plots.plantsource <- nonempty.plots.plantsource %>%
  mutate(Weedy = Unknown_recruit + Invasive,
         Desirable_recruit = Native_recruit + LikelyNative_recruit,
         Desirable = Seeded + Native_recruit + LikelyNative_recruit)

# Create equivalent table for empty plots (all 0s)
empty.plots.plantsource <- empty.plots %>%
  select(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix, PlotMix_Climate,
         SiteDateID, SitePlotID, SiteDatePlotID) %>%
  mutate(Native_recruit = 0,
         Unknown_recruit = 0,
         LikelyNative_recruit = 0,
         Seeded = 0,
         Invasive = 0,
         Weedy = 0,
         Desirable_recruit = 0,
         Desirable = 0)

# Combine empty and non-empty plots
plantsource <- bind_rows(nonempty.plots.plantsource, empty.plots.plantsource)

# Check that all SiteDatePlotIDs are present
length(unique(plantsource$SiteDatePlotID)) == nrow(monitor.info)
setdiff(monitor.info$SiteDatePlotID, plantsource$SiteDatePlotID) 



# Combine 2x2 richness, plantsource, and cover ----------------------------

# Check for matching row number
nrow(richness) == nrow(plantsource)
nrow(plantsource) == nrow(cover)

# Combine
p2x2.richness.cover <- left_join(richness, plantsource)
p2x2.richness.cover <- left_join(p2x2.richness.cover, cover) %>%
  select(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix, PlotMix_Climate,
         SiteDateID, SitePlotID, SiteDatePlotID, raw.row, Richness, Seeded, Native_recruit, 
         LikelyNative_recruit, Unknown_recruit, Invasive, Desirable_recruit, Weedy, Desirable,
         Seeded_Cover, Total_Veg_Cover) %>%
  arrange(SiteDatePlotID)



# Write clean tables ------------------------------------------------------

# List of species present
write_csv(present_species,
          file = "Sonoran-data/cleaned/04.2_2x2-species-present_clean.csv")


# Richness and cover
write_csv(p2x2.richness.cover,
          file = "Sonoran-data/cleaned/04.2_2x2-richness-cover_clean.csv")


save.image("Sonoran-RData/04.2_data-wrangling_2x2.RData")
