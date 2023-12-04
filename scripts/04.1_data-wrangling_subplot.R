# Created: 2023-09-18
# Last updated: 2023-12-04

# Purpose: Create clean data table for subplot data, with corrected and standardized species information,
#   and monitoring and plot information, and correct SpeciesSeeded column based on each site-specific
#     seed mix and plot. 
#   Essentially, add corrected metadata from 01.R and 02.R to subplot data, 
#     correct SpeciesSeeded column, and add PlantSource column.

library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/2023-09-15_Master 1.0 Germination Data_raw.xlsx", sheet = "AllSubplotData")
species.in <- read_csv("data/cleaned/01_subplot_species-list_location-independent_clean.csv")
species.de <- read_csv("data/cleaned/01_subplot_species-list_location-dependent_clean.csv")
mix <- read_xlsx("data/raw/from-Master_seed-mix_LO.xlsx", sheet = "with-site_R")
monitor.info <- read_csv("data/cleaned/02_corrected-monitoring-info_clean.csv")
monitor.wrong <- read_csv("data/data-wrangling-intermediate/02_subplot-wrong-monitor-events.csv")
monitor.fixed <- read_csv("data/data-wrangling-intermediate/02_subplot-wrong-monitor-events-corrected.csv")
SiteDatePlotID.replace <- read_csv("data/data-wrangling-intermediate/02_SiteDatePlotID-replacements.csv")
monitor.site <- read_csv("data/cleaned/02_corrected-monitoring-info-by-date-and-site_clean.csv")


# Organize columns --------------------------------------------------------

# Narrow down subplot.raw columns
subplot <- subplot.raw %>% 
  select(-Recorder_Initials, -Functional_Group, -`Certainty_of_ID(1-3)`, -Notes) %>% 
  mutate(raw.row = 1:nrow(subplot.raw)) %>% # row number is to be able to easily refer back to the raw data and excluded columns if needed
  rename(CodeOriginal = Species_Code,
         Count = Seedling_Count,
         Height = Average_Height_mm,
         SpeciesSeeded = `Seeded(Yes/No)`,
         PlotMix = Seed_Mix)

# Add Region col
subplot <- subplot %>% 
  mutate(Region = case_when(
    str_detect(subplot$Site, c("AguaFria|BabbittPJ|MOWE|Spiderweb|BarTBar|FlyingM|PEFO|TLE")) ~ "Colorado Plateau",
    str_detect(subplot$Site, c("CRC|UtahPJ|Salt_Desert")) ~ "Utah",
    str_detect(subplot$Site, c("29_Palms|AVRCD")) ~ "Mojave",
    str_detect(subplot$Site, c("Creosote|Mesquite")) ~ "Chihuahuan",
    str_detect(subplot$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(subplot$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central")) 

# Convert character "NA" to logical NA
sapply(subplot, class) # temporarily convert date columns to character to replace NAs
subplot <- subplot %>% 
  mutate(Date_Seeded = as.character(Date_Seeded),
         Date_Monitored = as.character(Date_Monitored))
subplot[subplot == "NA"] <- NA
apply(subplot, 2, anyNA) # check all columns for NAs; NAs in Code, Count, Height, Seeded
subplot <- subplot %>% 
  mutate(Date_Seeded = as.Date(Date_Seeded),
         Date_Monitored = as.Date(Date_Monitored)) # convert back to date



# Handle NA codes ---------------------------------------------------------

# Extract NA codes
filter(subplot, is.na(CodeOriginal)) # raw.row 8610, 9728, 12576

# Rows 8610 and 9728 are observations for empty plots; Code should be 0
subplot$CodeOriginal[subplot$raw.row == 8610] <- "0"
subplot$CodeOriginal[subplot$raw.row == 9728] <- "0"

# Examine non-empty subplots (12166)
subplot.raw[12576, ]
subplot.raw[12576, c("Species_Code", "Functional_Group", "Seeded(Yes/No)", "Notes")]
#   No notes for 12576, but a functional group was listed; not seeded, and probably an unknown

# Assign location-dependent code for 12576 (developed in 02.R)
species.de |> 
  filter(CodeOriginal == "UNFO.12576.assigned")
subplot$CodeOriginal[subplot$raw.row == 12576] <- "UNFO.12576.assigned"


# Check again for NA codes
filter(subplot, is.na(CodeOriginal)) # no NAs


# Check for missing codes by comparing subplot data to both species lists
sub.codes <- c(species.de$CodeOriginal, species.in$CodeOriginal)
setdiff(subplot$CodeOriginal, sub.codes) # should be 0



# Add species info for location-dependent ---------------------------------

# Separate out location-dependent observations
subplot.de <- subplot %>% 
  filter(CodeOriginal %in% species.de$CodeOriginal)

# Add species info
subplot.de <- left_join(subplot.de, species.de)

# Check for NA codes
filter(subplot.de, is.na(Code)) # no NAs
apply(subplot.de, 2, anyNA) # NAs for Count, Height, SpeciesSeeded are inherent to data



# Add species info for location-independent codes -------------------------

subplot.in <- subplot %>% 
  filter(CodeOriginal %in% species.in$CodeOriginal) 

subplot.in <- left_join(subplot.in, species.in) 

# Check for NA codes
filter(subplot.in, is.na(Code)) # no NAs
apply(subplot.in, 2, anyNA) # NAs for Count, Height, SpeciesSeeded inherent to data



# Combine location-dependent and independent ------------------------------

subplot <- bind_rows(subplot.in, subplot.de) %>% 
  arrange(raw.row)

# Check that there the same number of observations as the original subplot data
nrow(subplot) == nrow(subplot.raw) 



# Check if Introduced plants were marked as Seeded ------------------------

subplot.inva <- subplot %>% 
  filter(Native == "Introduced") %>% 
  select(Code, SpeciesSeeded, Name, Native) %>% 
  distinct(.keep_all = TRUE)
unique(subplot.inva$SpeciesSeeded) # something is mislabeled; no introduced species were seeded
subplot.inva %>% 
  filter(SpeciesSeeded == "Yes")

# Fix ERCU2 and ERCI6 - mark all observations as "No" for Seeded col
subplot$SpeciesSeeded[subplot$Name == "Eragrostis curvula"] <- "No"
subplot$SpeciesSeeded[subplot$Name == "Erodium cicutarium"] <- "No"



# Correct monitoring info -------------------------------------------------

# Separate out monitoring info
subplot.monitor <- subplot |> 
  select(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix, raw.row) 

# Find raw.row number of events that need to be fixed
wrong.raw.row <- left_join(monitor.wrong, subplot.monitor) 

# Attach raw.row to corrected monitoring info
#   some events that need to be fixed have multiple rows in subplot data
monitor.assign <- data.frame(SiteDatePlotID = wrong.raw.row$SiteDatePlotID)
monitor.assign <- left_join(monitor.assign, monitor.fixed)
monitor.assign$raw.row <- wrong.raw.row$raw.row

#   Separate monitor info that doesn't need to be fixed and add SiteDatePlotID
subplot.monitor <- subplot.monitor |> 
  filter(!raw.row %in% monitor.assign$raw.row) |> 
  left_join(monitor.info)
subplot.monitor |> 
  filter(is.na(SiteDatePlotID)) # all assigned SiteDatePlotID

#   Add corrected monitor info for complete list
subplot.monitor <- bind_rows(subplot.monitor, monitor.assign) |> 
  arrange(SiteDatePlotID)

#   Check for matching lengths
nrow(subplot.monitor) == nrow(subplot.raw)


# Attach correct monitoring info to subplot data
subplot <- subplot[ , -c(1:6)]
subplot <- left_join(subplot, subplot.monitor)


# Replace null SiteDatePlotIDs with correct ones
subplot.IDreplace <- subplot |> 
  filter(SiteDatePlotID %in% SiteDatePlotID.replace$SiteDatePlotID_old) |> 
  rename(SiteDatePlotID_old = SiteDatePlotID) |> 
  left_join(SiteDatePlotID.replace) |> 
  select(-SiteDatePlotID_old) |> 
  rename(SiteDatePlotID = SiteDatePlotID_replace)

subplot <- subplot |> 
  filter(!raw.row %in% subplot.IDreplace$raw.row) |> 
  bind_rows(subplot.IDreplace) |> 
  arrange(raw.row)


# Check for matching lengths
nrow(subplot) == nrow(subplot.raw)

# Check all cols for NAs
apply(subplot, 2, anyNA) 


# Check for all SiteDatePlotIDs
nrow(monitor.info) # 6384 IDs
length(unique(subplot$SiteDatePlotID)) == nrow(monitor.info)
length(unique(subplot$SiteDatePlotID)) # 6338

SiteDatePlotID.missing <- monitor.info |> 
  filter(!SiteDatePlotID %in% subplot$SiteDatePlotID)
#   The 46 missing IDs come from AVRCD and 29_Palms. I have manually checked and found
#     that data for 2 subplots at AVRCD were not recorded during the 2022-04-13 monitoring event,
#     and that at 29_Palms, all 44 of the  2x2m plots were monitored, but there is no 
#     corresponding subplot data.


# Save intermediate subplot with correct monitoring info, Native/Duration/Lifeform
#   but not SeededSpecies yet addressed
subplot1 <- subplot



# Correct SpeciesSeeded column --------------------------------------------

# Address seeded species codes not in mix 

# Examine possible SpeciesSeeded values
#   then deal with each separately in own section
unique(subplot$SpeciesSeeded)


# Species seeded but not listed in mix by code
subplot.seeded <- subplot %>% 
  filter(SpeciesSeeded %in% c("Yes", "Y", "y", "Yes?"))

setdiff(unique(subplot.seeded$Code), unique(mix$CodeOriginal)) # discrepancies exist

species.seeded.not.in.mix <- subplot.seeded |> 
  filter(Code %in% 
           setdiff(unique(subplot.seeded$Code), unique(mix$CodeOriginal))) |> 
  select(Site, Region, PlotMix, CodeOriginal, Code, Name, Native, Duration, Lifeform,
         SpeciesSeeded) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Name) |> 
  arrange(Site) |> 
  arrange(Region)

# OUTPUT: create list of species marked seeded but not in mix
write_csv(species.seeded.not.in.mix,
          file = "data/data-wrangling-intermediate/04.1a_output-species-seeded1_seeded-not-in-mix_subplot.csv")

# EDITED: manually review and fix SpeciesSeeded status
#   SpeciesSeeded only corrected if plant was identified to species level and definitely  
#     not seeded, as well as "local Bouteloua". Unknowns retain original classification.
species.seeded.not.in.mix <- read_xlsx("data/data-wrangling-intermediate/04.1b_edited-species-seeded1_corrected-seeded-not-in-mix_subplot.xlsx")


# Species seeded and in (a) mix
#   Need to look at manually because mixes are site-specific
species.seeded.in.mix <- subplot.seeded |> 
  filter(SpeciesSeeded %in% c("Yes", "Y", "y", "Yes?"),
         !Code %in% species.seeded.not.in.mix$Code) |> 
  select(Site, Region, PlotMix, CodeOriginal, Code, Name, Native, Duration, Lifeform,
         SpeciesSeeded) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Name) |> 
  arrange(Site) |> 
  arrange(Region)

# OUTPUT: create list of species marked seeded and in mix
write_csv(species.seeded.in.mix,
          file = "data/data-wrangling-intermediate/04.1a_output-species-seeded2_seeded-in-mix_subplot.csv")

# EDITED: manually review and fix SpeciesSeeded status
#   Standardize responses so all are in format "Yes"
#   Incorrect ones corrected
species.seeded.in.mix <- read_xlsx("data/data-wrangling-intermediate/04.1b_edited-species-seeded2_corrected-seeded-in-mix_subplot.xlsx")

# Remove duplicates that resulted from standardizing "Yes" 
species.seeded.in.mix <- species.seeded.in.mix |> 
  distinct(.keep_all = TRUE)


# Species marked Unknown
species.unk.seeded <- subplot |> 
  filter(SpeciesSeeded %in% c("?", "Unk", "UNK")) |> 
  select(Site, Region, PlotMix, CodeOriginal, Code, Name, Native, Duration, Lifeform,
         SpeciesSeeded) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Name) |> 
  arrange(Site) |> 
  arrange(Region)

# OUTPUT: create list of species of unknown seeding status
write_csv(species.unk.seeded,
          file = "data/data-wrangling-intermediate/04.1a_output-species-seeded3_unk_subplot.csv")

# EDITED: manually review and fix SpeciesSeeded status
#   Unknowns marked as not seeded.
species.unk.seeded <- read_xlsx("data/data-wrangling-intermediate/04.1b_edited-species-seeded3_unk-corrected_subplot.xlsx")



# Species marked NA
species.na.seeded <- subplot |> 
  filter(is.na(SpeciesSeeded)) |> 
  select(Site, Region, PlotMix, CodeOriginal, Code, Name, Native, Duration, Lifeform,
         SpeciesSeeded) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Name) |> 
  arrange(Site) |> 
  arrange(Region)

# OUTPUT: create list of species of NA seeding status
write_csv(species.na.seeded,
          file = "data/data-wrangling-intermediate/04.1a_output-species-seeded4_NA_subplot.csv")

# EDITED: manually review and fix SpeciesSeeded status
#   Unknowns marked as not seeded.
#   0 code marked as "0" (no plants were present)
#  Seeded status changed if identified to species level and it was seeded.
species.na.seeded <- read_xlsx("data/data-wrangling-intermediate/04.1b_edited-species-seeded4_NA-corrected_subplot.xlsx")




# Species marked not seeded
subplot.no <- subplot |> 
  filter(SpeciesSeeded %in% c("No", "N"))

species.no.seeded <- subplot.no |> 
  select(Site, Region, PlotMix, CodeOriginal, Code, Name, Native, Duration, Lifeform,
         SpeciesSeeded) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Name) |> 
  arrange(Site) |> 
  arrange(Region)

# OUTPUT: create list of species marked not seeded
write_csv(species.no.seeded,
          "data/data-wrangling-intermediate/04.1a_output-species-seeded5_no_subplot.csv")

# EDITED: manually review and fix SpeciesSeeded status
#   Standardize responses so all are in format "Yes"
#   Incorrect ones corrected
species.no.seeded <- read_xlsx("data/data-wrangling-intermediate/04.1b_edited-species-seeded5_corrected-not-seeded_subplot.xlsx")

# Remove duplicates that resulted from standardizing "No" 
species.no.seeded <- species.no.seeded|> 
  distinct(.keep_all = TRUE)


# Compile list of correct SpeciesSeeded
#   Correct SpeciesSeeded status based on PlotMix and Site
seeded.correct <- bind_rows(species.seeded.in.mix,
                            species.seeded.not.in.mix,
                            species.unk.seeded,
                            species.na.seeded,
                            species.no.seeded) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(PlotMix) |> 
  arrange(Name) |> 
  arrange(Site) |> 
  arrange(Region) |> 
  arrange(desc(SpeciesSeeded))

#   Look for duplicates with conflicting SpeciesSeeded info
seeded.correct.dup.codes <- seeded.correct[duplicated(seeded.correct[ , 1:9]), ] # this only gets
#                                                       half of the duplicates, not both
seeded.correct.dup <- seeded.correct |> 
  filter(Code %in% seeded.correct.dup.codes$Code) |> 
  arrange(PlotMix) |> 
  arrange(Code) |> 
  arrange(Site) # this gets ones that also aren't
#     duplicates, they are the same code but different plots or sites (I couldn't figure out
#                                                   how to get both duplicates in their own df)

# OUTPUT: list of codes with conflicting SpeciesSeeded
#   list also includes ones that are not duplicates
write_csv(seeded.correct.dup,
          file = "data/data-wrangling-intermediate/04.1a_output-species-seeded6_conflicting-SpeciesSeeded.csv")

# EDITED: manually add Retain column to indicate if rows should be retained or not
#   Retained: ones that are not actually duplicates/conflicts; ones marked "No" for SpeciesSeeded
seeded.correct.dup <- read_xlsx("data/data-wrangling-intermediate/04.1b_edited-species-seeded6_conflicting-SpeciesSeeded-fixed.xlsx")

# Create correct df of codes that had duplicates (including rows that weren't actually duplicates)
seeded.correct.fixed <- seeded.correct.dup |> 
  filter(Retain == 1) |> 
  select(-Retain)

# Remove all codes that have been fixed and add back the fixed ones
seeded.correct <- seeded.correct |> 
  filter(!Code %in% seeded.correct.fixed$Code) |> 
  bind_rows(seeded.correct.fixed) |> 
  arrange(Code) |> 
  arrange(Site) |> 
  arrange(Region)

# Check possible values of SpeciesSeeded
unique(seeded.correct$SpeciesSeeded) # should be 0, Yes, No

# Look for codes that exist in subplot data but not in seeded.correct
setdiff(unique(subplot$Code), unique(seeded.correct$Code)) # should be 0


# Assign corrected SpeciesSeeded to subplot data
subplot <- subplot |> 
  select(-SpeciesSeeded)
subplot <- left_join(subplot, seeded.correct)

nrow(subplot) == nrow(subplot.raw)


# Look for NAs
unique(subplot$SpeciesSeeded)
sub.spec.na <- subplot |> 
  filter(is.na(SpeciesSeeded))
sub.spec.na
seeded.correct.subspec.na <- seeded.correct |> 
  filter(Code %in% sub.spec.na$Code)
# Not sure why this Seeded unknown grass from TLE isn't working with left_join()
#   but it was seeded, so assign it as Yes for SpeciesSeeded
subplot <- subplot |> 
  mutate(SpeciesSeeded = case_when(
    Code == "SUNGR.TLE" ~ "Yes",
    TRUE ~ SpeciesSeeded))
unique(subplot$SpeciesSeeded) # standardized to "No", "Yes", and "0"


# Save intermediate subplot with correct SeededSpecies,
#   but PlantSource not yet addressed
subplot2 <- subplot



# Add PlantSource column --------------------------------------------------

# Because plants couldn't always be identified to the species level, H. Farrell
#   grouped them by Native_Recruited, Invasive, and Seeded. 

unique(subplot$Native)
unique(subplot$SpeciesSeeded)

subplot <- subplot |> 
  mutate(PlantSource = paste0(subplot$Native, "_", subplot$SpeciesSeeded))
unique(subplot$PlantSource)

# Create Source column
subplot <- subplot |> 
  mutate(PlantSource = case_when(
    PlantSource == "0_0" ~ "0",
    PlantSource == "Unknown_No" ~ "Unknown_recruit",
    PlantSource == "Native_No" ~ "Native_recruit",
    PlantSource == "Introduced_No" ~ "Introduced/Invasive",
    PlantSource == "Native_Yes" ~ "Seeded",
    PlantSource == "Native/Unknown_No" ~ "Likely native_recruit",
    PlantSource == "Unknown_Yes" ~ "Seeded"))
unique(subplot$PlantSource)


# Add SiteDateID ----------------------------------------------------------

subplot <- subplot |> 
  left_join(monitor.site) |> 
  select(Region, Site, Date_Seeded, Date_Monitored, SiteDateID,
         Plot, Treatment, PlotMix, SiteDatePlotID,
         CodeOriginal, Code, Name, Native, Duration, Lifeform, SpeciesSeeded, PlantSource,
         Count, Height, raw.row) |> 
  arrange(SiteDatePlotID) |> 
  arrange(SiteDateID)


# Write clean subplot data to csv -----------------------------------------

write_csv(subplot,
          file = "data/cleaned/04.1_subplot-data_clean.csv")


save.image("RData/04.1_data-wrangling_subplot.RData")

