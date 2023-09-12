# Created: 2022-11-29
# Last updated: 2023-09-12

# Purpose: Create clean data table for subplot data, with corrected and standardized species information,
#   and monitoring and plot information. 
#   Essentially, add corrected metadata to subplot data.

library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllSubplotData")
species.in <- read_csv("data/cleaned/subplot_species-list_location-independent_clean.csv")
species.de <- read_csv("data/cleaned/01_subplot_species-list_location-dependent_clean.csv")
species.de.all <- read_csv("data/cleaned/species-list_location-dependent_clean.csv")
mix <- read_xlsx("data/raw/from-Master_seed-mix_LO.xlsx", sheet = "with-site_R")
monitor.info <- read_csv("data/cleaned/corrected-monitoring-info_clean.csv")



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
filter(subplot, is.na(CodeOriginal)) # raw.row 8610, 9318, 12166

# Rows 8610 and 9318 are observations for empty plots; Code should be 0
subplot$CodeOriginal[subplot$raw.row == 8610] <- "0"
subplot$CodeOriginal[subplot$raw.row == 9318] <- "0"

# Examine non-empty subplots (12166)
subplot.raw[12166, ]
subplot.raw[12166, c("Species_Code", "Functional_Group", "Seeded(Yes/No)", "Notes")]
#   No notes for 12166, but a functional group was listed; not seeded, and probably an unknown

# Assign location-dependent code for 12166
subplot$CodeOriginal[subplot$raw.row == 12166] <- "UNFO.12166.Salt_Desert"

# Add code to subplot species.de list
subplot.raw[12166, c("Site", "Species_Code", "Functional_Group", "Seeded(Yes/No)")]
unfo12166 <- data.frame(Region = "Utah",
                        Site = "Salt_Desert",
                        CodeOriginal = "UNFO.12166.Salt_Desert",
                        Code = "UNFO.12166.Salt_Desert",
                        Name = "Unknown forb, not seeded",
                        Native = "Unknown",
                        Duration = "Unknown",
                        Lifeform = "Forb")
species.de <- bind_rows(species.de, unfo12166)

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
apply(subplot.de, 2, anyNA) # NAs for Count, Height, SpeciesSeeded inherent to data



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
nrow(subplot) == nrow(subplot.raw) # something is wrong

dup.rawrow <- count(subplot, raw.row) |> 
  filter(n > 1) |> 
  arrange(desc(n))

subplot.dup <- subplot |> 
  filter(raw.row %in% dup.rawrow$raw.row)



# Check if Introduced plants were marked as Seeded ------------------------

subplot.inva <- subplot %>% 
  filter(Native == "Introduced") %>% 
  select(Code, SpeciesSeeded, Name, Native) %>% 
  distinct(.keep_all = TRUE)
unique(subplot.inva$SpeciesSeeded) # something is mislabeled; no introduced species were seeded
subplot.inva %>% 
  filter(SpeciesSeeded == "Yes") # Eragrostis curvula was not seeded

# Fix Eragrostis curvula - mark all observations as "No" for Seeded col
subplot$SpeciesSeeded[subplot$Name == "Eragrostis curvula"] <- "No"



# Check again for NA codes ------------------------------------------------

filter(subplot, is.na(Code)) # no NA codes
filter(subplot, is.na(Name)) # no NA names




# Correct monitoring info -------------------------------------------------

# Create df of original (incorrect) monitoring data to generate MonitorID for subplot data
  # to match correct monitoring data (monitor.info) with left_join()
monitor.assign <- subplot %>% 
  select(Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix) %>% 
  distinct(.keep_all = TRUE) 
monitor.assign <- monitor.assign %>% 
  mutate(MonitorID = 1:nrow(monitor.assign)) # monitor.assign is in same row order as monitor.info

# Add MonitorID to subplot data
subplot <- left_join(subplot, monitor.assign)
filter(subplot, is.na(MonitorID)) # all assigned MonitorID

# Remove monitoring info from subplot data because some of it is wrong
subplot <- subplot %>% 
  select(-Date_Seeded, -Date_Monitored, -Plot, -Treatment, -PlotMix)

# Add corrected monitoring info with left_join()
subplot <- left_join(subplot, monitor.info)
subplot <- subplot %>% 
  select(Site, Region, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix,
         CodeOriginal, Code, Name, Native, Duration, Lifeform, Count, Height,
         SpeciesSeeded, raw.row, MonitorID) # reorder cols

# Check all cols for NAs
apply(subplot, 2, anyNA) 



# Address seeded species codes not in mix ---------------------------------

# Examine possible SpeciesSeeded values
#   then deal with each separately in own section
unique(subplot$SpeciesSeeded)


# Species seeded but not listed in mix by code
subplot.seeded <- subplot %>% 
  filter(SpeciesSeeded %in% c("Yes", "Y", "y", "Yes?"))

setdiff(unique(subplot.seeded$CodeOriginal), unique(mix$CodeOriginal)) # discrepancies exist

species.seeded.not.in.mix <- subplot.seeded |> 
  filter(CodeOriginal %in% 
           setdiff(unique(subplot.seeded$CodeOriginal), unique(mix$CodeOriginal))) |> 
  select(Site, Region, PlotMix, CodeOriginal, Code, Name, Native, Duration, Lifeform,
         SpeciesSeeded) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Name) |> 
  arrange(Site) |> 
  arrange(Region)

# OUTPUT: create list of species marked seeded but not in mix
write_csv(species.seeded.not.in.mix,
          file = "data/raw/03.1a_output-species-seeded1_seeded-not-in-mix_subplot.csv")

# EDITED: manually review and fix SpeciesSeeded status
#   SpeciesSeeded only corrected if plant was identified to species level and definitely  
#     not seeded, as well as "local Bouteloua". Unknowns retain original classification.
species.seeded.not.in.mix <- read_xlsx("data/raw/03.1b_edited-species-seeded1_corrected-seeded-not-in-mix_subplot.xlsx")


# Species seeded and in mix
species.seeded.in.mix <- subplot.seeded |> 
  filter(SpeciesSeeded %in% c("Yes", "Y", "y", "Yes?"),
         !CodeOriginal %in% species.seeded.not.in.mix$CodeOriginal) |> 
  select(Site, Region, PlotMix, CodeOriginal, Code, Name, Native, Duration, Lifeform,
         SpeciesSeeded) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Name) |> 
  arrange(Site) |> 
  arrange(Region)

# OUTPUT: create list of species marked seeded but not in mix
write_csv(species.seeded.in.mix,
          file = "data/raw/03.1a_output-species-seeded2_seeded-in-mix_subplot.csv")

# EDITED: manually review and fix SpeciesSeeded status
#   Standardize responses so all are in format "Yes"
#   Incorrect ones corrected
species.seeded.in.mix <- read_xlsx("data/raw/03.1b_edited-species-seeded2_corrected-seeded-in-mix_subplot.xlsx")



# Species marked NA or unknown
species.unk.seeded <- subplot |> 
  filter(SpeciesSeeded %in% c("?", "Unk", "UNK", NA)) |> 
  select(Site, Region, PlotMix, CodeOriginal, Code, Name, Native, Duration, Lifeform,
         SpeciesSeeded) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Name) |> 
  arrange(Site) |> 
  arrange(Region)

# OUTPUT: create list of species of unknown seeding status
write_csv(species.unk.seeded,
          file = "data/raw/03.1a_output-species-seeded3_unk_subplot.csv")

# EDITED: manually review and fix SpeciesSeeded status
#   Unknowns marked as not seeded.
#   0 code marked as "0" (no plants were present)
#  Seeded status changed if identified to species level and it was seeded.
species.unk.seeded <- read_xlsx("data/raw/03.1b_edited-species-seeded3_unk-corrected_subplot.xlsx")



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
          "data/raw/03.1a_output-species-seeded4_no_subplot.csv")

# EDITED: manually review and fix SpeciesSeeded status
#   Standardize responses so all are in format "Yes"
#   Incorrect ones corrected
species.no.seeded <- read_xlsx("data/raw/03.1b_edited-species-seeded4_corrected-not-seeded_subplot.xlsx")


# Compile list of correct species metadata
#   Correct SpeciesSeeded status based on PlotMix and Site
seeded.correct <- bind_rows(species.seeded.in.mix,
                            species.seeded.not.in.mix,
                            species.unk.seeded,
                            species.no.seeded) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(PlotMix) |> 
  arrange(Name) |> 
  arrange(Site) |> 
  arrange(Region) |> 
  arrange(desc(SpeciesSeeded))

setdiff(unique(subplot$Code), unique(seeded.correct$Code))


# Assign corrected species metadata to subplot data
subplot <- subplot |> 
  select(-SpeciesSeeded)
subplot <- left_join(subplot, seeded.correct)

nrow(subplot) == nrow(subplot.raw)


# Write clean subplot data to csv -----------------------------------------

write_csv(subplot,
          file = "data/cleaned/subplot-data_clean.csv")


save.image("RData/03.1_data-wrangling_subplot.RData")
