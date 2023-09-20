# Created: 2023-09-18
# Last updated: 2023-09-20

# Purpose: in merging the species data with the subplot data (actual observations), we see that some unknown
#   species were seeded, and therefore native, but it is impossible to know this without first producing
#   a species list. However, the cleaned species list should be final and generated all from one script,
#   so this script is to produce a dependency to be loaded/used partway through 01_curate-species-list.R
#   Having a separate script and writing out/reading in intermediate dependencies will ensure things 
#   don't break or are overwritten.

# The CSVs to be read in are created partway through 01_curate-species-list.R, in two different sections
#   (separate for location dependent vs. independent).


library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

species.in.intermed <- read_csv("data/data-wrangling-intermediate/01-dependency_species-list_location-independent.csv")
species.de.intermed <- read_csv("data/data-wrangling-intermediate/01-dependency_species-list_location-dependent.csv")
subplot.raw <- read_xlsx("data/raw/2023-09-15_Master 1.0 Germination Data_raw.xlsx", sheet = "AllSubplotData")


# Set up subplot data -----------------------------------------------------

# Narrow down subplot.raw columns
subplot <- subplot.raw %>% 
  select(-Recorder_Initials, -Functional_Group, -`Certainty_of_ID(1-3)`, -Notes) %>% 
  mutate(raw.row = 1:nrow(subplot.raw)) %>% # row number is to be able to easily refer back to the raw data and excluded columns if needed
  rename(CodeOriginal = Species_Code,
         Count = Seedling_Count,
         Height = Average_Height_mm,
         Seeded = `Seeded(Yes/No)`,
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

# Location independent
subplot.in <- subplot %>% 
  filter(CodeOriginal %in% species.in.intermed$CodeOriginal)

subplot.in <- left_join(subplot.in, species.in.intermed) |> 
  select(Site, CodeOriginal, Seeded, Region, Code, Name, Native, Duration, Lifeform) |> 
  distinct(.keep_all = TRUE)

# Location dependent
subplot.de.seeded <-subplot %>% 
  filter(CodeOriginal %in% species.de.intermed$CodeOriginal) |> 
  filter(Seeded == "Yes") |> 
  select(Site, CodeOriginal, Seeded) |> 
  distinct(.keep_all = TRUE)

subplot.de.seeded <- left_join(subplot.de.seeded, species.de.intermed)

# Combine
subplot <- bind_rows(subplot.in, subplot.de.seeded)



# Address native status for unknown seeded species ------------------------

# Filter out seeded species
subplot.seeded <- subplot %>% 
  filter(Seeded == "Yes")

# Check Native status
unique(subplot.seeded$Native) # seeded species should all be native


# Extract names that were seeded but not marked Native
seeded.marked.notnative <- subplot.seeded %>% 
  filter(Native != "Native") %>% 
  select(Code, Name) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Name)
seeded.marked.notnative |> 
  print(n = 30)

# Remove ERCU2 & ERCI6 because they are non-native and not seeded
seeded.marked.notnative <- seeded.marked.notnative %>% 
  filter(!Code %in% c("ERCU2", "ERCI6"))

# Write list of names to CSV for 01_curate-species-list.R
  # Codes are already location-specific
write_csv(seeded.marked.notnative,
          file = "data/data-wrangling-intermediate/01-dependency_seeded-species-to-be-marked-native.csv")



