# Created: 2024-12-11
# Last updated: 2024-12-11

# Purpose: Categorize species as cool season (C3) or warm season (C4) for the Sonoran Desert.

library(tidyverse)
library(readxl)

# Load data ---------------------------------------------------------------

subplot <- read_csv("data/cleaned/04.1_subplot-data_clean.csv")
present_species <- read_csv("data/cleaned/04.2_2x2-species-present_clean.csv")


# Notes about manual edits ------------------------------------------------

# For manual edits to CSVs, the CSV is written from R, copied and named a new name, edited,
#   and new file is read into R.

# Files in the format "output_xx.csv" are ones written from R.
# Files in the format "edited_xx.csv" are manually edited and read back in as new objects,
#   but then usually used to alter existing objects.
#   See README_rawdata.md for more details.


# Compile list of Sonoran Desert species ----------------------------------

sonoran.species <- present_species |> 
  filter(Region %in% c("Sonoran SE", "Sonoran Central")) |> 
  select(CodeOriginal, Code, Name, Native, Duration, Lifeform) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Code) |> 
  arrange(Lifeform) |> 
  arrange(Native)


# Assign Season (Warm/Cool) -----------------------------------------------

# OUTPUT: list of Sonoran Desert species
write_csv(sonoran.species,
          file = "data/data-wrangling-intermediate/14.1a_output1_Sonoran-species.csv")

# EDITED: add Seasonality column and assign species as Warm/Cool/Unknown
sonoran.species <- read_xlsx("data/data-wrangling-intermediate/14.1b_edited1_Sonoran-species-with-seasonality.xlsx")
sonoran.species <- sonoran.species |> 
  select(-Flowering, -Source)

# Check for NAs
apply(sonoran.species, 2, anyNA)



# Join Seasonality with Sonoran Desert subplot data -----------------------

sonoran.subplot <- subplot |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  left_join(sonoran.species)

# Check for NAs
apply(sonoran.subplot, 2, anyNA)


# Join Seasonality with Sonoran Desert 2x2 data ---------------------------

sonoran.2x2 <- present_species |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  left_join(sonoran.species)

# Check for NAs
apply(sonoran.2x2, 2, anyNA)



# Write out clean data ----------------------------------------------------

write_csv(sonoran.subplot,
          file = "data/cleaned/14.1_Sonoran-Desert_subplot_with-seasonality_clean.csv")

write_csv(sonoran.2x2,
          file = "data/cleaned/14.1_Sonoran-Desert_present-species_with-seasonality_clean.csv")
