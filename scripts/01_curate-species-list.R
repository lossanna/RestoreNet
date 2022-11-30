library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllSubplotData") %>% 
  rename(Code = Species_Code) # rename to standardize 
plot.2x2.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")
species.raw <- read_xlsx("data/raw/master-species_native.xlsx")


# Notes about manual edits ------------------------------------------------

# For manual edits to CSVs, the CSV is written from R, copied and named a new name, edited, 
      # and new file is read into R

# Files in the format "output-species_xx.csv" are ones written from R
# Files in the format "edited-species_xx.csv" are manually edited and read back in as new objects,
  # but then usually used to alter existing objects


# Add lifeform information to species list --------------------------------

# Remove location data from working species list
species <- species.raw %>% 
  select(-Region)

# Extract lifeform (functional group) information from subplot.raw data
subplot.lifeform <- subplot.raw %>% 
  select(Code, Functional_Group) %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(Code != "0") %>% 
  rename(Code = Code,
         Lifeform = Functional_Group)

# Add subplot lifeform information to working species list
species <- left_join(species, subplot.lifeform)

# Standardize lifeform names to Grass/Forb/Shrub
unique(species$Lifeform)

species <- species %>% 
  mutate(Lifeform = case_when(
    str_detect(species$Name, "forb") ~ "Forb",
    str_detect(species$Name, "grass") ~ "Grass",
    str_detect(species$Name, "shrub") ~ "Shrub",
    TRUE ~ species$Lifeform))

species <- species %>% 
  select(Code, Name, Native, Lifeform) %>% 
  mutate(Lifeform = case_when(
    str_detect(species$Lifeform, "Shrub/subshrub") ~ "Shrub", # standardize to grass/forb/shrub
    str_detect(species$Lifeform, "shrub") ~ "Shrub",
    str_detect(species$Lifeform, "C3 grass") ~ "Grass",
    TRUE ~ species$Lifeform)) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code) # >345 because some don't have lifeform assigned

unique(species$Lifeform) # lifeform names have been standardized, with NAs


# Check number of unique codes
length(unique(species.raw$Code)) # should be 345 unique codes from original table
length(species.raw$Code) # Species at multiple locations create duplicate codes
length(unique(species$Code)) # 345 unique codes in working species list


# Find species without lifeform information and write to csv, to manually add information
unique(species$Lifeform) 

lifeform.na <- species %>% 
  filter(is.na(Lifeform) |
         Lifeform == "NA") %>% 
  select(Code, Name, Lifeform) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code)

write_csv(lifeform.na,
          file = "data/raw/output-species1_xlsx_lifeform-na.csv")


#### edit new file manually to add lifeform ######

lifeform.na.edit <- read_csv("data/raw/edited-species1_xlsx_lifeform-na.csv")


# Add manually-edited lifeform information to working species list 
  # split up species because left_join() will not override and will create duplicates,
    # and information from edited version is definitely correct (information from subplot.raw could be wrong)

species.lifeform <- species %>%  # known lifeform species
  filter(!Code %in% lifeform.na.edit$Code) 

species.lifeform.na <- species %>%  # unknown lifeform species
  filter(Code %in% lifeform.na.edit$Code) %>% 
  select(-Lifeform) %>% 
  distinct(.keep_all = TRUE) %>%
  arrange(Code) # dimensions are different than lifeform.na.edit because HAGL was a duplicate now removed
species.lifeform.na <- left_join(species.lifeform.na, lifeform.na.edit) %>% 
  distinct(.keep_all = TRUE)

species <- bind_rows(species.lifeform, species.lifeform.na) %>% # combine known & unknown 
  arrange(Code) # some duplicates removed, but not yet to 345 unique codes
  
unique(species$Lifeform) # lifeform has been standardized, with NAs as "Unknown"




# Add duration information to species list --------------------------------

  # (All duration information must be added manually from USDA Plants)
  # Multiple lifeforms for same species are also corrected (wrong ones deleted)

# Write output with native status and lifeform columns
write_csv(species,
          file = "data/raw/output-species2_xlsx_native-lifeform.csv")


#### edit new file manually to add duration and check lifeform ####

species <- read_csv("data/raw/edited-species2_xlsx_native-lifeform-duration.csv")




# Add codes from data not included in species list ------------------------

# Codes that were observed and recorded in subplot data but not Excel master-species list
codes.missing.sub.out <- data.frame(Code = sort(setdiff(unique(subplot.raw$Code), unique(species$Code))))
codes.missing.sub.out$Name <- rep(NA, nrow(codes.missing.sub.out))
codes.missing.sub.out$Native <- rep(NA, nrow(codes.missing.sub.out))
codes.missing.sub.out$Lifeform <- rep(NA, nrow(codes.missing.sub.out))

write_csv(codes.missing.sub.out,
          file = "data/raw/output-species3_codes-missing-subplot.csv")

codes.missing.subplot <- subplot.raw %>% # look at subplot data for comparison and ID certainty
  filter(Code %in% codes.missing.sub.out$Code) %>% 
  select(Code, Functional_Group, `Certainty_of_ID(1-3)`) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code)


#### edited new file manually to add native status, lifeform, and duration ####

codes.missing.sub.edit <- read_csv("data/raw/edited-species3_codes-missing-subplot.csv")


# Add manually-edited unknown codes to working species list
species <- bind_rows(species, codes.missing.sub.edit) %>% 
  arrange(Code) %>% 
  distinct(.keep_all = TRUE) # 407 rows



# Write clean species list to CSV -----------------------------------------

# Check for missing information
unique(species$Native)
unique(species$Duration)
unique(species$Lifeform)

# Write to csv
write_csv(species,
          file = "data/raw/output-species_final.csv")


# Codes from AllPlotData (2x2 plots) --------------------------------------

# Codes from these plots are really different and usually long descriptions
  # so they get their own separate list

# Codes from AllPlotData (2 x 2 m plots)
codes.missing.2x2 <- plot.2x2.raw %>% 
  select(starts_with("Additional")) %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(everything(), names_to = "drop", values_to = "Code") %>% 
  select(Code) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code) %>% # 382 codes
  filter(!Code %in% species$Code)

write_csv(codes.missing.2x2,
          file = "data/raw/output-species4_codes-missing-2x2plot.csv")




save.image("RData/01_curate-species-list.RData")
