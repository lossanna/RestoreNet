library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllSubplotData") %>% 
  rename(Code = Species_Code) # rename to standardize 
plot.2x2.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")
species.raw <- read_xlsx("data/raw/plant-species_native-status.xlsx")
mix <- read_xlsx("data/raw/seed-mix.xlsx")


# Notes about manual edits ------------------------------------------------

# For manual edits to CSVs, the CSV is written from R, copied and named a new name, edited, 
      # and new file is read into R

# Files that start with "output" are ones written from R
# Files that start with "edited" are manually edited and read back in as new objects,
  # but then usually used to alter existing objects



####### Unique (location-independent) #####################################


# Add lifeform information to unique species list -------------------------

# Remove location data from working species list
species.unique <- species.raw %>% 
  select(-Region)

# Extract lifeform (functional group) information from subplot.raw data
subplot.lifeform <- subplot.raw %>% 
  select(Code, Functional_Group) %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(Code != "0") %>% 
  rename(Code = Code,
         Lifeform = Functional_Group)

# Add subplot lifeform information to working species list
species.unique <- left_join(species.unique, subplot.lifeform)

# Standardize lifeform names to Grass/Forb/Shrub
unique(species.unique$Lifeform)

species.unique <- species.unique %>% 
  mutate(Lifeform = case_when(
    str_detect(species.unique$Name, "forb") ~ "Forb",
    str_detect(species.unique$Name, "grass") ~ "Grass",
    str_detect(species.unique$Name, "shrub") ~ "Shrub",
    TRUE ~ species.unique$Lifeform))

species.unique <- species.unique %>% 
  select(Code, Name, Native, Lifeform) %>% 
  mutate(Lifeform = case_when(
    str_detect(species.unique$Lifeform, "Shrub/subshrub") ~ "Shrub", # standardize to grass/forb/shrub
    str_detect(species.unique$Lifeform, "shrub") ~ "Shrub",
    str_detect(species.unique$Lifeform, "C3 grass") ~ "Grass",
    TRUE ~ species.unique$Lifeform)) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code) # >345 because some don't have lifeform assigned

unique(species.unique$Lifeform) # lifeform names have been standardized, with NAs


# Check number of unique codes
length(unique(species.raw$Code)) # should be 345 unique codes from original table
length(species.raw$Code) # Species at multiple locations create duplicate codes
length(unique(species.unique$Code)) # 345 unique codes in working species list


# Find species without lifeform information and write to csv, to manually add information
unique(species.unique$Lifeform) 

lifeform.na <- species.unique %>% 
  filter(is.na(Lifeform) |
         Lifeform == "NA") %>% 
  select(Code, Name, Lifeform) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code)

write_csv(lifeform.na,
          file = "data/raw/output_lifeform-na.csv")


#### edit new file manually to add lifeform ######

lifeform.na.edit <- read_csv("data/raw/edited_lifeform-na.csv")


# Add manually edited lifeforms (species without lifeform) to working species list 
  # split up species.unique because left_join() will not override and will create duplicates,
    # and information from edited version is definitely correct (information from subplot.raw could be wrong)

species.lifeform <- species.unique %>%  # known lifeform species
  filter(!Code %in% lifeform.na.edit$Code) 

species.lifeform.na <- species.unique %>%  # unknown lifeform species
  filter(Code %in% lifeform.na.edit$Code) %>% 
  select(-Lifeform) %>% 
  distinct(.keep_all = TRUE) %>%
  arrange(Code) # dimensions are different than lifeform.na.edit because HAGL was a duplicate now removed
species.lifeform.na <- left_join(species.lifeform.na, lifeform.na.edit) %>% 
  distinct(.keep_all = TRUE)

species.unique <- bind_rows(species.lifeform, species.lifeform.na) %>% # combine known & unknown 
  arrange(Code) # some duplicates removed, but not yet to 345 unique codes
  
unique(species.unique$Lifeform) # lifeform has been standardized, with NAs as "Unknown"




# Add duration information to unique species list -------------------------
  # (All duration information must be added manually from USDA Plants)
  # Multiple lifeforms for same species are also corrected (wrong ones deleted)

# Write output with native status and lifeform columns
write_csv(species.unique,
          file = "data/raw/output_species-unique-subplot_known-code_native-lifeform.csv")


#### edit new file manually to add duration ####

species.unique <- read_csv("data/raw/edited_species-unique-subplot_known-code_native-lifeform-duration.csv")




# Add codes from data not included in species list ------------------------

# Codes that were observed and recorded in subplot data but not species list
codes.missing.out.sub <- data.frame(Code = sort(setdiff(unique(subplot.raw$Code), unique(species.unique$Code))))
codes.missing.out.sub$Name <- rep(NA, nrow(codes.missing.out.sub))
codes.missing.out.sub$Native <- rep(NA, nrow(codes.missing.out.sub))
codes.missing.out.sub$Lifeform <- rep(NA, nrow(codes.missing.out.sub))

write_csv(codes.missing.out.sub,
          file = "data/raw/output_codes-missing-subplot.csv")

codes.missing.subplot <- subplot.raw %>% 
  filter(Code %in% codes.missing.out.sub$Code) %>% 
  select(Code, Functional_Group, `Certainty_of_ID(1-3)`) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code)


#### edited new file manually to add native status, lifeform, and duration ####

codes.missing.sub.edit <- read_csv("data/raw/edited_codes-missing-subplot.csv")


# Add manually edited unknown codes to unique species list
species.unique <- bind_rows(species.unique, codes.missing.sub.edit) %>% 
  arrange(Code) %>% 
  distinct(.keep_all = TRUE)



# Codes from AllPlotData (2 x 2 m plots)
codes.missing.2x2 <- plot.2x2.raw %>% 
  select(starts_with("Additional"))
codes.missing.2x2 <- codes.missing.2x2 %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(everything(), names_to = "drop", values_to = "Code") %>% 
  select(Code) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code)







####### Location dependent ################################################

# Add details about seeded species to location-dependent species l --------
# Location-dependent has information about 

# Add columns from seed mix table (mix) to working species list (species)
species <- left_join(species.raw, mix)

# Extract seeded species from subplot data (subplot.raw) and assign Region based on Site
# (subplot data only has Site information, not Region)
seeded.subplot <- subplot.raw %>% 
  filter(`Seeded(Yes/No)` == "Yes") %>% 
  select(Site, Seed_Mix, Code) %>% 
  distinct(.keep_all = TRUE)
seeded.subplot <- seeded.subplot %>% 
  mutate(Region = case_when(
    str_detect(seeded.subplot$Site, c("AguaFria|BabbittPJ|MOWE|Spiderweb|BarTBar|FlyingM|PEFO|TLE")) ~ "Colorado Plateau",
    str_detect(seeded.subplot$Site, c("CRC|UtahPJ|Salt_Desert")) ~ "Utah",
    str_detect(seeded.subplot$Site, c("29_Palms|AVRCD")) ~ "Mojave",
    str_detect(seeded.subplot$Site, c("Creosote|Mesquite")) ~ "Chihuahuan",
    str_detect(seeded.subplot$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(seeded.subplot$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central",
    TRUE ~ "unk")) %>% 
  rename(Code = Code)
filter(seeded.subplot, Region == "unk") # all have been assigned a Region

# Add new Region column to working species list (species)
species <- left_join(species, seeded.subplot)
species <- species %>% 
  mutate(SeedingRate = ifelse(is.na(SeedingRate), 0, SeedingRate),
         Mix = ifelse(is.na(Mix), "not seeded", Mix))




save.image("RData/curate-species-list.RData")
