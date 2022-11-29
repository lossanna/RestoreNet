# manual edits made to fill in lifeform information

library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllSubplotData")
species.raw <- read_xlsx("data/raw/plant-species_native-status.xlsx")
mix <- read_xlsx("data/raw/seed-mix.xlsx")


# Add details about seeded species ----------------------------------------

species <- left_join(species.raw, mix)

seeded.subplot <- subplot.raw %>% 
  filter(`Seeded(Yes/No)` == "Yes") %>% 
  select(Site, Seed_Mix, Species_Code) %>% 
  distinct(.keep_all = TRUE)
seeded.subplot <- seeded.subplot %>% 
  mutate(Region = case_when(
    str_detect(seeded.subplot$Site, c("AguaFria|BabbittPJ|MOWE|Spiderweb|BarTBar|FlyingM|PEFO|TLE")) ~ "Colorado Plateau",
    str_detect(seeded.subplot$Site, c("CRC|UtahPJ|Salt_Desert")) ~ "Utah",
    str_detect(seeded.subplot$Site, c("29_Palms|AVRCD")) ~ "Mojave",
    str_detect(seeded.subplot$Site, c("Creosote|Mesquite")) ~ "Chihuahuan",
    str_detect(seeded.subplot$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(seeded.subplot$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central",
    TRUE ~ "idk")) %>% 
  rename(Code = Species_Code)
filter(seeded.subplot, Region == "idk")

species <- left_join(species, seeded.subplot)
species <- species %>% 
  mutate(SeedingRate = ifelse(is.na(SeedingRate), 0, SeedingRate),
         Mix = ifelse(is.na(Mix), "not seeded", Mix))


# Add details about lifeform (functional group) ---------------------------

subplot.lifeform <- subplot.raw %>% 
  select(Species_Code, Functional_Group) %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(Species_Code != "0") %>% 
  rename(Code = Species_Code,
         Lifeform = Functional_Group)

species <- left_join(species, subplot.lifeform)

species <- species %>% 
  mutate(Lifeform = case_when(
    str_detect(species$Name, "forb") ~ "Forb",
    str_detect(species$Name, "grass") ~ "Grass",
    str_detect(species$Name, "shrub") ~ "Shrub",
    TRUE ~ species$Lifeform))

# Preserve location data
species.location <- species %>% 
  select(-Lifeform) %>% 
  distinct(.keep_all = TRUE)

# Unique plants, no location data 
length(unique(species.raw$Code)) # should be 345 unique codes from original

species.unique <- species %>% 
  select(Code, Name, Native, Lifeform) %>% 
  mutate(Lifeform = case_when(
    str_detect(species$Lifeform, "Shrub/subshrub") ~ "Shrub", # standardize to grass/forb/shrub
    str_detect(species$Lifeform, "shrub") ~ "Shrub",
    str_detect(species$Lifeform, "C3 grass") ~ "Grass",
    TRUE ~ species$Lifeform)) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code) # >345 because some don't have lifeform assigned


# Find species without lifeform information and write to csv
unique(species.unique$Lifeform)
unique(subplot.raw$Functional_Group)

lifeform.na <- species.unique %>% 
  filter(is.na(Lifeform) |
         Lifeform == "NA") %>% 
  select(Code, Name, Lifeform) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code)

write_csv(lifeform.na,
          file = "data/raw/lifeform-na_output.csv")

#### edited new file manually to add lifeform ####

lifeform.na.edit <- read_csv("data/raw/lifeform-na_edited.csv")

# Add manually edited lifeforms to species list
species.lifeform <- species.unique %>% 
  filter(!Code %in% lifeform.na.edit$Code) # split up species.unique because left_join() will not override and create duplicates

species.lifeform.na <- species.unique %>% 
  filter(Code %in% lifeform.na.edit$Code) %>% 
  select(-Lifeform) %>% 
  distinct(.keep_all = TRUE) %>%
  arrange(Code) # dimensions are different than lifeform.na.edit because HAGL was a duplicate now removed
species.lifeform.na <- left_join(species.lifeform.na, lifeform.na.edit) %>% 
  distinct(.keep_all = TRUE)

species.unique <- bind_rows(species.lifeform, species.lifeform.na) %>% 
  arrange(Code) # some duplicates removed, but not yet to 345 unique codes
  
unique(species.unique$Lifeform) # lifeform has been standardized

write_csv(species.unique,
          file = "data/raw/species-unique_native-lifeform_output.csv")

# Codes that were observed and recorded in subplot data but not species list
codes.missing <- data.frame(Code = sort(setdiff(unique(subplot.raw$Species_Code), unique(species.unique$Code))))





save.image("RData/curate-species-list.RData")
