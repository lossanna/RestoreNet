# manual edits made to fill in lifeform information

library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllSubplotData")
species.raw <- read_xlsx("data/raw/plant-species.xlsx")
mix <- read_xlsx("data/raw/seed-mix.xlsx")


# Curate species list -----------------------------------------------------

# Add details about seeded species
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

# Add details about lifeform (functional group)
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
    TRUE ~ species$Lifeform))


# Unique plants, no location data
species.unique <- species %>% 
  select(Code, Name, Native, Common, Mix, Lifeform) %>% 
  distinct(.keep_all = TRUE)


save.image("RData/curate-species-list.RData")
