library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllSubplotData") %>% 
  rename(Code = Species_Code) # rename to standardize 
plot.2x2.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")
species.raw <- read_xlsx("data/raw/master-species_native.xlsx")
mix <- read_xlsx("data/raw/master-seed-mix.xlsx")



# Add details about seeded species to location-dependent species list ------
# Location-dependent has information about 

# Add columns from seed mix table (mix) to working species list (species)
species.location <- left_join(subplot.raw, mix)

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
    TRUE ~ "unk")) 
filter(seeded.subplot, Region == "unk") # all have been assigned a Region

# Add new Region column to working species list (species)
species.location <- left_join(species.location, seeded.subplot)
species.location <- species.location %>% 
  mutate(SeedingRate = ifelse(is.na(SeedingRate), 0, SeedingRate),
         Mix = ifelse(is.na(Mix), "not seeded", Mix)) %>% 
  rename(SpeciesSeeded = Mix,
         PlotSeedMix = Seed_Mix) %>% 
  filter(Code %in% )
