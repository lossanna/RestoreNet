library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllSubplotData") %>% 
  rename(Code = Species_Code) # rename to standardize 
plot.2x2.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")
species <- read_csv("data/raw/output-species_final.csv")
mix <- read_xlsx("data/raw/master-seed-mix.xlsx")


# All subplot data --------------------------------------------------------

# Narrow down subplot.raw columns
subplot <- subplot.raw %>% 
  select(-Recorder_Initials, -Functional_Group, -`Certainty_of_ID(1-3)`, -Notes) %>% 
  mutate(raw.row = 1:nrow(subplot.raw)) %>% # row number is to be able to easily refer back to the raw data and excluded columns if needed
  rename(Count = Seedling_Count,
         Height = Average_Height_mm,
         Seeded = `Seeded(Yes/No)`)

# Add Region
subplot <- subplot %>% 
  mutate(Region = case_when(
    str_detect(subplot$Site, c("AguaFria|BabbittPJ|MOWE|Spiderweb|BarTBar|FlyingM|PEFO|TLE")) ~ "Colorado Plateau",
    str_detect(subplot$Site, c("CRC|UtahPJ|Salt_Desert")) ~ "Utah",
    str_detect(subplot$Site, c("29_Palms|AVRCD")) ~ "Mojave",
    str_detect(subplot$Site, c("Creosote|Mesquite")) ~ "Chihuahuan",
    str_detect(subplot$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(subplot$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central",
    TRUE ~ "unk")) 
filter(subplot, Region == "unk") # all have been assigned a Region

# Convert character NAs
sapply(subplot, class) # temporarily convert date columns to character to replace NAs
subplot <- subplot %>% 
  mutate(Date_Seeded = as.character(Date_Seeded),
         Date_Monitored = as.character(Date_Monitored))
subplot[subplot == "NA"] <- NA
apply(subplot, 2, anyNA) # check all columns for NAs
  # NAs in Code, Count, Height, Seeded
subplot <- subplot %>% # convert to date
  mutate(Date_Seeded = as.Date(Date_Seeded),
         Date_Monitored = as.Date(Date_Monitored))


# Standardize codes for known species -------------------------------------

# Extract species with multiple codes for the same name, retaining all codes
codes.multiple <- species %>% 
  filter(Name %in% filter(species, duplicated(Name))$Name) %>% 
  arrange(Name) 

# Examine exact species only
codes.fix <- codes.multiple %>% 
  filter(!str_detect(Name, "spp.|Unk|0"))

# Compare codes with those from species list
species %>% 
  filter(Name %in% codes.fix$Name) # both versions are in species list, also

# Compare codes with those from seed mix
mix.codes <- mix %>% 
  filter(Code %in% codes.fix$Code) %>% 
  select(Scientific, Code) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code)



# Subplot data for seeded species only ------------------------------------

subplot.seeded <- subplot %>% 
  filter(Seeded == "Yes")
