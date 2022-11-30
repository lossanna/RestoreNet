library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllSubplotData") %>% 
  rename(Code = Species_Code) # rename to standardize 
plot.2x2.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")
species <- read_csv("data/cleaned/species_cleaned.csv")
mix <- read_xlsx("data/raw/master-seed-mix.xlsx")


# Organize columns --------------------------------------------------------

# Narrow down subplot.raw columns
subplot <- subplot.raw %>% 
  select(-Recorder_Initials, -Functional_Group, -`Certainty_of_ID(1-3)`, -Notes) %>% 
  mutate(raw.row = 1:nrow(subplot.raw)) %>% # row number is to be able to easily refer back to the raw data and excluded columns if needed
  rename(Count = Seedling_Count,
         Height = Average_Height_mm,
         Seeded = `Seeded(Yes/No)`,
         PlotMix = Seed_Mix)

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


# Replace codes with standardized ones ------------------------------------

# Extract incorrect codes
setdiff(unique(subplot$Code), unique(species$Code))

# Replace codes
subplot$Code[subplot$Code == "S-PASM"] <- "PASM"
subplot$Code[subplot$Code == "S-HEBO"] <- "HEBO"
subplot$Code[subplot$Code == "SPAMA"] <- "SPAM2"
subplot$Code[subplot$Code == "ARPUP6"] <- "ARPU9"
subplot$Code[subplot$Code == "EUPO3"] <- "CHPO12"

# Check codes
setdiff(unique(subplot$Code), unique(species$Code))


# Add native, duration, and lifeform information --------------------------

subplot <- left_join(subplot, species) %>% 
  select(raw.row, Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix,
         Code, Name, Native, Duration, Lifeform, Seeded, Count, Height) %>% 
  arrange(raw.row)

# Extract rows with multiple raw.row
raw.row.multiple <- subplot %>% 
  filter(raw.row %in% filter(subplot, duplicated(raw.row))$raw.row)

nrow(raw.row.multiple) / 2 # there are 657 rows with a duplicate raw.row (all have just 1 additional duplicate)
(nrow(subplot) - nrow(raw.row.multiple) / 2) == nrow(subplot.raw) # confirms duplicates are all in pairs

# All the duplicates are from unknown species or plants only defined to genus level
filter(raw.row.multiple, !str_detect(raw.row.multiple$Name, "Unk"))$Name



# Write clean subplot data to csv -----------------------------------------

write_csv(subplot,
          file = "data/cleaned/subplot-data_clean.csv")


# Subplot data for seeded species only ------------------------------------

subplot.seeded <- subplot %>% 
  filter(Seeded == "Yes")

subplot.seeded <- left_join(subplot.seeded, mix) %>% 
  select(-Family, -Scientific, -Common)


save.image("RData/02_data-wrangling.RData")
