library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllSubplotData")
plot.2x2.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")
species.in <- read_csv("data/cleaned/species-list_subplot_location-independent_clean.csv")
species.de <- read_csv("data/cleaned/species-list_subplot_location-dependent_clean.csv")
mix <- read_xlsx("data/raw/master-seed-mix.xlsx")



############################ SUBPLOT DATA ##########################


# Organize subplot data columns -------------------------------------------

# Narrow down subplot.raw columns
subplot <- subplot.raw %>% 
  select(-Recorder_Initials, -Functional_Group, -`Certainty_of_ID(1-3)`, -Notes) %>% 
  mutate(raw.row = 1:nrow(subplot.raw)) %>% # row number is to be able to easily refer back to the raw data and excluded columns if needed
  rename(Code = Species_Code,
         Count = Seedling_Count,
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




# Subplot codes -----------------------------------------------------------


# Handle NA codes for subplot data ----------------------------------------

# Extract NA codes
filter(subplot, is.na(Code)) # rows 8610, 9318, 12166

# Rows 8610 and 9318 are observations for empty plots; Code should be 0
subplot$Code[subplot$raw.row == 8610] <- "0"
subplot$Code[subplot$raw.row == 9318] <- "0"


# Examine non-empty subplots
subplot.raw[12166, ]
subplot.raw[12166, c("Species_Code", "Functional_Group", "Seeded(Yes/No)", "Notes")]
  # No notes for 12166, but a functional group was listed; not seeded, and probably an unknown

# Assign location-dependent code for 12166
subplot$Code[subplot$raw.row == 12166] <- "UNFO.12166.assigned"

# Check again for NA codes
filter(subplot, is.na(Code)) # no NAs



# Standardize incorrect codes for subplot data ----------------------------

# Extract incorrect codes
setdiff(unique(subplot$Code), unique(c(species.de$Code, species.in$Code)))

# Replace codes
subplot$Code[subplot$Code == "S-PASM"] <- "PASM"
subplot$Code[subplot$Code == "S-HEBO"] <- "HEBO"
subplot$Code[subplot$Code == "SPAMA"] <- "SPAM2"
subplot$Code[subplot$Code == "ARPUP6"] <- "ARPU9"
subplot$Code[subplot$Code == "EUPO3"] <- "CHPO12"


# Check for missing codes by comparing subplot data to both species lists
sub.codes <- c(species.de$Code, species.in$Code)
setdiff(subplot$Code, sub.codes) 




# Replace location-dependent codes with Code.Site and add species info -----

# Separate out location-dependent observations
subplot.de <-subplot %>% 
  filter(Code %in% species.de$Code)
subplot.de <- left_join(subplot.de, species.de)

# Remove old Code column with Code.Site and rename to match location-independent col
subplot.de <- subplot.de %>% 
  select(-Code) %>% 
  rename(Code = Code.Site)



# Add species info for location-independent codes -------------------------

subplot.in <- subplot %>% 
  filter(Code %in% species.in$Code)

subplot.in <- left_join(subplot.in, species.in) 


# Combine location in/de for subplot --------------------------------------

subplot <- bind_rows(subplot.in, subplot.de) %>% 
  arrange(raw.row)

# Check that there the same number of observations as the original subplot data
nrow(subplot) == nrow(subplot.raw)




# Write clean subplot data to csv -----------------------------------------

write_csv(subplot,
          file = "data/cleaned/subplot-data_clean.csv")






#### This chunk relates to 01_curate-species-list.R ###################


# Address native status for unknown seeded species ------------------------

# After making a species list and combining that with the subplot data, we see that some unknown species
  # were actually seeded, and therefore native. The native status is fixed in the 01_curate-species-list.R
  # script (they do not need to be fixed here), but this section shows how I determined which species to address
  # in the 01_curate-species-list.R script.

subplot.seeded <- subplot %>% 
  filter(Seeded == "Yes")

subplot.seeded <- left_join(subplot.seeded, mix) %>% 
  select(-Family, -Scientific, -Common)

# Extract Genus spp. observations
seeded.spp <- subplot.seeded %>% 
  filter(str_detect(subplot.seeded$Name, "spp."))
unique(seeded.spp$Name)


# Check if they are location-dependent
elymus.spp <- subplot.seeded %>% 
  filter(Name == "Elymus spp.")
unique(elymus.spp$Site) # not location-dependent

solanum.spp <- subplot.seeded %>% 
  filter(Name == "Solanum spp.")
unique(solanum.spp$Site) # not location-dependent

stipa.spp <- subplot.seeded %>% 
  filter(Name == "Stipa spp.")
unique(stipa.spp$Site) # not location-dependent

sporobolus.spp <- subplot.seeded %>% 
  filter(Name == "Sporobolus spp.")
unique(sporobolus.spp$Site) # not location-dependent

# Write "Genus spp." names to CSV because they are not location-dependent and existing list can be altered
seeded.spp.names <- data.frame(V1 = unique(seeded.spp$Name)) 

write_csv(seeded.spp.names,
          file = "data/raw/output-wrangling_genusspp.-seeded-species.csv")


# Extract "unk" unknown species
seeded.unk <- subplot.seeded %>% 
  filter(str_detect(subplot.seeded$Name, "Unk|unk"))
unique(seeded.unk$Name)
seeded.unk.names <- data.frame(V1 = unique(seeded.unk$Name)) 

write_csv(seeded.unk.names,
          file = "data/raw/output-wrangling_unknown-seeded-species.csv")


# Check observations of known species

# Check if it worked after fixing 01_curate-species-list.R and writing new CSVs
unique(subplot.seeded$Native)
subplot.seeded %>% 
  filter(Native == "Introduced")

whyisthishappening <- subplot.seeded %>% 
  filter(Native == "Native/Unknown")

reallywhy <- subplot.seeded %>% 
  filter(Native == "Unknown")


########### Chunk related to 01_curate-species-list.R complete ############




# Subplot data for seeded species only ------------------------------------

subplot.seeded <- subplot %>% 
  filter(Seeded == "Yes")

subplot.seeded <- left_join(subplot.seeded, mix) %>% 
  select(-Family, -Scientific, -Common)


save.image("RData/02_data-wrangling.RData")
