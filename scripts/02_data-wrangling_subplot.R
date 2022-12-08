library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllSubplotData")
species.all.in <- read_csv("data/cleaned/species-list_all_location-independent.csv")
species.all.de <- read_csv("data/cleaned/species-list_all_location-dependent.csv")
subplot.codes <- read_csv("data/cleaned/subplot-codes_clean.csv")
mix <- read_xlsx("data/raw/master-seed-mix.xlsx")


# Organize columns --------------------------------------------------------

# Narrow down subplot.raw columns
subplot <- subplot.raw %>% 
  select(-Recorder_Initials, -Functional_Group, -`Certainty_of_ID(1-3)`, -Notes) %>% 
  mutate(raw.row = 1:nrow(subplot.raw)) %>% # row number is to be able to easily refer back to the raw data and excluded columns if needed
  rename(Code = Species_Code,
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
    str_detect(subplot$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central",
    TRUE ~ "unk")) 
filter(subplot, Region == "unk") # all have been assigned a Region

# Convert character "NA" to logical NA
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



# Handle NA codes ---------------------------------------------------------

# Extract NA codes
filter(subplot, is.na(Code)) # raw.row 8610, 9318, 12166

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



# Standardize incorrect codes ---------------------------------------------

# Extract incorrect codes
setdiff(unique(subplot$Code), unique(c(species.de$CodeOriginal, species.in$CodeOriginal)))

# Replace codes
subplot$Code[subplot$Code == "S-PASM"] <- "PASM"
subplot$Code[subplot$Code == "S-HEBO"] <- "HEBO"
subplot$Code[subplot$Code == "SPAMA"] <- "SPAM2"
subplot$Code[subplot$Code == "ARPUP6"] <- "ARPU9"
subplot$Code[subplot$Code == "EUPO3"] <- "CHPO12"


# Check for missing codes by comparing subplot data to both species lists
sub.codes <- c(species.de$CodeOriginal, species.in$CodeOriginal)
setdiff(subplot$Code, sub.codes) 

# Check again for NA codes
filter(subplot, is.na(Code)) # no NAs




# Add species info and change location-dependent code name ----------------

# Separate out location-dependent observations
subplot.de <-subplot %>% 
  filter(Code %in% species.de$CodeOriginal)

# Rename code col so subplot data matches species list
subplot.de <- subplot.de %>% 
  rename(CodeOriginal = Code)

# Add species info
subplot.de <- left_join(subplot.de, species.de)

# Check for NA codes
filter(subplot.de, is.na(Code))



# Add species info for location-independent codes -------------------------

subplot.in <- subplot %>% 
  filter(Code %in% species.in$Code)

subplot.in <- left_join(subplot.in, species.in) 


# Combine location-dependent and independent ------------------------------

subplot <- bind_rows(subplot.in, subplot.de) %>% 
  arrange(raw.row)

# Check that there the same number of observations as the original subplot data
nrow(subplot) == nrow(subplot.raw)



# Check if Introduced plants were marked as Seeded ------------------------

subplot.inva <- subplot %>% 
  filter(Native == "Introduced") %>% 
  select(Code, Seeded, Name, Native) %>% 
  distinct(.keep_all = TRUE)
unique(subplot.inva$Seeded) # something is mislabeled; no introduced species were seeded
subplot.inva %>% 
  filter(Seeded == "Yes") # Eragrostis curvula was not seeded

# Fix Eragrostis curvula - mark all observations as "No" for Seeded col
subplot$Seeded[subplot$Name == "Eragrostis curvula"] <- "No"



# Check again for NA codes ------------------------------------------------

filter(subplot, is.na(Code)) # no NA codes
filter(subplot, is.na(Name)) # no NA names


# Write clean subplot data to csv -----------------------------------------

write_csv(subplot,
          file = "data/cleaned/subplot-data_clean.csv")






#### This chunk relates to 01_curate-species-list.R ###################
#### do not run again #####


# Address native status for unknown seeded species ------------------------

# After making a species list and combining that with the subplot data, we see that some unknown species
  # were actually seeded, and therefore native. The native status is fixed in the 01_curate-species-list.R
  # script (they do not need to be fixed here), but this section shows how I determined which species to address
  # in the 01_curate-species-list.R script.

subplot.seeded <- subplot %>% 
  filter(Seeded == "Yes")

subplot.seeded <- left_join(subplot.seeded, mix) %>% 
  select(-Family, -Scientific, -Common)

# Extract names that were seeded but not marked Native
seeded.notnative <- subplot.seeded %>% 
  filter(Native != "Native")
seeded.notnative.names <- data.frame(V1 = unique(seeded.notnative$Name))

# Write list of names to CSV for 01_curate-species-list.R
  # The "spp." ones are not location-dependent, and the unknowns have location-specific names now
    # so they do not need to be separated
### DO NOT OVERWRITE CSV!!! DO NOT RUN #####
write_csv(seeded.notnative.names,
          file = "data/raw/output-wrangling_seeded-species-to-be-marked-native.csv")

# Remove unnecessary objects
rm(seeded.notnative, seeded.notnative.names)


# Check if it worked after fixing 01_curate-species-list.R and writing new CSVs
unique(subplot.seeded$Native) # only option should be "Native"

########### Chunk related to 01_curate-species-list.R complete ############








# Subplot data for seeded species only ------------------------------------

subplot.seeded <- subplot %>% 
  filter(Seeded == "Yes")

subplot.seeded <- left_join(subplot.seeded, mix) %>% 
  select(-Family, -Scientific, -Common)

# Check which ones are missing seeding rate data
seed.rate.na <- subplot.seeded %>% 
  filter(is.na(SeedingRate)) %>% 
  select(Code, Name, Mix, SeedingRate, NicheValue) %>% 
  distinct(.keep_all = TRUE) 

seed.rate.na.known <- seed.rate.na %>% 
  filter(!str_detect(seed.rate.na$Name, "Unk|unk|spp."))


save.image("RData/02_data-wrangling_subplot.RData")
