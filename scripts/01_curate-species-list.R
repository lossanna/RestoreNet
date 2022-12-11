library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllSubplotData")
plot.2x2.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")
species.raw <- read_xlsx("data/raw/master-species_native.xlsx")
mix <- read_xlsx("data/raw/master-seed-mix.xlsx")

# Dependency created in 01-dependency_assign-seeded-species-native-status.R
native.fix <- read_csv("data/raw/01-dependency_seeded-species-to-be-marked-native.csv")


# Notes about manual edits ------------------------------------------------

# For manual edits to CSVs, the CSV is written from R, copied and named a new name, edited, 
      # and new file is read into R

# Files in the format "output-species_xx.csv" are ones written from R
# Files in the format "edited-species_xx.csv" are manually edited and read back in as new objects,
  # but then usually used to alter existing objects
  # See README_rawdata.md for more details



# Add Region and row numbers to subplot data ------------------------------

subplot <- subplot.raw %>% 
  mutate(Region = case_when(
    str_detect(subplot.raw$Site, c("AguaFria|BabbittPJ|MOWE|Spiderweb|BarTBar|FlyingM|PEFO|TLE")) ~ "Colorado Plateau",
    str_detect(subplot.raw$Site, c("CRC|UtahPJ|Salt_Desert")) ~ "Utah",
    str_detect(subplot.raw$Site, c("29_Palms|AVRCD")) ~ "Mojave",
    str_detect(subplot.raw$Site, c("Creosote|Mesquite")) ~ "Chihuahuan",
    str_detect(subplot.raw$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(subplot.raw$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central",
    TRUE ~ "unk"),
    raw.row = 1:nrow(subplot.raw)) %>% 
  rename(Code = Species_Code)



# Assign names to codes in subplot data but not species list --------------

# Extract missing codes
codes.missing.sub <- setdiff(subplot$Code, species.raw$Code)

# Narrow columns and remove duplicates for missing subplot data
subplot.missing <- subplot %>%
  filter(Code %in% codes.missing.sub) %>% 
  select(Region, Site, Code) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code) %>% 
  filter(!Code %in% c("0", "NA"),
         !is.na(Code))

# OUTPUT: write to csv to manually fill in information
write_csv(subplot.missing,
          file = "data/raw/01a_output-species1_subplot-codes-missing.csv")
head(subplot.missing)

# EDITED: manually edit new file to add Name, Native, Lifeform, and Duration cols
sub.missing.edit <- read_csv("data/raw/01b_edited-species1_subplot-codes-missing_native-duration-lifeform.csv")
head(sub.missing.edit)



# Separate location-dependent species (unknowns) --------------------------

# Knowns and unknowns must be separated; 
  # plants not defined to species level are location-specific and Site must be retained

# Unknowns (location-dependent)
species.m.unk <- species.raw %>% # from original master list (xlsx)
  filter(str_detect(species.raw$Name, "Unk|unk|spp.")) %>% 
  arrange(Region) %>% 
  arrange(Code)

sub.missing.unk <- sub.missing.edit %>% # ones missing from original master list (.xlsx)
  filter(str_detect(sub.missing.edit$Name, "Unk|unk|spp.")) %>% 
  arrange(Region) %>% 
  arrange(Code)

# Knowns (location-independent)
species.m.known <- species.raw %>% # from original master list (.xlsx)
  filter(!str_detect(species.raw$Name, "Unk|unk|spp.")) %>% 
  select(-Region) %>% 
  arrange(Code)

sub.missing.known <- sub.missing.edit %>% # ones missing from original master list (.xlsx)
  filter(!str_detect(sub.missing.edit$Name, "Unk|unk|spp.")) %>% 
  select(-Region, -Site) %>% 
  arrange(Code)




#### Location-independent species (known genus & species) ###############

# Add lifeform information to species list --------------------------------

# Extract lifeform information from subplot.raw data for knowns from master.xlsx
subplot.in.lifeform <- subplot %>% 
  select(Code, Functional_Group) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(Lifeform = Functional_Group)

# Add subplot lifeform information to location-independent species list
species.m.known <- left_join(species.m.known, subplot.in.lifeform)

# Standardize Lifeform to Grass/Forb/Shrub
unique(species.m.known$Lifeform)

species.m.known <- species.m.known %>% 
  mutate(Lifeform = case_when(
    str_detect(species.m.known$Name, "forb") ~ "Forb",
    str_detect(species.m.known$Name, "grass") ~ "Grass",
    str_detect(species.m.known$Name, "shrub") ~ "Shrub",
    TRUE ~ species.m.known$Lifeform))

species.m.known <- species.m.known %>% 
  mutate(Lifeform = case_when(
    str_detect(species.m.known$Lifeform, "Shrub/subshrub") ~ "Shrub", # standardize to grass/forb/shrub
    str_detect(species.m.known$Lifeform, "shrub") ~ "Shrub",
    str_detect(species.m.known$Lifeform, "C3 grass") ~ "Grass",
    TRUE ~ species.m.known$Lifeform)) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code) 

unique(species.m.known$Lifeform) # Lifeform names have been standardized, with NAs
species.m.known$Lifeform[species.m.known$Lifeform == "NA"] <- NA # convert to real (logical) NA
unique(species.m.known$Lifeform)


# OUTPUT: find species without lifeform information and write to csv
lifeform.na <- species.m.known %>% 
  filter(is.na(Lifeform)) %>% 
  select(Code, Name, Lifeform) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code)

write_csv(lifeform.na,
          file = "data/raw/01a_output-species2_xlsx_lifeform-na.csv")
head(lifeform.na)

# EDITED: manually edit new file and fill in Lifeform col
lifeform.na.edit <- read_csv("data/raw/01b_edited-species2_xlsx_lifeform-na.csv")
head(lifeform.na.edit)


# Add lifeform.na.edit to working location-independent species list

  # Species are split up because left_join() will not override and will create duplicates,
    # and information from edited version is definitely correct (information from subplot.raw could be wrong)

species.lifeform <- species.m.known %>%  # known species with lifeform info
  filter(!Code %in% lifeform.na.edit$Code) 

species.lifeform.na <- species.m.known %>%  # unknown species with lifeform info
  filter(Code %in% lifeform.na.edit$Code) %>% 
  select(-Lifeform) %>% 
  distinct(.keep_all = TRUE) %>%
  arrange(Code)

species.lifeform.na <- left_join(species.lifeform.na, lifeform.na.edit) %>% 
  distinct(.keep_all = TRUE)

species.m.known <- bind_rows(species.lifeform, species.lifeform.na) %>% 
  arrange(Code) 

unique(species.m.known$Lifeform) # lifeform has been standardized



# Add duration to location-independent list -------------------------------

  # (All duration information must be added manually from USDA Plants)
  # Multiple lifeforms for same species are also corrected (wrong ones deleted)

# OUTPUT: write output with Native and Lifeform columns
write_csv(species.m.known,
          file = "data/raw/01a_output-species3_xlsx_native-lifeform.csv")
head(species.m.known)


# EDITED: edit new file manually to add Duration and correct Lifeform if needed
species.m.known <- read_csv("data/raw/01b_edited-species3_xlsx_native-lifeform-duration.csv")
head(species.m.known)



# Add codes to location-independent not in master -------------------------

# Combine species.m.known and sub.missing.known
species.in <- bind_rows(species.m.known, sub.missing.known) %>% 
  arrange(Code)

# Check for absent information (NAs)
apply(species.in, 2, anyNA) # should all be FALSE



# Standardize codes for location-independent species ----------------------

# Extract species with multiple codes for the same name, retaining all codes
codes.fix.in <- species.in %>% 
  filter(Name %in% filter(species.in, duplicated(Name))$Name) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Name) 
codes.fix.in

# Compare codes with those from seed mix
mix.codes <- mix %>% 
  filter(Code %in% codes.fix.in$Code) %>% 
  select(Scientific, Code) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code) 
mix.codes # codes need to match ones from seed mix

# Create df standardized codes based on USDA Plants
codes.standardized.in <- codes.fix.in %>% 
  filter(Code %in% c(mix.codes$Code, "CHPO12", "SIAL2")) %>% 
  mutate(Old_Code = c("ARPUP6", "BOER", "EUPO3", "S-HEBO", "S-PASM", "SIAL", "SPAMA")) %>% 
  select(Old_Code, Code, Name)
  # DRCU/DRCUI and ESCA/ESCAM refer to different varieties, so specificity is retained

# Remove wrong codes from species list
species.in <- species.in %>% 
  filter(!Code %in% codes.standardized.in$Old_Code) %>% 
  arrange(Code)

species.in %>% 
  filter(Code %in% codes.standardized.in$Old_Code) # no old codes show up
species.in %>% 
  filter(Code %in% codes.standardized.in$Code) # all codes have been correctly replaced


# Find codes with multiple species
names.fix.in <- species.in %>% 
  filter(Code %in% filter(species.in, duplicated(Code))$Code) %>% 
  arrange(Code) 
names.fix.in # Eragrostis ciliaris is mislabeled; E. cilianensis code is correct
filter(species.in, Code == "ERCI2") # E. ciliaris code is not present in species list

# Fix code for E. cilaris
species.in$Code[species.in$Name == "Eragrostis ciliaris"] <- "ERCI2"


# Unique codes
length(unique(species.in$Code)) == nrow(species.in) # TRUE, all codes in species list are unique


# Check for absent information (NAs; "0" is okay)
unique(species.in$Native)
unique(species.in$Duration)
unique(species.in$Lifeform)

# Add CodeOriginal col
species.in <- species.in %>% 
  mutate(CodeOriginal = Code)



# Write intermediate location-dependent to CSV ----------------------------

# This is an intermediate because it does not yet have fixed native status for species that
  # were marked as seeded in the subplot data
  # Used in 01.1-dependency_assign-seeded-species-native-status.R

write_csv(species.in,
          file = "data/raw/01-dependency_species-list_location-independent.csv")
head(species.in)



#### Location-dependent species (species unknown) ##########################

# Location-dependent species (unknowns) -----------------------------------

# Combine all location-dependent species 
  # (ones from master species list and from subplot data)
species.de <- bind_rows(species.m.unk, sub.missing.unk)

# OUTPUT: write to CSV to fill in information for species.m.unk
write_csv(species.de,
          file = "data/raw/01a_output-species4.1_location-dependent.csv")
head(species.de) # skeleton to edit

# OUTPUT: extract Site information for species.m.unk to add to location-dependent list
  # and write to CSV
sites.m.unk <- subplot %>% 
  filter(Code %in% species.m.unk$Code) %>% 
  select(Code, Region, Site) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Region) %>% 
  arrange(Code)
write_csv(sites.m.unk,
          file = "data/raw/01a_output-species4.2_location-dependent_xlsx_sites.csv")
head(sites.m.unk) # use for reference to connect codes to sites

# EDITED: manually add/correct Site, Native, Duration, and Lifeform cols
species.de <- read_csv("data/raw/01b_edited-species4_location-dependent_native-duration-lifeform.csv")


# Add Site to Name col for location-dependent
species.de$Name <- apply(species.de[ , c("Name", "Site")], 1, paste, collapse = ", ")
  
# Rename Code col as CodeOriginal
species.de <- species.de %>% 
  rename(CodeOriginal = Code)

# Create new Code col that has the site info
species.de$Code <- apply(species.de[ , c("CodeOriginal", "Site")], 1, paste, collapse = ".")

# Look for overlapping codes between location-dependent and independent 
intersect(species.de$CodeOriginal, species.in$Code)
  

# Check for unique codes
length(unique(species.de$Code)) == nrow(species.de) # all codes in species list are unique
intersect(species.de$CodeOriginal, species.in$Code) 
intersect(species.de$Code, species.in$Code) 
  # location-dependent codes are also unique from location-dependent ones, both original code and new code with site info


# Check for absent information (NAs)
unique(species.de$Native)
unique(species.de$Duration)
unique(species.de$Lifeform)



# Write intermediate to CSV -----------------------------------------------

# This is an intermediate because it does not yet have fixed native status for species that
  # were marked as seeded in the subplot data, used in 01.1-dependency_assign-seeded-species-native-status.R
write_csv(species.de,
          file = "data/raw/01-dependency_species-list_location-dependent.csv")




# Fix native status for select seeded species -----------------------------

# Load dependency list created in 01.1-dependency_assign-seeded-species-native-status.R
native.fix <- read_csv("data/raw/01-dependency_seeded-species-to-be-marked-native.csv")


# Fix location independent
species.in <- species.in %>% 
  mutate(Native.new = if_else(Name %in% native.fix$Name, "Native", Native))

# Visually inspect to make sure things changed
species.in %>% 
  filter(Code %in% native.fix$Code)

# Keep the fixed version of Native col
species.in <- species.in %>% 
  select(-Native) %>% 
  rename(Native = Native.new)


# Fix location dependent
species.de <- species.de %>% 
  mutate(Native.test = if_else(Name %in% native.fix$Name, "Native", Native))

# Visually inspect to make sure things changed
species.de %>% 
  filter(Code %in% native.fix$Code) %>% 
  select(CodeOriginal, Name, Native, Native.test)

# Keep the fixed version of Native col
species.de <- species.de %>% 
  select(-Native) %>% 
  rename(Native = Native.test)



# Write complete list of codes for subplot data ---------------------------

# Location-independent codes
subplot.codes.de <- species.de %>% 
  select(CodeOriginal, Code, Name) 

# Location-dependent codes
subplot.codes.in <- species.in %>% 
  select(CodeOriginal, Code, Name) 

# Combine
subplot.codes <- bind_rows(subplot.codes.de, subplot.codes.in) %>% 
  filter(CodeOriginal %in% subplot.raw$Species_Code) %>% 
  arrange(Name)

# Write to CSV
write_csv(subplot.codes,
          file = "data/cleaned/subplot-codes_clean.csv")

species.subplot.in <- species.in
species.subplot.de <- species.de



# Codes from AllPlotData (2x2 plots) --------------------------------------

# Codes from AllPlotData (2 x 2 m plots) that missing from location-independent species list

# Codes from these plots are really different and usually long descriptions
  # so they get their own separate list


p2x2.codes.missing <- plot.2x2.raw %>% 
  select(Site, starts_with("Additional")) %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(!Site, names_to = "drop", values_to = "Code") %>% 
  select(-drop) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code) %>%
  filter(!Code %in% species.subplot.in$Code) %>% 
  filter(!is.na(Code),
         Code != "O")

# OUTPUT: create list of sites and codes that need more info
write_csv(p2x2.codes.missing,
          file = "data/raw/01a_output-species5_codes-missing-2x2plot.csv")
head(p2x2.codes.missing)

# EDITED: add/correct Native, Duration, Lifeform info
  # create multiple rows for codes that mention more than one species (happens at SRER and Patagonia)
p2x2.codes.missing <- read_csv("data/raw/01b_edited-species5_codes-missing-2x2plot.csv")
head(p2x2.codes.missing)

# Check for NAs
apply(p2x2.codes.missing, 2, anyNA) # should be no NA


# Add site to code and name for location-dependent species
p2x2.codes.de <- p2x2.codes.missing %>% 
  filter(LocationDependence == "dependent") 
p2x2.codes.de$Code <- apply(p2x2.codes.de[ , c("Code", "Site")], 1, paste, collapse = ".")
p2x2.codes.de$Name <- apply(p2x2.codes.de[ , c("Name", "Site")], 1, paste, collapse = ", ")

# Combine location-independent and dependent with new codes
p2x2.codes.missing <- p2x2.codes.missing %>% 
  filter(LocationDependence != "dependent") %>% # remove in correct location-dependent
  bind_rows(p2x2.codes.de) # add correct location-dependent


# Extract codes that need duplicates (same code refers to multiple species) and write to csv
p2x2.codes.dup <- p2x2.codes.missing %>% 
  filter(NeedsItsDuplicate == "Yes") %>% 
  arrange(CodeOriginal)
write_csv(p2x2.codes.dup,
          file = "data/raw/01c_output-species6_2x2-codes_need-duplicate-rows.csv")



# Full species list for location-independent ------------------------------

# Combine location-independent 2x2 codes with subplot codes
species.2x2.in <- p2x2.codes.missing %>% 
  filter(LocationDependence == "independent") %>% 
  select(-Site, -NeedsItsDuplicate, -LocationDependence, -DuplicateNum)

setdiff(colnames(species.2x2.in), colnames(species.subplot.in)) # columns are the same

species.in <- bind_rows(species.2x2.in, species.subplot.in) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Name) %>% 
  arrange(Code)

# Look for codes previously standardized, now that we have added 2x2 data
species.in %>% 
  filter(Code %in% codes.standardized.in$Old_Code) # no old codes show up


# Write to csv
write_csv(species.in,
          file = "data/cleaned/species-list_location-independent_clean.csv")




# Full species list for location-dependent --------------------------------

# Add Region col to 2x2 codes
p2x2.codes.missing <- p2x2.codes.missing %>% 
  mutate(Region = case_when(
    str_detect(p2x2.codes.missing$Site, c("AguaFria|BabbittPJ|MOWE|Spiderweb|BarTBar|FlyingM|PEFO|TLE")) ~ "Colorado Plateau",
    str_detect(p2x2.codes.missing$Site, c("CRC|UtahPJ|Salt_Desert")) ~ "Utah",
    str_detect(p2x2.codes.missing$Site, c("29_Palms|AVRCD")) ~ "Mojave",
    str_detect(p2x2.codes.missing$Site, c("Creosote|Mesquite")) ~ "Chihuahuan",
    str_detect(p2x2.codes.missing$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(p2x2.codes.missing$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central",
    TRUE ~ "unk"))

species.2x2.de <- p2x2.codes.missing %>% 
  filter(LocationDependence == "dependent") %>% 
  select(-NeedsItsDuplicate, -LocationDependence, -DuplicateNum) 

setdiff(colnames(species.2x2.de), colnames(species.subplot.de)) # columns are the same

species.de <- bind_rows(species.2x2.de, species.subplot.de) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Name) %>% 
  arrange(Code)

# Look for codes previously standardized, now that we have added 2x2 data
species.de %>% 
  filter(Code %in% codes.standardized.in$Old_Code) # no old codes show up

# Write to csv
write_csv(species.de,
          file = "data/cleaned/species-list_location-dependent_clean.csv")




save.image("RData/01_curate-species-list.RData")
