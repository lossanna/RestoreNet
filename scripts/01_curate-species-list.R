library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllSubplotData")
plot.2x2.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")
species.raw <- read_xlsx("data/raw/master-species_native.xlsx")
mix <- read_xlsx("data/raw/master-seed-mix.xlsx")


# Notes about manual edits ------------------------------------------------

# For manual edits to CSVs, the CSV is written from R, copied and named a new name, edited, 
      # and new file is read into R

# Files in the format "output-species_xx.csv" are ones written from R
# Files in the format "edited-species_xx.csv" are manually edited and read back in as new objects,
  # but then usually used to alter existing objects



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
  arrange(Code)

# Write to csv to manually fill in information
write_csv(subplot.missing,
          file = "data/raw/output-species1_subplot-codes-missing.csv")


#### manually edit new file to add name, native status, lifeform, and duration ####

sub.missing.edit <- read_csv("data/raw/edited-species1_subplot-codes-missing_native-duration-lifeform.csv")



# Separate location-dependent species (unknowns) --------------------------

# Knowns and unknowns must be separated; 
  # plants not defined to species level are location-specific and Site must be retained

# Unknowns (location-dependent)
species.m.unk <- species.raw %>% # from original master list
  filter(str_detect(species.raw$Name, "Unk|unk")) %>% 
  arrange(Region) %>% 
  arrange(Code)

sub.missing.unk <- sub.missing.edit %>% # ones missing from original master list
  filter(str_detect(sub.missing.edit$Name, "Unk|unk")) %>% 
  arrange(Region) %>% 
  arrange(Code)

# Knowns (location-independent)
species.m.known <- species.raw %>% # from original master list
  filter(!str_detect(species.raw$Name, "Unk|unk")) %>% 
  select(-Region) %>% 
  arrange(Code)

sub.missing.known <- sub.missing.edit %>% # ones missing from original master list
  filter(!str_detect(sub.missing.edit$Name, "Unk|unk")) %>% 
  select(-Region, -Site) %>% 
  arrange(Code)
  # although there are a couple names with ?s, the names and codes are unique 
      # and not repeated across sites to refer to different plants,
      # and therefore location-independent
  # combine dfs after doing left_join() assignment of lifeform



#### Location-independent species (known genus & species) ###############

# Add lifeform information to species list --------------------------------

# Extract lifeform information from subplot.raw data for knowns from master
subplot.in.lifeform <- subplot %>% 
  select(Code, Functional_Group) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(Lifeform = Functional_Group)

# Add subplot lifeform information to location-independent species list
species.m.known <- left_join(species.m.known, subplot.in.lifeform)

# Standardize lifeform names to Grass/Forb/Shrub
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

unique(species.m.known$Lifeform) # lifeform names have been standardized, with NAs
species.m.known$Lifeform[species.m.known$Lifeform == "NA"] <- NA # convert to real (logical) NA
unique(species.m.known$Lifeform)


# Find species without lifeform information and write to csv, to manually add information
lifeform.na <- species.m.known %>% 
  filter(is.na(Lifeform)) %>% 
  select(Code, Name, Lifeform) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code)

write_csv(lifeform.na,
          file = "data/raw/output-species2_xlsx_lifeform-na.csv")


#### edit new file manually to add lifeform ######

lifeform.na.edit <- read_csv("data/raw/edited-species2_xlsx_lifeform-na.csv")


# Add manually-edited lifeform information to working species list 
  # split up species because left_join() will not override and will create duplicates,
    # and information from edited version is definitely correct (information from subplot.raw could be wrong)

species.lifeform <- species.m.known %>%  # known lifeform species
  filter(!Code %in% lifeform.na.edit$Code) 

species.lifeform.na <- species.m.known %>%  # unknown lifeform species
  filter(Code %in% lifeform.na.edit$Code) %>% 
  select(-Lifeform) %>% 
  distinct(.keep_all = TRUE) %>%
  arrange(Code) # dimensions are different than lifeform.na.edit because HAGL was a duplicate now removed
species.lifeform.na <- left_join(species.lifeform.na, lifeform.na.edit) %>% 
  distinct(.keep_all = TRUE)

species.m.known <- bind_rows(species.lifeform, species.lifeform.na) %>% 
  arrange(Code)
  
unique(species.m.known$Lifeform) # lifeform has been standardized



# Add duration information to species list --------------------------------

  # (All duration information must be added manually from USDA Plants)
  # Multiple lifeforms for same species are also corrected (wrong ones deleted)

# Write output with native status and lifeform columns
write_csv(species.m.known,
          file = "data/raw/output-species3_xlsx_native-lifeform.csv")


#### edit new file manually to add duration and check lifeform ####

species.m.known <- read_csv("data/raw/edited-species3_xlsx_native-lifeform-duration.csv")




# Add codes from data not included in species list ------------------------

# Combine species.m.known and sub.missing.known
species.in <- bind_rows(species.m.known, sub.missing.known) %>% 
  arrange(Code)

# Check for absent information (NAs)
apply(species.in, 2, anyNA)



# Standardize codes for location-independent species ----------------------

# Extract species with multiple codes for the same name, retaining all codes
codes.fix.in <- species.in %>% 
  filter(Name %in% filter(species.in, duplicated(Name))$Name) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Name) 

# Compare codes with those from seed mix
mix.codes <- mix %>% 
  filter(Code %in% codes.fix.in$Code) %>% 
  select(Scientific, Code) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code) # codes need to match ones from seed mix

# Gather standardized codes based on USDA Plants
codes.standardized.in <- codes.fix.in %>% 
  filter(Code %in% c(mix.codes$Code, "CHPO12", "SIAL2")) %>% 
  mutate(Old_Code = c("ARPUP6", "BOER", "EUPO3", "S-HEBO", "S-PASM", "SIAL", "SPAMA")) %>% 
  select(Old_Code, Code, Name)
  # DRCU/DRCUI and ESCA/ESCAM refer to different varieties, so specificity is retained,
      # and "spp." are unknown species, so they can have different codes

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
filter(species.in, Name == "Eragrostis ciliaris") # name only occurs once
filter(species.in, Code == "ERCI2") # correct code is not present in species list

# Fix code for ERCI
species.in$Code[species.in$Name == "Eragrostis ciliaris"] <- "ERCI2"


# Unique codes
length(unique(species.in$Code)) == nrow(species.in) # all codes in species list are unique



# Fix native status for select seeded species -----------------------------

# In merging the species data with the subplot data (actual observations), we see that some unknown
  # species were seeded, and therefore native, but it is impossible to know this without first producing
  # a species list. However, the cleaned species list should be final and generated all from one script,
  # so although the analysis is not in this script to identify which species need to be changed, it is documented
  # in the 02_data-wrangling.R script.

native.fix <- read_csv("data/raw/output-wrangling_seeded-species-to-be-marked-native.csv")
  # CSV generated from 02_data-wrangling.R script

species.in <- species.in %>% 
  mutate(Native.new = if_else(Name %in% native.fix$V1, "Native", Native))

# Visually inspect to make sure things changed, then keep fixed column
species.in <- species.in %>% 
  select(-Native) %>% 
  rename(Native = Native.new)




# Write clean location-independent species list to CSV --------------------

# Check for absent information (NAs)
unique(species.in$Native)
unique(species.in$Duration)
unique(species.in$Lifeform)

# Add LocationDependence and CodeOriginal col
species.in <- species.in %>% 
  mutate(LocationDependence = rep("location-independent", nrow(species.in)),
         CodeOriginal = Code)

# Write to csv
write_csv(species.in,
          file = "data/cleaned/species-list_subplot_location-independent_clean.csv")





# Location-dependent species (unknowns) -----------------------------------

# Combine all location-dependent species (ones from master species list and from subplot data)
species.de <- bind_rows(species.m.unk, sub.missing.unk)

# Write to CSV to fill in information for species.m.unk
write_csv(species.de,
          file = "data/raw/output-species4.1_location-dependent.csv")

# Extract Site information for species.m.unk to add to location-dependent list
  # and write to CSV
sites.m.unk <- subplot %>% 
  filter(Code %in% species.m.unk$Code) %>% 
  select(Code, Region, Site) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Region) %>% 
  arrange(Code)
write_csv(sites.m.unk,
          file = "data/raw/output-species4.2_location-dependent_xlsx_sites.csv")

#### edit new file manually #########
  # make sure all have site, native, duration, lifeform information
species.de <- read_csv("data/raw/edited-species4_location-dependent_native-duration-lifeform.csv")



# Add Site to Name col for location-dependent
species.de$Name <- apply(species.de[ , c("Name", "Site")], 1, paste, collapse = ", ")
  

# Create column of Code.Site
species.de$Code.Site <- apply(species.de[ , c("Code", "Site")], 1, paste, collapse = ".")

# Look for overlapping codes between location-dependent and independent 
intersect(species.de$Code, species.in$Code) # "Unkcrypt"  "Unksporob"

# "Unkcrypt"  "Unksporob" are not location dependent and in location-independent list already; 
    # remove from location-dependent list
species.de <- species.de %>% 
  filter(!Code %in% c("Unkcrypt", "Unksporob"))
  

# Check for unique codes
length(unique(species.de$Code.Site)) == nrow(species.de) # all codes in species list are unique
intersect(species.de$Code, species.in$Code) # location-dependent codes are also unique from location-dependent ones




# Fix native status for select seeded species (again) ---------------------

# In merging the species data with the subplot data (actual observations), we see that some unknown
  # species were seeded, and therefore native, but it is impossible to know this without first producing
  # a species list. However, the cleaned species list should be final and generated all from one script,
  # so although the analysis is not in this script to identify which species need to be changed, it is documented
  # in the 02_data-wrangling.R script.

native.fix <- read_csv("data/raw/output-wrangling_seeded-species-to-be-marked-native.csv")
  # CSV generated from 02_data-wrangling.R script

species.de <- species.de %>% 
  mutate(Native.test = if_else(Name %in% native.fix$V1, "Native", Native))

# Visually inspect to make sure things changed, then keep fixed column
species.de <- species.de %>% 
  select(-Native) %>% 
  rename(Native = Native.test)



# Write cleaned location-dependent species list to CSV --------------------

# Check for absent information (NAs)
unique(species.de$Native)
unique(species.de$Duration)
unique(species.de$Lifeform)

# Add LocationDependence col
species.de <- species.de %>% 
  mutate(LocationDependence = rep("location-dependent", nrow(species.de)))

# Change code col names to standardize
species.de <- species.de %>% 
  rename(CodeOriginal = Code,
         Code = Code.Site)

# Write to csv
write_csv(species.de,
          file = "data/cleaned/species-list_subplot_location-dependent_clean.csv")



# Write complete list of codes for subplot data ---------------------------

# Location-independent codes
subplot.codes.de <- species.de %>% 
  select(CodeOriginal, Code, Name, LocationDependence) 

# Location-dependent codes
subplot.codes.in <- species.in %>% 
  select(CodeOriginal, Code, Name, LocationDependence) 

# Combine
subplot.codes <- bind_rows(subplot.codes.de, subplot.codes.in) %>% 
  filter(CodeOriginal %in% subplot.raw$Species_Code) %>% 
  arrange(Name)

# Write to CSV
write_csv(subplot.codes,
          file = "data/cleaned/subplot-codes_clean.csv")


# Codes from AllPlotData (2x2 plots) --------------------------------------

# Codes from these plots are really different and usually long descriptions
  # so they get their own separate list

# Location-independent codes from AllPlotData (2 x 2 m plots) missing from location-independent list
codes.missing.2x2 <- plot.2x2.raw %>% 
  select(starts_with("Additional")) %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(everything(), names_to = "drop", values_to = "Code") %>% 
  select(Code) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Code) %>% # 176 codes
  filter(!Code %in% species.in$Code)

write_csv(codes.missing.2x2,
          file = "data/raw/output-species5_codes-missing-2x2plot.csv")




save.image("RData/01_curate-species-list.RData")
