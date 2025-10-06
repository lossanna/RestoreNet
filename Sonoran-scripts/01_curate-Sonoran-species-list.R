# Created: 2025-01-21
# Last updated: 2025-04-01

# Purpose: Curate a complete species list with Code, Code Original, Name, Native, Duration, Lifeform info
#   for Sonoran sites, using subplot and 2x2 data.
#   Two lists must be created, a location-independent version (known species), and a
#     location-dependent version (unknown species, which includes location information).
# The species lists are essentially metadata for the codes.

# Dependency script not needed, because no unknowns were marked as native
#   at Sonoran Desert sites, so nothing needs to be corrected.



library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("Sonoran-data/raw/2023-09-15_Master 1.0 Germination Data_raw.xlsx", 
                         sheet = "AllSubplotData")
plot.2x2.raw <- read_xlsx("Sonoran-data/raw/2023-09-15_Master 1.0 Germination Data_raw.xlsx",
                          sheet = "AllPlotData")
species.raw <- read_xlsx("Sonoran-data/raw/from-Master_species-list-with-native-status_LO.xlsx")
mix <- read_xlsx("Sonoran-data/raw/from-Master_seed-mix_LO_Sonoran.xlsx", sheet = "with-site_R")


# Notes about manual edits ------------------------------------------------

# For manual edits to CSVs, the CSV is written from R, copied and named a new name, edited,
#   and new file is read into R.

# Files in the format "output_xx.csv" are ones written from R.
# Files in the format "edited_xx.csv" are manually edited and read back in as new objects,
#   but then usually used to alter existing objects.
#   See README_Sonoran-intermediate-data.md for more details.



# Organize subplot and 2x2 data -------------------------------------------

# Retain Sonoran Desert sites only
subplot <- subplot.raw %>% 
  filter(Site %in% c("SRER", "Patagonia", "Preserve", "Pleasant", "SCC", "Roosevelt"))
p2x2 <- plot.2x2.raw %>% 
  filter(Site %in% c("SRER", "Patagonia", "Preserve", "Pleasant", "SCC", "Roosevelt"))

# Add Region column
subplot <- subplot %>%
  mutate(Region = case_when(
     str_detect(subplot$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(subplot$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central")) %>% 
  mutate(raw.row = 1:nrow(subplot)) %>%
  rename(CodeOriginal = Species_Code)

p2x2 <- p2x2 %>% 
  mutate(Region = case_when(
    str_detect(subplot$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(subplot$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central"))

# Get codes
subplot.codes <- subplot %>%
  select(CodeOriginal, Region, Site) %>%
  distinct(.keep_all = TRUE)


# Assign names to codes in subplot data but not species list --------------

#   This is dealing with codes that are present in the subplot data, but not present in
#     the species list from from-Master_species-list-with-native-status_LO.xlsx
#     (which was adapted from Master.xlsx).
#   Manually adding the missing information (Name, Native, Lifeform, and Duration cols).

# Extract missing codes
codes.missing.sub <- setdiff(subplot$CodeOriginal, species.raw$CodeOriginal)

# Narrow columns and remove duplicates for missing subplot data
subplot.missing <- subplot %>%
  filter(CodeOriginal %in% codes.missing.sub) %>%
  select(Region, Site, CodeOriginal) %>%
  distinct(.keep_all = TRUE) %>%
  arrange(CodeOriginal) %>%
  filter(!CodeOriginal %in% c("0", "NA"),
         !is.na(CodeOriginal))

# OUTPUT: write to csv to manually fill in information
write_csv(subplot.missing,
          file = "Sonoran-data/data-wrangling-intermediate/01a_output1_subplot-codes-missing.csv")
head(subplot.missing)

# EDITED: manually edit new file to add Name, Native, Lifeform, and Duration cols
sub.missing <- read_csv("Sonoran-data/data-wrangling-intermediate/01b_edited1_subplot-codes-missing_native-duration-lifeform.csv")
head(sub.missing)



# Separate species by location dependence (knowns/unknowns) ---------------

# Knowns and unknowns must be separated;
#   plants not defined to species level are location-specific and Site must be retained.

# Unknowns (location-dependent)
#   From original master species list
species.m.unk <- species.raw %>%
  filter(str_detect(species.raw$Name, "Unk|unk|spp\\.|sp\\.|Could be|Very similar to GIsp 1")) %>%
  filter(CodeOriginal != "VEPEX2") %>% # name contains "ssp." but it is a known
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) %>% 
  arrange(Region) %>%
  arrange(CodeOriginal)

#   Ones missing from original master species list
sub.missing.unk <- sub.missing %>%
  filter(str_detect(sub.missing$Name, "Unk|unk|spp\\.")) %>%
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) %>% 
  arrange(Region) %>%
  arrange(CodeOriginal)


# Knowns (location-independent)
#   From original master species list
VEPEX2 <- species.raw %>%
  filter(CodeOriginal == "VEPEX2") # make separate row because it contains "ssp." but isn't an unknown

species.m.known <- species.raw %>%
  filter(!str_detect(species.raw$Name, "Unk|unk|spp\\.|sp\\.|Could be|Very similar to GIsp 1")) %>%
  bind_rows(VEPEX2) %>%
  select(-Region) %>%
  arrange(CodeOriginal) %>%
  distinct(.keep_all = TRUE)

#   Ones missing from original master species list
sub.missing.known <- sub.missing %>% # ones missing from original master list (.xlsx)
  filter(!str_detect(sub.missing$Name, "Unk|unk|spp\\.")) %>%
  select(-Region, -Site) %>%
  distinct(.keep_all = TRUE) %>%
  arrange(CodeOriginal)




# Location-independent species from master list & subplot data ------------

## Add Lifeform to master list --------------------------------------------

# Extract lifeform information from subplot data for knowns
subplot.in.lifeform <- subplot %>%
  filter(!str_detect(subplot$CodeOriginal, "Unk|unk|spp\\.")) %>%
  select(CodeOriginal, Functional_Group) %>%
  distinct(.keep_all = TRUE) %>%
  rename(Lifeform = Functional_Group) %>%
  arrange(CodeOriginal)

# Correct codes with conflicting lifeform info
sub.in.lifeform.duplicate <- count(subplot.in.lifeform, CodeOriginal) %>%
  filter(n > 1) %>%
  arrange(CodeOriginal)
subplot.in.lifeform.inspect <- subplot.in.lifeform %>%
  filter(CodeOriginal %in% sub.in.lifeform.duplicate$CodeOriginal) %>%
  arrange(CodeOriginal)

# OUTPUT: list of lifeform info according to subplot data
write_csv(subplot.in.lifeform.inspect,
          file = "Sonoran-data/data-wrangling-intermediate/01a_output2_subplot-lifeform-info.csv")

# EDITED: delete incorrect rows so there is only one lifeform assignment per code
#   Fix conflicting lifeform according to USDA Plants
#   Standardize spelling of lifeform to Grass, Forb, or Shrub
subplot.in.duplicate <- read_csv("Sonoran-data/data-wrangling-intermediate/01b_edited2_subplot-lifeform-info-corrected.csv")

# Replace lifeform info for subplot knowns with corrections based on edited csv
subplot.in.lifeform <- subplot.in.lifeform %>%
  filter(!CodeOriginal %in% subplot.in.duplicate$CodeOriginal) %>%
  bind_rows(subplot.in.duplicate) %>%
  arrange(CodeOriginal)
length(unique(subplot.in.lifeform$CodeOriginal)) == nrow(subplot.in.lifeform) # check for matching lengths


# Add subplot lifeform information to master list (working species list)
species.m.known <- left_join(species.m.known, subplot.in.lifeform)

# Compile list of lifeform information thus far of species from master
lifeform.known <- species.m.known %>%
  filter(!is.na(Lifeform)) %>%
  select(CodeOriginal, Name, Lifeform) %>%
  arrange(CodeOriginal)


# Add missing lifeform information to master list (working species list)
# OUTPUT: create list of species without lifeform information
lifeform.na <- species.m.known %>%
  filter(is.na(Lifeform)) %>%
  select(CodeOriginal, Name, Lifeform) %>%
  arrange(CodeOriginal)

write_csv(lifeform.na,
          file = "Sonoran-data/data-wrangling-intermediate/01a_output3_xlsx_lifeform-na.csv")
head(lifeform.na)

# EDITED: manually edit new file and fill in Lifeform col
lifeform.na.edit <- read_csv("Sonoran-data/data-wrangling-intermediate/01b_edited3_xlsx_lifeform-na.csv")
head(lifeform.na.edit)

# Add newly edited lifeform data (previously NAs) to existing list
lifeform.known <- bind_rows(lifeform.known, lifeform.na.edit)

# Add lifeform info to master list (working species list)
species.m.known <- species.m.known %>%
  select(-Lifeform) %>% # must remove Lifeform col so left_join() does not conflict
  left_join(lifeform.known)


# Standardize Lifeform to Grass/Forb/Shrub
unique(species.m.known$Lifeform)

species.m.known <- species.m.known %>%
  mutate(Lifeform = case_when(
    str_detect(species.m.known$Lifeform, "shrub") ~ "Shrub",
    str_detect(species.m.known$Lifeform, "forb") ~ "Forb",
    str_detect(species.m.known$Lifeform, "grass") ~ "Grass",
    species.m.known$Lifeform == "NA" ~ NA,
    TRUE ~ species.m.known$Lifeform))

unique(species.m.known$Lifeform) # Lifeform names have been standardized



## Add Duration to master list ---------------------------------------------

#   All duration information needed to be added manually from USDA Plants.

# OUTPUT: write out current master/subplot list, which has Native and Lifeform columns
write_csv(species.m.known,
          file = "Sonoran-data/data-wrangling-intermediate/01a_output4_xlsx_native-lifeform.csv")
head(species.m.known)

# EDITED: edit new file manually to add Duration
#   Also delete 2 duplicate rows (BOAR & SATR12) with misspelled name (B. aristoides)/old name (S. iberica)
#     so only correct rows remain; and add row of all 0s for empty plots
species.m.known <- read_csv("Sonoran-data/data-wrangling-intermediate/01b_edited4_xlsx_native-lifeform-duration.csv")
head(species.m.known)


## Combine master and subplot codes for complete list ---------------------

# Add species from subplot data not in master to ongoing master list
#   Combine species.m.known and sub.missing.known
species.in <- bind_rows(species.m.known, sub.missing.known) %>%
  arrange(CodeOriginal)

# Check for absent information (NAs)
apply(species.in, 2, anyNA) # should all be FALSE



## Standardize codes for location-independent species list ----------------

# Add Code col
species.in$Code <- species.in$CodeOriginal

# Extract species with multiple codes for the same name, retaining all codes
codes.fix.in <- species.in %>%
  filter(Name %in% filter(species.in, duplicated(Name))$Name) %>%
  distinct(.keep_all = TRUE) %>%
  arrange(Name)
print(codes.fix.in, n = 14)

# Compare codes with those from seed mix
mix.codes <- mix %>%
  filter(CodeOriginal %in% codes.fix.in$CodeOriginal) %>%
  select(Scientific, CodeOriginal) %>%
  distinct(.keep_all = TRUE) %>%
  arrange(CodeOriginal)
mix.codes # codes need to match ones from seed mix

# Create df standardized codes based on USDA Plants
codes.standardized.in <- codes.fix.in %>%
  filter(Code %in% c(mix.codes$CodeOriginal, "CHPO12", "SIAL2")) %>%
  mutate(CodeOriginal = c("ARPUP6", "BOER", "EUPO3", "SIAL", "SPAMA"))

# Remove wrong codes from species list and add correct ones
species.in <- species.in %>%
  filter(!CodeOriginal %in% codes.standardized.in$CodeOriginal) %>%
  bind_rows(codes.standardized.in) %>%
  arrange(CodeOriginal)

# DRCU/DRCUI and ESCA/ESCAM refer to different varieties, so specificity is retained
#   change name for DRCUI & ESCAM
species.in$Name[species.in$CodeOriginal == "DRCUI"] <- "Draba cuneifolia var. integrifolia"
species.in$Name[species.in$CodeOriginal == "ESCAM"] <- "Eschscholzia californica ssp. mexicana"

# Reorder cols
species.in <- species.in %>%
  select(CodeOriginal, Code, Name, Native, Duration, Lifeform)



# Find codes with multiple species
names.fix.in <- species.in %>%
  filter(CodeOriginal %in% filter(species.in, duplicated(CodeOriginal))$CodeOriginal) %>%
  arrange(CodeOriginal)
names.fix.in # look at master species list for clarification
# Cross-referencing original species list from master shows that ERCI referred to Eragrostis cilianensis
#   at Sonoran Central and Co Plateau, but referred to E. ciliaris at Chihuahuan
#   USDA codes:
#     ERCI = Eragrostis cilianensis
#     ERCI2 = Eragrostis ciliaris
# Sonoran Central is correct, ERCI should be Eragrostis cilianensis.

# Remove incorrect row from species.in
species.in <- species.in %>% 
  filter(Name != "Eragrostis ciliaris")


# Unique codes
length(unique(species.in$CodeOriginal)) == nrow(species.in) # TRUE, all codes in species list are unique


# Check for absent information (NAs)
#   "0" is okay
unique(species.in$Native)
unique(species.in$Duration)
unique(species.in$Lifeform)



# Location-dependent species for subplot ----------------------------------

# Combine location-dependent species
#   from master species list and from subplot data
species.de <- bind_rows(species.m.unk, sub.missing.unk) %>%
  filter(CodeOriginal %in% subplot$CodeOriginal) %>% 
  arrange(CodeOriginal)

# OUTPUT: write to CSV to fill in information for location-dependent list (species.de)
write_csv(species.de,
          file = "Sonoran-data/data-wrangling-intermediate/01a_output5.1_location-dependent.csv")
head(species.de) # contains Name & Native

# OUTPUT: extract Site information from subplot data to add to location-dependent list
#   and write to CSV
sites.m.unk <- subplot %>%
  filter(CodeOriginal %in% c(species.m.unk$CodeOriginal, sub.missing.unk$CodeOriginal)) %>%
  select(CodeOriginal, Region, Site) %>%
  distinct(.keep_all = TRUE) %>%
  arrange(Region) %>%
  arrange(CodeOriginal)
write_csv(sites.m.unk,
          file = "Sonoran-data/data-wrangling-intermediate/01a_output5.2_location-dependent_xlsx_sites.csv")
head(sites.m.unk) # contains Site

# Look for NA codes that had observations (Count or Height) in subplot data
filter(subplot, is.na(CodeOriginal)) %>%
  select(Site, `Seeded(Yes/No)`, Functional_Group, Seedling_Count, Average_Height_mm, raw.row) # none
filter(subplot, CodeOriginal == "NA") %>%
  select(Site, `Seeded(Yes/No)`, Functional_Group, Seedling_Count, Average_Height_mm, raw.row) # none

# EDITED: manually add/correct Site, Native, Duration, and Lifeform cols
#   Use 5.1 as skeleton to edit
#   Create extra rows for unknowns (they are defined by Region but need a row for each Site), as cross-referenced
#     with output5.2.csv): ARISPP, CRYPT, MENSPP, SIDSPP, UNGR1, UNGRS1, Unk sp.
species.de <- read_csv("Sonoran-data/data-wrangling-intermediate/01b_edited5_location-dependent_native-duration-lifeform.csv")
head(species.de)



## Rename Code & Name for location-dependent ------------------------------

# Add Site to Name col for location-dependent
species.de$Name <- apply(species.de[, c("Name", "Site")], 1, paste, collapse = ", ")

# Create new Code col that has the site info
species.de$Code <- apply(species.de[, c("CodeOriginal", "Site")], 1, paste, collapse = ".")


# Look for overlapping codes between location-dependent and independent
intersect(species.de$CodeOriginal, species.in$CodeOriginal) # should be 0

# Reorder columns
species.de <- species.de %>%
  select(Region, Site, Code, CodeOriginal, Name, Native, Duration, Lifeform)


# Check for duplicate codes in species list
species.de %>%
  count(Code) %>%
  filter(n > 1) %>%
  arrange(desc(n))

# 2 instances of UNGRS1.SRER
#   but based on name, one applies to spring 2022
species.de %>%
  filter(Code == "UNGRS1.SRER") %>%
  select(Code, Name, Native)

#   Look for UNGRS1 in spring 2022 in subplot data
subplot %>%
  mutate(Date_Monitored = as.character(Date_Monitored)) %>%
  filter(Site == "SRER",
         str_detect(Date_Monitored, "2022"),
         str_detect(CodeOriginal, "UNGRS")) # CodeOriginal is actually UNGRS-1

#   Look for UNGRS-1 at SRER in species list
species.de %>%
  filter(Site == "SRER",
         CodeOriginal == "UNGRS-1")

#   Change Code & CodeOriginal of UNGRS1 from spring 2022 to UNGRS-1
species.de %>%
  filter(Code == "UNGRS1.SRER") %>%
  select(Code, Name, Native, Duration, Lifeform)
species.de$CodeOriginal[str_detect(species.de$Name, "most likely ERLE, spring 2022")] <- "UNGRS-1"
species.de$Code[str_detect(species.de$Name, "most likely ERLE, spring 2022")] <- "UNGRS-1.SRER"


#   Remove old UNGRS-1 row
species.de <- species.de %>%
  filter(Name != "Unknown grass 1, SRER")

#   Check to see it was removed
species.de %>%
  filter(Site == "SRER",
         CodeOriginal == "UNGRS-1")


# Check for unique codes with lo-depen and lo-indepen
intersect(species.de$CodeOriginal, species.in$CodeOriginal)
intersect(species.de$Code, species.in$CodeOriginal)
#   location-dependent codes are also unique from location-dependent ones, both original code and new code with site info


# Check for absent information (NAs)
unique(species.de$Native)
unique(species.de$Duration)
unique(species.de$Lifeform)


# Reorder rows by Region and Site 
species.de <- species.de %>% 
  arrange(Site) %>% 
  arrange(Region)


# Final manual check ------------------------------------------------------

# Location independent
# OUTPUT: Final manual check of location-independent species list
write_csv(species.in,
          file = "Sonoran-data/data-wrangling-intermediate/01a_output6_location-independent-final-check.csv")

# EDITED: fixed a few codes
#   Changed codes that did not match USDA Plants code (AMIN3, EUAB, EUME3, STSP3, URLI5)
species.in <- read_xlsx("Sonoran-data/data-wrangling-intermediate/01b_edited6_location-independent-final-fix.xlsx")



# Location dependent
# OUTPUT: Final manual check of location-dependent species list; check Native status
write_csv(species.de,
          file = "Sonoran-data/data-wrangling-intermediate/01a_output7_location-dependent-final-check.csv")

# EDITED: Change Native status based on name for unknown grass at SCC,
#     and switched names for UNFO-1 at SRER & Patagonia (name was specific to site)
species.de <- read_xlsx("Sonoran-data/data-wrangling-intermediate/01b_edited7_location-dependent-final-fix.xlsx")




# Write species lists to CSVs ---------------------------------------------

# Location independent
write_csv(species.in,
          file = "Sonoran-data/cleaned/01_species-list_location-independent_clean.csv")


# Location dependent
write_csv(species.de,
          file = "Sonoran-data/cleaned/01_species-list_location-dependent_clean.csv")





save.image("Sonoran-RData/01_curate-species-list.RData")

