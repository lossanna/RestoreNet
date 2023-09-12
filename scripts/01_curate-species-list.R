# Created: 2022-11-28
# Last updated: 2023-09-12

# Purpose: Curate a complete species list with Code, Code Original, Name, Native, Duration, Lifeform info.
#   Two lists must be created, a location-independent version (known species), and a 
#     location-dependent version (unknown species, which includes location information).
#   The lists will include every code in the subplot and 2x2 plot data.
#   There will also be separate lists for the subplot and 2x2 data, but all the species info will be the same.
# The species lists are essentially metadata for the codes.

# Note about dependency: To create the dependency CSV, read in .xlsx files and run
#   code from beginning to "Write intermediate to CSV" section to create dependency. 
#   Then run the 01-dependency_assign-seeded-species-native-status.R to create the dependency that must be
#     read in for this script, and continue running this script until the end.
#   CSVs of intermediate species lists for both location dependent and independent.

# Workflow:
#   Start with species list (from-Master_species-list-with-native-status_LO) adapted from
#     Master Germination Data 2022.xlsx, and codes from subplot data not included in species list.
#     Started with these because it makes sense to start from the original species list, and because
#     subplot data had information about lifeform with observation.
#   Create location-independent and location-dependent lists from species list and subplot data.
#   Then add codes from 2x2 data.



library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllSubplotData")
plot.2x2.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")
species.raw <- read_xlsx("data/raw/from-Master_species-list-with-native-status_LO.xlsx")
mix <- read_xlsx("data/raw/from-Master_seed-mix_LO.xlsx", sheet = "with-site_R")

# Dependency created in 01-dependency_assign-seeded-species-native-status.R
native.fix <- read_csv("data/raw/01-dependency_seeded-species-to-be-marked-native.csv")


# Notes about manual edits ------------------------------------------------

# For manual edits to CSVs, the CSV is written from R, copied and named a new name, edited, 
#   and new file is read into R.

# Files in the format "output-species_xx.csv" are ones written from R.
# Files in the format "edited-species_xx.csv" are manually edited and read back in as new objects,
#   but then usually used to alter existing objects.
#   See README_rawdata.md for more details.



# Organize subplot and 2x2 data -------------------------------------------

# Add Region column
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
  rename(CodeOriginal = Species_Code)

# Add Region column
p2x2 <- plot.2x2.raw |> 
  mutate(Region = case_when(
    str_detect(plot.2x2.raw$Site, c("AguaFria|BabbittPJ|MOWE|Spiderweb|BarTBar|FlyingM|PEFO|TLE")) ~ "Colorado Plateau",
    str_detect(plot.2x2.raw$Site, c("CRC|UtahPJ|Salt_Desert")) ~ "Utah",
    str_detect(plot.2x2.raw$Site, c("29_Palms|AVRCD")) ~ "Mojave",
    str_detect(plot.2x2.raw$Site, c("Creosote|Mesquite")) ~ "Chihuahuan",
    str_detect(plot.2x2.raw$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(plot.2x2.raw$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central"))

# Compile 2x2 codes
p2x2.codes <- p2x2 %>% 
  select(Region, Site, starts_with("Additional")) %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(starts_with("Additional"), names_to = "drop", values_to = "CodeOriginal") %>% 
  select(-drop) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(CodeOriginal)


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
          file = "data/raw/01a_output-species1_subplot-codes-missing.csv")
head(subplot.missing)

# EDITED: manually edit new file to add Name, Native, Lifeform, and Duration cols
sub.missing <- read_csv("data/raw/01b_edited-species1_subplot-codes-missing_native-duration-lifeform.csv")
head(sub.missing)



# Separate species by location dependence (knowns/unknowns) ---------------

# Knowns and unknowns must be separated; 
#   plants not defined to species level are location-specific and Site must be retained.

# Unknowns (location-dependent) 
#   From original master species list
species.m.unk <- species.raw %>%
  filter(str_detect(species.raw$Name, "Unk|unk|spp.|Could be")) %>% 
  arrange(Region) %>% 
  arrange(CodeOriginal)

#   Ones missing from original master species list
sub.missing.unk <- sub.missing %>%
  filter(str_detect(sub.missing$Name, "Unk|unk|spp.|Could be")) %>% 
  arrange(Region) %>% 
  arrange(CodeOriginal)


# Knowns (location-independent)
#   From original master species list
species.m.known <- species.raw %>%
  filter(!str_detect(species.raw$Name, "Unk|unk|spp.|Could be")) %>% 
  select(-Region) %>% 
  arrange(CodeOriginal) |> 
  distinct(.keep_all = TRUE)

#   Ones missing from original master species list
sub.missing.known <- sub.missing %>% # ones missing from original master list (.xlsx)
  filter(!str_detect(sub.missing$Name, "Unk|unk|spp.|Could be")) %>% 
  select(-Region, -Site) %>% 
  arrange(CodeOriginal)




# Location-independent species from master list & subplot data ------------


# Add lifeform to species/subplot list ------------------------------------

# Extract lifeform information from subplot data for knowns from Master.xlsx
subplot.in.lifeform <- subplot %>% 
  select(CodeOriginal, Functional_Group) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(Lifeform = Functional_Group)

# Correct codes with conflicting lifeform info
sub.in.lifeform.duplicate <- count(subplot.in.lifeform, CodeOriginal) |> 
  filter(n > 1) |> 
  arrange(CodeOriginal)
subplot.in.lifeform.inspect <- subplot.in.lifeform |> 
  filter(CodeOriginal %in% sub.in.lifeform.duplicate$CodeOriginal) |> 
  arrange(CodeOriginal)

# OUTPUT: list of lifeform info according to subplot data
write_csv(subplot.in.lifeform.inspect,
          file = "data/raw/01a_output-species2_subplot-lifeform-info.csv")

# EDITED: delete incorrect rows so there is only one lifeform assignment per code
subplot.in.duplicate <- read_csv("data/raw/01b_edited-species2_subplot-lifeform-info-corrected.csv")

subplot.in.lifeform <- subplot.in.lifeform |> 
  filter(!CodeOriginal %in% subplot.in.duplicate$CodeOriginal) |> 
  bind_rows(subplot.in.duplicate)


# Add subplot lifeform information to working species list
species.m.known <- left_join(species.m.known, subplot.in.lifeform)

# Standardize Lifeform to Grass/Forb/Shrub
unique(species.m.known$Lifeform)

species.m.known <- species.m.known %>% 
  mutate(Lifeform = case_when(
    str_detect(species.m.known$Lifeform, "shrub") ~ "Shrub",
    TRUE ~ species.m.known$Lifeform))

unique(species.m.known$Lifeform) # Lifeform names have been standardized, with NAs
species.m.known$Lifeform[species.m.known$Lifeform == "NA"] <- NA # convert to real (logical) NA
unique(species.m.known$Lifeform) # lifeform has been standardized

# Compile list of lifeform information thus far
lifeform.known <- species.m.known %>% 
  filter(!is.na(Lifeform)) %>% 
  select(CodeOriginal, Name, Lifeform) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(CodeOriginal)

# Add missing lifeform information to working species list
# OUTPUT: create list of species without lifeform information
lifeform.na <- species.m.known %>% 
  filter(is.na(Lifeform)) %>% 
  select(CodeOriginal, Name, Lifeform) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(CodeOriginal)

write_csv(lifeform.na,
          file = "data/raw/01a_output-species3_xlsx_lifeform-na.csv")
head(lifeform.na)

# EDITED: manually edit new file and fill in Lifeform col
lifeform.na.edit <- read_csv("data/raw/01b_edited-species3_xlsx_lifeform-na.csv")
head(lifeform.na.edit)

# Add newly edited lifeform data to existing list
lifeform.known <- bind_rows(lifeform.known, lifeform.na.edit)

# Add lifeform info to working species list
species.m.known <- species.m.known |> 
  select(-Lifeform) |> # must remove Lifeform col so left_join() does not conflict
  left_join(lifeform.known)



# Add duration to species/subplot list ------------------------------------

#   All duration information needed to be added manually from USDA Plants.

# OUTPUT: write output with Native and Lifeform columns
write_csv(species.m.known,
          file = "data/raw/01a_output-species4_xlsx_native-lifeform.csv")
head(species.m.known)

# EDITED: edit new file manually to add Duration
#   Also delete 2 duplicate rows (BOAR & SATR12) with misspelled name/updated name (correct row remains)
#     and add row of all 0s for empty plots
species.m.known <- read_csv("data/raw/01b_edited-species4_xlsx_native-lifeform-duration.csv")
head(species.m.known)



# Add species from subplot data not in master

# Combine species.m.known and sub.missing.known
species.in <- bind_rows(species.m.known, sub.missing.known) %>% 
  arrange(CodeOriginal)

# Check for absent information (NAs)
apply(species.in, 2, anyNA) # should all be FALSE



# Standardize codes for lo-indepen species/subplot ------------------------

# Add Code col
species.in$Code <- species.in$CodeOriginal

# Extract species with multiple codes for the same name, retaining all codes
codes.fix.in <- species.in %>% 
  filter(Name %in% filter(species.in, duplicated(Name))$Name) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Name) 
codes.fix.in

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
  mutate(CodeOriginal = c("ARPUP6", "BOER", "EUPO3", "S-HEBO", "S-PASM", "SIAL", "SPAMA")) 

# Remove wrong codes from species list and add correct ones
species.in <- species.in %>% 
  filter(!CodeOriginal %in% codes.standardized.in$CodeOriginal) %>% 
  bind_rows(codes.standardized.in) |> 
  arrange(CodeOriginal)

# DRCU/DRCUI and ESCA/ESCAM refer to different varieties, so specificity is retained
#   change name for DRCUI & ESCAM
species.in$Name[species.in$CodeOriginal == "DRCUI"] <- "Draba cuneifolia var. integrifolia"
species.in$Name[species.in$CodeOriginal == "ESCAM"] <- "Eschscholzia californica ssp. mexicana"

# Reorder cols
species.in <- species.in |> 
  select(CodeOriginal, Code, Name, Native, Duration, Lifeform)



# Find codes with multiple species
names.fix.in <- species.in %>% 
  filter(CodeOriginal %in% filter(species.in, duplicated(CodeOriginal))$CodeOriginal) %>% 
  arrange(CodeOriginal) 
names.fix.in # look at master species list for clarification
# Cross-referencing original species list from master shows that ERCI referred to correct species
#   (Eragrostis cilianensis) at Sonoran Central, but referred to E. ciliaris at Chihuahuan

#   Check for ERCI in subplot and 2x2 data
filter(subplot, CodeOriginal == "ERCI")
filter(p2x2.codes, CodeOriginal == "ERCI")
# ERCI is actually now location-dependent

# For now, remove both
species.in <- species.in |> 
  filter(CodeOriginal != "ERCI")


# Unique codes
length(unique(species.in$CodeOriginal)) == nrow(species.in) # TRUE, all codes in species list are unique


# Check for absent information (NAs; "0" is okay)
unique(species.in$Native)
unique(species.in$Duration)
unique(species.in$Lifeform)



# Write intermediate location-independent to CSV --------------------------

# This is an intermediate because it does not yet have fixed native status for species that
#   were marked as seeded in the subplot data.
#   Output here is used in 01-dependency_assign-seeded-species-native-status.R

write_csv(species.in,
          file = "data/raw/01-dependency_species-list_location-independent.csv")
head(species.in)





# Location-dependent species from master list & subplot -------------------

# Combine location-dependent species 
#   from master species list and from subplot data
species.de <- bind_rows(species.m.unk, sub.missing.unk)

# OUTPUT: write to CSV to fill in information for species.m.unk
write_csv(species.de,
          file = "data/raw/01a_output-species5.1_location-dependent.csv")
head(species.de) # skeleton to edit

# OUTPUT: extract Site information for species.m.unk to add to location-dependent list
#   and write to CSV
sites.m.unk <- subplot %>% 
  filter(CodeOriginal %in% species.m.unk$CodeOriginal) %>% 
  select(CodeOriginal, Region, Site) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Region) %>% 
  arrange(CodeOriginal)
write_csv(sites.m.unk,
          file = "data/raw/01a_output-species5.2_location-dependent_xlsx_sites.csv")
head(sites.m.unk) # use for reference to connect codes to sites

# EDITED: manually add/correct Site, Native, Duration, and Lifeform cols
species.de <- read_csv("data/raw/01b_edited-species5_location-dependent_native-duration-lifeform.csv")
head(species.de)


# Add ERCI
#   ERCI = ERCI at Sonoran Central, ERCI = ERCI2 at Chihuahuan
erci.soc <- names.fix.in[1, ]
erci.soc <- rbind(erci.soc, erci.soc, erci.soc, erci.soc)
erci.soc <- erci.soc |> 
  mutate(Region = "Chihuahuan",
         Site = c("Preserve", "SCC", "Roosevelt", "Pleasant")) |> 
  select(-Code)

erci.chi <- names.fix.in[2, ]
erci.chi <- rbind(erci.chi, erci.chi)
erci.chi <- erci.chi |> 
  mutate(Region = "Sonoran Central",
         Site = c("Creosote", "Mesquite"),
         Code = "ERCI2") |> 
  select(-Code)


species.de <- bind_rows(species.de, erci.soc, erci.chi) |> 
  arrange(Region) |> 
  arrange(CodeOriginal)



# Renane code & name for lo-depen -----------------------------------------

# Add Site to Name col for location-dependent
species.de$Name <- apply(species.de[ , c("Name", "Site")], 1, paste, collapse = ", ")

# Create new Code col that has the site info
species.de$Code <- apply(species.de[ , c("CodeOriginal", "Site")], 1, paste, collapse = ".")

# Look for overlapping codes between location-dependent and independent 
intersect(species.de$CodeOriginal, species.in$CodeOriginal) # should be 0

# Reorder columns
species.de <- species.de |> 
  select(Region, Site, Code, CodeOriginal, Name, Native, Duration, Lifeform)
  

# Check for unique codes
length(unique(species.de$Code)) == nrow(species.de) # all codes in species list are unique
intersect(species.de$CodeOriginal, species.in$CodeOriginal) 
intersect(species.de$Code, species.in$CodeOriginal) 
#   location-dependent codes are also unique from location-dependent ones, both original code and new code with site info


# Check for absent information (NAs)
unique(species.de$Native)
unique(species.de$Duration)
unique(species.de$Lifeform)



# Write intermediate to CSV -----------------------------------------------

#   This is an intermediate because it does not yet have fixed native status for species that
#     were marked as seeded in the subplot data.
#   Output here used in 01-dependency_assign-seeded-species-native-status.R

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

# Rename
species.subplot.in <- species.in
species.subplot.de <- species.de



# Add species from 2x2 plot data ------------------------------------------

#   These are codes from AllPlotData (2 x 2 m plots) that missing from location-independent species list.
#     Codes from these plots are really different and are usually long descriptions.

# Compile codes (includes both location dependent and independent)
p2x2.codes.missing <- p2x2.codes %>% 
  filter(!CodeOriginal %in% species.subplot.in$CodeOriginal) %>% 
  filter(!is.na(CodeOriginal),
         CodeOriginal != "0") |> 
  arrange(CodeOriginal)



# Address ones that need duplicate rows from 2x2 --------------------------

#   Some CodeOriginal values give a description that includes more than one plant;
#     hence, more than one row is needed for these

# OUTPUT: create list of sites and codes that need more info
write_csv(p2x2.codes.missing,
          file = "data/raw/01a_output-species6_codes-missing-2x2plot.csv")
head(p2x2.codes.missing)

# EDITED: add/correct Native, Duration, Lifeform info
#   Create multiple rows for codes that mention more than one species (happens only at SRER and Patagonia)
#   These codes are technically location-specific, but because they are only used at one location,
#     they can be considered location independent
p2x2.codes.missing <- read_csv("data/raw/01b_edited-species6_codes-missing-2x2plot.csv")
head(p2x2.codes.missing)


# Investigate ones that need duplicates
p2x2.codes.dup <- p2x2.codes.missing |> 
  filter(NeedsItsDuplicate == "Yes")

# Location dependence
count(p2x2.codes.dup, LocationDependence) # most are location-independent
#   but there's an odd number, which means that one of the codes got split between
#     location dependent vs independent

p2x2.codes.dup |> 
  filter(LocationDependence == "dependent")
#   one that starts with "SW conmod is" is split
dup.split <- p2x2.codes.dup |> 
  filter(str_detect(CodeOriginal, "SW conmod is"))
dup.split
dup.split$CodeOriginal 
#   split location dependence makes sense; one is blue grama, and one is possible PEPA

# Split one now now longer needs its duplicate row because they will be in separate lists
#   Change NeedsItsDuplicate to "No"
p2x2.codes.missing$NeedsItsDuplicate[str_detect(p2x2.codes.missing$CodeOriginal, "SW conmod is")] <- "No"




# Add site to code and name for 2x2 lo-dependent --------------------------

# Add site to code and name for location-dependent species
#   for all rows, not just ones that need duplicate
p2x2.codes.de <- p2x2.codes.missing %>% 
  filter(LocationDependence == "dependent") 
p2x2.codes.de$Code <- apply(p2x2.codes.de[ , c("Code", "Site")], 1, paste, collapse = ".")
p2x2.codes.de$Name <- apply(p2x2.codes.de[ , c("Name", "Site")], 1, paste, collapse = ", ")


# Check for Codes with multiple species
p2x2.codes.de %>% 
  filter(Code %in% filter(p2x2.codes.de, duplicated(Code))$Code) %>% 
  arrange(Code) 
#   SRER ones are okay - they describe the same species in a number of ways, 
#     but CodeOriginal is still unique except for one that needs its duplicate



# Compare 2x2 and subplot species info ------------------------------------

# Combine location-independent and dependent with new codes
p2x2.codes.missing <- p2x2.codes.missing %>% 
  filter(LocationDependence != "dependent") %>% # remove in correct location-dependent
  bind_rows(p2x2.codes.de) # add correct location-dependent


# Compare 2x2 and subplot species info
#   because 2x2 species info was entered manually based on just the original code, and may be wrong
#   But this is only possible now because Code has site info also

# 2x2 species info for overlapping codes
de.overlap.2x2 <- p2x2.codes.de %>% 
  filter(Code %in% species.subplot.de$Code) %>% 
  arrange(Code)  
newcol <- paste0(colnames(de.overlap.2x2), "_2x2")
colnames(de.overlap.2x2) <- newcol

# Subplot species info for overlapping codes
de.overlap.sub <- species.subplot.de %>% 
  filter(Code %in% p2x2.codes.de$Code) %>% 
  arrange(Code)

# Combine and compare
de.overlap <- bind_cols(de.overlap.2x2, de.overlap.sub) %>% 
  select(Region, Site, CodeOriginal, CodeOriginal_2x2, Code, Code_2x2, Name, Name_2x2,
         Native, Native_2x2, Duration, Duration_2x2, Lifeform, Lifeform_2x2)

# Visually inspect differences in table by column
#   Code
identical(de.overlap$CodeOriginal, de.overlap$CodeOriginal_2x2) # no difference
identical(de.overlap$Code, de.overlap$Code_2x2) # no difference

#   Name
identical(de.overlap$Name, de.overlap$Name_2x2)
setdiff(de.overlap$Name, de.overlap$Name_2x2) 
de.overlap.name <- de.overlap |> 
  filter(Name != Name_2x2)
#  manually check and see that subplot names are generally more detailed, 
#   or correct in matching with master species list; subplot Names should be used

#   Native status
identical(de.overlap$Native, de.overlap$Native_2x2)
count(de.overlap, Native)
count(de.overlap, Native_2x2)
#   manually check and see that subplot Native status is more specific, and should be used
#     subplot is more specific because ones marked seeded were also marked as Native
#       (see 01-dependency.R), including unknowns

#   Duration
identical(de.overlap$Duration, de.overlap$Duration_2x2) # no difference

#   Lifeform
identical(de.overlap$Lifeform, de.overlap$Lifeform_2x2) # no difference

# Conclusion: suplot Name & Native status should be used over 2x2

# Replace correct Name and Native cols from subplot data for 2x2 data
colnames(de.overlap.2x2) <- colnames(p2x2.codes.de)
identical(de.overlap.2x2$Code, de.overlap.sub$Code)
de.overlap.2x2$Name <- de.overlap.sub$Name
de.overlap.2x2$Native <- de.overlap.sub$Native


p2x2.codes.de <- p2x2.codes.de |> 
  filter(!Code %in% de.overlap.2x2$Code) |> 
  bind_rows(de.overlap.2x2) |> 
  arrange(Code) |> 
  arrange(Site)

p2x2.codes.missing <- p2x2.codes.missing |> 
  filter(LocationDependence == "independent") |> 
  bind_rows(p2x2.codes.de) |> 
  arrange(Code) |> 
  arrange(Site)



# Full species list for location-independent ------------------------------

#   This is the finalized version of the complete species list for location-independent

# Combine location-independent 2x2 codes with subplot codes
species.2x2.in <- p2x2.codes.missing %>% 
  filter(LocationDependence == "independent") %>% 
  select(-Site, -NeedsItsDuplicate, -LocationDependence, -DuplicateNum, -Region)

setdiff(colnames(species.2x2.in), colnames(species.subplot.in)) # columns are the same

species.in <- bind_rows(species.2x2.in, species.subplot.in) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Name) %>% 
  arrange(Code)

# Look for codes previously standardized, now that we have added 2x2 data
species.in %>% 
  filter(Code %in% codes.standardized.in$CodeOriginal) # no old codes show up

# OUTPUT: Final manual check of location-independent species list
write_csv(species.in,
          file = "data/raw/01a_output-species7_location-independent-final-check.csv")

# EDITED: fixed a few codes 
#   Changed a few codes with wrong numbers (ELEL5 and SPAM2)
#   Changed codes that did not match USDA Plants code (EUAB, EUME3, URLI5)
species.in <- read_xlsx("data/raw/01b_edited-species7_location-independent-final-fix.xlsx")

# Write to csv
write_csv(species.in,
          file = "data/cleaned/species-list_location-independent_clean.csv")




# Full species list for location-dependent --------------------------------

#   This is the finalized version of the complete species list for location-dependent

species.2x2.de <- p2x2.codes.missing %>% 
  filter(LocationDependence == "dependent") %>% 
  select(-NeedsItsDuplicate, -LocationDependence, -DuplicateNum) 

setdiff(colnames(species.2x2.de), colnames(species.subplot.de)) # columns are the same

species.de <- bind_rows(species.2x2.de, species.subplot.de) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Name) %>% 
  arrange(CodeOriginal) |> 
  arrange(Site)

# Look for codes previously standardized, now that we have added 2x2 data
species.de %>% 
  filter(Code %in% codes.standardized.in$CodeOriginal) # no old codes show up

# OUTPUT: Final manual check of location-dependent species list; look for duplicate codes
write_csv(species.de,
          file = "data/raw/01a_output-species8_location-dependent-final-check.csv")

# EDITED: remove rows that are (functionally) duplicates
#   See Excel file textbox for changes
species.de <- read_xlsx("data/raw/01b_edited-species8_location-dependent-final-fix.xlsx")

species.de %>% 
  filter(Code %in% filter(species.de, duplicated(Code))$Code) %>% 
  arrange(Code) # SRER duplicates are okay because CodeOriginal is different

# Write to csv
write_csv(species.de,
          file = "data/cleaned/species-list_location-dependent_clean.csv")



# Full lists of subplot codes ---------------------------------------------

#   Make subset lists for subplot and 2x2 using finalized species info (created above)
#     in case there were any small changes not represented otherwise

# Location-independent codes
subplot.codes.in <- species.in %>% 
  filter(CodeOriginal %in% subplot$CodeOriginal)

subplot.codes.in %>% 
  filter(CodeOriginal %in% filter(subplot.codes.in, duplicated(CodeOriginal))$CodeOriginal) %>% 
  arrange(CodeOriginal) # no duplicates

write_csv(subplot.codes.in,
          file = "data/cleaned/subplot_species-list_location-independent_clean.csv")


# Location-dependent codes
subplot.codes.de <- species.de %>% 
  filter(CodeOriginal %in% subplot$CodeOriginal)

subplot.codes.de %>% 
  filter(Code %in% filter(subplot.codes.de, duplicated(Code))$Code) %>% 
  arrange(Code) # no duplicates

write_csv(subplot.codes.de,
          file = "data/cleaned/subplot_species-list_location-dependent_clean.csv")



# Full lists of 2x2 codes -------------------------------------------------

#   Make subset lists for subplot and 2x2 using finalized species info (created above)
#     in case there were any small changes not represented otherwise

# Location-independent codes
p2x2.codes.in <- species.in %>% 
  filter(CodeOriginal %in% p2x2.codes$CodeOriginal)

# Mark ones that need duplicates
p2x2.codes.in.dup <- p2x2.codes.in %>% 
  filter(CodeOriginal %in% filter(p2x2.codes.in, duplicated(CodeOriginal))$CodeOriginal) %>% 
  arrange(CodeOriginal) |> 
  mutate(NeedsItsDuplicate = "Yes")

# OUTPUT: list of location-independent species from 2x2 that need a duplicate row 
#   (CodeOriginal includes more than one species)
write_csv(p2x2.codes.in.dup,
          file = "data/raw/01a_output-species9_p2x2-location-independent-need-duplicate-number.csv")

# EDITED: list of location-independent species from 2x2 with DuplicateNum col
#   This was much easier to just do manually than to try and do in R
p2x2.codes.in.dup <- read_csv("data/raw/01b_edited-species9_p2x2-location-independent-duplicate-number-added.csv")

# Add ones that need duplicates to ones that don't for complete list
p2x2.codes.in <- p2x2.codes.in |> 
  filter(!CodeOriginal %in% p2x2.codes.in.dup$CodeOriginal) |> 
  mutate(NeedsItsDuplicate = "No",
         DuplicateNum = 0) |> 
  bind_rows(p2x2.codes.in.dup)

# Write to csv
write_csv(p2x2.codes.in,
          file = "data/cleaned/p2x2_species-list_location-independent_clean.csv")



# Location-dependent codes
p2x2.codes.de <- species.de %>% 
  filter(CodeOriginal %in% p2x2.codes$CodeOriginal)

# Mark ones that need duplicates
#   Use p2x2.codes.missing, a table previously created that has correct duplicate information
#    to find ones that need duplicates
p2x2.codes.de.dup <- p2x2.codes.missing %>% 
  filter(LocationDependence == "dependent",
         NeedsItsDuplicate == "Yes") %>% 
  arrange(CodeOriginal)

#   Remove species info in case it has changed to ensure standardized version from species.de
p2x2.codes.de.dup

species.de.compare


# Add ones that need duplicates to ones that don't for complete list
p2x2.codes.de <- p2x2.codes.de |> 
  filter(!CodeOriginal %in% p2x2.codes.de.dup$CodeOriginal) |> 
  mutate(NeedsItsDuplicate = "No",
         DuplicateNum = 0) |> 
  bded_rows(p2x2.codes.de.dup)

# Write to csv
write_csv(p2x2.codes.de,
          file = "data/cleaned/p2x2_species-list_location-dependent_clean.csv")



save.image("RData/01_curate-species-list.RData")
