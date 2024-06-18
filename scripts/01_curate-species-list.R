# Created: 2023-09-18
# Last updated: 2024-04-29

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
#     `2023-09-15 Master 1.0 Germination Data_raw.xlsx`, and codes from subplot data not included in species list.
#     Started with these because it makes sense to start from the original species list, and because
#     subplot data had information about lifeform with observation.
#   Create location-independent and location-dependent lists from species list and subplot data.
#   Then add codes from 2x2 data.
#   Data wrangling (including species info assignment) will be handled separately for 2x2 and subplot data.



library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/2023-09-15_Master 1.0 Germination Data_raw.xlsx", sheet = "AllSubplotData")
plot.2x2.raw <- read_xlsx("data/raw/2023-09-15_Master 1.0 Germination Data_raw.xlsx", sheet = "AllPlotData")
species.raw <- read_xlsx("data/raw/from-Master_species-list-with-native-status_LO.xlsx")
mix <- read_xlsx("data/raw/from-Master_seed-mix_LO.xlsx", sheet = "with-site_R")

# Dependency created in 01-dependency_assign-seeded-species-native-status.R
native.fix <- read_csv("data/data-wrangling-intermediate/01-dependency_seeded-species-to-be-marked-native.csv")


# Notes about manual edits ------------------------------------------------

# For manual edits to CSVs, the CSV is written from R, copied and named a new name, edited,
#   and new file is read into R.

# Files in the format "output_xx.csv" are ones written from R.
# Files in the format "edited_xx.csv" are manually edited and read back in as new objects,
#   but then usually used to alter existing objects.
#   See README_rawdata.md for more details.



# Organize subplot and 2x2 data -------------------------------------------

# Add Region column
subplot <- subplot.raw %>%
  mutate(
    Region = case_when(
      str_detect(subplot.raw$Site, c("AguaFria|BabbittPJ|MOWE|Spiderweb|BarTBar|FlyingM|PEFO|TLE")) ~ "Colorado Plateau",
      str_detect(subplot.raw$Site, c("CRC|UtahPJ|Salt_Desert")) ~ "Utah",
      str_detect(subplot.raw$Site, c("29_Palms|AVRCD")) ~ "Mojave",
      str_detect(subplot.raw$Site, c("Creosote|Mesquite")) ~ "Chihuahuan",
      str_detect(subplot.raw$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
      str_detect(subplot.raw$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central",
      TRUE ~ "unk"
    ),
    raw.row = 1:nrow(subplot.raw)
  ) %>%
  rename(CodeOriginal = Species_Code)

subplot.codes <- subplot |>
  select(CodeOriginal, Region, Site) |>
  distinct(.keep_all = TRUE)

# Add Region column
p2x2 <- plot.2x2.raw |>
  mutate(Region = case_when(
    str_detect(plot.2x2.raw$Site, c("AguaFria|BabbittPJ|MOWE|Spiderweb|BarTBar|FlyingM|PEFO|TLE")) ~ "Colorado Plateau",
    str_detect(plot.2x2.raw$Site, c("CRC|UtahPJ|Salt_Desert")) ~ "Utah",
    str_detect(plot.2x2.raw$Site, c("29_Palms|AVRCD")) ~ "Mojave",
    str_detect(plot.2x2.raw$Site, c("Creosote|Mesquite")) ~ "Chihuahuan",
    str_detect(plot.2x2.raw$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(plot.2x2.raw$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central"
  ))

p2x2.code.cols <- plot.2x2.raw |>
  select(starts_with("Additional"))

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
  filter(
    !CodeOriginal %in% c("0", "NA"),
    !is.na(CodeOriginal)
  )

# OUTPUT: write to csv to manually fill in information
write_csv(subplot.missing,
  file = "data/data-wrangling-intermediate/01a_output1_subplot-codes-missing.csv"
)
head(subplot.missing)

# EDITED: manually edit new file to add Name, Native, Lifeform, and Duration cols
#   I assumed DACA from CO Plateau was DACA7, because only DACA7 appears in the CO Plateau tabs of Master.xlsx,
#     so I think this was a typo (and no other species has code similar to DACA).
#   I also assumed ATCA was ATCA2  at Mesquite, because ATCA2 was seeded there and ATCA
#     is generally only found in California, according to USDA Plants. In the subplot data,
#     the observation of AVCA was also marked seeded.
#   Also I assumed LILE was LILE3 at CRC because it was marked as seeded and LILE3 was in seed mix
sub.missing <- read_csv("data/data-wrangling-intermediate/01b_edited1_subplot-codes-missing_native-duration-lifeform.csv")
head(sub.missing)



# Separate species by location dependence (knowns/unknowns) ---------------

# Knowns and unknowns must be separated;
#   plants not defined to species level are location-specific and Site must be retained.

# Unknowns (location-dependent)
#   From original master species list
species.m.unk <- species.raw %>%
  filter(str_detect(species.raw$Name, "Unk|unk|spp\\.|sp\\.|Could be|Very similar to GIsp 1")) %>%
  filter(CodeOriginal != "VEPEX2") |> # name contains "ssp." but it is a known
  arrange(Region) %>%
  arrange(CodeOriginal)

#   Ones missing from original master species list
sub.missing.unk <- sub.missing %>%
  filter(str_detect(sub.missing$Name, "Unk|unk|spp\\.")) %>%
  arrange(Region) %>%
  arrange(CodeOriginal)


# Knowns (location-independent)
#   From original master species list
VEPEX2 <- species.raw |>
  filter(CodeOriginal == "VEPEX2") # make separate row because it contains "ssp." but isn't an unknown

species.m.known <- species.raw %>%
  filter(!str_detect(species.raw$Name, "Unk|unk|spp\\.|sp\\.|Could be|Very similar to GIsp 1")) %>%
  bind_rows(VEPEX2) |>
  select(-Region) %>%
  arrange(CodeOriginal) |>
  distinct(.keep_all = TRUE)

#   Ones missing from original master species list
sub.missing.known <- sub.missing %>% # ones missing from original master list (.xlsx)
  filter(!str_detect(sub.missing$Name, "Unk|unk|spp\\.")) %>%
  select(-Region, -Site) %>%
  distinct(.keep_all = TRUE) |>
  arrange(CodeOriginal)




# Location-independent species from master list & subplot data ------------


# Add lifeform to master/subplot list -------------------------------------

# Extract lifeform information from subplot data for knowns from Master.xlsx
subplot.in.lifeform <- subplot %>%
  select(CodeOriginal, Functional_Group) %>%
  distinct(.keep_all = TRUE) %>%
  rename(Lifeform = Functional_Group) |>
  arrange(CodeOriginal)

# Correct codes with conflicting lifeform info
sub.in.lifeform.duplicate <- count(subplot.in.lifeform, CodeOriginal) |>
  filter(n > 1) |>
  arrange(CodeOriginal)
subplot.in.lifeform.inspect <- subplot.in.lifeform |>
  filter(CodeOriginal %in% sub.in.lifeform.duplicate$CodeOriginal) |>
  arrange(CodeOriginal)

# OUTPUT: list of lifeform info according to subplot data
write_csv(subplot.in.lifeform.inspect,
  file = "data/data-wrangling-intermediate/01a_output2_subplot-lifeform-info.csv"
)

# EDITED: delete incorrect rows so there is only one lifeform assignment per code
#   Fix conflicting lifeform according to USDA Plants
#   Standardize spelling of lifeform to Grass, Forb, or Shrub
subplot.in.duplicate <- read_csv("data/data-wrangling-intermediate/01b_edited2_subplot-lifeform-info-corrected.csv")

subplot.in.lifeform <- subplot.in.lifeform |>
  filter(!CodeOriginal %in% subplot.in.duplicate$CodeOriginal) |>
  bind_rows(subplot.in.duplicate) |>
  arrange(CodeOriginal)
length(unique(subplot.in.lifeform$CodeOriginal)) == nrow(subplot.in.lifeform)


# Add subplot lifeform information to working species list
species.m.known <- left_join(species.m.known, subplot.in.lifeform)

# Compile list of lifeform information thus far from master
lifeform.known <- species.m.known %>%
  filter(!is.na(Lifeform)) %>%
  select(CodeOriginal, Name, Lifeform) %>%
  arrange(CodeOriginal)

# Add missing lifeform information to working species list
# OUTPUT: create list of species without lifeform information
lifeform.na <- species.m.known %>%
  filter(is.na(Lifeform)) %>%
  select(CodeOriginal, Name, Lifeform) %>%
  arrange(CodeOriginal)

write_csv(lifeform.na,
  file = "data/data-wrangling-intermediate/01a_output3_xlsx_lifeform-na.csv"
)
head(lifeform.na)

# EDITED: manually edit new file and fill in Lifeform col
lifeform.na.edit <- read_csv("data/data-wrangling-intermediate/01b_edited3_xlsx_lifeform-na.csv")
head(lifeform.na.edit)

# Add newly edited lifeform data to existing list
lifeform.known <- bind_rows(lifeform.known, lifeform.na.edit)

# Add lifeform info to working species list
species.m.known <- species.m.known |>
  select(-Lifeform) |> # must remove Lifeform col so left_join() does not conflict
  left_join(lifeform.known)


# Standardize Lifeform to Grass/Forb/Shrub
unique(species.m.known$Lifeform)

species.m.known <- species.m.known %>%
  mutate(Lifeform = case_when(
    str_detect(species.m.known$Lifeform, "shrub") ~ "Shrub",
    str_detect(species.m.known$Lifeform, "forb") ~ "Forb",
    species.m.known$Lifeform == "NA" ~ NA,
    TRUE ~ species.m.known$Lifeform
  ))

unique(species.m.known$Lifeform) # Lifeform names have been standardized



# Add duration to species/subplot list ------------------------------------

#   All duration information needed to be added manually from USDA Plants.

# OUTPUT: write output with Native and Lifeform columns
write_csv(species.m.known,
  file = "data/data-wrangling-intermediate/01a_output4_xlsx_native-lifeform.csv"
)
head(species.m.known)

# EDITED: edit new file manually to add Duration
#   Also delete 2 duplicate rows (BOAR & SATR12) with misspelled name/updated name (correct row remains)
#     and add row of all 0s for empty plots
species.m.known <- read_csv("data/data-wrangling-intermediate/01b_edited4_xlsx_native-lifeform-duration.csv")
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
print(codes.fix.in, n = 24)

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
  mutate(CodeOriginal = c("ARPUP6", "ATCA", "BOER", "EUPO3", "DACA", "S-HEBO", "LILE", "S-PASM", "SIAL", "SPAMA"))

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
# Cross-referencing original species list from master shows that ERCI referred to Eragrostis cilianensis
#   at Sonoran Central and Co Plateau, but referred to E. ciliaris at Chihuahuan
#   USDA codes:
#     ERCI = Eragrostis cilianensis
#     ERCI2 = Eragrostis ciliaris

#   Check for ERCI in subplot and 2x2 data
filter(subplot, CodeOriginal == "ERCI")
filter(p2x2.codes, CodeOriginal == "ERCI")
#     ERCI is actually now location-dependent because it means different things in different regions

# For now, remove all
species.in <- species.in |>
  filter(CodeOriginal != "ERCI")


# Unique codes
length(unique(species.in$CodeOriginal)) == nrow(species.in) # TRUE, all codes in species list are unique


# Check for absent information (NAs)
#   "0" is okay
unique(species.in$Native)
unique(species.in$Duration)
unique(species.in$Lifeform)



# Write intermediate location-independent to CSV --------------------------

# This is an intermediate because it does not yet have fixed native status for species that
#   were marked as seeded in the subplot data.
#   Output here is used in 01-dependency_assign-seeded-species-native-status.R

write_csv(species.in,
  file = "data/data-wrangling-intermediate/01-dependency_species-list_location-independent.csv"
)
head(species.in)



# Location-dependent species for subplot ----------------------------------

#   Site information is necessary for unknowns, but only provided for codes in subplot
#     or 2x2 codes. There might be some codes that were in the master species list
#     but never used in either the subplot or 2x2 data, and those codes will not be included
#     because there is no site information (and they aren't necessary).

# Combine location-dependent species
#   from master species list and from subplot data
species.de <- bind_rows(species.m.unk, sub.missing.unk) |>
  filter(CodeOriginal %in% subplot$CodeOriginal)

# OUTPUT: write to CSV to fill in information for species.m.unk
write_csv(species.de,
  file = "data/data-wrangling-intermediate/01a_output5.1_location-dependent.csv"
)
head(species.de) # contains Name & Native

# OUTPUT: extract Site information for species.m.unk to add to location-dependent list
#   and write to CSV
sites.m.unk <- subplot %>%
  filter(CodeOriginal %in% species.m.unk$CodeOriginal) %>%
  select(CodeOriginal, Region, Site) %>%
  distinct(.keep_all = TRUE) %>%
  arrange(Region) %>%
  arrange(CodeOriginal)
write_csv(sites.m.unk,
  file = "data/data-wrangling-intermediate/01a_output5.2_location-dependent_xlsx_sites.csv"
)
head(sites.m.unk) # contains Site

# Look for NA codes that were observations in subplot data
filter(subplot, is.na(CodeOriginal)) |>
  select(Site, `Seeded(Yes/No)`, Functional_Group, Seedling_Count, Average_Height_mm, raw.row) # raw.row 9728, 12576
filter(subplot, CodeOriginal == "NA") |>
  select(Site, `Seeded(Yes/No)`, Functional_Group, Seedling_Count, Average_Height_mm, raw.row) # raw.row 8610
#   Rows 8610 and 9728 are observations of nothing (0)
#   Row 12576 is an observation of an actual plant
#     add this to edited5 as extra row; assign it a CodeOriginal and then manually change CodeOriginal
#     during data wrangling (03.1_data-wrangling_subplot.R)

# EDITED: manually add/correct Site, Native, Duration, and Lifeform cols
#   Use 5.1 as skeleton to edit
#   Delete rows that do not appear in subplot data (don't appear in output5.2.csv)
#   Create extra rows for unknowns (they are defined by Region but need a row for each Site), as cross-referenced
#     with output5.2.csv)
#   Manually add row for NA code that was actually an observation of a plant
species.de <- read_csv("data/data-wrangling-intermediate/01b_edited5_location-dependent_native-duration-lifeform.csv")
head(species.de)



# Renane code & name for lo-depen -----------------------------------------

# Add Site to Name col for location-dependent
species.de$Name <- apply(species.de[, c("Name", "Site")], 1, paste, collapse = ", ")

# Create new Code col that has the site info
species.de$Code <- apply(species.de[, c("CodeOriginal", "Site")], 1, paste, collapse = ".")


# Look for overlapping codes between location-dependent and independent
intersect(species.de$CodeOriginal, species.in$CodeOriginal) # should be 0

# Reorder columns
species.de <- species.de |>
  select(Region, Site, Code, CodeOriginal, Name, Native, Duration, Lifeform)


# Check for duplicate codes in species list
species.de |>
  count(Code) |>
  filter(n > 1) |>
  arrange(desc(n))

# 2 instances of UNGRS1.SRER
#   but based on name, one applies to spring 2022
species.de |>
  filter(Code == "UNGRS1.SRER") |>
  select(Code, Name, Native)

#   Look for UNGRS1 in spring 2022 in subplot data
subplot |>
  mutate(Date_Monitored = as.character(Date_Monitored)) |>
  filter(
    Site == "SRER",
    str_detect(Date_Monitored, "2022"),
    str_detect(CodeOriginal, "UNGRS")
  ) # CodeOriginal is actually UNGRS-1

#   Look for UNGRS-1 at SRER in species list
species.de |>
  filter(
    Site == "SRER",
    CodeOriginal == "UNGRS-1"
  )

#   Change Code & CodeOriginal of UNGRS1 from spring 2022 to UNGRS-1
species.de |>
  filter(Code == "UNGRS1.SRER") |>
  select(Code, Name, Native, Duration, Lifeform)
species.de$CodeOriginal[str_detect(species.de$Name, "most likely ERLE, spring 2022")] <- "UNGRS-1"
species.de$Code[str_detect(species.de$Name, "most likely ERLE, spring 2022")] <- "UNGRS-1.SRER"


#   Remove old UNGRS-1 row
species.de <- species.de |>
  filter(Name != "Unknown grass 1, SRER")

#   Check to see it was removed
species.de |>
  filter(
    Site == "SRER",
    CodeOriginal == "UNGRS-1"
  )


# Check for unique codes with lo-depen and lo-indepen
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
  file = "data/data-wrangling-intermediate/01-dependency_species-list_location-dependent.csv"
)




# Fix native status for select seeded species -----------------------------

# Load dependency list created in 01.1-dependency_assign-seeded-species-native-status.R
native.fix <- read_csv("data/data-wrangling-intermediate/01-dependency_seeded-species-to-be-marked-native.csv")


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
  filter(
    !is.na(CodeOriginal),
    CodeOriginal != "0"
  ) |>
  arrange(CodeOriginal)



# Address ones that need duplicate rows from 2x2 --------------------------

#   Some CodeOriginal values give a description that includes more than one plant;
#     hence, more than one row is needed for these

# OUTPUT: create list of sites and codes that need more info
write_csv(p2x2.codes.missing,
  file = "data/data-wrangling-intermediate/01a_output6_codes-missing-2x2plot.csv"
)
head(p2x2.codes.missing)

# EDITED: add/correct Native, Duration, Lifeform info
#   Create multiple rows for codes that mention more than one species (happens only at SRER and Patagonia)
#   These codes are technically location-specific, but because they are only used at one location,
#     they can be considered location independent
#   Cross reference ambiguous codes with master species list and notes from raw 2x2 data
#   Changed CRES1 to CRES11 because there's no such thing as CRES1 and CRES11 was seeded at the site
p2x2.codes.missing <- read_csv("data/data-wrangling-intermediate/01b_edited6_codes-missing-2x2plot.csv")
head(p2x2.codes.missing)


# Investigate ones that need duplicates
p2x2.codes.dup <- p2x2.codes.missing |>
  filter(NeedsItsDuplicate == "Yes")

# Location dependence
count(p2x2.codes.dup, LocationDependence) # most are location-independent

p2x2.codes.dup |>
  filter(LocationDependence == "dependent") |>
  select(Site, CodeOriginal)
#   both codes refer to only location-dependent species





# Add site to code and name for 2x2 lo-dependent --------------------------

# Add site to code and name for location-dependent species
#   for all rows, not just ones that need duplicate
p2x2.codes.de <- p2x2.codes.missing %>%
  filter(LocationDependence == "dependent")
p2x2.codes.de$Code <- apply(p2x2.codes.de[, c("Code", "Site")], 1, paste, collapse = ".")
p2x2.codes.de$Name <- apply(p2x2.codes.de[, c("Name", "Site")], 1, paste, collapse = ", ")


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
  select(
    Region, Site, CodeOriginal, CodeOriginal_2x2, Code, Code_2x2, Name, Name_2x2,
    Native, Native_2x2, Duration, Duration_2x2, Lifeform, Lifeform_2x2
  )

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
#   or correct in matching with master species list (Chaenactis); subplot Names should be used

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

# Conclusion: subplot Name & Native status should be used over 2x2

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
  arrange(CodeOriginal)

# Look for codes previously standardized, now that we have added 2x2 data
species.in %>%
  filter(Code %in% codes.standardized.in$CodeOriginal) # no old codes show up

# OUTPUT: Final manual check of location-independent species list
write_csv(species.in,
  file = "data/data-wrangling-intermediate/01a_output7_location-independent-final-check.csv"
)

# EDITED: fixed a few codes
#   Changed a few codes with wrong numbers (ELEL5 and SPAM2)
#   Changed codes that did not match USDA Plants code (EUAB, EUME3, URLI5)
species.in <- read_xlsx("data/data-wrangling-intermediate/01b_edited7_location-independent-final-fix.xlsx")

# Write to csv
write_csv(species.in,
  file = "data/cleaned/01_species-list_location-independent_clean.csv"
)




# Full species list for location-dependent --------------------------------

#   This is the finalized version of the complete species list for location-dependent

species.2x2.de <- p2x2.codes.missing %>%
  filter(LocationDependence == "dependent") %>%
  select(-NeedsItsDuplicate, -LocationDependence, -DuplicateNum)

setdiff(colnames(species.2x2.de), colnames(species.subplot.de)) # columns are the same

species.de <- bind_rows(species.2x2.de, species.subplot.de) %>%
  distinct(.keep_all = TRUE) %>%
  arrange(Name) %>%
  arrange(Code) |>
  arrange(Site) |>
  arrange(Region)

# Look for codes previously standardized, now that we have added 2x2 data
species.de %>%
  filter(Code %in% codes.standardized.in$CodeOriginal) # no old codes show up

# OUTPUT: Final manual check of location-dependent species list; look for duplicate codes
write_csv(species.de,
  file = "data/data-wrangling-intermediate/01a_output8_location-dependent-final-check.csv"
)

# EDITED: remove rows that are (functionally) duplicates
#   See Excel file textbox for changes
species.de <- read_xlsx("data/data-wrangling-intermediate/01b_edited8_location-dependent-final-fix.xlsx")

# Write to csv
write_csv(species.de,
  file = "data/cleaned/01_species-list_location-dependent_clean.csv"
)



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
  file = "data/cleaned/01_subplot_species-list_location-independent_clean.csv"
)


# Location-dependent codes
subplot.codes.de <- species.de %>%
  filter(CodeOriginal %in% subplot$CodeOriginal)

unfo.12576 <- species.de |>
  filter(CodeOriginal == "UNFO.12576.assigned")
subplot.codes.de <- bind_rows(subplot.codes.de, unfo.12576) # add back in manually assigned code

subplot.codes.de %>%
  filter(Code %in% filter(subplot.codes.de, duplicated(Code))$Code) %>%
  arrange(Code) # no duplicates

write_csv(subplot.codes.de,
  file = "data/cleaned/01_subplot_species-list_location-dependent_clean.csv"
)



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
  file = "data/data-wrangling-intermediate/01a_output9_2x2-location-independent-need-duplicate-number.csv"
)

# EDITED: list of location-independent species from 2x2 with DuplicateNum col
#   This was much easier to just do manually than to try and do in R
p2x2.codes.in.dup <- read_csv("data/data-wrangling-intermediate/01b_edited9_2x2-location-independent-duplicate-number-added.csv")

# Add ones that need duplicates to ones that don't for complete list
p2x2.codes.in <- p2x2.codes.in |>
  filter(!CodeOriginal %in% p2x2.codes.in.dup$CodeOriginal) |>
  mutate(
    NeedsItsDuplicate = "No",
    DuplicateNum = 0
  ) |>
  bind_rows(p2x2.codes.in.dup) |>
  arrange(Code)

# Write to csv
write_csv(p2x2.codes.in,
  file = "data/cleaned/01_2x2_species-list_location-independent_clean.csv"
)



# Location-dependent codes
p2x2.codes.de <- species.de %>%
  filter(CodeOriginal %in% p2x2.codes$CodeOriginal)

# Mark ones that need duplicates
#   Use p2x2.codes.missing, a table previously created that has correct duplicate information
#    to find ones that need duplicates
p2x2.codes.de.dup <- p2x2.codes.missing %>%
  filter(
    LocationDependence == "dependent",
    NeedsItsDuplicate == "Yes"
  ) %>%
  arrange(CodeOriginal)

#   Check to see if any information has changed (species.de is standardized info)
species.de.compare <- species.de |>
  filter(Code %in% p2x2.codes.de.dup$Code)
setdiff(species.de.compare$Code, p2x2.codes.de.dup$Code)
setdiff(species.de.compare$Name, p2x2.codes.de.dup$Name)
setdiff(species.de.compare$Native, p2x2.codes.de.dup$Native)
setdiff(species.de.compare$Duration, p2x2.codes.de.dup$Duration)
setdiff(species.de.compare$Lifeform, p2x2.codes.de.dup$Lifeform)
#   nothing is different so p2x2.codes.de.dup can be used

p2x2.codes.de.dup <- p2x2.codes.de.dup |>
  select(-LocationDependence) # remove col to prepare to bind rows


# Add NeedsItsDuplicate and DuplicateNum columns to non-duplicate codes and then compile list
p2x2.codes.de <- species.de %>%
  filter(CodeOriginal %in% p2x2.codes$CodeOriginal) |>
  filter(!CodeOriginal %in% p2x2.codes.de.dup$CodeOriginal) |>
  mutate(
    NeedsItsDuplicate = "No",
    DuplicateNum = 0
  ) |>
  bind_rows(p2x2.codes.de.dup) |>
  arrange(Code)

# Write to csv
write_csv(p2x2.codes.de,
  file = "data/cleaned/01_2x2_species-list_location-dependent_clean.csv"
)



save.image("RData/01_curate-species-list.RData")
