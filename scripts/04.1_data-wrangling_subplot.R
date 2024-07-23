# Created: 2023-09-18
# Last updated: 2024-04-30

# Purpose: Create clean data table for subplot data, with corrected and standardized species information,
#   and monitoring and plot information, and correct SpeciesSeeded column based on each site-specific
#     seed mix and plot.
#   Essentially, add corrected metadata from 01.R and 02.R to subplot data,
#     correct SpeciesSeeded column, and add other grouping columns (PlantSource, PlantSource2, Weedy, PlotMix_Climate).

library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/2023-09-15_Master 1.0 Germination Data_raw.xlsx", sheet = "AllSubplotData")
add.29palms <- read_xlsx("data/raw/29Palms_Spr22.xlsx", sheet = "to merge with raw_LO")
species.in <- read_csv("data/cleaned/01_subplot_species-list_location-independent_clean.csv")
species.de <- read_csv("data/cleaned/01_subplot_species-list_location-dependent_clean.csv")
mix <- read_xlsx("data/raw/from-Master_seed-mix_LO.xlsx", sheet = "with-site_R")
monitor.info <- read_csv("data/cleaned/02_corrected-monitoring-info_clean.csv")
monitor.wrong <- read_csv("data/data-wrangling-intermediate/02_subplot-wrong-monitor-events.csv")
monitor.fixed <- read_csv("data/data-wrangling-intermediate/02_subplot-wrong-monitor-events-corrected.csv")
monitor.site <- read_csv("data/cleaned/02_SiteDateID_clean.csv")
monitor.add.AVRCD <- read_csv("data/data-wrangling-intermediate/02_subplot-wrong-monitor-events-add-AVRCD.csv")
monitor.siteplot <- read_csv("data/cleaned/02_SitePlotID_clean.csv")


# Organize columns --------------------------------------------------------

# Add 29 Palms data from spring 2022
add.29palms$Species_Code <- as.character(add.29palms$Species_Code)
subplot <- bind_rows(subplot.raw, add.29palms)

# Narrow down subplot.raw columns
subplot <- subplot %>%
  select(-Recorder_Initials, -Functional_Group, -`Certainty_of_ID(1-3)`, -Notes) %>%
  mutate(raw.row = 1:nrow(subplot)) %>% # row number is to be able to easily refer back to the raw data and excluded columns if needed
  rename(
    CodeOriginal = Species_Code,
    Count = Seedling_Count,
    Height = Average_Height_mm,
    SpeciesSeeded = `Seeded(Yes/No)`,
    PlotMix = Seed_Mix
  )

# Add Region col
subplot <- subplot %>%
  mutate(Region = case_when(
    str_detect(subplot$Site, c("AguaFria|BabbittPJ|MOWE|Spiderweb|BarTBar|FlyingM|PEFO|TLE")) ~ "Colorado Plateau",
    str_detect(subplot$Site, c("CRC|UtahPJ|Salt_Desert")) ~ "Utah",
    str_detect(subplot$Site, c("29_Palms|AVRCD")) ~ "Mojave",
    str_detect(subplot$Site, c("Creosote|Mesquite")) ~ "Chihuahuan",
    str_detect(subplot$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(subplot$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central"
  ))

# Convert character "NA" to logical NA
sapply(subplot, class) # temporarily convert date columns to character to replace NAs
subplot <- subplot %>%
  mutate(
    Date_Seeded = as.character(Date_Seeded),
    Date_Monitored = as.character(Date_Monitored)
  )
subplot[subplot == "NA"] <- NA
apply(subplot, 2, anyNA) # check all columns for NAs; NAs in CodeOriginal, Count, Height, SpeciesSeeded are expected
subplot <- subplot %>%
  mutate(
    Date_Seeded = as.Date(Date_Seeded),
    Date_Monitored = as.Date(Date_Monitored)
  ) # convert back to date



# Handle NA codes ---------------------------------------------------------

# Extract NA codes
filter(subplot, is.na(CodeOriginal)) # raw.row 8610, 9728, 12576

# Rows 8610 and 9728 are observations for empty plots; Code should be 0
subplot$CodeOriginal[subplot$raw.row == 8610] <- "0"
subplot$CodeOriginal[subplot$raw.row == 9728] <- "0"

# Examine non-empty subplots (12166)
subplot.raw[12576, ]
subplot.raw[12576, c("Species_Code", "Functional_Group", "Seeded(Yes/No)", "Notes")]
#   No notes for 12576, but a functional group was listed; not seeded, and probably an unknown

# Assign location-dependent code for 12576 (developed in 02.R)
species.de |>
  filter(CodeOriginal == "UNFO.12576.assigned")
subplot$CodeOriginal[subplot$raw.row == 12576] <- "UNFO.12576.assigned"


# Check again for NA codes
filter(subplot, is.na(CodeOriginal)) # no NAs


# Check for missing codes by comparing subplot data to both species lists
sub.codes <- c(species.de$CodeOriginal, species.in$CodeOriginal)
setdiff(subplot$CodeOriginal, sub.codes) # should be 0



# Add species info for location-dependent ---------------------------------

# Separate out location-dependent observations
subplot.de <- subplot %>%
  filter(CodeOriginal %in% species.de$CodeOriginal)

# Add species info
subplot.de <- left_join(subplot.de, species.de)

# Check for NA codes
filter(subplot.de, is.na(Code)) # no NAs
apply(subplot.de, 2, anyNA) # NAs for Count, Height, SpeciesSeeded are expected



# Add species info for location-independent codes -------------------------

subplot.in <- subplot %>%
  filter(CodeOriginal %in% species.in$CodeOriginal)

subplot.in <- left_join(subplot.in, species.in)

# Check for NA codes
filter(subplot.in, is.na(Code)) # no NAs
apply(subplot.in, 2, anyNA) # NAs for Count, Height, SpeciesSeeded inherent to data



# Combine location-dependent and independent ------------------------------

subplot <- bind_rows(subplot.in, subplot.de) %>%
  arrange(raw.row)

# Check that there the same number of observations as the original subplot data
#   (+44 fom 29_Palms added)
nrow(subplot) == (nrow(subplot.raw) + 44)



# Check if Introduced plants were marked as Seeded ------------------------

subplot.inva <- subplot %>%
  filter(Native == "Introduced") %>%
  select(Code, SpeciesSeeded, Name, Native) %>%
  distinct(.keep_all = TRUE)
unique(subplot.inva$SpeciesSeeded) # something is mislabeled; no introduced species were seeded
subplot.inva %>%
  filter(SpeciesSeeded == "Yes")

# Fix ERCU2 and ERCI6 - mark all observations as "No" for Seeded col
subplot$SpeciesSeeded[subplot$Name == "Eragrostis curvula"] <- "No"
subplot$SpeciesSeeded[subplot$Name == "Erodium cicutarium"] <- "No"



# Correct monitoring info -------------------------------------------------

# Separate out monitoring info from rest of subplot data
subplot.monitor <- subplot |>
  select(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix, raw.row)

# Find raw.row number of events that need to be fixed
#   some events that need to be fixed have multiple rows in subplot data
wrong.raw.row <- left_join(monitor.wrong, subplot.monitor)

# Attach raw.row to corrected monitoring info
monitor.assign <- data.frame(SiteDatePlotID = wrong.raw.row$SiteDatePlotID)
monitor.assign <- left_join(monitor.assign, monitor.fixed)
#   wrong.raw.row and monitor.assign have the same row order (rows already correspond)
monitor.assign$raw.row <- wrong.raw.row$raw.row

# Separate monitor info that doesn't need to be fixed and add SiteDatePlotID
subplot.monitor <- subplot.monitor |>
  filter(!raw.row %in% monitor.assign$raw.row) |>
  left_join(monitor.info)
subplot.monitor |>
  filter(is.na(SiteDatePlotID)) # all assigned SiteDatePlotID

# Add corrected monitor info for complete list
subplot.monitor <- bind_rows(subplot.monitor, monitor.assign) |>
  arrange(SiteDatePlotID)

# Check for matching lengths
#   (+44 fom 29_Palms added)
nrow(subplot.monitor) == (nrow(subplot.raw) + 44)


# Attach correct monitoring info to subplot data
subplot <- subplot[, -c(1:6)] # remove incorrect info
subplot <- left_join(subplot, subplot.monitor) |> # add correct info
  arrange(raw.row)

# Reorder columns
subplot <- subplot |>
  select(
    Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix, SiteDatePlotID,
    CodeOriginal, Code, Name, Native, Duration, Lifeform, SpeciesSeeded,
    Count, Height, raw.row
  )

# Check for matching lengths
#   (+44 fom 29_Palms added)
nrow(subplot) == (nrow(subplot.raw) + 44)

# Check all cols for NAs
apply(subplot, 2, anyNA) # NA in Count, Height, SpeciesSeeded is okay



# Check for all SiteDatePlotIDs
nrow(monitor.info) # 6384 IDs
length(unique(subplot$SiteDatePlotID)) == nrow(monitor.info)
length(unique(subplot$SiteDatePlotID)) # 6382 IDs
(length(unique(subplot$SiteDatePlotID))) + 2 == nrow(monitor.info)
#   The two missing SiteDatePlotIDs come from the AVRCD events that must be added separately.
#     These subplots didn't have lines in the raw data, but they were measured and
#     recruitment was 0.

# Add the two missing AVRCD events
#   Manually add in the data columns
nrow(subplot)
add.AVRCD <- monitor.add.AVRCD |>
  mutate(
    CodeOriginal = "0", Code = "0", Name = "0", Native = "0", Duration = "0", Lifeform = "0",
    SpeciesSeeded = NA, Count = 0, Height = 0,
    raw.row = c(18038, 18039)
  )

# Add to the rest of the subplot data
subplot <- bind_rows(subplot, add.AVRCD)



# Add SiteDateID and SitePlotID now that subplot data is complete
subplot <- left_join(subplot, monitor.site) |>
  left_join(monitor.siteplot) |> 
  arrange(raw.row)

# Reorder columns again
subplot <- subplot |>
  select(
    Region, Site, Date_Seeded, Date_Monitored, SiteDateID, Plot, Treatment, PlotMix, SitePlotID, SiteDatePlotID,
    CodeOriginal, Code, Name, Native, Duration, Lifeform, SpeciesSeeded,
    Count, Height, raw.row
    )

# Save intermediate subplot df, which has:
#   Mojave events added (29 Palms and AVRCD)
#   Correct monitoring info
#   Correct Native, Duration, Lifeform
#   Incorrect SeededSpecies (not yet addressed)
subplot1 <- subplot



# Correct SpeciesSeeded column --------------------------------------------

# Address seeded species codes not in mix

# Examine possible SpeciesSeeded values
#   then deal with each separately in own section
unique(subplot$SpeciesSeeded)


# Species seeded but not listed in mix by code
subplot.seeded <- subplot %>%
  filter(SpeciesSeeded %in% c("Yes", "Y", "y", "Yes?"))

setdiff(unique(subplot.seeded$Code), unique(mix$CodeOriginal)) # discrepancies exist

species.seeded.not.in.mix <- subplot.seeded |>
  filter(Code %in%
    setdiff(unique(subplot.seeded$Code), unique(mix$CodeOriginal))) |>
  select(
    Site, Region, PlotMix, CodeOriginal, Code, Name, Native, Duration, Lifeform,
    SpeciesSeeded
  ) |>
  distinct(.keep_all = TRUE) |>
  arrange(Name) |>
  arrange(Site) |>
  arrange(Region)

# OUTPUT: create list of species marked seeded but not in mix
write_csv(species.seeded.not.in.mix,
  file = "data/data-wrangling-intermediate/04.1a_output1_seeded-not-in-mix_subplot.csv"
)

# EDITED: manually review and fix SpeciesSeeded status
#   SpeciesSeeded only corrected if plant was identified to species level and definitely
#     not seeded, as well as "local Bouteloua". Unknowns retain original classification.
species.seeded.not.in.mix <- read_xlsx("data/data-wrangling-intermediate/04.1b_edited1_corrected-seeded-not-in-mix_subplot.xlsx")


# Species seeded and in (a) mix
#   Need to look at manually because mixes are site-specific
species.seeded.in.mix <- subplot.seeded |>
  filter(
    SpeciesSeeded %in% c("Yes", "Y", "y", "Yes?"),
    !Code %in% species.seeded.not.in.mix$Code
  ) |>
  select(
    Site, Region, PlotMix, CodeOriginal, Code, Name, Native, Duration, Lifeform,
    SpeciesSeeded
  ) |>
  distinct(.keep_all = TRUE) |>
  arrange(Name) |>
  arrange(Site) |>
  arrange(Region)

# OUTPUT: create list of species marked seeded and in mix
write_csv(species.seeded.in.mix,
  file = "data/data-wrangling-intermediate/04.1a_output2_seeded-in-mix_subplot.csv"
)

# EDITED: manually review and fix SpeciesSeeded status
#   Standardize responses so all are in format "Yes"
#   Incorrect ones corrected
species.seeded.in.mix <- read_xlsx("data/data-wrangling-intermediate/04.1b_edited-species-seeded2_corrected-seeded-in-mix_subplot.xlsx")

# Remove duplicates that resulted from standardizing "Yes"
species.seeded.in.mix <- species.seeded.in.mix |>
  distinct(.keep_all = TRUE)


# Species marked Unknown
species.unk.seeded <- subplot |>
  filter(SpeciesSeeded %in% c("?", "Unk", "UNK")) |>
  select(
    Site, Region, PlotMix, CodeOriginal, Code, Name, Native, Duration, Lifeform,
    SpeciesSeeded
  ) |>
  distinct(.keep_all = TRUE) |>
  arrange(Name) |>
  arrange(Site) |>
  arrange(Region)

# OUTPUT: create list of species of unknown seeding status
write_csv(species.unk.seeded,
  file = "data/data-wrangling-intermediate/04.1a_output3_unk-if-seeded_subplot.csv"
)

# EDITED: manually review and fix SpeciesSeeded status
#   Unknowns marked as not seeded.
species.unk.seeded <- read_xlsx("data/data-wrangling-intermediate/04.1b_edited3_corrected-unk-if-seeded_subplot.xlsx")



# Species marked NA
species.na.seeded <- subplot |>
  filter(is.na(SpeciesSeeded)) |>
  select(
    Site, Region, PlotMix, CodeOriginal, Code, Name, Native, Duration, Lifeform,
    SpeciesSeeded
  ) |>
  distinct(.keep_all = TRUE) |>
  arrange(Name) |>
  arrange(Site) |>
  arrange(Region)

# OUTPUT: create list of species of NA seeding status
write_csv(species.na.seeded,
  file = "data/data-wrangling-intermediate/04.1a_output4_NA-seeded_subplot.csv"
)

# EDITED: manually review and fix SpeciesSeeded status
#   Unknowns marked as not seeded.
#   0 code marked as "0" (no plants were present)
#  Seeded status changed if identified to species level and it was seeded.
species.na.seeded <- read_xlsx("data/data-wrangling-intermediate/04.1b_edited4_corrected-NA-seeded_subplot.xlsx")




# Species marked not seeded
subplot.no <- subplot |>
  filter(SpeciesSeeded %in% c("No", "N"))

species.no.seeded <- subplot.no |>
  select(
    Site, Region, PlotMix, CodeOriginal, Code, Name, Native, Duration, Lifeform,
    SpeciesSeeded
  ) |>
  distinct(.keep_all = TRUE) |>
  arrange(Name) |>
  arrange(Site) |>
  arrange(Region)

# OUTPUT: create list of species marked not seeded
write_csv(
  species.no.seeded,
  "data/data-wrangling-intermediate/04.1a_output5_not-seeded_subplot.csv"
)

# EDITED: manually review and fix SpeciesSeeded status
#   Standardize responses so all are in format "Yes"
#   Incorrect ones corrected
species.no.seeded <- read_xlsx("data/data-wrangling-intermediate/04.1b_edited5_corrected-not-seeded_subplot.xlsx")

# Remove duplicates that resulted from standardizing "No"
species.no.seeded <- species.no.seeded |>
  distinct(.keep_all = TRUE)


# Compile list of correct SpeciesSeeded
#   Correct SpeciesSeeded status based on PlotMix and Site
seeded.correct <- bind_rows(
  species.seeded.in.mix,
  species.seeded.not.in.mix,
  species.unk.seeded,
  species.na.seeded,
  species.no.seeded
) |>
  distinct(.keep_all = TRUE) |>
  arrange(PlotMix) |>
  arrange(Name) |>
  arrange(Site) |>
  arrange(Region) |>
  arrange(desc(SpeciesSeeded))

#   Look for duplicates with conflicting SpeciesSeeded info
seeded.correct.dup.codes <- seeded.correct[duplicated(seeded.correct[, 1:9]), ] # this only gets
#                                                       half of the duplicates, not both
seeded.correct.dup <- seeded.correct |>
  filter(Code %in% seeded.correct.dup.codes$Code) |>
  arrange(PlotMix) |>
  arrange(Code) |>
  arrange(Site) # this gets ones that also aren't
#     duplicates, they are the same code but different plots or sites (I couldn't figure out
#                                                   how to get both duplicates in their own df)

# OUTPUT: list of codes with conflicting SpeciesSeeded
#   list also includes ones that are not duplicates
write_csv(seeded.correct.dup,
  file = "data/data-wrangling-intermediate/04.1a_output6_conflicting-SpeciesSeeded.csv"
)

# EDITED: manually add Retain column to indicate if rows should be retained or not
#   Retained: ones that are not actually duplicates/conflicts; ones marked "No" for SpeciesSeeded
seeded.correct.dup <- read_xlsx("data/data-wrangling-intermediate/04.1b_edited6_corrected-conflicting-SpeciesSeeded.xlsx")

# Create correct df of codes that had duplicates (including rows that weren't actually duplicates)
seeded.correct.fixed <- seeded.correct.dup |>
  filter(Retain == 1) |>
  select(-Retain)

# Remove all codes that have been fixed and add back the fixed ones
seeded.correct <- seeded.correct |>
  filter(!Code %in% seeded.correct.fixed$Code) |>
  bind_rows(seeded.correct.fixed) |>
  arrange(Code) |>
  arrange(Site) |>
  arrange(Region)

# Check possible values of SpeciesSeeded
unique(seeded.correct$SpeciesSeeded) # should be 0, Yes, No

# Look for codes that exist in subplot data but not in seeded.correct
setdiff(unique(subplot$Code), unique(seeded.correct$Code)) # should be 0


# Assign corrected SpeciesSeeded to subplot data
subplot <- subplot |>
  select(-SpeciesSeeded)
subplot <- left_join(subplot, seeded.correct)

# Check for matching lengths
#   (+44 from 29 Palms, +2 from AVRCD)
nrow(subplot) == (nrow(subplot.raw) + 46)


# Look for NAs
unique(subplot$SpeciesSeeded) # should be no NAs



# Save intermediate subplot with correct SeededSpecies,
#   but PlantSource not yet addressed
subplot2 <- subplot



# Add additional grouping columns -----------------------------------------

# PlantSource: Unknown_recruit, Native_recruit, Introduced/Invasive, Seeded, Likely native_recruit, 0
# Weedy: Weedy, Desirable, 0
# PlantSource2: Recruit, Native_recruit, Introduced/Invasive, Seeded, 0
# PlotMix_Climate: None, Current, Projected, 0


## Add PlantSource & PlantSource2 columns ---------------------------------

# Because plants couldn't always be identified to the species level, H. Farrell
#   grouped them by Native_Recruited, Invasive, and Seeded.

unique(subplot$Native)
unique(subplot$SpeciesSeeded)

subplot <- subplot |>
  mutate(PlantSource = paste0(subplot$Native, "_", subplot$SpeciesSeeded))
unique(subplot$PlantSource)

# Create PlantSource column
subplot <- subplot |>
  mutate(PlantSource = case_when(
    PlantSource == "0_0" ~ "0",
    PlantSource == "Unknown_No" ~ "Unknown_recruit",
    PlantSource == "Native_No" ~ "Native_recruit",
    PlantSource == "Introduced_No" ~ "Introduced/Invasive",
    PlantSource == "Native_Yes" ~ "Seeded",
    PlantSource == "Native/Unknown_No" ~ "Likely native_recruit",
    PlantSource == "Unknown_Yes" ~ "Seeded"
  ))
unique(subplot$PlantSource)

# Create PlantSource2 column
unique(subplot$PlantSource)
subplot <- subplot |>
  mutate(PlantSource2 = case_when(
    subplot$PlantSource == "Unknown_recruit" ~ "Recruit",
    str_detect(subplot$PlantSource, "Native_recruit|Likely native_recruit") ~ "Native recruit",
    TRUE ~ subplot$PlantSource
  ))
unique(subplot$PlantSource2)


## Add Weedy column -------------------------------------------------------

# To further simplify weedy vs. desirable species

# Add Weedy column
unique(subplot$PlantSource)
subplot <- subplot |>
  mutate(Weedy = case_when(
    str_detect(subplot$PlantSource, "Unknown_recruit|Introduced/Invasive") ~ "Weedy",
    str_detect(subplot$PlantSource, "Native_recruit|Likely native_recruit|Seeded") ~ "Desirable",
    TRUE ~ subplot$PlantSource
  ))
unique(subplot$Weedy)


## Add PlotMix_Climate column ---------------------------------------------

# PlotMix names alone cannot be used to group what is climate-adapted and what is current-adapted
#   because mixes are site-specific, and some mixes that are current-adapted are climate-adapted at other sites
#   and vise versa.
# Need ot manually check each site and its seedmix to figure out which is which.

# Add PlotMix_Climate col
subplot <- subplot |>
  mutate(PlotMix_Climate = case_when(
    str_detect(subplot$Site, "Creosote|Mesquite|Patagonia|SRER") &
      subplot$PlotMix == "Medium" ~ "Current",
    str_detect(subplot$Site, "Creosote|Mesquite|Patagonia|SRER") &
      subplot$PlotMix == "Warm" ~ "Projected",
    str_detect(subplot$Site, "AguaFria|MOWE|PEFO|Spiderweb") &
      subplot$PlotMix == "Med-Warm" ~ "Current",
    str_detect(subplot$Site, "AguaFria|MOWE|PEFO|Spiderweb") &
      subplot$PlotMix == "Warm" ~ "Projected",
    str_detect(subplot$Site, "BarTBar|FlyingM|CRC|Salt_Desert") &
      subplot$PlotMix == "Cool-Med" ~ "Current",
    str_detect(subplot$Site, "BarTBar|FlyingM|CRC|Salt_Desert") &
      subplot$PlotMix == "Med-Warm" ~ "Projected",
    str_detect(subplot$Site, "BabbittPJ|UtahPJ") &
      subplot$PlotMix == "Cool" ~ "Current",
    str_detect(subplot$Site, "BabbittPJ|UtahPJ") &
      subplot$PlotMix == "Cool-Med" ~ "Projected",
    str_detect(subplot$Site, "29_Palms|AVRCD|Preserve|SCC|Roosevelt|Pleasant|TLE") &
      subplot$PlotMix == "Cool" ~ "Current",
    str_detect(subplot$Site, "29_Palms|AVRCD|Preserve|SCC|Roosevelt|Pleasant|TLE") &
      subplot$PlotMix == "Warm" ~ "Projected",
    TRUE ~ subplot$PlotMix
  ))
subplot$PlotMix_Climate <- factor(subplot$PlotMix_Climate,
  levels = c("None", "Current", "Projected")
)


# Save intermediate subplot with all correct plant species info, grouping cols added,
#   but response variables Count and Height not yet addressed
subplot3 <- subplot



# Address Count values of 0 -----------------------------------------------

# Examine rows with Count of 0
count0 <- subplot |>
  filter(Count == 0)
unique(count0$Code)
#   Code 0 should have a Count of 0 also, but non-0 Codes shouldn't exist

# Examine non-0 Codes
count0.non0code <- count0 |>
  filter(Code != "0")
#   These rows should just be removed, because we want rows only of plants that were
#     actually observed to be present.

# Remove non-0 Codes from subplot data
subplot <- subplot |>
  filter(!raw.row %in% count0.non0code$raw.row)


# Address Height values of 0 ----------------------------------------------

# Examine rows with Height of 0
height0 <- subplot |>
  filter(Height == 0)
unique(height0$Code)
#   Code 0 should have a Height of 0 also, but non-0 Codes shouldn't exist

# Examine non-0 Codes
height0.non0code <- height0 |>
  filter(Code != "0")
#   No fix needed because this is a rare occurrence and it's possible that
#     the height was so small it was essentially 0.



# Address NAs in Count ----------------------------------------------------

# Examine NAs for Count
na.count <- subplot |>
  filter(is.na(Count))
#   Most rows are for Code 0, in which case Count and Height should also be 0

# Fix Code 0s
na.count.0 <- na.count |>
  filter(Code == "0")
na.count.0.fix <- na.count.0 |>
  mutate(
    Count = 0,
    Height = 0
  )

# Replace fixed rows in subplot data for 0 Codes
subplot <- subplot |>
  filter(!raw.row %in% na.count.0.fix$raw.row) |>
  bind_rows(na.count.0.fix)


# Examine non-0 Codes
na.count.non0 <- na.count |>
  filter(Code != "0")

# OUTPUT: create list of observations with NA for Count but a non-0 Code (an actual plant)
write_csv(na.count.non0,
  file = "data/data-wrangling-intermediate/04.1a_output7_NA-count-of-non-0-Code.csv"
)

# EDITED: manually review scanned original data sheets to check where there are NAs
#   Not all data sheets were available, so some remain NAs and will have to be removed.
na.count.non0.fix <- read_xlsx("data/data-wrangling-intermediate/04.1b_edited7_NA-count-of-non-0-Code-corrected.xlsx")

# Replace fixed rows in subplot data for non-0 Codes
subplot <- subplot |>
  filter(!raw.row %in% na.count.non0.fix$raw.row) |>
  bind_rows(na.count.non0.fix)

# Remove remaining NAs for non-0 Codes that could not be fixed (scanned data sheet not available)
subplot <- subplot |>
  filter(!is.na(Count))



# Address NAs in Height ---------------------------------------------------

# Examine NAs for Height
na.height <- subplot |>
  filter(is.na(Height))

# OUTPUT: create list of observations with NA for Height and non-0 Code
write_csv(na.height,
  file = "data/data-wrangling-intermediate/04.1a_output8_NA-height-of-non-0-Code.csv"
)

# EDITED: manually review scanned original data sheets to check where there are NAs
#   Not all data sheets were available, so some remain NAs and will have to be removed.
na.height.fix <- read_xlsx("data/data-wrangling-intermediate/04.1b_edited8_NA-height-of-non-0-Code-corrected.xlsx")

# Nothing actually changed/there were no fixes to data.
#   All 57 observations have no Height recorded in scanned data sheet, but do have Count,
#   so we will not drop the rows.



# Examine completely empty plots ------------------------------------------

#  Empty subplots have Code 0, and should have only one row (one SiteDatePlotID observation).

# Empty subplots
empty.subplot <- subplot |>
  filter(Code == "0")
length(unique(empty.subplot$SiteDatePlotID)) == nrow(empty.subplot) # some subplots are not actually empty

# Examine subplots with Code 0 and multiple rows
empty.subplot.multiple.row.id <- empty.subplot |>
  count(SiteDatePlotID) |>
  filter(n > 1)
empty.subplot.multiple.row <- subplot |>
  filter(SiteDatePlotID %in% empty.subplot.multiple.row.id$SiteDatePlotID)
#   SiteDatePlotIDs 2065, 2230, and 4374 are actually empty subplots with duplicate rows
#     (two Code 0 rows).
#   SiteDatePlotID 4478 is not actually empty, even though it has a bunch of Code 0 rows
#     (idk what happened there).

# Remove duplicate Code 0 rows from subplot data
subplot <- subplot |>
  filter(!raw.row %in% c(7114, 7325, 11877, 12292, 12293, 12295, 12296))


subplot <- subplot |>
  arrange(SiteDatePlotID)


# Reorder columns ---------------------------------------------------------

subplot <- subplot |> 
  select(
    Region, Site, Date_Seeded, Date_Monitored, SiteDateID, Plot, Treatment, PlotMix, 
    PlotMix_Climate, SitePlotID, SiteDatePlotID,
    CodeOriginal, Code, Name, Native, Duration, Lifeform, SpeciesSeeded,
    PlantSource, PlantSource2, Weedy,
    Count, Height, raw.row
  )


# Write clean subplot data to csv -----------------------------------------

write_csv(subplot,
  file = "data/cleaned/04.1_subplot-data_clean.csv"
)


save.image("RData/04.1_data-wrangling_subplot.RData")
