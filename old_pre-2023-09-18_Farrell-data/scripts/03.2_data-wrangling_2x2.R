# Created: 2022-12-07
# Last updated: 2023-09-18

# Purpose: Create 2 clean data tables for 2x2 plot data: one with cover data and one with
#   the list of species present for each monitoring event.
#  Ensure corrected and standardized species information, and monitoring and plot information,
#   and SpeciesSeeded column based on site-specific seed mixes and plot.


library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

p2x2.raw <- read_xlsx("old_pre-2023-09-18_Farrell-data/data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")
species.in <- read_csv("old_pre-2023-09-18_Farrell-data/data/cleaned/p2x2_species-list_location-independent_clean.csv")
species.de <- read_csv("old_pre-2023-09-18_Farrell-data/data/cleaned/p2x2_species-list_location-dependent_clean.csv")
mix <- read_xlsx("old_pre-2023-09-18_Farrell-data/data/raw/from-Master_seed-mix_LO.xlsx", sheet = "with-site_R")
monitor.info <- read_csv("old_pre-2023-09-18_Farrell-data/data/cleanedS/corrected-monitoring-info_clean.csv")
monitor.wrong <- read_csv("old_pre-2023-09-18_Farrell-data/data/data-wrangling-intermediate/02_2x2-wrong-monitor-events.csv")
monitor.fixed <- read_csv("old_pre-2023-09-18_Farrell-data/data/data-wrangling-intermediate/02_2x2-wrong-monitor-events-corrected.csv")
subplot <- read_csv("old_pre-2023-09-18_Farrell-data/data/cleaned/subplot-data_clean.csv")



# Organize columns and correct monitoring info ----------------------------

# Add raw.row and Region cols
p2x2.wide <- p2x2.raw %>% 
  select(-Recorder_Initials, -`%_mulch_remaining_in_plot`, -`%_mulch_remaining_in_subplot`, -Notes) %>% 
  mutate(raw.row = 1:nrow(p2x2.raw)) %>% # row number is to be able to easily refer back to the raw data and excluded columns if needed
  rename(PlotMix = Seed_Mix) %>% 
  mutate(Region = case_when(
    str_detect(p2x2.raw$Site, c("AguaFria|BabbittPJ|MOWE|Spiderweb|BarTBar|FlyingM|PEFO|TLE")) ~ "Colorado Plateau",
    str_detect(p2x2.raw$Site, c("CRC|UtahPJ|Salt_Desert")) ~ "Utah",
    str_detect(p2x2.raw$Site, c("29_Palms|AVRCD")) ~ "Mojave",
    str_detect(p2x2.raw$Site, c("Creosote|Mesquite")) ~ "Chihuahuan",
    str_detect(p2x2.raw$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(p2x2.raw$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central"))

# Fix monitoring info
#   Separate out monitoring info
p2x2.monitor <- p2x2.wide |> 
  select(Region, Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix, raw.row) 

#   Find raw.row number of events that need to be fixed
wrong.raw.row <- left_join(monitor.wrong, p2x2.monitor) 

#   Attach raw.row to corrected monitoring info
monitor.assign <- monitor.fixed
monitor.assign$raw.row <- wrong.raw.row$raw.row

#   Separate monitor info that doesn't need to be fixed
p2x2.monitor <- p2x2.monitor |> 
  filter(!raw.row %in% monitor.assign$raw.row) |> 
  left_join(monitor.info)
p2x2.monitor |> 
  filter(is.na(MonitorID)) # all assigned MonitorID

#   Add corrected monitor info for complete list
p2x2.monitor <- bind_rows(p2x2.monitor, monitor.assign) |> 
  arrange(MonitorID)

#   Check for matching lengths
nrow(p2x2.monitor) == nrow(p2x2.raw)


# Attach correct monitoring info to p2x2 data
p2x2.wide <- p2x2.wide[ , -c(1:6)]
p2x2.wide <- left_join(p2x2.monitor, p2x2.wide)



# Create cover table ------------------------------------------------------

# Create cover table
cover <- p2x2.wide |> 
  select(-starts_with("Additional")) 


# Change Seeded_Cover and Total_Veg_Cover to numeric
#   See possible values of Seeded_Cover
unique(cover$Seeded_Cover)

#   Replace TR and 0/TR with 0.5
cover$Seeded_Cover[cover$Seeded_Cover == "TR"] <- 0.5
cover$Seeded_Cover[cover$Seeded_Cover == "0/TR"] <- 0.5

#   Replace "NA" with logical NA
cover$Seeded_Cover[cover$Seeded_Cover == "NA"] <- NA

#   Convert Seeded_Cover to numeric
cover$Seeded_Cover <- as.numeric(cover$Seeded_Cover)


#   See possible values of Total_Veg_Cover
unique(cover$Total_Veg_Cover)

#   Replace TR with 0.5
cover$Total_Veg_Cover[cover$Total_Veg_Cover == "TR"] <- 0.5

# Replace "NA" with logical NA
cover$Total_Veg_Cover[cover$Total_Veg_Cover == "NA"] <- NA

# Convert Seeded_Cover to numeric
cover$Total_Veg_Cover <- as.numeric(cover$Total_Veg_Cover)

# Add "Not_Seeded_Cover" column
cover$Not_Seeded_Cover <- cover$Total_Veg_Cover - cover$Seeded_Cover
summary(cover$Not_Seeded_Cover) # creates negative values

cover.neg.notseed <- cover |> 
  filter(Not_Seeded_Cover < 0)



# Create present_species table --------------------------------------------

# Inspect Additional_species_in_plot cols
unique(p2x2.wide$Additional_Species_In_Plot...22)
unique(p2x2.wide$Additional_Species_In_Plot...21)
unique(p2x2.wide$Additional_Species_In_Plot...20)
unique(p2x2.wide$Additional_Species_In_Plot...19) # observations in 19 and smaller

# Drop empty Additional_species_in_plot cols
p2x2.wide <- p2x2.wide %>% 
  select(-Additional_Species_In_Plot...22, -Additional_Species_In_Plot...21,
         -Additional_Species_In_Plot...20)

# Remove cover columns
present_species <- p2x2.wide |> 
  select(-Seeded_Cover, -Total_Veg_Cover)


# Pivot species columns
present_species <- present_species %>% 
  pivot_longer(c(starts_with("Additional")), names_to = "source", values_to = "CodeOriginal") %>% 
  distinct(.keep_all = TRUE) %>% 
  mutate(source = case_when(
    source == "Additional_Species_In_Plot...12" ~ "add1",
    source == "Additional_Species_In_Plot...13" ~ "add2",
    source == "Additional_Species_In_Plot...14" ~ "add3",
    source == "Additional_Species_In_Plot...15" ~ "add4",
    source == "Additional_Species_In_Plot...16" ~ "add5",
    source == "Additional_Species_In_Plot...17" ~ "add6",
    source == "Additional_Species_In_Plot...18" ~ "add7",
    source == "Additional_Species_In_Plot...19" ~ "add8",
    TRUE ~ source)) %>% 
  filter(CodeOriginal != "0")

# Check all cols for NAs
apply(present_species, 2, anyNA) 



# Attach species info to present_species table ----------------------------

#   Handle location-dependent and location-independent separately. Also handle
#     codes that need duplicate rows separately.

# Unique CodeOriginal
species.in.unique <- species.in |> 
  filter(NeedsItsDuplicate == "No")

species.de.unique <- species.de |> 
  filter(NeedsItsDuplicate == "No")


# Location independent, no duplicate
ps.in.unq <- present_species |> 
  filter(CodeOriginal %in% species.in.unique$CodeOriginal)

ps.in.unq <- left_join(ps.in.unq, species.in.unique) 


# Location dependent, no duplicate
ps.de.unq <- present_species |> 
  filter(CodeOriginal %in% species.de.unique$CodeOriginal)

ps.de.unq <- left_join(ps.de.unq, species.de.unique)



# Non-unique CodeOriginal
species.in.dup <- species.in |> 
  filter(NeedsItsDuplicate == "Yes")

species.de.dup <- species.de |> 
  filter(NeedsItsDuplicate == "Yes")


# Location independent with duplicate
ps.in.dup <- present_species |> 
  filter(CodeOriginal %in% species.in.dup$CodeOriginal)

ps.in.dup <- left_join(ps.in.dup, species.in.dup) 


# Location dependent with duplicate
ps.de.dup <- present_species |> 
  filter(CodeOriginal %in% species.de.dup$CodeOriginal)

ps.de.dup <- left_join(ps.de.dup, species.de.dup) 


# Combine
present_species <- bind_rows(ps.in.unq, ps.de.unq, ps.in.dup, ps.de.dup) |> 
  arrange(MonitorID)

ps1 <- present_species



# Create SpeciesSeeded column ---------------------------------------------

# Extract info from subplot data
seeded.subplot <- subplot |> 
  select(Region, Site, PlotMix, CodeOriginal, Code, SpeciesSeeded) |> 
  distinct(.keep_all = TRUE)


# Attach subplot info to present_species
present_species <- left_join(ps1, seeded.subplot)

# Inspect those without SpeciesSeeded assignment
ps.ss.na <- present_species |> 
  filter(is.na(SpeciesSeeded)) |> 
  select(Region, Site, PlotMix, CodeOriginal, Code, Name, SpeciesSeeded) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Code) |> 
  arrange(Site) |> 
  arrange(Region)


# Those not in seed mix were not seeded regardless of plot
ps.ss.not.in.mix <- ps.ss.na |> 
  filter(!Code %in% mix$CodeOriginal) |> 
  arrange(Code) |> 
  arrange(Site) |> 
  arrange(Region)

# Some SRER codes should be inspected manually
#   Code does not match exactly, but mentions something to species level
ps.ss.not.in.mix |> 
  filter(str_detect(Code, "BOCU|PLJA"))

ps.ss.not.in.mix <- ps.ss.not.in.mix |> 
  filter(!str_detect(Code, "BOCU|PLJA")) |> 
  mutate(SpeciesSeeded = "No") 

# Manually inspect species that exist in (a) seed mix
ps.ss.mix <- ps.ss.na |> 
  filter(!Code %in% ps.ss.not.in.mix$Code)

# OUTPUT: list of species that need SpeciesSeeded assignment
write_csv(ps.ss.mix,
          file = "old_pre-2023-09-18_Farrell-data/data/data-wrangling-intermediate/03.2a_output-species-seeded1_in-mix-need-assignment.csv")

# EDITED: manually check if the species was seeded based on site-specific plot mix
ps.ss.mix <- read_xlsx("old_pre-2023-09-18_Farrell-data/data/data-wrangling-intermediate/03.2b_edited-species-seeded1_SpeciesSeeded-in-mix-assigned.xlsx")

# Combine
ps.ss.assigned <- bind_rows(ps.ss.not.in.mix, ps.ss.mix)
nrow(ps.ss.assigned) == nrow(ps.ss.na)


# Assign SpeciesSeeded to present_species
ps.ss <- present_species |> 
  filter(is.na(SpeciesSeeded)) |> 
  left_join(ps.ss.assigned)

# Compile all with correct SeededSpecies
present_species <- present_species |> 
  filter(!is.na(SpeciesSeeded)) |> 
  bind_rows(ps.ss)

ps2 <- present_species



# Combine with subplot to get all species present -------------------------

#   Currently present_species is only the additional species not found in subplot

# Add subplot species
subplot.species <- subplot |> 
  select(-Count, -Height, -raw.row)

present_species <- ps2 |> 
  select(-raw.row, -source, -NeedsItsDuplicate, -DuplicateNum)

present_species <- bind_rows(present_species, subplot.species) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Code) |> 
  arrange(MonitorID) 
  


# Create table of species richness ----------------------------------------

#   This is the number of species present at each plot during each monitoring event, 
#     without taking the species themselves or their abundance into account.

# Some Utah plots did not have additional species in plot recorded; note this

no.add.recorded <- monitor.info |>
  mutate(Date_Monitored = as.character(Date_Monitored)) |> 
  filter(Date_Monitored %in% c("2018-11-02", "2018-11-16", "2018-11-27",
                               "2018-11-28", "2018-12-11", "2018-12-12",
                               "2019-03-22", "2019-03-29", "2019-04-05",
                               "2019-04-19", "2019-04-25", "2019-04-26",
                               "2019-05-09", "2019-05-16", "2019-05-17",
                               "2019-05-28", "2019-05-29", "2019-06-13",
                               "2019-07-01", "2019-07-02", "2019-07-24",
                               "2019-08-06", "2019-08-07")) |> 
  filter(Region == "Utah")
count(no.add.recorded, Plot) |> 
  print(n = 40) # Plot 34, Plot 39 have extra

no.add.recorded |> 
  count(Date_Monitored) |> 
  print(n = 23)

x <- no.add.recorded |> 
  filter(Date_Monitored == "2019-05-29")

count(monitor.info, Plot) |> 
  print(n = 100)
  
present_species <- present_species |> 
  mutate(Additional_species_recorded = case_when(
    Date_Monitored == no.add.recorded ~ "No additional recorded",
    TRUE ~ "Additional recorded"))

count(present_species, Additional_species_recorded)

richness <- present_species |> 
  group_by(Region, Site, Date_Monitored, Plot, Treatment, PlotMix, MonitorID)



save.image("old_pre-2023-09-18_Farrell-data/RData/02.2_data-wrangling_2x2.RData")
