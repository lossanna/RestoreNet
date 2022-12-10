library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

p2x2.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")
subplot.clean <- read_csv("data/cleaned/subplot-data_clean.csv")
species.in <- read_csv("data/cleaned/species-list_all_location-independent_clean.csv")
species.de <- read_csv("data/cleaned/species-list_all_location-dependent_clean.csv")
p2x2.codes.dup <- read_csv("data/raw/output-species6_2x2-codes_need-duplicate-rows.csv")
mix <- read_xlsx("data/raw/master-seed-mix.xlsx")
monitor.info <- read_csv("data/cleaned/corrected-monitoring-info_clean.csv")


# Organize columns and pivot to longer ------------------------------------

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
    str_detect(p2x2.raw$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central",
    TRUE ~ "unk"))
  
# Inspect Additional_species_in_plot cols
unique(p2x2.wide$Additional_Species_In_Plot...22)
unique(p2x2.wide$Additional_Species_In_Plot...21)
unique(p2x2.wide$Additional_Species_In_Plot...20)
unique(p2x2.wide$Additional_Species_In_Plot...19) # observations in col

# Drop empty Additional_species_in_plot cols
p2x2.wide <- p2x2.wide %>% 
  select(-Additional_Species_In_Plot...22, -Additional_Species_In_Plot...21,
         -Additional_Species_In_Plot...20)

# Change Date_Seeded to 7/18 for all of FlyingM
p2x2.wide <- p2x2.wide %>% 
  mutate(Date_Seeded = as.Date(Date_Seeded)) %>% 
  mutate(Date_Seeded = if_else(Site == "FlyingM", as.Date("2018-07-18"), Date_Seeded))

# Add subplot species observations
subplot <- subplot.clean %>% 
  select(Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix, Code, MonitorID) %>% 
  rename(subplot = Code)

p2x2.wide <- left_join(p2x2.wide, subplot) # NA codes (subplot col) created because of conflicts between subplot and 2x2 monitoring info

# Check for all cols for NAs
apply(p2x2.wide, 2, anyNA)
to.drop <- p2x2.wide %>% # remove completely empty rows (not counting raw.row, Region, or subplot cols)
  filter(is.na(Site) & is.na(Date_Seeded) & is.na(Date_Monitored) & is.na(Plot) &
         is.na(Treatment) & is.na(PlotMix))
p2x2.wide <- p2x2.wide %>% 
  filter(!raw.row %in% to.drop$raw.row)
rm(to.drop)


# Pivot species columns
p2x2.long.intermediate <- p2x2.wide %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(c(starts_with("Additional"), subplot), names_to = "source", values_to = "Code") %>% 
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
    TRUE ~ source))

# Check all cols for NAs
apply(p2x2.long.intermediate, 2, anyNA) 
  # NA codes and MonitorID created because of conflicts between subplot and 2x2 monitoring info



# Correct monitoring info -------------------------------------------------

# Complete corrected monitoring info was derived from 02.2_data-wrangling_2x2.R script
monitor.info <- read_csv("data/cleaned/corrected-monitoring-info_clean.csv")

# Check NA codes and MonitorID
  # NA are due to differences in monitoring info between 2x2 and subplot,
    # which is why no code was created after left_join()
p2x2.monitorid.na <- p2x2.long.intermediate %>% 
  filter(is.na(MonitorID))

# Extract distinct monitoring events without MonitorID
monitor.diff <- p2x2.monitorid.na %>% 
  select(Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Site)

# Write to CSV to correct manually
write_csv(monitor.diff,
          file = "data/raw/output-wrangling-2x2_1monitor-info-to-be-fixed.csv")


##### edited manually to correct monitoring info ############################
  # similar to intermediate-dependency2-edited1_conflicting-monitoring-info-resolved.xlsx,
    # but only the ones where 2x2 was wrong
monitor.fix <- read_xlsx("data/raw/edited-wrangling-2x2_1monitor-info-fixed.xlsx")

# Assign MonitorID based on fixed monitoring info
monitor.fix <- left_join(monitor.fix, monitor.info)
filter(monitor.fix, is.na(MonitorID)) # all assigned MonitorID

# Add MonitorID to monitor.diff to match with original (incorrect) values in 2x2 data
monitor.diff$MonitorID <- monitor.fix$MonitorID

# Convert monitor.diff cols to character, to be able to left_join()
monitor.diff <- monitor.diff %>% 
  mutate(across(everything(), as.character))

# Add MonitorID to p2x2 data that was missing MonitorID
p2x2.monitor.fix <- p2x2.monitorid.na %>% 
  select(-MonitorID) %>% 
  left_join(monitor.diff)
filter(p2x2.monitor.fix, is.na(MonitorID)) # all assigned MonitorID

# Convert monitor.fix cols to character, to be able to left_join()
monitor.fix <- monitor.fix %>% 
  mutate(across(everything(), as.character))

# Fix the monitoring info in p2x2 data now there is a MonitorID to join by
p2x2.monitor.fix <- p2x2.monitor.fix %>% 
  select(-Date_Seeded, -Date_Monitored, -Plot, -Treatment, -PlotMix) %>% # remove incorrect monitoring info
  left_join(monitor.fix)

# Combine fixed p2x2 data with p2x2 data already correct
p2x2.long.monitor.fixed <- p2x2.long.intermediate %>% 
  filter(!raw.row %in% p2x2.monitor.fix$raw.row) %>% # remove old incorrect rows
  bind_rows(p2x2.monitor.fix) %>% # add fixed rows
  select(Site, Region, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix,
         Code, Seeded_Cover, Total_Veg_Cover, raw.row, source, MonitorID) # reorder cols

# Convert cols back to date/numermic if needed
p2x2.long.monitor.fixed <- p2x2.long.monitor.fixed %>% 
  mutate(Date_Seeded = as.Date(Date_Seeded),
         Date_Monitored = as.Date(Date_Monitored),
         raw.row = as.numeric(raw.row),
         Plot = as.numeric(Plot),
         MonitorID = as.numeric(MonitorID))

# Check all cols for NAs
apply(p2x2.long.monitor.fixed, 2, anyNA) 
  # monitoring info is correct now, but codes still need to be corrected





# Correct codes -----------------------------------------------------------

# Workflow:
  # handle observations of additional species in plot (not subplot)
      # add extra rows for codes that need duplicate rows because codes refer to more than one species
      # make Code and CodeOriginal cols, but do not add other species info yet
  # handle observations of species in subplot
      # made Code and CodeOriginal cols, but do not add other species info yet
  # combine additional species and subplot species obs, and standardize codes



# Handle additional species obs first -------------------------------------

# Remove "0"s from additional cols
p2x2.add <- p2x2.long.intermediate %>% 
  filter(source != "subplot") %>% 
  filter(Code != "0") %>% 
  rename(CodeOriginal = Code)

# Extract codes that need duplicate rows, to add duplicates manually
p2x2.add.dup <- p2x2.add %>% 
  filter(CodeOriginal %in% p2x2.codes.dup$CodeOriginal) %>% 
  arrange(CodeOriginal)

write_csv(p2x2.add.dup,
          file = "data/raw/output-wrangling-2x2_2need-duplicate-rows.csv")

#### edited manually to add correct duplicate rows #################
p2x2.add.dup <- read_csv("data/raw/edited-wrangling-2x2_2duplicate-rows-added.csv")

p2x2.add.dup <- p2x2.add.dup %>% 
  mutate(Total_Veg_Cover = as.character(p2x2.add.dup$Total_Veg_Cover)) # correct the col class to match p2x2.long.intermediate



# Standard Code and CodeOriginal cols for non-duplicates 

# Add Code.Site to location-dependent non-duplicate rows
  # obs that need duplicate rows already have Code.Site as Code, because it was manually entered
# Remove codes for duplicate rows that were fixed separately
p2x2.add.single <- p2x2.add %>% 
  filter(!CodeOriginal %in% p2x2.add.dup$CodeOriginal) 

# Separate out location-dependent
p2x2.add.single.de <- p2x2.add.single %>% 
  filter(CodeOriginal %in% species.all.de$CodeOriginal)

# Add Code col (Code.Site) to location-dependent
p2x2.add.single.de$Code <- apply(p2x2.add.single.de[ , c("CodeOriginal", "Site")], 1, paste, collapse = ".")


# Add Code col to location-independent and combine with location-dependent
p2x2.add.single <- p2x2.add.single %>% 
  mutate(Code = p2x2.add.single$CodeOriginal) %>% # add Code col
  filter(!CodeOriginal %in% species.all.de$CodeOriginal) %>% # remove location-dependent because they have incorrect Codes
  bind_rows(p2x2.add.single.de) %>% # add location-dependent with correct codes
  select(Site, Region, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix,
         CodeOriginal, Code, Seeded_Cover, Total_Veg_Cover, raw.row, source, MonitorID) # reorder cols


# Combine all additional species obs
p2x2.add <- bind_rows(p2x2.add.dup, p2x2.add.single) %>% 
  arrange(MonitorID)

filter(p2x2.add, is.na(Code))

# p2x2.add is 2x2 plot data with correct monitoring info and codes for all observations of
  # additional species (not subplot obs) 



# Handle subplot species obs ----------------------------------------------

p2x2.subplot <- p2x2.long.intermediate %>% 
  filter(source == "subplot") 

# Separate out location-dependent
p2x2.subplot.de <- p2x2.subplot %>% 
  filter(Code %in% species.all.de$Code)

# Add CodeOriginal col to location-dependent (remove .Site from Code)
p2x2.subplot.de$CodeOriginal <- gsub("\\..*", "", p2x2.subplot.de$Code)  

# Add CodeOriginal col to location-independent and combine with location-dependent
p2x2.subplot <- p2x2.subplot %>% 
  mutate(CodeOriginal = p2x2.subplot$Code) %>% # add CodeOriginal col
  filter(!CodeOriginal %in% species.all.de$Code) %>% # remove location-dependent because they have incorrect CodeOriginal
  bind_rows(p2x2.subplot.de) %>% # add location-dependent with correct CodeOriginal
  select(Site, Region, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix,
         CodeOriginal, Code, Seeded_Cover, Total_Veg_Cover, raw.row, source, MonitorID) %>%  # reorder cols
  arrange(MonitorID)

# p2x2.subplot is 2x2 plot data with correct monitoring info and codes for all observations of
  # subplot species (not additional species in plot) 




# Combine subplot and additional and standardize codes --------------------

p2x2 <- bind_rows(p2x2.subplot, p2x2.add) %>% 
  arrange(MonitorID)

# Remove rows with NA code 
  # these were assigned NA because of conflicting monitoring info, and the correct obs have already been retained

# Standardize codes (corrections derived from 02.1_data-wrangling_subplot.R)
filter(p2x2, is.na(Code))


save.image("RData/02.2_data-wrangling_2x2.RData")
