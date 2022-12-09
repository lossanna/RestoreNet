library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

p2x2.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")
subplot.clean <- read_csv("data/cleaned/subplot-data_clean.csv")
subo <- read_csv("data/raw/intermediate-dependency2_subplot-data_original-monitoring-info.csv")
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
monitor.diff <-p2x2.monitorid.na %>% 
  select(Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Site)

# Write to CSV to correct manually
write_csv(monitor.diff,
          file = "data/raw/output-wrangling-2x2_monitor-info-to-be-fixed.csv")


##### edited manually to correct monitoring info ############################
  # similar to intermediate-dependency2-edited1_conflicting-monitoring-info-resolved.xlsx,
    # but only the ones where 2x2 was wrong
monitor.fix <- read_xlsx("data/raw/edited-wrangling-2x2_monitor-info-fixed.xlsx")








# Add MonitorID to monitor.diff to match with original (incorrect) values in 2x2 data
monitor.diff$MonitorID <- monitor.fix$MonitorID

# Add original values with MonitorID to already correct values with MonitorID for full list
# needed for a succesful left_join()
monitor.assign <- monitor.2x2 %>% 
  filter(!is.na(MonitorID)) %>% 
  bind_rows(monitor.diff)

# Add MonitorID to 2x2 data
p2x2.long.intermediate <- left_join(p2x2.long.intermediate, monitor.assign)

# Remove monitoring info from 2x2 data because some of it is wrong
p2x2.long.intermediate <- p2x2.long.intermediate %>% 
  select(-Date_Seeded, -Date_Monitored, -Plot, -Treatment, -PlotMix)

# Add corrected monitoring info with left_join()
p2x2.long.intermediate <- left_join(p2x2.long.intermediate, monitor.info)
p2x2.long.intermediate <- p2x2.long.intermediate %>% 
  select(Site, Region, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix,
         Code, Seeded_Cover, Total_Veg_Cover, raw.row, source, MonitorID) %>% # reorder cols
  mutate(raw.row = as.numeric(p2x2.long.intermediate$raw.row)) # convert raw.row back to numeric so all cols are of correct class

# Check all cols for NAs
apply(p2x2.long.intermediate, 2, anyNA) 
# monitoring info is correct now, but codes still need to be corrected
















# Assign monitoring events an ID based on subplot monitoring info
  # use subplot monitoring info because some monitoring events were not recorded in 2x2 data
monitor.sub <- subo %>% 
  select(Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix) %>% 
  distinct(.keep_all = TRUE) 
monitor.sub <- monitor.sub %>% 
  mutate(across(everything(), as.character)) %>% 
  mutate(MonitorID = 1:nrow(monitor.sub))

# Add monitoring IDs to 2x2 plot monitoring information
monitor.2x2 <- p2x2.wide %>% 
  mutate(across(everything(), as.character)) %>% 
  select(Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix) %>% 
  distinct(.keep_all = TRUE) %>% 
  left_join(monitor.sub)

# Examine differences by looking at NAs
monitor.diff <- monitor.2x2 %>% 
  filter(is.na(MonitorID)) %>% 
  arrange(Site) 


# Manually inspect differences by site
  # and write df of corresponding rows using subplot monitoring info
# AVRCD 
monitor.sub.AVRCD <- monitor.sub %>% 
  filter(Site == "AVRCD") %>% 
  filter(Date_Monitored == "2021-04-06")
monitor.sub.AVRCD <- monitor.sub %>% 
  filter(Site == "AVRCD", Date_Monitored == "2020-04-30", Plot == "14") %>% 
  bind_rows(monitor.sub.AVRCD)

# FlyingM
monitor.sub.FlyingM <- monitor.sub %>% 
  filter(Site == "FlyingM") %>% 
  filter(Treatment == "Pits") %>% 
  filter(PlotMix == "Med-Warm") %>% 
  filter(Plot == "36") %>% 
  filter(Date_Monitored == "2019-06-12")

# Mesquite
monitor.sub.Mesquite <- monitor.sub %>% 
  filter(Site == "Mesquite") %>% 
  filter(Date_Monitored == "2020-12-12") %>% 
  bind_rows(filter(monitor.sub,
                   Site == "Mesquite",
                   Date_Monitored == "2021-10-10",
                   Plot %in% c("233", "234", "235")))

# Patagonia
monitor.sub.Patagonia <- monitor.sub %>% 
  filter(Site == "Patagonia") %>% 
  filter(Date_Monitored == "2021-03-12") %>% 
  filter(Plot == "33")  

# Pleasant  
monitor.sub.Pleasant <- monitor.sub %>% 
  filter(Site == "Pleasant") %>% 
  filter(Date_Monitored %in% c("2021-04-02", "2021-10-04")) %>% 
  filter(Plot %in% c("4", "8", "12", "15", "22", "23", "30", "32"))

# Preserve
monitor.sub.Preserve <- monitor.sub %>% 
  filter(Site == "Preserve") %>% 
  filter(Date_Monitored == "2020-03-26") %>% 
  bind_rows(filter(monitor.sub,
                   Site == "Preserve",
                   Date_Monitored == "2021-03-30",
                   Plot %in% c("2", "6", "10", "11", "15", "21", "23", "32"))) %>% 
  bind_rows(filter(monitor.sub,
                   Site == "Preserve",
                   Date_Monitored == "2021-10-06",
                   Plot == "11"))

# Roosevelt
monitor.sub.Roosevelt <- monitor.sub %>% 
  filter(Site == "Roosevelt") %>% 
  filter(Date_Monitored %in% c("2020-03-25")) %>% 
  bind_rows(filter(monitor.sub,
                   Site == "Roosevelt",
                   Date_Monitored == "2021-04-01",
                   Plot %in% c("2", "7", "11", "16", "20", "30", "34", "36"))) %>% 
  bind_rows(filter(monitor.sub,
                   Site == "Roosevelt",
                   Date_Monitored == "2021-10-08",
                   Plot %in% c("2", "16", "30", "34")))

# Salt Desert
monitor.sub.SaltDesert <- monitor.sub %>% 
  filter(Site == "Salt_Desert") %>% 
  filter(Date_Monitored == "2019-03-29") %>% 
  filter(Plot == "32") %>% 
  filter(Treatment == "Pits")

# SCC
monitor.sub.SCC <- monitor.sub %>% 
  filter(Site == "SCC") %>% 
  filter(Date_Monitored == "2020-03-27") %>% 
  bind_rows(filter(monitor.sub,
                   Site == "SCC",
                   Date_Monitored == "2021-03-31",
                   Plot %in% c("4", "8", "13", "16", "22", "25", "32", "35"))) %>% 
  bind_rows(filter(monitor.sub,
                   Site == "SCC",
                   Date_Monitored == "2021-10-13",
                   Plot %in% c("16", "25", "35")))

# Combing monitoring info for all sites
monitor.sub.all <- bind_rows(monitor.sub.AVRCD,
                             monitor.sub.FlyingM,
                             monitor.sub.Mesquite,
                             monitor.sub.Patagonia,
                             monitor.sub.Pleasant,
                             monitor.sub.Preserve,
                             monitor.sub.Roosevelt,
                             monitor.sub.SaltDesert,
                             monitor.sub.SCC)

nrow(monitor.sub.all) == nrow(monitor.diff)

# Combine subplot codes and 2x2 codes for comparison
monitor.fix <- monitor.diff %>% 
  select(-Site, -MonitorID) %>% 
  rename(Date_Seeded_2x2 = Date_Seeded,
         Date_Monitored_2x2 = Date_Monitored,
         Plot_2x2 = Plot,
         Treatment_2x2 = Treatment,
         PlotMix_2x2 = PlotMix)
monitor.fix <- bind_cols(monitor.sub.all, monitor.fix)
monitor.fix <- monitor.fix %>% 
  select(Site, Date_Seeded, Date_Seeded_2x2, Date_Monitored, Date_Monitored_2x2,
         Plot, Plot_2x2, Treatment, Treatment_2x2, PlotMix, PlotMix_2x2, MonitorID)

# Write to csv
write_csv(monitor.fix,
          file = "data/raw/output-wrangling-2x2_1conflicting-monitoring-info.csv")


#### edited manually to include correct monitoring info only #########
monitor.fix <- read_xlsx("data/raw/edited-wrangling-2x2_1conflicting-monitoring-info-resolved.xlsx",
                         sheet = "corrected")

# Compile a complete corrected list of monitoring info
monitor.info <- monitor.sub %>% 
  mutate(Date_Seeded = as.Date(monitor.sub$Date_Seeded),
         Date_Monitored = as.Date(monitor.sub$Date_Monitored),
         Plot = as.numeric(monitor.sub$Plot)) %>% 
  filter(!MonitorID %in% monitor.fix$MonitorID) %>% 
  bind_rows(monitor.fix) %>% 
  arrange(MonitorID)

# Write to CSV
write_csv(monitor.info,
          file = "data/cleaned/corrected-monitoring-info_clean.csv")


# Add MonitorID to monitor.diff to match with original (incorrect) values in 2x2 data
monitor.diff$MonitorID <- monitor.fix$MonitorID

# Add original values with MonitorID to already correct values with MonitorID for full list
  # needed for a succesful left_join()
monitor.assign <- monitor.2x2 %>% 
  filter(!is.na(MonitorID)) %>% 
  bind_rows(monitor.diff)

# Add MonitorID to 2x2 data
p2x2.long.intermediate <- left_join(p2x2.long.intermediate, monitor.assign)

# Remove monitoring info from 2x2 data because some of it is wrong
p2x2.long.intermediate <- p2x2.long.intermediate %>% 
  select(-Date_Seeded, -Date_Monitored, -Plot, -Treatment, -PlotMix)

# Add corrected monitoring info with left_join()
p2x2.long.intermediate <- left_join(p2x2.long.intermediate, monitor.info)
p2x2.long.intermediate <- p2x2.long.intermediate %>% 
  select(Site, Region, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix,
         Code, Seeded_Cover, Total_Veg_Cover, raw.row, source, MonitorID) %>% # reorder cols
  mutate(raw.row = as.numeric(p2x2.long.intermediate$raw.row)) # convert raw.row back to numeric so all cols are of correct class

# Check all cols for NAs
apply(p2x2.long.intermediate, 2, anyNA) 
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
