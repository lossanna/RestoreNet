# This script is to produce an intermediate depedency table with correct monitoring info. In comparing
  # the monitoring information from the subplot vs. 2x2 plot data, there were descrepancies.
  # However, one version is not completely right, so we must compare differences and figure out the 
  # correct values.

# "Monitoring info" refers to columns Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix.
  # A unique ID for each plot monitored at each time point, without taking into account
    # any actual data collection (species present, species measurements).

library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

p2x2.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")
subplot.original.monitor.info <- read_csv("data/raw/intermediate-dependency2_subplot-data_original-monitoring-info.csv")



# Set up 2x2 plot data ----------------------------------------------------

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
subplot <- subplot.original.monitor.info %>% 
  select(Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix, Code) %>% 
  rename(subplot = Code)

p2x2.wide <- left_join(p2x2.wide, subplot) # left_join() creates NA codes because of conflicting monitoring info

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


# Correct monitoring info -------------------------------------------------

# Check for NA codes
  # NA are due to differences in monitoring info between 2x2 and subplot,
  # which is why no code was created after left_join()
p2x2.long.na.code <- p2x2.long.intermediate %>% 
  filter(is.na(Code))


# Assign monitoring events an ID based on subplot monitoring info
  # use subplot monitoring info because some monitoring events were not recorded in 2x2 data
monitor.sub <- subplot %>% 
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
          file = "data/raw/intermediate-dependency2-output1_conflicting-monitoring-info.csv")


#### edited manually to include correct monitoring info only #########
monitor.fix <- read_xlsx("data/raw/intermediate-dependency2-edited1_conflicting-monitoring-info-resolved.xlsx",
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