# This script is to produce an intermediate depedency table with correct monitoring info. In comparing
  # the monitoring information from the subplot vs. 2x2 plot data, there were descrepancies.
  # However, one version is not completely right, so we must compare differences and figure out the 
  # correct values.

# "Monitoring info" refers to columns Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix.
  # A unique ID for each plot monitored at each time point, without taking into account
    # any actual data collection (species present, species measurements).

library(tidyverse)

# Load data ---------------------------------------------------------------

p2x2.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")





# Set up 2x2 plot data ----------------------------------------------------

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
subplot <- subplot.original.monitor.info %>% 
  select(Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix, Code) %>% 
  rename(subplot = Code)

p2x2.wide <- left_join(p2x2.wide, subplot) 

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

