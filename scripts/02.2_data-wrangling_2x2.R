library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

p2x2.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")
subplot.clean <- read_csv("data/cleaned/subplot-data_clean.csv")
species.all.in <- read_csv("data/cleaned/species-list_all_location-independent.csv")
species.all.de <- read_csv("data/cleaned/species-list_all_location-dependent.csv")
p2x2.codes.dup <- read_csv("data/raw/2x2-codes_need-duplicate-rows.csv")
p2x2.codes.dup.count <- read_csv("data/raw/2x2-codes_need-duplicate-rows_count.csv")
mix <- read_xlsx("data/raw/master-seed-mix.xlsx")


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
  select(Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix, Code) %>% 
  rename(subplot = Code)

p2x2.wide <- left_join(p2x2.wide, subplot) 

# Pivot species columns
p2x2.long <- p2x2.wide %>% 
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

# Check for NA codes
filter(p2x2.long, is.na(Code))

# Monitoring events
monitor.sub <- subplot.clean %>% 
  select(Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix) %>% 
  distinct(.keep_all = TRUE) 
monitor.sub <- monitor.sub %>% 
  mutate(MonitorID = 1:nrow(monitor.sub))
monitor.sub.plots <- count(monitor.sub, Plot)

monitor.2x2 <- p2x2.wide %>% 
  select(Site, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix) %>% 
  distinct(.keep_all = TRUE) %>% 
  left_join(monitor.sub)
monitor.2x2.plots <- count(monitor.2x2, Plot)

# Examine differences
monitor.diff <- monitor.2x2 %>% 
  filter(is.na(MonitorID)) %>% 
  arrange(Site) %>% 
  mutate(across(everything(), as.character))

# AVRCD
monitor.sub.AVRCD <- monitor.sub %>% 
  filter(Site == "AVRCD")

# FlyingM
monitor.sub.FlyingM <- monitor.sub %>% 
  filter(Site == "FlyingM") %>% 
  filter(Treatment == "Pits") %>% 
  filter(PlotMix == "Med-Warm") %>% 
  filter(Plot == "36")
  
  




# Handle additional species cols first ------------------------------------

# Remove "0"s from additional cols
p2x2.add <- p2x2.long %>% 
  filter(source != "subplot") %>% 
  filter(Code != "0")

# Remove codes that need duplicate rows, to add duplicates manually
p2x2.add.dup <- p2x2.add %>% 
  filter(Code %in% p2x2.codes.dup.count$CodeOriginal)

write_csv(p2x2.add.dup,
          file = "data/raw/output-wrangling-2x2_need-duplicate-rows.csv")




# Handle subplot species obs ----------------------------------------------

p2x2.subplot <- p2x2.long %>% 
  filter(source == "subplot")

# Codes are Code.Site, not CodeOriginal

save.image("RData/02.2_data-wrangling_2x2.RData")
