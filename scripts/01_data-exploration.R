library(readxl)
library(tidyverse)
library(prism)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllSubplotData")



# Data wrangling ----------------------------------------------------------

subplot <- subplot.raw


# Monitoring dates --------------------------------------------------------

monitor.date <- subplot %>% 
  group_by(Site, Date_Monitored) %>% 
  summarise(count = n(),
            .groups = "keep") 
monitor.date <- monitor.date %>% 
  ungroup() %>% 
  mutate(site_group = case_when(
    str_detect(monitor.date$Site, c("AguaFria|BabbittPJ|MOWE|Spiderweb|BarTBar|FlyingM|PEFO|TLE")) ~ "CO",
    str_detect(monitor.date$Site, c("CRC|UtahPJ|Salt_Desert")) ~ "UT",
    str_detect(monitor.date$Site, c("29_Palms|AVRCD")) ~ "MO",
    str_detect(monitor.date$Site, c("Creosote|Mesquite")) ~ "CHI",
    str_detect(monitor.date$Site, c("SRER|Patagonia")) ~ "SOse",
    str_detect(monitor.date$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "SOce",
    TRUE ~ "idk"))

monitor.date2 <- monitor.date %>% 
  group_by(Site, site_group) %>% 
  summarise(count = n(),
            .groups = "keep")


# PRISM data --------------------------------------------------------------

prism_set_dl_dir("prism-dat")


save.image("RData/01_data-exploration.RData")
