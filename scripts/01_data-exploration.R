library(readxl)
library(tidyverse)
library(prism)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllSubplotData")
species.raw <- read_xlsx("data/raw/plant-species.xlsx")
mix <- read_xlsx("data/raw/seed-mix.xlsx")


# Data wrangling ----------------------------------------------------------

subplot <- subplot.raw


# Monitoring dates --------------------------------------------------------

monitor.date <- subplot %>% 
  group_by(Site, Date_Monitored) %>% 
  summarise(count = n(),
            .groups = "keep") 
monitor.date <- monitor.date %>% 
  ungroup() %>% 
  mutate(Region = case_when(
    str_detect(monitor.date$Site, c("AguaFria|BabbittPJ|MOWE|Spiderweb|BarTBar|FlyingM|PEFO|TLE")) ~ "Colorado Plateau",
    str_detect(monitor.date$Site, c("CRC|UtahPJ|Salt_Desert")) ~ "Utah",
    str_detect(monitor.date$Site, c("29_Palms|AVRCD")) ~ "Mojave",
    str_detect(monitor.date$Site, c("Creosote|Mesquite")) ~ "Chihuahuan",
    str_detect(monitor.date$Site, c("SRER|Patagonia")) ~ "Sonoran SE",
    str_detect(monitor.date$Site, c("Preserve|SCC|Roosevelt|Pleasant")) ~ "Sonoran Central",
    TRUE ~ "idk"))
filter(monitor.date, Region == "idk")

monitor.date2 <- monitor.date %>% 
  group_by(Site, Region) %>% 
  summarise(count = n(),
            .groups = "keep")


# PRISM data --------------------------------------------------------------

prism_set_dl_dir("prism-dat")







save.image("RData/01_data-exploration.RData")
