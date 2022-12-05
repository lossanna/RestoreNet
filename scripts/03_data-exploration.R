library(readxl)
library(tidyverse)
library(prism)

# Load data ---------------------------------------------------------------

subplot <- read_csv("data/cleaned/subplot-data_clean.csv") 


# Monitoring dates --------------------------------------------------------

monitor.date <- subplot %>% 
  group_by(Site, Date_Monitored, Region) %>% 
  summarise(count = n(),
            .groups = "keep") # number of observations from each monitoring date at each site (includes 0s/no observation)

monitor.date2 <- monitor.date %>% 
  group_by(Site, Region) %>% 
  summarise(count = n(),
            .groups = "keep") # number of monitoring dates at each site


# PRISM data --------------------------------------------------------------

prism_set_dl_dir("prism-dat")







save.image("RData/03_data-exploration.RData")
