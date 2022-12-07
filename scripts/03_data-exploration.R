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



# Native vs Introduced density at sites -----------------------------------

# Sum seedling native/introduced density for each (36) subplot 
native.density <- subplot %>% 
  filter(Native %in% c("Native", "Native/Unknown")) %>% 
  group_by(Region, Site, Date_Monitored, Plot, Treatment, PlotMix) %>% 
  summarise(sum.count = sum(Count),
            .groups = "keep")

introduced.density <- subplot %>% 
  filter(Native == "Introduced") %>% 
  group_by(Region, Site, Date_Monitored, Plot, Treatment, PlotMix) %>% 
  summarise(sum.count = sum(Count),
            .groups = "keep")


# Average across PlotMix to separate seeded vs unseeded subplots
unique(native.density$PlotMix)
seeded.mixes <- c("Medium", "Warm", "Med-Warm", "Cool", "Cool-Med")
native.density.seed <- native.density %>% 
  mutate(plotmix.seed = if_else(PlotMix %in% seeded.mixes, "seeded", "unseeded")) %>% 
  group_by(Region, Site, Date_Monitored, plotmix.seed) %>% 
  summarise(avg = mean(sum.count),
            .groups = "keep")
native.density.seed$Native <- rep("native", nrow(native.density.seed))


intro.density.seed <- introduced.density %>% 
  mutate(plotmix.seed = if_else(PlotMix %in% seeded.mixes, "seeded", "unseeded")) %>% 
  group_by(Region, Site, Date_Monitored, plotmix.seed) %>% 
  summarise(avg = mean(sum.count),
            .groups = "keep")
intro.density.seed$Native <- rep("introduced", nrow(intro.density.seed))

density.seed <- bind_rows(native.density.seed, intro.density.seed) 
density.seed$seed.native <- apply(density.seed[ , c("plotmix.seed", "Native")], 1, paste, collapse = ", ")


# Plot all sites
density.seed %>% 
  ggplot(aes(x = Date_Monitored, y = avg, group = seed.native,
             color = seed.native)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Site) +
  scale_color_brewer(palette = "Paired")

# All sites, minus Roosevelt
density.seed %>% 
  filter(Site != "Roosevelt") %>% 
  ggplot(aes(x = Date_Monitored, y = avg, group = seed.native,
             color = seed.native)) +
  geom_line() +
  geom_point(aes(shape = Native)) +
  facet_wrap(~Site) +
  scale_color_brewer(palette = "Paired")
  
# Single site
density.seed %>% 
  filter(Site == "FlyingM") %>% 
  ggplot(aes(x = Date_Monitored, y = avg, group = seed.native,
             color = seed.native)) + 
  geom_point(aes(shape = Native)) +
  geom_line() +
  scale_color_brewer(palette = "Paired")

density.seed %>% 
  filter(Site == "BarTBar") %>% 
  ggplot(aes(x = Date_Monitored, y = avg, group = seed.native,
             color = seed.native)) + 
  geom_point(aes(shape = Native)) +
  geom_line() +
  scale_color_brewer(palette = "Paired")

density.seed %>% 
  filter(Site == "SRER") %>% 
  ggplot(aes(x = Date_Monitored, y = avg, group = seed.native,
             color = seed.native)) +
  geom_point(aes(shape = Native)) +
  geom_line() +
  scale_color_brewer(palette = "Paired")

density.seed %>% 
  filter(Site == "Patagonia") %>% 
  ggplot(aes(x = Date_Monitored, y = avg, group = seed.native,
             color = seed.native)) +
  geom_point(aes(shape = Native)) +
  geom_line() +
  scale_color_brewer(palette = "Paired")




# PRISM data --------------------------------------------------------------

prism_set_dl_dir("data/prism-dat")

min(subplot$Date_Seeded)
max(subplot$Date_Monitored)

# Downloaded the following data from PRISM, but used FTP, as that is much faster
  # actually running this code would take forever

# get_prism_dailys(
#   type = "ppt",
#   minDate = "2018-01-01",
#   maxDate = "2021-12-31",
#   keepZip = FALSE
# )

# get_prism_normals(
#   type = "ppt",
#   resolution = "4km",
#   annual = TRUE,
#   keepZip = FALSE
# )




save.image("RData/03_data-exploration.RData")
