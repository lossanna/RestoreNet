# Created: 2023-09-28
# Last updated: 2023-10-20

# Purpose: Explore precip trends. Compare actual precip values (from PRISM daily values, see 03.2.R)
#   with 30-year normals. Actual precip is recorded as either cumulative precip
#   since seeding, or cumulative precip since the previous monitoring event. Because the time
#   interval between monitoring events and seeding differs by site, and because normals are
#   site- and and month-specific, each site must be dealt with on an individual basis 
#   to determine which normals months should be included for a (roughly equal) comparison.

# Normals must be "rounded" to the start of the month, the end of the month,
#   or mid-month, because normals are reported as annual or monthly. Hence, while the actual 
#   precip experienced is defined to the day, comparable normals are defined to the
#   half month. This also assumes that an even amount of rain falls in the first and
#   second half of the month, and sometimes for sites monitored every 2 weeks the dates round
#   such that there is assumed to be no interval, and therefore no precip (for normals). 
#   See 03.3.xlsx README tab for more explanation.


library(tidyverse)
library(readxl)
library(scales)

# Load data ---------------------------------------------------------------

ppt <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
month.normal <- read_csv("data/cleaned/03.2_PRISM-month-normals-all-sites_clean.csv")
daily <- read_csv("data/cleaned/03.2_PRISM-daily-all-sites_clean.csv")
normals.since.raw <- read_xlsx("data/data-wrangling-intermediate/03.3_months-to-include-for-precip-normals-comparison.xlsx",
                                       sheet = "since_last")
normals.cum.raw <- read_xlsx("data/data-wrangling-intermediate/03.3_months-to-include-for-precip-normals-comparison.xlsx",
                                       sheet = "cum")
  


# Explore monthly normals -------------------------------------------------

# Order months
month.normal.graph <- month.normal |> 
  filter(Date != "Annual") |> 
  mutate(Date = factor(Date, levels = c("January", "February", "March", "April", "May",
                                        "June", "July", "August", "September", "October",
                                        "November", "December")))

# Chihuahuan
#   Unimodal: most rain in July-Sept
month.normal.graph |> 
  filter(Region == "Chihuahuan") |> 
  ggplot(aes(x = Date, y = ppt_mm, color = Site, group = Site)) +
  geom_point() +
  geom_line() +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

# Colorado Plateau
#   Biomodal, July-Sept and Dec-March (summer rains slightly greater than winter)
month.normal.graph |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Date, y = ppt_mm, color = Site, group = Site)) +
  geom_point() +
  geom_line() +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

# Mojave
#   Unimodal: most rain in Dec-March
month.normal.graph |> 
  filter(Region == "Mojave") |> 
  ggplot(aes(x = Date, y = ppt_mm, color = Site, group = Site)) +
  geom_point() +
  geom_line() +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

# Sonoran Central
#   Bimodal: July-Sept, Dec-March (summer and winter rains roughly equal)
month.normal.graph |> 
  filter(Region == "Sonoran Central") |> 
  ggplot(aes(x = Date, y = ppt_mm, color = Site, group = Site)) +
  geom_point() +
  geom_line() +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

# Sonoran SE
#   Slightly bimodal: most rain July-Sept, some rain Dec-March
month.normal.graph |> 
  filter(Region == "Sonoran SE") |> 
  ggplot(aes(x = Date, y = ppt_mm, color = Site, group = Site)) +
  geom_point() +
  geom_line() +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

# Utah
#   CRC & Salt Desert: Biomodal: July-Oct, March-May
#   UtahPJ: Biomdal: July-Oct, Dec-Feb
month.normal.graph |> 
  filter(Region == "Utah") |> 
  ggplot(aes(x = Date, y = ppt_mm, color = Site, group = Site)) +
  geom_point() +
  geom_line() +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom")



# Precip since last monitoring event --------------------------------------


# Since_last data wrangling -----------------------------------------------

# pivot_longer() and convert NAs to 0 (blank cells are 0)
normals.since <- normals.since.raw |> 
  select(-Seed_month_estimate, -Monitor_month_estimate) |> 
  pivot_longer(cols = c("Annual", "January", "February", "March", "April", "May",
                        "June", "July", "August", "September", "October",
                        "November", "December"),
              names_to = "Date",
              values_to = "include") 
normals.since$include[is.na(normals.since$include)] <- 0

# Add normals values
normals.since <- normals.since |> 
  left_join(month.normal) |> 
  select(-tmin, -tmax, -tmean) 

# Multiply include * ppt_mm for new ppt_mm value
#   "include" column is the amount of times that month/annual should be included
normals.since <- normals.since |> 
  mutate(ppt_mm = include * ppt_mm) |> 
  select(-include)

# Sum normals precip
normals.since <- normals.since |> 
  select(-Date_Seeded, -Date_Monitored) |> 
  mutate(SiteDateID = as.character(SiteDateID)) |> 
  group_by(Region, Site, SiteDateID, Seed_estimate, Monitor_estimate) |> 
  summarise(ppt_mm = sum(ppt_mm),
            .groups = "keep") |> 
  mutate(SiteDateID = as.numeric(SiteDateID)) 

# Add normals to actual precip values (Since_last_precip from ppt) to graph together
#   Rename normals columns to match and add "source" col to note they are normals
normals.since.bind <- normals.since |> 
  rename(Date_Seeded = Seed_estimate,
         Date_Monitored = Monitor_estimate) |> 
  mutate(source = "normals")
since.long <- ppt |> 
  select(Region, Site, Date_Seeded, Date_Monitored, SiteDateID, Since_last_precip) |> 
  rename(ppt_mm = Since_last_precip) |> 
  mutate(source = "actual") |> 
  bind_rows(normals.since.bind)

# Find percent change (deviation from normals)
since.pc <- ppt |> 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Since_last_precip) |> 
  left_join(normals.since) |> 
  mutate(perc_change = (Since_last_precip - ppt_mm) / ppt_mm)



# Since_last graph --------------------------------------------------------

# All sites
since.long |> 
  ggplot(aes(x = Date_Monitored, y = ppt_mm, color = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom") 

since.pc |> 
  filter(perc_change != Inf) |> 
  ggplot(aes(x = Date_Monitored, y = perc_change)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent)

since.pc |> 
  filter(perc_change != Inf,
         Region !="Mojave") |> 
  ggplot(aes(x = Date_Monitored, y = perc_change)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")


  
# By Region
# Sonoran Central
since.long |> 
  filter(Region == "Sonoran Central") |> 
  ggplot(aes(x = Date_Monitored, y = ppt_mm, color = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom") 

since.pc |> 
  filter(perc_change != Inf) |> 
  filter(Region == "Sonoran Central") |> 
  ggplot(aes(x = Date_Monitored, y = perc_change)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")

# Sonoran SE
since.long |> 
  filter(Region == "Sonoran SE") |> 
  ggplot(aes(x = Date_Monitored, y = ppt_mm, color = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom") 

since.pc |> 
  filter(perc_change != Inf) |> 
  filter(Region == "Sonoran SE") |> 
  ggplot(aes(x = Date_Monitored, y = perc_change)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")

# Chihuahuan
since.long |> 
  filter(Region == "Chihuahuan") |> 
  ggplot(aes(x = Date_Monitored, y = ppt_mm, color = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

since.pc |> 
  filter(perc_change != Inf) |> 
  filter(Region == "Chihuahuan") |> 
  ggplot(aes(x = Date_Monitored, y = perc_change)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")

# Mojave
since.long |> 
  filter(Region == "Mojave") |> 
  ggplot(aes(x = Date_Monitored, y = ppt_mm, color = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom") 

since.pc |> 
  filter(perc_change != Inf) |> 
  filter(Region == "Mojave") |> 
  ggplot(aes(x = Date_Monitored, y = perc_change)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")

# Utah
since.long |> 
  filter(Region == "Utah") |> 
  ggplot(aes(x = Date_Monitored, y = ppt_mm, color = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom") 

since.pc |> 
  filter(perc_change != Inf) |> 
  filter(Region == "Utah") |> 
  ggplot(aes(x = Date_Monitored, y = perc_change)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")

# Colorado Plateau
since.long |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Date_Monitored, y = ppt_mm, color = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom") 

since.pc |> 
  filter(perc_change != Inf) |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Date_Monitored, y = perc_change)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")

since.pc |> 
  filter(perc_change != Inf,
         Site != "MOWE") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Date_Monitored, y = perc_change)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")





# Cumulative precip since seeding -----------------------------------------


# Cumulative data wrangling -----------------------------------------------

# pivot_longer() and convert NAs to 0 (blank cells are 0)
normals.cum <- normals.cum.raw |> 
  select(-Seed_month_estimate, -Monitor_month_estimate) |> 
  pivot_longer(cols = c("Annual", "January", "February", "March", "April", "May",
                        "June", "July", "August", "September", "October",
                        "November", "December"),
               names_to = "Date",
               values_to = "include") 
normals.cum$include[is.na(normals.cum$include)] <- 0

# Add normals values
normals.cum <- normals.cum |> 
  left_join(month.normal) |> 
  select(-tmin, -tmax, -tmean) 

# Multiply include * ppt_mm for new ppt_mm value
#   "include" column is the amount of times that month/annual should be included
normals.cum <- normals.cum |> 
  mutate(ppt_mm = include * ppt_mm) |> 
  select(-include)

# Sum normals precip
normals.cum <- normals.cum |> 
  select(-Date_Seeded, -Date_Monitored) |> 
  mutate(SiteDateID = as.character(SiteDateID)) |> 
  group_by(Region, Site, SiteDateID, Seed_estimate, Monitor_estimate) |> 
  summarise(ppt_mm = sum(ppt_mm),
            .groups = "keep") |> 
  mutate(SiteDateID = as.numeric(SiteDateID)) 

# Add normals to actual precip values (Cum_precip from ppt) to graph together
#   Rename normals columns to match and add "source" col to note they are normals
normals.cum.bind <- normals.cum |> 
  rename(Date_Seeded = Seed_estimate,
         Date_Monitored = Monitor_estimate) |> 
  mutate(source = "normals")
cum.long <- ppt |> 
  select(Region, Site, Date_Seeded, Date_Monitored, SiteDateID, Cum_precip) |> 
  rename(ppt_mm = Cum_precip) |> 
  mutate(source = "actual") |> 
  bind_rows(normals.cum.bind)

# Find percent change (deviation from normals)
cum.pc <- ppt |> 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Cum_precip) |> 
  left_join(normals.cum) |> 
  mutate(perc_change = (Cum_precip - ppt_mm) / ppt_mm)




# Cumulative graph --------------------------------------------------------

# All sites
cum.long |> 
  ggplot(aes(x = Date_Monitored, y = ppt_mm, color = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom") 

cum.pc |> 
  filter(perc_change != Inf) |> 
  ggplot(aes(x = Date_Monitored, y = perc_change)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent)

cum.pc |> 
  filter(perc_change != Inf,
         Region !="Mojave") |> 
  ggplot(aes(x = Date_Monitored, y = perc_change)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")



# By Region
# Sonoran Central
cum.long |> 
  filter(Region == "Sonoran Central") |> 
  ggplot(aes(x = Date_Monitored, y = ppt_mm, color = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom") 

cum.pc |> 
  filter(perc_change != Inf) |> 
  filter(Region == "Sonoran Central") |> 
  ggplot(aes(x = Date_Monitored, y = perc_change)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")

# Sonoran SE
cum.long |> 
  filter(Region == "Sonoran SE") |> 
  ggplot(aes(x = Date_Monitored, y = ppt_mm, color = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom") 

cum.pc |> 
  filter(perc_change != Inf) |> 
  filter(Region == "Sonoran SE") |> 
  ggplot(aes(x = Date_Monitored, y = perc_change)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")

# Chihuahuan
cum.long |> 
  filter(Region == "Chihuahuan") |> 
  ggplot(aes(x = Date_Monitored, y = ppt_mm, color = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

cum.pc |> 
  filter(perc_change != Inf) |> 
  filter(Region == "Chihuahuan") |> 
  ggplot(aes(x = Date_Monitored, y = perc_change)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")

# Mojave
cum.long |> 
  filter(Region == "Mojave") |> 
  ggplot(aes(x = Date_Monitored, y = ppt_mm, color = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom") 

cum.pc |> 
  filter(perc_change != Inf) |> 
  filter(Region == "Mojave") |> 
  ggplot(aes(x = Date_Monitored, y = perc_change)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")

# Utah
cum.long |> 
  filter(Region == "Utah") |> 
  ggplot(aes(x = Date_Monitored, y = ppt_mm, color = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom") 

cum.pc |> 
  filter(perc_change != Inf) |> 
  filter(Region == "Utah") |> 
  ggplot(aes(x = Date_Monitored, y = perc_change)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")

# Colorado Plateau
cum.long |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Date_Monitored, y = ppt_mm, color = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom") 

cum.pc |> 
  filter(perc_change != Inf) |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Date_Monitored, y = perc_change)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")



save.image("RData/03.3_explore-precip-trends.RData")
