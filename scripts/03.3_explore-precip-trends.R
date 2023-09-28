# Created: 2023-09-28
# Last updated: 2023-09-28

# Purpose: Explore precip trends


library(tidyverse)
library(readxl)

# Load data ---------------------------------------------------------------

ppt <- read_csv("data/cleaned/monitoring-events-with-PRISM-climate-data_clean.csv")
month.normal <- read_csv("data/data-wrangling-intermediate/03.2_PRISM-month-normals-all-sites.csv")
daily <- read_csv("data/data-wrangling-intermediate/03.2_PRISM-daily-all-sites.csv")
normals.include.raw <- read_xlsx("data/data-wrangling-intermediate/03.3_months-to-include-for-precip-normals-comparison.xlsx")
  
  

# Explore precip since last monitor trends --------------------------------

# With MAP as horizontal line
ppt |> 
  ggplot(aes(x = Date_Monitored, y = Since_last_precip)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  geom_hline(aes(yintercept = ppt$MAP),
             color = "blue",
             linetype = "dashed")
#   29_Palms, AVRCD, Creosote, Mesquite didn't have 3 point variation
#     (either low-high-low or high-low-high)



# Create summer/winter normals --------------------------------------------

# To better compare with precip since last monitoring event, separate half the
#   year as winter and half as summer

season.normal <- month.normal |> 
  filter(Date != "Annual") %>%
  mutate(Season = case_when(
    Date %in% c("May", "June", "July", "August", "September", "October") ~ "Summer",
    Date %in% c("November", "December", "January", "February", "March", "April") ~ "Winter")) %>%
  group_by(Site, Season) %>%
  summarize(season.precip = sum(ppt_mm),
            .groups = "keep") %>%
  pivot_wider(names_from = Season, values_from = season.precip) %>%
  ungroup()

# Add to ppt data
ppt.season <- ppt |> 
  left_join(season.normal)


# Graph
ppt |> 
  ggplot(aes(x = Date_Monitored, y = Since_last_precip)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  geom_hline(aes(yintercept = ppt.season$Summer),
             color = "red",
             linetype = "dashed") +
  geom_hline(aes(yintercept = ppt.season$Winter),
             color = "blue",
             linetype = "dashed")



# Event-specific normals comparison ---------------------------------------

# Because the time interval between monitoring events and the points are cumulative,
#   to best compare with month normals, they also must be summed over the same
#   time intervals. I made a table of which months should be included for which
#   monitoring events in Excel (normals.include.raw).

# pivot_longer()
normals.include <- normals.include.raw |> 
  select(-Month_estimate) |> 
  pivot_longer(cols = c("January", "February", "March", "April", "May",
                        "June", "July", "August", "September", "October",
                        "November", "December"),
              names_to = "Month",
              values_to = "include") |> 
    filter(!is.na(include)) |> 
  select(-include) 

# Reformat month normals df to left_join()
month.normal.add <- month.normal |> 
  filter(Date != "Annual") |> 
  rename(Month = Date)

# Add month normals
normals.include <- left_join(normals.include, month.normal.add)


# Sum normals precip
normals.ppt <- normals.include |> 
  select(-tmin, -tmax, -tmean ,-Date_Seeded, -Date_Monitored) |> 
  mutate(MonitorSiteID = as.character(MonitorSiteID)) |> 
  rename(Date_Monitored = Date_normals) |> 
  group_by(Region, Site, Date_Monitored, MonitorSiteID) |> 
  summarise(ppt = sum(ppt_mm),
            .groups = "keep") |> 
  mutate(MonitorSiteID = as.numeric(MonitorSiteID)) |> 
  mutate(source = "normals")

# Narrow down columns to add normals to Since_last_precip to graph together
normals.ppt.long <- ppt |> 
  select(Region, Site, Date_Monitored, MonitorSiteID, Since_last_precip) |> 
  rename(ppt = Since_last_precip) |> 
  mutate(source = "actual") |> 
  bind_rows(normals.ppt)

# Graph
normals.ppt.long |> 
  ggplot(aes(x = Date_Monitored, y = ppt, color = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme(legend.position = "none")

normals.ppt.long |> 
  filter(Region == "Sonoran Central") |> 
  ggplot(aes(x = Date_Monitored, y = ppt, color = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme(legend.position = "none")




# Explore cumulative precip trends ----------------------------------------

# Cumulative
ppt |> 
  ggplot(aes(x = Date_Monitored, y = Cum_precip)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) 

