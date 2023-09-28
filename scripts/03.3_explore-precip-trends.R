# Created: 2023-09-28
# Last updated: 2023-09-28

# Purpose: Explore precip trends

library(tidyverse)

# Load data ---------------------------------------------------------------

ppt.raw <- read_csv("data/cleaned/monitoring-events-with-PRISM-climate-data_clean.csv")


# Graph -------------------------------------------------------------------

# Cumulative
ppt.raw |> 
  ggplot(aes(x = Date_Monitored, y = Cum_precip)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) 

# Since last monitoring event
ppt.raw |> 
  ggplot(aes(x = Date_Monitored, y = Since_last_precip)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  geom_hline(aes(yintercept = ppt.raw$MAP),
             color = "blue",
             linetype = "dashed")
#   29_Palms, AVRCD, Creosote, Mesquite didn't have 3 point variation
#     (either low-high-low or high-low-high)
