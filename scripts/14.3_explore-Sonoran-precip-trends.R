# Created: 2024-12-12
# Last updated: 2024-12-12

# Purpose:

library(tidyverse)
library(scales)

# Load data ---------------------------------------------------------------

since.pd <- read_csv("data/cleaned/03.3_since-last-precip_percent-deviation-from-norm_clean.csv")
sonoran.monitor <- read_csv("data/cleaned/14.2_Sonoran-Desert_monitoring-events.csv")


# Data wrangling ----------------------------------------------------------

# Separate out Sonoran sites and add seasonality
since.pd <- since.pd |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  left_join(sonoran.monitor)


# Graph Since_last --------------------------------------------------------

since.pd |> 
  ggplot(aes(x = Date_Monitored, y = Perc_deviation)) +
  geom_point(aes(color = Monitor_season, shape = Monitor_season),
             size = 2) +
  geom_line(color = "gray") +
  facet_wrap(~Site) +
  xlab(NULL) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")


