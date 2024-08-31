# Created: 2024-08-31
# Last updated: 2024-08-31

# Purpose: Examine linear relationship between Perc_dev_cum_abs and Count & Height.
#   Who knows what is happening

library(tidyverse)
library(scales)
library(viridis)
library(ggbreak)
library(ggpmisc)

# Load data ---------------------------------------------------------------

subplot <- read_csv("data/cleaned/04.1_subplot-data_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")


# Data wrangling ----------------------------------------------------------

# Reorganize columns for left_join()
cum.pd.subplot <- cum.pd |> 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Perc_deviation, Deviation_mm) |> 
  rename(Perc_dev_cum = Perc_deviation,
         Dev_mm_cum = Deviation_mm)

# Combine all variables
dat <- subplot |> 
  left_join(prism.data) |> 
  left_join(ai) |> 
  left_join(cum.pd.subplot)

# Data without Infinity
dat <- dat |> 
  filter(Perc_dev_cum != Inf)

# Add Perc_dev_cum_abs
dat <- dat |> 
  mutate(Perc_dev_cum_abs = abs(Perc_dev_cum))



# Count -------------------------------------------------------------------

## All sites --------------------------------------------------------------

### Desirable -------------------------------------------------------------

# Linear regression
dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Perc_dev_cum_abs < 8) |> 
  ggplot(aes(x = Perc_dev_cum_abs, y = Count)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  ggtitle("All sites, desirable species") +
  theme_minimal() # lol the R^2 is terrible

#   Without x & y outliers
dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Perc_dev_cum_abs < 1.5) |> 
  filter(Count < 150)|> 
  ggplot(aes(x = Perc_dev_cum_abs, y = Count)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  ggtitle("All sites, desirable species") +
  theme_minimal() # lol the R^2 is still terrible


### Weedy -----------------------------------------------------------------

# Linear regression
dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Perc_dev_cum_abs < 8) |> 
  ggplot(aes(x = Perc_dev_cum_abs, y = Count)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  ggtitle("All sites, weedy species") +
  theme_minimal() # lol the R^2 is terrible

#   Without x & y outliers
dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Perc_dev_cum_abs < 1.5) |> 
  filter(Count < 150)|> 
  ggplot(aes(x = Perc_dev_cum_abs, y = Count)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  ggtitle("All sites, weedy species") +
  theme_minimal() # lol the R^2 is still terrible



## Sonoran Desert ---------------------------------------------------------

### Desirable -------------------------------------------------------------

# Linear regression
dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum_abs, y = Count)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal() # lol the R^2 is terrible

# Quadratic
dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum_abs, y = Count)) +
  stat_poly_line(formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), use_label(c("eq", "R2"))) +
  geom_point() +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal() # idk what we're even doing at this point

# Cubic
dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum_abs, y = Count)) +
  stat_poly_line(formula = y ~ poly(x, 3, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 3, raw = TRUE), use_label(c("eq", "R2"))) +
  geom_point() +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal()

# Smooth
dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum_abs, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Sonoran Desert, desirable species") +
  theme_minimal()


### Weedy -----------------------------------------------------------------

# Linear regression
dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum_abs, y = Count)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  ggtitle("Sonoran Desert, weedy species") +
  theme_minimal() # R^2 is pretty rough

# Cubic
dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum_abs, y = Count)) +
  stat_poly_line(formula = y ~ poly(x, 3, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 3, raw = TRUE), use_label(c("eq", "R2"))) +
  geom_point() +
  ggtitle("Sonoran Desert, weedy species") +
  theme_minimal() # maybe marginally a tiny bit better


## Northern AZ Plateau ----------------------------------------------------

### Desirable -------------------------------------------------------------

# Linear regression
dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum_abs, y = Count)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  ggtitle("Northern Arizona Plateau, desirable species") +
  theme_minimal() # what is happening

#   Without high Perc_dev_cum_abs
dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum_abs < 1.5) |> 
  ggplot(aes(x = Perc_dev_cum_abs, y = Count)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  ggtitle("Northern Arizona Plateau, desirable species") +
  theme_minimal() # this is still terrible

# Quadratic
dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum_abs, y = Count)) +
  stat_poly_line(formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), use_label(c("eq", "R2"))) +
  geom_point() +
  ggtitle("Northern Arizona Plateau, desirable species") +
  theme_minimal()

#   Without high Perc_dev_cum_abs
dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum_abs < 1.5) |> 
  ggplot(aes(x = Perc_dev_cum_abs, y = Count)) +
  stat_poly_line(formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), use_label(c("eq", "R2"))) +
  geom_point() +
  ggtitle("Northern Arizona Plateau, desirable species") +
  theme_minimal() # lol this is still terrible


### Weedy -----------------------------------------------------------------

# Linear regression
dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum_abs, y = Count)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  ggtitle("Northern Arizona Plateau, weedy species") +
  theme_minimal() 
