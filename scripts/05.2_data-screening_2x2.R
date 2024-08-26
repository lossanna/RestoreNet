# Created: 2024-05-29
# Last updated: 2024-08-26

# Purpose: Examine distributions, outliers, and variable relationships for 2x2 data response
#   variables, Seeded Cover and Total Cover.

library(tidyverse)
library(GGally)

# Load data ---------------------------------------------------------------

p2x2.rich.cover.raw <- read_csv("data/cleaned/04.2_2x2-richness-cover_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")


# Data wrangling ----------------------------------------------------------

# Reorganize columns for left_join()
cum.pd.2x2 <- cum.pd |>
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Perc_deviation, Deviation_mm) |>
  rename(Perc_dev_cum = Perc_deviation,
         Dev_mm_cum = Deviation_mm)

# Combine all variables
p2x2.rich.cover <- p2x2.rich.cover.raw |>
  left_join(prism.data) |>
  left_join(ai) |>
  left_join(cum.pd.2x2)

# Check for NAs
apply(p2x2.rich.cover, 2, anyNA)

# Separate out continuous variables
pairs.cont <- p2x2.rich.cover |>
  select(Region, Perc_dev_cum, AridityIndex, MAT, MAP, Cum_precip, Elevation_ft)
pairs.cont <- pairs.cont |>
  distinct(.keep_all = TRUE)


# Response variable: Seeded cover -----------------------------------------

## Histogram --------------------------------------------------------------

# Histograms generally show what we would expect (Poisson distribution of cover,
#   possible overdispersion or zero-inflation).

# All
hist(p2x2.rich.cover$Seeded_Cover)

# All, by region
hist(filter(p2x2.rich.cover, Region == "Colorado Plateau")$Seeded_Cover)
hist(filter(p2x2.rich.cover, Region == "Sonoran SE")$Seeded_Cover)
hist(filter(p2x2.rich.cover, Region == "Sonoran Central")$Seeded_Cover)
hist(filter(p2x2.rich.cover, Region == "Utah")$Seeded_Cover)
hist(filter(p2x2.rich.cover, Region == "Mojave")$Seeded_Cover)
hist(filter(p2x2.rich.cover, Region == "Chihuahuan")$Seeded_Cover)


## Boxplot ----------------------------------------------------------------

# All, by region
p2x2.rich.cover |>
  ggplot(aes(x = Region, y = Seeded_Cover)) +
  geom_boxplot()


## Dotchart ---------------------------------------------------------------

# All, by region
dotchart(filter(p2x2.rich.cover, Region == "Colorado Plateau")$Seeded_Cover,
         ylab = "Observations", xlab = "Seeded_Cover", main = "CO Plateau")
dotchart(filter(p2x2.rich.cover, Region == "Sonoran SE")$Seeded_Cover,
         ylab = "Observations", xlab = "Seeded_Cover", main = "Sonoran SE")
dotchart(filter(p2x2.rich.cover, Region == "Sonoran Central")$Seeded_Cover,
         ylab = "Observations", xlab = "Seeded_Cover", main = "Sonoran Central")
dotchart(filter(p2x2.rich.cover, Region == "Utah")$Seeded_Cover,
         ylab = "Observations", xlab = "Seeded_Cover", main = "Utah")
dotchart(filter(p2x2.rich.cover, Region == "Mojave")$Seeded_Cover,
         ylab = "Observations", xlab = "Seeded_Cover", main = "Mojave")
dotchart(filter(p2x2.rich.cover, Region == "Chihuahuan")$Seeded_Cover,
         ylab = "Observations", xlab = "Seeded_Cover", main = "Chihuahuan")

