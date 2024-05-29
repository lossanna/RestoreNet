# Created: 2024-04-30
# Last updated: 2024-05-29

# Purpose: Examine distributions, outliers, and variable relationships for subplot data response
#   variables, Count and Height.

library(tidyverse)
library(GGally)

# Load data ---------------------------------------------------------------

subplot.raw <- read_csv("data/cleaned/04.1_subplot-data_clean.csv")
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
subplot <- subplot.raw |> 
  left_join(prism.data) |> 
  left_join(ai) |> 
  left_join(cum.pd.subplot)

# Check for NAs
apply(subplot, 2, anyNA)

# Remove Inf from Perc_dev_cum
#   Infinity created when there was no rain during the monitoring period. This only happens
#     twice and these instances can be dropped.
subplot <- subplot |> 
  filter(Perc_dev_cum != Inf)

# Separate out Pits, Seed, and Control only
pitseed <- subplot |> 
  filter(Treatment %in% c("Control", "Seed", "Pits"))

# Separate out continuous variables
pairs.cont <- subplot |> 
  select(Region, Perc_dev_cum, AridityIndex, MAT, MAP, Cum_precip, Elevation_ft) 
pairs.cont <- pairs.cont |> 
  distinct(.keep_all = TRUE)




# Response variable: Count ------------------------------------------------

## Histogram --------------------------------------------------------------

# Histograms generally show what we would expect (Poisson distribution of counts,
#   possible overdispersion or zero-inflation).

# All
hist(subplot$Count, breaks = 100)

# All, by region
hist(filter(subplot, Region == "Colorado Plateau")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central")$Count, breaks = 50)
hist(filter(subplot, Region == "Utah")$Count, breaks = 50)
hist(filter(subplot, Region == "Mojave")$Count)
hist(filter(subplot, Region == "Chihuahuan")$Count)


# All, by Region and PlantSource2
unique(subplot$PlantSource2)

#   Recruit
hist(filter(subplot, Region == "Colorado Plateau",
            PlantSource2 == "Recruit")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE",
            PlantSource2 == "Recruit")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central",
            PlantSource2 == "Recruit")$Count) # did not have a lot of recruits
hist(filter(subplot, Region == "Utah",
            PlantSource2 == "Recruit")$Count, breaks = 50)
hist(filter(subplot, Region == "Mojave",
            PlantSource2 == "Recruit")$Count, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan",
            PlantSource2 == "Recruit")$Count, breaks = 20)

#   Native recruit
hist(filter(subplot, Region == "Colorado Plateau",
            PlantSource2 == "Native recruit")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE",
            PlantSource2 == "Native recruit")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central",
            PlantSource2 == "Native recruit")$Count) 
hist(filter(subplot, Region == "Utah",
            PlantSource2 == "Native recruit")$Count, breaks = 50)
hist(filter(subplot, Region == "Mojave",
            PlantSource2 == "Native recruit")$Count, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan",
            PlantSource2 == "Native recruit")$Count, breaks = 20)

#   Introduced/Invasive
hist(filter(subplot, Region == "Colorado Plateau",
            PlantSource2 == "Introduced/Invasive")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE",
            PlantSource2 == "Introduced/Invasive")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central",
            PlantSource2 == "Introduced/Invasive")$Count) 
hist(filter(subplot, Region == "Utah",
            PlantSource2 == "Introduced/Invasive")$Count, breaks = 50)
hist(filter(subplot, Region == "Mojave",
            PlantSource2 == "Introduced/Invasive")$Count, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan",
            PlantSource2 == "Introduced/Invasive")$Count, breaks = 20)

#   Seeded
hist(filter(subplot, Region == "Colorado Plateau",
            PlantSource2 == "Seeded")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE",
            PlantSource2 == "Seeded")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central",
            PlantSource2 == "Seeded")$Count) 
hist(filter(subplot, Region == "Utah",
            PlantSource2 == "Seeded")$Count, breaks = 50)
hist(filter(subplot, Region == "Mojave",
            PlantSource2 == "Seeded")$Count, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan",
            PlantSource2 == "Seeded")$Count, breaks = 20)


# All, by Region and PlotMix_Climate

#   None
hist(filter(subplot, Region == "Colorado Plateau",
            PlotMix_Climate == "None")$Count, breaks = 30)
hist(filter(subplot, Region == "Sonoran SE",
            PlotMix_Climate == "None")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central",
            PlotMix_Climate == "None")$Count) 
hist(filter(subplot, Region == "Utah",
            PlotMix_Climate == "None")$Count)
hist(filter(subplot, Region == "Mojave",
            PlotMix_Climate == "None")$Count, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan",
            PlotMix_Climate == "None")$Count, breaks = 20)


## Boxplot ----------------------------------------------------------------

# All, by region
subplot |> 
  ggplot(aes(x = Region, y = Count)) +
  geom_boxplot()

# All, by Region and PlantSource2
subplot |> 
  ggplot(aes(x = Region, y = Count)) +
  geom_boxplot() +
  facet_wrap(~PlantSource2)
#   outliers in Chihuahuan native recruit and CO Plateau recruit

# Pits/Seed/Control by Region and PlantSource2
pitseed |> 
  ggplot(aes(x = Region, y = Count)) +
  geom_boxplot() +
  facet_wrap(~PlantSource2)
#   outliers still in Chihuahuan native recruit and CO Plateau recruit

# All, by Region and PlotMix_Climate
subplot |> 
  ggplot(aes(x = PlotMix_Climate, y = Count)) +
  geom_boxplot() +
  facet_wrap(~Region)


## Dotchart ---------------------------------------------------------------

# All, by region
dotchart(filter(subplot, Region == "Colorado Plateau")$Count,
         ylab = "Observations", xlab = "Count", main = "CO Plateau") # outliers present
dotchart(filter(subplot, Region == "Sonoran SE")$Count,
         ylab = "Observations", xlab = "Count", main = "Sonoran SE")
dotchart(filter(subplot, Region == "Sonoran Central")$Count,
         ylab = "Observations", xlab = "Count", main = "Sonoran Central")
dotchart(filter(subplot, Region == "Utah")$Count,
     ylab = "Observations", xlab = "Count", main = "Utah")
dotchart(filter(subplot, Region == "Mojave")$Count,
     ylab = "Observations", xlab = "Count", main = "Mojave")
dotchart(filter(subplot, Region == "Chihuahuan")$Count,
     ylab = "Observations", xlab = "Count", main = "Chihuahuan") # outliers present


## Pairplot ---------------------------------------------------------------

# Response variables, all
subplot |> 
  ggplot(aes(x = Count, y = Height)) +
  geom_point()

# Response variables, by region
subplot |> 
  ggplot(aes(x = Count, y = Height)) +
  geom_point() +
  facet_wrap(~Region)

# Count and continuous variables
subplot |> 
  select(Count, Perc_dev_cum, MAT, MAP, Cum_precip, Elevation_ft) |> 
  distinct(.keep_all = TRUE) |> 
  ggpairs() # nothing strongly correlated with Count




# Response variable: Height -----------------------------------------------

# Note that Height is missing values for a few observations.

## Histogram --------------------------------------------------------------

# Histograms generally show what we would expect (Poisson distribution of Heights,
#   possible overdispersion or zero-inflation).

# All
hist(subplot$Height, breaks = 100)

# All, by region
hist(filter(subplot, Region == "Colorado Plateau")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central")$Height, breaks = 50)
hist(filter(subplot, Region == "Utah")$Height, breaks = 50)
hist(filter(subplot, Region == "Mojave")$Height)
hist(filter(subplot, Region == "Chihuahuan")$Height)


# All, by Region and PlantSource2
unique(subplot$PlantSource2)

#   Recruit
hist(filter(subplot, Region == "Colorado Plateau",
            PlantSource2 == "Recruit")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE",
            PlantSource2 == "Recruit")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central",
            PlantSource2 == "Recruit")$Height, breaks = 20) 
hist(filter(subplot, Region == "Utah",
            PlantSource2 == "Recruit")$Height, breaks = 50)
hist(filter(subplot, Region == "Mojave",
            PlantSource2 == "Recruit")$Height, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan",
            PlantSource2 == "Recruit")$Height, breaks = 20)

#   Native recruit
hist(filter(subplot, Region == "Colorado Plateau",
            PlantSource2 == "Native recruit")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE",
            PlantSource2 == "Native recruit")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central",
            PlantSource2 == "Native recruit")$Height) 
hist(filter(subplot, Region == "Utah",
            PlantSource2 == "Native recruit")$Height, breaks = 50)
hist(filter(subplot, Region == "Mojave",
            PlantSource2 == "Native recruit")$Height, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan",
            PlantSource2 == "Native recruit")$Height, breaks = 20)

#   Introduced/Invasive
hist(filter(subplot, Region == "Colorado Plateau",
            PlantSource2 == "Introduced/Invasive")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE",
            PlantSource2 == "Introduced/Invasive")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central",
            PlantSource2 == "Introduced/Invasive")$Height, breaks = 20) 
hist(filter(subplot, Region == "Utah",
            PlantSource2 == "Introduced/Invasive")$Height, breaks = 50)
hist(filter(subplot, Region == "Mojave",
            PlantSource2 == "Introduced/Invasive")$Height, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan",
            PlantSource2 == "Introduced/Invasive")$Height, breaks = 20)

#   Seeded
hist(filter(subplot, Region == "Colorado Plateau",
            PlantSource2 == "Seeded")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE",
            PlantSource2 == "Seeded")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central",
            PlantSource2 == "Seeded")$Height, breaks = 20) 
hist(filter(subplot, Region == "Utah",
            PlantSource2 == "Seeded")$Height, breaks = 50)
hist(filter(subplot, Region == "Mojave",
            PlantSource2 == "Seeded")$Height, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan",
            PlantSource2 == "Seeded")$Height, breaks = 20)


# All, by Region and PlotMix_Climate

#   None
hist(filter(subplot, Region == "Colorado Plateau",
            PlotMix_Climate == "None")$Height, breaks = 30)
hist(filter(subplot, Region == "Sonoran SE",
            PlotMix_Climate == "None")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central",
            PlotMix_Climate == "None")$Height) 
hist(filter(subplot, Region == "Utah",
            PlotMix_Climate == "None")$Height, breaks = 20)
hist(filter(subplot, Region == "Mojave",
            PlotMix_Climate == "None")$Height, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan",
            PlotMix_Climate == "None")$Height, breaks = 20)


## Boxplot ----------------------------------------------------------------

# All, by region
subplot |> 
  ggplot(aes(x = Region, y = Height)) +
  geom_boxplot()

# All, by Region and PlantSource2
subplot |> 
  ggplot(aes(x = Region, y = Height)) +
  geom_boxplot() +
  facet_wrap(~PlantSource2)
#   outliers in Chihuahuan native recruit and CO Plateau recruit

# Pits/Seed/Control by Region and PlantSource2
pitseed |> 
  ggplot(aes(x = Region, y = Height)) +
  geom_boxplot() +
  facet_wrap(~PlantSource2)
#   outliers still in Chihuahuan native recruit and CO Plateau recruit

# All, by Region and PlotMix_Climate
subplot |> 
  ggplot(aes(x = PlotMix_Climate, y = Height)) +
  geom_boxplot() +
  facet_wrap(~Region)


## Dotchart ---------------------------------------------------------------

# All, by region
dotchart(filter(subplot, Region == "Colorado Plateau")$Height,
         ylab = "Observations", xlab = "Height", main = "CO Plateau")
dotchart(filter(subplot, Region == "Sonoran SE")$Height,
         ylab = "Observations", xlab = "Height", main = "Sonoran SE")
dotchart(filter(subplot, Region == "Sonoran Central")$Height,
         ylab = "Observations", xlab = "Height", main = "Sonoran Central")
dotchart(filter(subplot, Region == "Utah")$Height,
         ylab = "Observations", xlab = "Height", main = "Utah")
dotchart(filter(subplot, Region == "Mojave")$Height,
         ylab = "Observations", xlab = "Height", main = "Mojave")
dotchart(filter(subplot, Region == "Chihuahuan")$Height,
         ylab = "Observations", xlab = "Height", main = "Chihuahuan")


## Pairplot ---------------------------------------------------------------

# Response variables, all
subplot |> 
  ggplot(aes(x = Count, y = Height)) +
  geom_point()

# Response variables, by region
subplot |> 
  ggplot(aes(x = Count, y = Height)) +
  geom_point() +
  facet_wrap(~Region)

# Height and continuous variables
subplot |> 
  select(Height, Perc_dev_cum, MAT, MAP, Cum_precip, Elevation_ft) |> 
  distinct(.keep_all = TRUE) |> 
  ggpairs() # nothing strongly correlated with Height


save.image("RData/05.1_data-screening_subplot.RData")