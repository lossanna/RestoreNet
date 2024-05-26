# Created: 2024-04-30
# Last updated: 2024-05-26

# Purpose: Examine distributions.

library(tidyverse)


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
#   Infinity created when there was no rain during the monitoring period
subplot <- subplot |> 
  filter(Perc_dev_cum != Inf)

# Separate out Pits, Seed, and Control only
pitseed <- subplot |> 
  filter(Treatment %in% c("Control", "Seed", "Pits"))


# Count -------------------------------------------------------------------

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


# All, by region and PlantSource2
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





# Cum precip, percent deviation from normals ------------------------------

# I think I will go with cum.pd as the measure of precip variation (percent deviation from normals
#   of cumulative precip since time of seeding to monitoring event), because this captures inter- and 
#   intra-annual precip variation and accounts for some sites being monitored longer than others, which
#   precip since last monitoring event does not. The CV calculation doesn't really make sense, and to be
#   comparable to other papers, they were doing CV of precip itself because the time intervals were equal.
#   Since this project does not have equal time intervals, CV will not be an equal comparison.

hist(cum.pd$Perc_deviation, breaks = 50) # this seems possibly normally distributed (minus outliers)
hist(cum.pd$Perc_deviation, breaks = 100) # but with more breaks, it does not seem normally distributed
dotchart(cum.pd$Perc_deviation)

# Identify outliers
cum.pd |> 
  filter(Perc_deviation > 8) |> 
  select(Region, Site, Date_Monitored, SiteDateID, Perc_deviation)
#   Inf from CO Plateau can be dropped: Date_Monitored is 2018-08-10 for SiteDateID 37,
#     and Date_Monitored for SiteDateID 38 is 2018-08-23. Infinity created because
#     there was no rain during the time period, which in this case is 2028-07-25 (time of seeding)
#     to 2018-08-10 (first monitoring event).
#   29_Palms only has 3 monitoring events, so the outlier should probably not be dropped?
#     There is nothing wrong with the data - I manually checked and the site just got a bunch
#     of rain in March-April 2020 for some reason.
  
