# Created: 2024-05-29
# Last updated: 2024-08-29

# Purpose: Examine distributions, outliers, and variable relationships for continuous
#   explanatory variables (applies to both subplot and 2x2 data response variables).

# There isn't much difference between the weedy and desirable data sets, or the full set (except for the
#   huge Perc_dev_cum outlier).
# Square-root transformation seemed to help normalize Cum_precip.
# Log transformation seemed to help normalize AridityIndex.

library(tidyverse)
library(GGally)
library(ggpubr)

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

# Separate out continuous variables
pairs.cont <- subplot |> 
  select(Region, Perc_dev_cum, AridityIndex, MAT, MAP, Cum_precip, Elevation_ft) 
pairs.cont <- pairs.cont |> 
  distinct(.keep_all = TRUE)

# Make separate Desirable/Weedy data sets, with 8000% precip deviation removed
subplot.des8rm <- subplot |> 
  filter(Weedy != "Weedy",
         Perc_dev_cum < 8)

subplot.weed8rm <- subplot |> 
  filter(Weedy != "Desirable",
         Perc_dev_cum < 8)

pairs.cont.des8rm <- subplot.des8rm |> 
  select(Region, Perc_dev_cum, AridityIndex, MAT, MAP, Cum_precip, Elevation_ft) 
pairs.cont.des8rm <- pairs.cont.des8rm |> 
  distinct(.keep_all = TRUE)
pairs.cont.weed8rm <- subplot.weed8rm |> 
  select(Region, Perc_dev_cum, AridityIndex, MAT, MAP, Cum_precip, Elevation_ft) 
pairs.cont.weed8rm <- pairs.cont.weed8rm |> 
  distinct(.keep_all = TRUE)



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

# Relationship between Perc_dev_cum and Count
subplot |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth()

#   Without outliers
subplot |> 
  filter(SiteDateID != 112,
         Count < 200) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth()

#   As quadratic?
subplot |> 
  filter(SiteDateID != 112,
         Count < 100,
         Perc_dev_cum < 1) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  stat_smooth(method = "lm",
              formula = y ~ poly(x, 2)) 
summary(lm(Count ~ poly(Perc_dev_cum, 2), data = subplot))



# Other continuous variables ----------------------------------------------

# Histogram
hist(pairs.cont$AridityIndex)
hist(pairs.cont$MAT)
hist(pairs.cont$MAP)
hist(pairs.cont$Cum_precip) # not normal
hist(pairs.cont$Elevation_ft)

# QQ plots
qqnorm(pairs.cont$Perc_dev_cum)
qqline(pairs.cont$Perc_dev_cum) # normal except for the outlier
qqnorm(pairs.cont.des8rm$Perc_dev_cum)
qqline(pairs.cont.des8rm$Perc_dev_cum)
qqnorm(pairs.cont.weed8rm$Perc_dev_cum)
qqline(pairs.cont.weed8rm$Perc_dev_cum)

qqnorm(pairs.cont$AridityIndex)
qqline(pairs.cont$AridityIndex) # not that normal?
qqnorm(pairs.cont$MAT)
qqline(pairs.cont$MAT) # sort of normal
qqnorm(pairs.cont$MAP)
qqline(pairs.cont$MAP) # not that normal?
qqnorm(pairs.cont$Cum_precip)
qqline(pairs.cont$Cum_precip) # not normal
qqnorm(pairs.cont$Elevation_ft)
qqline(pairs.cont$Elevation_ft)



# Cum_precip transformation -----------------------------------------------

# As is
summary(pairs.cont.des8rm$Cum_precip)
qqnorm(pairs.cont.des8rm$Cum_precip)
qqline(pairs.cont.des8rm$Cum_precip)
qqnorm(pairs.cont.weed8rm$Cum_precip)
qqline(pairs.cont.weed8rm$Cum_precip)

# Log transformation
des8rm.log <- pairs.cont.des8rm |> 
  mutate(Cum_precip_log = log(Cum_precip))
weed8rm.log <- pairs.cont.weed8rm |> 
  mutate(Cum_precip_log = log(Cum_precip))

qqnorm(des8rm.log$Cum_precip_log) 
qqline(des8rm.log$Cum_precip_log) # did not really help?
qqnorm(weed8rm.log$Cum_precip_log) 
qqline(weed8rm.log$Cum_precip_log)

# Square root transformation
des8rm.sqrt <- pairs.cont.des8rm |> 
  mutate(Cum_precip_sqrt = sqrt(Cum_precip))
weed8rm.sqrt <- pairs.cont.weed8rm |> 
  mutate(Cum_precip_sqrt = sqrt(Cum_precip))

qqnorm(des8rm.sqrt$Cum_precip_sqrt)
qqline(des8rm.sqrt$Cum_precip_sqrt)
hist(weed8rm.sqrt$Cum_precip_sqrt)
qqnorm(weed8rm.sqrt$Cum_precip_sqrt)
qqline(weed8rm.sqrt$Cum_precip_sqrt) # did help


# AridityIndex transformation ---------------------------------------------

# As is
summary(pairs.cont.des8rm$AridityIndex)
hist(pairs.cont.des8rm$AridityIndex, breaks = 20)
qqnorm(pairs.cont.des8rm$AridityIndex)
qqline(pairs.cont.des8rm$AridityIndex)
qqnorm(pairs.cont.weed8rm$AridityIndex)
qqline(pairs.cont.weed8rm$AridityIndex)

# Log transformation
des8rm.log <- pairs.cont.des8rm |> 
  mutate(AridityIndex_log = log(AridityIndex))
weed8rm.log <- pairs.cont.weed8rm |> 
  mutate(AridityIndex_log = log(AridityIndex))

qqnorm(des8rm.log$AridityIndex_log) 
qqline(des8rm.log$AridityIndex_log) # not great, but seemed to help the best
qqnorm(weed8rm.log$AridityIndex_log) 
qqline(weed8rm.log$AridityIndex_log)

# Square root transformation
des8rm.sqrt <- pairs.cont.des8rm |> 
  mutate(AridityIndex_sqrt = sqrt(AridityIndex))
weed8rm.sqrt <- pairs.cont.weed8rm |> 
  mutate(AridityIndex_sqrt = sqrt(AridityIndex))

qqnorm(des8rm.sqrt$AridityIndex_sqrt)
qqline(des8rm.sqrt$AridityIndex_sqrt)
hist(weed8rm.sqrt$AridityIndex_sqrt)
qqnorm(weed8rm.sqrt$AridityIndex_sqrt)
qqline(weed8rm.sqrt$AridityIndex_sqrt) # also did not help


## Pairplot ---------------------------------------------------------------

# All
pairs.cont |> 
  select(-Region) |> 
  ggpairs() # Elevation & MAT strongly correlated (R = 0.9); Cum_precip & MAP loosely correlated (R = 0.4)

# By region
pairs.cont |> 
  filter(Region == "Colorado Plateau") |> 
  select(-Region) |> 
  ggpairs()
pairs.cont |> 
  filter(Region == "Sonoran SE") |> 
  select(-Region) |> 
  ggpairs()
pairs.cont |> 
  filter(Region == "Sonoran Central") |> 
  select(-Region) |> 
  ggpairs()
pairs.cont |> 
  filter(Region == "Utah") |> 
  select(-Region) |> 
  ggpairs()
pairs.cont |> 
  filter(Region == "Mojave") |> 
  select(-Region) |> 
  ggpairs()
pairs.cont |> 
  filter(Region == "Chihuahuan") |> 
  select(-Region) |> 
  ggpairs()

