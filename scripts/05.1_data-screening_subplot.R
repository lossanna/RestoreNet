# Created: 2024-04-30
# Last updated: 2024-08-29

# Purpose: Examine distributions, outliers, and variable relationships for subplot data response
#   variables, Count and Height.

# Determine if the relationship between response variables and precip variables is linear.
#   Perc_dev_cum seems not linear with both Height and Count.
#   Unsure what is happening with Cum_precip and Cum_precip_sqrt. Seems generally linear for Count and Height?

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
  rename(
    Perc_dev_cum = Perc_deviation,
    Dev_mm_cum = Deviation_mm
  )

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

# Add AridityIndex_log and Cum_precip_sqrt
#   05.3.R showed these transformations help improve normality
subplot <- subplot |> 
  mutate(Cum_precip_sqrt = sqrt(Cum_precip)) |> 
  mutate(AridityIndex_log = log(AridityIndex))

# Make separate Desirable/Weedy data sets, with 800% precip deviation removed
subplot.des8rm <- subplot |> 
  filter(Weedy != "Weedy",
         Perc_dev_cum < 8)

subplot.weed8rm <- subplot |> 
  filter(Weedy != "Desirable",
         Perc_dev_cum < 8)

# Separate out Sonoran sites by Weedy/Desirable
sonoran.des <- subplot |> 
  filter(Region %in% c("Sonoran SE", "Sonoran Central")) |> 
  filter(Weedy != "Weedy")
sonoran.weed <- subplot |> 
  filter(Region %in% c("Sonoran SE", "Sonoran Central")) |> 
  filter(Weedy != "Desirable")

# Separate out CO Plateau (Northern Arizona) sites by Weedy/Desirable
naz.des <- subplot |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Weedy != "Weedy")
naz.weed <- subplot |> 
  filter(Region == "Colorado Plateau") |>
  filter(Weedy != "Desirable")

# Separate out continuous variables
pairs.cont <- subplot |>
  select(Region, Perc_dev_cum, AridityIndex, MAT, MAP, Cum_precip, Elevation_ft)
pairs.cont <- pairs.cont |>
  distinct(.keep_all = TRUE)

pairs.cont.des8rm <- subplot.des8rm |> 
  select(Region, Perc_dev_cum, AridityIndex, AridityIndex_log, MAT, MAP, Cum_precip, Cum_precip_sqrt) 
pairs.cont.des8rm <- pairs.cont.des8rm |> 
  distinct(.keep_all = TRUE)
pairs.cont.weed8rm <- subplot.weed8rm |> 
  select(Region, Perc_dev_cum, AridityIndex, AridityIndex_log, MAT, MAP, Cum_precip, Cum_precip_sqrt) 
pairs.cont.weed8rm <- pairs.cont.weed8rm |> 
  distinct(.keep_all = TRUE)



# Response variable: Count ------------------------------------------------

## Histogram --------------------------------------------------------------

# Histograms generally show what we would expect (Poisson distribution of counts,
#   possible overdispersion or zero-inflation).

# All, desirable
hist(subplot.des8rm$Count, breaks = 50)

# All, weedy
hist(subplot.weed8rm$Count, breaks = 50)

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
hist(filter(subplot, Region == "Colorado Plateau", PlantSource2 == "Recruit")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE", PlantSource2 == "Recruit")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central", PlantSource2 == "Recruit")$Count) # did not have a lot of recruits
hist(filter(subplot, Region == "Utah", PlantSource2 == "Recruit")$Count, breaks = 50)
hist(filter(subplot, Region == "Mojave", PlantSource2 == "Recruit")$Count, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan", PlantSource2 == "Recruit")$Count, breaks = 20)

#   Native recruit
hist(filter(subplot, Region == "Colorado Plateau", PlantSource2 == "Native recruit")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE", PlantSource2 == "Native recruit")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central", PlantSource2 == "Native recruit")$Count)
hist(filter(subplot, Region == "Utah", PlantSource2 == "Native recruit")$Count, breaks = 50)
hist(filter(subplot, Region == "Mojave", PlantSource2 == "Native recruit")$Count, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan", PlantSource2 == "Native recruit")$Count, breaks = 20)

#   Introduced/Invasive
hist(filter(subplot, Region == "Colorado Plateau", PlantSource2 == "Introduced/Invasive")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE", PlantSource2 == "Introduced/Invasive")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central", PlantSource2 == "Introduced/Invasive")$Count)
hist(filter(subplot, Region == "Utah", PlantSource2 == "Introduced/Invasive")$Count, breaks = 50)
hist(filter(subplot, Region == "Mojave", PlantSource2 == "Introduced/Invasive")$Count, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan", PlantSource2 == "Introduced/Invasive")$Count, breaks = 20)

#   Seeded
hist(filter(subplot, Region == "Colorado Plateau", PlantSource2 == "Seeded")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE", PlantSource2 == "Seeded")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central", PlantSource2 == "Seeded")$Count)
hist(filter(subplot, Region == "Utah", PlantSource2 == "Seeded")$Count, breaks = 50)
hist(filter(subplot, Region == "Mojave", PlantSource2 == "Seeded")$Count, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan", PlantSource2 == "Seeded")$Count, breaks = 20)


# All, by Region and PlotMix_Climate

#   None
hist(filter(subplot, Region == "Colorado Plateau", PlotMix_Climate == "None")$Count, breaks = 30)
hist(filter(subplot, Region == "Sonoran SE", PlotMix_Climate == "None")$Count, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central", PlotMix_Climate == "None")$Count)
hist(filter(subplot, Region == "Utah", PlotMix_Climate == "None")$Count)
hist(filter(subplot, Region == "Mojave", PlotMix_Climate == "None")$Count, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan", PlotMix_Climate == "None")$Count, breaks = 20)


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

# Response variables (Height vs. Count)
subplot |>
  ggplot(aes(x = Count, y = Height)) +
  geom_point()

# Response variables, by region
subplot |>
  ggplot(aes(x = Count, y = Height)) +
  geom_point() +
  facet_wrap(~Region)

# Continuous variables
subplot |>
  select(Perc_dev_cum, AridityIndex, MAT, MAP, Cum_precip, Elevation_ft) |>
  distinct(.keep_all = TRUE) |>
  ggpairs() # Elevation & MAT correlated; AridityIndex and MAP correlated

# Count and continuous variables
subplot |>
  select(Count, Perc_dev_cum, AridityIndex, MAT, MAP, Cum_precip, Elevation_ft) |>
  distinct(.keep_all = TRUE) |>
  ggpairs() 


## Perc_dev_cum -----------------------------------------------------------

# All sites, desirable
subplot.des8rm |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, desirable species") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

#   Zoom in by removing high Count
subplot.des8rm |> 
  filter(Count < 100) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, desirable species, Count outliers removed") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# All sites, weedy
subplot.weed8rm |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, weedy species") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

#   Zoom in by removing high Count
subplot.weed8rm |> 
  filter(Count < 100) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, weedy species, Count outliers removed") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 


# Sonoran Desert, desirable
sonoran.des |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Sonoran Desert, desirable species") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# Sonoran Desert, weedy
sonoran.weed |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Sonoran Desert, weedy species") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 


# CO Plateau, desirable
naz.des |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, desirable species") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# CO Plateau, weedy
naz.weed |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Nothern Arizona Plateau, weedy species") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

#   Zoom in by removing high Count
naz.weed |> 
  filter(Count < 100) |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, weedy species, Count outliers removed") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 


## Cum_precip -------------------------------------------------------------

# All sites, desirable
subplot.des8rm |> 
  ggplot(aes(x = Cum_precip, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, desirable species") +
  xlab("Cumulative precip")  

#   Zoom in by removing high Count
subplot.des8rm |> 
  filter(Count < 100) |> 
  ggplot(aes(x = Cum_precip, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, desirable species") +
  xlab("Cumulative precip") 

# All sites, weedy
subplot.weed8rm |> 
  ggplot(aes(x = Cum_precip, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, weedy species") +
  xlab("Cumulative precip")   

#   Zoom in by removing high Count
subplot.weed8rm |> 
  filter(Count < 100) |> 
  ggplot(aes(x = Cum_precip, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, weedy species") +
  xlab("Cumulative precip")  


# Sonoran Desert, desirable
sonoran.des |> 
  ggplot(aes(x = Cum_precip, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Sonoran Desert, desirable species") +
  xlab("Cumulative precip")  

# Sonoran Desert, weedy
sonoran.weed |> 
  ggplot(aes(x = Cum_precip, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Sonoran Desert, weedy species") +
  xlab("Cumulative precip") 


# CO Plateau, desirable
naz.des |> 
  ggplot(aes(x = Cum_precip, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, desirable species") +
  xlab("Cumulative precip")  

# CO Plateau, weedy
naz.weed |> 
  ggplot(aes(x = Cum_precip, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Nothern Arizona Plateau, weedy species") +
  xlab("Cumulative precip")  



## Cum_precip_sqrt --------------------------------------------------------

# All sites, desirable
subplot.des8rm |> 
  ggplot(aes(x = Cum_precip_sqrt, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, desirable species") +
  xlab("Cumulative precip (sqrt)")  

#   Zoom in by removing high Count
subplot.des8rm |> 
  filter(Count < 100) |> 
  ggplot(aes(x = Cum_precip_sqrt, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, desirable species") +
  xlab("Cumulative precip (sqrt)") 

# All sites, weedy
subplot.weed8rm |> 
  ggplot(aes(x = Cum_precip_sqrt, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, weedy species") +
  xlab("Cumulative precip (sqrt)")   

#   Zoom in by removing high Count
subplot.weed8rm |> 
  filter(Count < 100) |> 
  ggplot(aes(x = Cum_precip_sqrt, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, weedy species") +
  xlab("Cumulative precip (sqrt)")  


# Sonoran Desert, desirable
sonoran.des |> 
  ggplot(aes(x = Cum_precip_sqrt, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Sonoran Desert, desirable species") +
  xlab("Cumulative precip (sqrt)")  

# Sonoran Desert, weedy
sonoran.weed |> 
  ggplot(aes(x = Cum_precip_sqrt, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Sonoran Desert, weedy species") +
  xlab("Cumulative precip (sqrt)") 


# CO Plateau, desirable
naz.des |> 
  ggplot(aes(x = Cum_precip_sqrt, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, desirable species") +
  xlab("Cumulative precip (sqrt)")  

# CO Plateau, weedy
naz.weed |> 
  ggplot(aes(x = Cum_precip_sqrt, y = Count)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Nothern Arizona Plateau, weedy species") +
  xlab("Cumulative precip (sqrt)")  



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
hist(filter(subplot, Region == "Colorado Plateau", PlantSource2 == "Recruit")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE", PlantSource2 == "Recruit")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central", PlantSource2 == "Recruit")$Height, breaks = 20)
hist(filter(subplot, Region == "Utah", PlantSource2 == "Recruit")$Height, breaks = 50)
hist(filter(subplot, Region == "Mojave", PlantSource2 == "Recruit")$Height, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan", PlantSource2 == "Recruit")$Height, breaks = 20)

#   Native recruit
hist(filter(subplot, Region == "Colorado Plateau", PlantSource2 == "Native recruit")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE", PlantSource2 == "Native recruit")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central", PlantSource2 == "Native recruit")$Height)
hist(filter(subplot, Region == "Utah", PlantSource2 == "Native recruit")$Height, breaks = 50)
hist(filter(subplot, Region == "Mojave", PlantSource2 == "Native recruit")$Height, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan", PlantSource2 == "Native recruit")$Height, breaks = 20)

#   Introduced/Invasive
hist(filter(subplot, Region == "Colorado Plateau", PlantSource2 == "Introduced/Invasive")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE", PlantSource2 == "Introduced/Invasive")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central", PlantSource2 == "Introduced/Invasive")$Height, breaks = 20)
hist(filter(subplot, Region == "Utah", PlantSource2 == "Introduced/Invasive")$Height, breaks = 50)
hist(filter(subplot, Region == "Mojave", PlantSource2 == "Introduced/Invasive")$Height, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan", PlantSource2 == "Introduced/Invasive")$Height, breaks = 20)

#   Seeded
hist(filter(subplot, Region == "Colorado Plateau", PlantSource2 == "Seeded")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran SE", PlantSource2 == "Seeded")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central", PlantSource2 == "Seeded")$Height, breaks = 20)
hist(filter(subplot, Region == "Utah", PlantSource2 == "Seeded")$Height, breaks = 50)
hist(filter(subplot, Region == "Mojave", PlantSource2 == "Seeded")$Height, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan", PlantSource2 == "Seeded")$Height, breaks = 20)


# All, by Region and PlotMix_Climate

#   None
hist(filter(subplot, Region == "Colorado Plateau", PlotMix_Climate == "None")$Height, breaks = 30)
hist(filter(subplot, Region == "Sonoran SE", PlotMix_Climate == "None")$Height, breaks = 50)
hist(filter(subplot, Region == "Sonoran Central", PlotMix_Climate == "None")$Height)
hist(filter(subplot, Region == "Utah", PlotMix_Climate == "None")$Height, breaks = 20)
hist(filter(subplot, Region == "Mojave", PlotMix_Climate == "None")$Height, breaks = 20)
hist(filter(subplot, Region == "Chihuahuan", PlotMix_Climate == "None")$Height, breaks = 20)


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



## Perc_dev_cum -----------------------------------------------------------

# All sites, desirable
subplot.des8rm |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, desirable species") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

#   Zoom in by removing high Height
subplot.des8rm |> 
  filter(Height < 400) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, desirable species, Height outliers removed") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# All sites, weedy
subplot.weed8rm |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, weedy species") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

#   Zoom in by removing high Height
subplot.weed8rm |> 
  filter(Height < 100) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, weedy species, Height outliers removed") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 


# Sonoran Desert, desirable
sonoran.des |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Sonoran Desert, desirable species") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# Sonoran Desert, weedy
sonoran.weed |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Sonoran Desert, weedy species") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 


# CO Plateau, desirable
naz.des |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, desirable species") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# CO Plateau, weedy
naz.weed |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Nothern Arizona Plateau, weedy species") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

#   Zoom in by removing high Height
naz.weed |> 
  filter(Height < 100) |> 
  ggplot(aes(x = Perc_dev_cum, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, weedy species, Height outliers removed") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 


## Cum_precip -------------------------------------------------------------

# All sites, desirable
subplot.des8rm |> 
  ggplot(aes(x = Cum_precip, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, desirable species") +
  xlab("Cumulative precip")  

# All sites, weedy
subplot.weed8rm |> 
  ggplot(aes(x = Cum_precip, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, weedy species") +
  xlab("Cumulative precip")   

#   Zoom in by removing high Height
subplot.weed8rm |> 
  filter(Height < 500) |> 
  ggplot(aes(x = Cum_precip, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, weedy species") +
  xlab("Cumulative precip")  


# Sonoran Desert, desirable
sonoran.des |> 
  ggplot(aes(x = Cum_precip, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Sonoran Desert, desirable species") +
  xlab("Cumulative precip")  

# Sonoran Desert, weedy
sonoran.weed |> 
  ggplot(aes(x = Cum_precip, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Sonoran Desert, weedy species") +
  xlab("Cumulative precip") 


# CO Plateau, desirable
naz.des |> 
  ggplot(aes(x = Cum_precip, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, desirable species") +
  xlab("Cumulative precip")  

# CO Plateau, weedy
naz.weed |> 
  ggplot(aes(x = Cum_precip, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Nothern Arizona Plateau, weedy species") +
  xlab("Cumulative precip")  



## Cum_precip_sqrt --------------------------------------------------------

# All sites, desirable
subplot.des8rm |> 
  ggplot(aes(x = Cum_precip_sqrt, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, desirable species") +
  xlab("Cumulative precip (sqrt)")  

# All sites, weedy
subplot.weed8rm |> 
  ggplot(aes(x = Cum_precip_sqrt, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, weedy species") +
  xlab("Cumulative precip (sqrt)")   

#   Zoom in by removing high Height
subplot.weed8rm |> 
  filter(Height < 500) |> 
  ggplot(aes(x = Cum_precip_sqrt, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All sites, weedy species") +
  xlab("Cumulative precip (sqrt)")  


# Sonoran Desert, desirable
sonoran.des |> 
  ggplot(aes(x = Cum_precip_sqrt, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Sonoran Desert, desirable species") +
  xlab("Cumulative precip (sqrt)")  

# Sonoran Desert, weedy
sonoran.weed |> 
  ggplot(aes(x = Cum_precip_sqrt, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Sonoran Desert, weedy species") +
  xlab("Cumulative precip (sqrt)") 


# CO Plateau, desirable
naz.des |> 
  ggplot(aes(x = Cum_precip_sqrt, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Northern Arizona Plateau, desirable species") +
  xlab("Cumulative precip (sqrt)")  

# CO Plateau, weedy
naz.weed |> 
  ggplot(aes(x = Cum_precip_sqrt, y = Height)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Nothern Arizona Plateau, weedy species") +
  xlab("Cumulative precip (sqrt)") 
