# Created: 024-09-11
# Last updated: 2024-09-11

# Purpose: Characterize the explanatory variables for the Sonoran Desert and Northern 
#   Arizona Plateau.

# Types of explanatory variables:
#   Site-specific: AridityIndex, MAT, MAP, Sand_content
#   Site-specific and temporal-specific: Perc_dev_cum, Since_last_precip
#   Restoration treatment: soil surface treatment (Treatment), seed mix (PlotMix_Climate)
#   Plant characteristics: Duration, Lifeform, PlantSource2

# Sonoran Desert:
#   MAT range: 16.6 - 22.9
#   AridityIndex range: 0.0911 - 0.2269
#   Perc_dev_cum range: -43% to + 64%
#   Since_last_precip range: 14 mm to 448 mm
#   Note: take caution with results about Sand_content, because only one site had
#     high sand content (SCC).

# Northern Arizona Plateau:
#   MAT range: 10.1 - 19.0
#   AridityIndex range: 0.0942 - 0.2264
#   Perc_dev_cum range: -99% to +193%
#   Since_last_precip range: 0 mm to 440 mm


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

# Add transformation for normalization
subplot <- subplot |> 
  mutate(Cum_precip_sqrt = sqrt(Cum_precip)) |> 
  mutate(AridityIndex_log = log(AridityIndex),
         Since_last_precip_sqrt = sqrt(Since_last_precip))
#   AI: higher negative AI values = drier sites

# Add Perc_dev_cum_abs col
subplot <- subplot |> 
  mutate(Perc_dev_cum_abs = abs(Perc_dev_cum))

# Sonoran Desert
sonoran.subplot <- subplot |> 
  filter(Region %in% c("Sonoran SE", "Sonoran Central"))

# N Arizona Plateau
naz.subplot <- subplot |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Perc_dev_cum != Inf)


# Sonoran Desert ----------------------------------------------------------

# PRISM data and precip extremes (one value per site)
s1 <- sonoran.subplot |> 
  select(Region, Site, Elevation_ft, Sand_content, Clay_content, MAP, MAT, AridityIndex) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Site) |> 
  arrange(Region)

s.precip.max <- sonoran.subplot |> 
  group_by(Region, Site) |> 
  summarise(Dry_max = min(Perc_dev_cum),
            Wet_max = max(Perc_dev_cum),
            Abs_max = max(Perc_dev_cum_abs),
            .groups = "keep")

sonoran.sum <- s1 |> 
  left_join(s.precip.max)
sonoran.sum

# highest dry: Roosevelt, -43%
# highest wet: SCC, +64%
# highest abs: SCC, 64%
# MAT range: 16.6 - 22.9
# AI range: 0.0911 - 0.2269


# Precip ranges (one value per monitoring event at each site)
sonoran.sum2 <- sonoran.subplot |> 
  select(Region, Site, Cum_precip, Perc_dev_cum, Perc_dev_cum_abs, Since_last_precip) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Site) |> 
  arrange(Region)

s.precip.drier <- sonoran.sum2 |> 
  filter(Perc_dev_cum < 0)

s.precip.wetter <- sonoran.sum2 |> 
  filter(Perc_dev_cum > 0)

# Drier range
range(s.precip.drier$Perc_dev_cum) # -43% to -2%

# Wetter range
range(s.precip.wetter$Perc_dev_cum) # +0.6% to +64%

# Precip extremes for both wetter and drier occurred at Sonoran Central;
#   For Sonoran SE, the range is -23% to +46%

# Since_last_precip
summary(sonoran.sum2$Since_last_precip) # 14 mm to 448 mm



# Northern Arizona Plateau ------------------------------------------------

# PRISM data and precip extremes (one value per site)
n1 <- naz.subplot |> 
  select(Site, Elevation_ft, Sand_content, Clay_content, MAP, MAT, AridityIndex) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Site)

n.precip.max <- naz.subplot |> 
  group_by(Site) |> 
  summarise(Dry_max = min(Perc_dev_cum),
            Wet_max = max(Perc_dev_cum),
            Abs_max = max(Perc_dev_cum_abs),
            .groups = "keep")

naz.sum <- n1 |> 
  left_join(n.precip.max)
naz.sum

# highest dry: Spiderweb, -99%
# highest wet: BabbittPJ, +193%
# highest abs: BabbittPJ, 193%
# MAT range: 10.1 - 19.0
# AI range: 0.0942 - 0.2264


# Precip ranges (one value per monitoring event at each site)
naz.sum2 <- naz.subplot |> 
  select(Site, Cum_precip, Perc_dev_cum, Perc_dev_cum_abs, Since_last_precip) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Site) 

n.precip.drier <- naz.sum2 |> 
  filter(Perc_dev_cum < 0)

n.precip.wetter <- naz.sum2 |> 
  filter(Perc_dev_cum > 0)

# Drier range
range(n.precip.drier$Perc_dev_cum) # -99% to -1%

# Wetter range
range(n.precip.wetter$Perc_dev_cum) # +2% to +193%

# There were four different Current-Projected seed mix combinations:
#   AguaFria, MOWE, PEFO, Spiderweb: experienced -99% to 185% [had most successful Projected mix by Count]
#   BarTBar and FlyingM: experienced -83% to +49% [had most successful Current mix by Count]
#   BabbittPJ: -68% to +193%
#   TLE: -55% to -31% (basically no seeded species grew from either mix)

# Since_last_precip
summary(naz.sum2$Since_last_precip) # 0 mm to 440 mm
