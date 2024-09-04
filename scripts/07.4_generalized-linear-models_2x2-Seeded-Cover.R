# Created: 2024-09-04
# Last updated: 2024-09-04

# Purpose: Run generalized linear models for 2x2 m plot data, with Seeded_Cover as response variable. 
# Check for overdispersion and zero-inflation.

library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)

# Load data ---------------------------------------------------------------

p2x2.rich.cover.raw <- read_csv("data/cleaned/04.2_2x2-richness-cover_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")


# Data wrangling ----------------------------------------------------------

## Combine ----------------------------------------------------------------

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


## Remove Inf and outliers from Perc_dev_cum ------------------------------

richcover <- p2x2.rich.cover |> 
  filter(Perc_dev_cum != Inf) |> 
  filter(Perc_dev_cum < 8)


## Add PlotMix_Climate col ------------------------------------------------

richcover <- richcover |>
  mutate(PlotMix_Climate = case_when(
    str_detect(richcover$Site, "Creosote|Mesquite|Patagonia|SRER") &
      richcover$PlotMix == "Medium" ~ "Current",
    str_detect(richcover$Site, "Creosote|Mesquite|Patagonia|SRER") &
      richcover$PlotMix == "Warm" ~ "Projected",
    str_detect(richcover$Site, "AguaFria|MOWE|PEFO|Spiderweb") &
      richcover$PlotMix == "Med-Warm" ~ "Current",
    str_detect(richcover$Site, "AguaFria|MOWE|PEFO|Spiderweb") &
      richcover$PlotMix == "Warm" ~ "Projected",
    str_detect(richcover$Site, "BarTBar|FlyingM|CRC|Salt_Desert") &
      richcover$PlotMix == "Cool-Med" ~ "Current",
    str_detect(richcover$Site, "BarTBar|FlyingM|CRC|Salt_Desert") &
      richcover$PlotMix == "Med-Warm" ~ "Projected",
    str_detect(richcover$Site, "BabbittPJ|UtahPJ") &
      richcover$PlotMix == "Cool" ~ "Current",
    str_detect(richcover$Site, "BabbittPJ|UtahPJ") &
      richcover$PlotMix == "Cool-Med" ~ "Projected",
    str_detect(richcover$Site, "29_Palms|AVRCD|Preserve|SCC|Roosevelt|Pleasant|TLE") &
      richcover$PlotMix == "Cool" ~ "Current",
    str_detect(richcover$Site, "29_Palms|AVRCD|Preserve|SCC|Roosevelt|Pleasant|TLE") &
      richcover$PlotMix == "Warm" ~ "Projected",
    TRUE ~ richcover$PlotMix))
richcover$PlotMix_Climate <- factor(richcover$PlotMix_Climate)


## Re-level categorical variables to set reference ------------------------

# Treatment
unique(richcover$Treatment)
richcover$Treatment <- as.factor(richcover$Treatment)
richcover$Treatment <- relevel(richcover$Treatment, ref = "Control")

# PlotMix_Climate
unique(richcover$PlotMix_Climate)
#   Allow Current to remain reference; because of uneven sample sizes, if
#     None is reference then Projected will be dropped from models. Better
#     to drop None and be able to compare Current & Projected.


## Transform AridityIndex and Cum_precip for normalization ----------------

# 05.3.R showed these transformations help improve normality
richcover <- richcover |> 
  mutate(Cum_precip_sqrt = sqrt(Cum_precip)) |> 
  mutate(AridityIndex_log = log(AridityIndex)) |> 
  mutate(Since_last_precip_sqrt = sqrt(Since_last_precip))

# Add Perc_dev_cum_abs
richcover <- richcover |> 
  mutate(Perc_dev_cum_abs = abs(Perc_dev_cum))


## Round cover values -----------------------------------------------------

# Sometimes Height was missing (not recorded)
richcover <- richcover |> 
  filter(!is.na(Seeded_Cover))

# Check for non-integers in Seeded_Cover
non_integers <- richcover$Seeded_Cover != floor(richcover$Seeded_Cover)

# Round non-integers
richcover$Seeded_Cover <- ifelse(richcover$Seeded_Cover != floor(richcover$Seeded_Cover), round(richcover$Seeded_Cover), richcover$Seeded_Cover)



## Separate out Sonoran sites (6) -----------------------------------------

sonoran <- richcover |> 
  filter(Region %in% c("Sonoran SE", "Sonoran Central"))

# No control plots, adjust reference levels to match Count & Height
sonoran.seed <- sonoran |> 
  filter(Treatment != "Control")
sonoran.seed$Treatment <- relevel(sonoran.seed$Treatment, ref = "Seed")


## Separate out Northern AZ sites (8) -------------------------------------

naz <- richcover |> 
  filter(Region == "Colorado Plateau")



# All sites ---------------------------------------------------------------

# All variables, no random effect
pos.all0 <- glm(Seeded_Cover ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + 
                      PlotMix_Climate + MAT + MAP + Sand_content + Since_last_precip_sqrt,
                    data = richcover, family = "poisson")
summary(pos.all0)
check_overdispersion(pos.all0) # overdispersion detected
check_zeroinflation(pos.all0) # zero-inflation detected
check_collinearity(pos.all0)


# All variables, with random effect
#   nested random effect takes care of dispersion issue
pos.all <- glmmTMB(Seeded_Cover ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + 
                         PlotMix_Climate + MAT + MAP + Sand_content + Since_last_precip_sqrt + 
                         (1 | Site / Plot),
                       data = richcover, family = genpois)
summary(pos.all)
r2(pos.all)
res.pos.all <- simulateResiduals(pos.all)
plotQQunif(pos.all)
plotResiduals(pos.all)
check_overdispersion(pos.all) # no overdispersion detected
check_zeroinflation(pos.all) # model is overfitting zeros
check_collinearity(pos.all)


# Sonoran Desert ----------------------------------------------------------

## Poisson ----------------------------------------------------------------

# All variables, no random effect
pos.sonoran0 <- glm(Seeded_Cover ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + 
                      PlotMix_Climate + MAT + MAP + Sand_content + Since_last_precip_sqrt,
                    data = sonoran, family = "poisson")
summary(pos.sonoran0)
check_overdispersion(pos.sonoran0) # overdispersion detected
check_zeroinflation(pos.sonoran0) # zero-inflation detected
check_collinearity(pos.sonoran0)


# All variables, with random effect
#   nested random effect takes care of dispersion issue
pos.sonoran <- glmmTMB(Seeded_Cover ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + 
                         PlotMix_Climate + MAT + MAP + Sand_content + Since_last_precip_sqrt + 
                         (1 | Site / Plot),
                       data = sonoran, family = genpois)
summary(pos.sonoran)
r2(pos.sonoran)
res.pos.sonoran <- simulateResiduals(pos.sonoran)
plotQQunif(pos.sonoran)
plotResiduals(pos.sonoran)
check_overdispersion(pos.sonoran) # no overdispersion detected
check_zeroinflation(pos.sonoran) # model is overfitting zeros
check_collinearity(pos.sonoran)

#   1: Drop MAP (collinearity)
pos.sonoran1 <- glmmTMB(Seeded_Cover ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + 
                         PlotMix_Climate + MAT + Sand_content + Since_last_precip_sqrt + 
                         (1 | Site / Plot),
                       data = sonoran, family = genpois)
summary(pos.sonoran1)
r2(pos.sonoran1)
res.pos.sonoran1 <- simulateResiduals(pos.sonoran1)
plotQQunif(pos.sonoran1)
plotResiduals(pos.sonoran1) # first time I have a residuals plot that actually looks okay lol
check_overdispersion(pos.sonoran1) # no overdispersion detected
check_zeroinflation(pos.sonoran1) # model is overfitting zeros
check_collinearity(pos.sonoran1)


# *** No Control plots ***
pos.sonoran.seed <- glmmTMB(Seeded_Cover ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + 
                          PlotMix_Climate + MAT + Sand_content + Since_last_precip_sqrt + 
                          (1 | Site / Plot),
                        data = sonoran.seed, family = genpois)
summary(pos.sonoran.seed)
r2(pos.sonoran.seed)
res.pos.sonoran.seed <- simulateResiduals(pos.sonoran.seed)
plotQQunif(pos.sonoran.seed)
plotResiduals(pos.sonoran.seed) # first time I have a residuals plot that actually looks okay lol
check_overdispersion(pos.sonoran.seed) # no overdispersion detected
check_zeroinflation(pos.sonoran.seed) # model is overfitting zeros
check_collinearity(pos.sonoran.seed)


# Northern Arizona Plateau ------------------------------------------------

# All variables, with random effect
pos.naz <- glmmTMB(Seeded_Cover ~ Perc_dev_cum_abs + AridityIndex_log + Treatment + 
                         PlotMix_Climate + MAT + MAP + Sand_content + Since_last_precip_sqrt + 
                         (1 | Site / Plot),
                       data = naz, family = genpois)
summary(pos.naz)
r2(pos.naz)
res.pos.naz <- simulateResiduals(pos.naz)
plotQQunif(pos.naz)
plotResiduals(pos.naz)
check_overdispersion(pos.naz) # no overdispersion detected
check_zeroinflation(pos.naz) # model is overfitting zeros
check_collinearity(pos.naz)


save.image("RData/07.4_generalized-linear-models_2x2-Seeded-Cover.RData")
