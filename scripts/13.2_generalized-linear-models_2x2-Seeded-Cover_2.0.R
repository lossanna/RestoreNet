# Created: 024-10-17
# Last updated: 2024-12-11

# Purpose: Run *finalized* (update: these are not actually finalized, they were 1st draft dissertation) 
#   generalized linear models for 2x2 data, with Seeded_Cover as response variable.
#   Models already have some variables dropped to improve convergence. Is the same as
#   10.3.R because Treatment reference was already set to "Seed" before (no seeded cover was
#   measured in control plots).

# For previous exploration, see 10.3_generalized-linear-models_2x2-Seeded-Cover.R.

# 2 models: Sonoran Desert, Northern Arizona


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


## Remove Inf from Perc_dev_cum and add Perc_dev_cum_abs ------------------

richcover <- p2x2.rich.cover |> 
  filter(Perc_dev_cum != Inf) |> 
  mutate(Perc_dev_cum_abs = abs(Perc_dev_cum))


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
richcover$Treatment <- relevel(richcover$Treatment, ref = "Seed")

# PlotMix_Climate
unique(richcover$PlotMix_Climate)
#   Allow Current to remain reference; because of uneven sample sizes, if
#     None is reference then Projected will be dropped from models. Better
#     to drop None and be able to compare Current & Projected.


## Add Days_elapsed col ----------------------------------------------------

richcover <- richcover |> 
  mutate(Days_elapsed = difftime(Date_Monitored, Date_Seeded)) |> 
  mutate(Days_elapsed = as.numeric(Days_elapsed))


## Round cover values -----------------------------------------------------

# Sometimes Seeded_Cover was missing (not recorded)
richcover <- richcover |> 
  filter(!is.na(Seeded_Cover))

# Check for non-integers in Seeded_Cover
non_integers <- richcover$Seeded_Cover != floor(richcover$Seeded_Cover)

# Round non-integers
richcover$Seeded_Cover <- ifelse(richcover$Seeded_Cover != floor(richcover$Seeded_Cover), round(richcover$Seeded_Cover), richcover$Seeded_Cover)



## Separate out Sonoran sites (6) -----------------------------------------

# Remove control plots to match Count GLM for seeded
sonoran.seed <- richcover |> 
  filter(Region %in% c("Sonoran SE", "Sonoran Central")) |> 
  filter(Treatment != "Control")


## Separate out Northern AZ sites (8) -------------------------------------

# Remove control plots to match Count GLM for seeded
naz.seed <- richcover |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Treatment != "Control")


# Sonoran Desert ----------------------------------------------------------

# From 10.3.R: pos.sonoran1.seed
pos.sonoran.seed <- glmmTMB(Seeded_Cover ~ Perc_dev_cum_abs + AridityIndex + Treatment + 
                               PlotMix_Climate + MAT + Since_last_precip + 
                               Days_elapsed + (1 | Site / Plot),
                             data = sonoran.seed, family = genpois)
summary(pos.sonoran.seed)
r2(pos.sonoran.seed)
res.pos.sonoran.seed <- simulateResiduals(pos.sonoran.seed)
plotQQunif(pos.sonoran.seed)
plotResiduals(pos.sonoran.seed) # residuals are actually okay lol
check_overdispersion(pos.sonoran.seed) # no overdispersion detected
check_zeroinflation(pos.sonoran.seed) # no zero-inflation detected
check_collinearity(pos.sonoran.seed)


# Northern Arizona --------------------------------------------------------

# From 10.3.R: pos.naz1.seed
pos.naz.seed <- glmmTMB(Seeded_Cover ~ Perc_dev_cum_abs + AridityIndex + Treatment + 
                           PlotMix_Climate + MAT + Sand_content + Since_last_precip + 
                           Days_elapsed + (1 | Site / Plot),
                         data = naz.seed, family = genpois)
summary(pos.naz.seed)
r2(pos.naz.seed)
res.pos.naz.seed <- simulateResiduals(pos.naz.seed)
plotQQunif(pos.naz.seed)
plotResiduals(pos.naz.seed) 
check_overdispersion(pos.naz.seed) # no overdispersion detected
check_zeroinflation(pos.naz.seed) # no zero-inflation
check_collinearity(pos.naz.seed)


save.image("RData/13.2_generalized-linear-models_2x2-Seeded-Cover_final.RData")
