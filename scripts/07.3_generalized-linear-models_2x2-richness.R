# Created: 2024-08-30
# Last updated: 2024-08-30

# Purpose: Run generalized linear models for 2x2 m plot data, with richness of 
#   Weedy & Desirable as response variable. 
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
  mutate(AridityIndex_log = log(AridityIndex))


# Separate out pellets
nopellet <- richcover |> 
  filter(Treatment != "Pellets")

## Separate out Sonoran sites (6) -----------------------------------------

sonoran <- richcover |> 
  filter(Region %in% c("Sonoran SE", "Sonoran Central"))

## Separate out Northern AZ sites (8) -------------------------------------

# All
naz <- richcover |> 
  filter(Region == "Colorado Plateau")



# All sites, desirable richness -------------------------------------------

## Poisson -----------------------------------------------------------------

# All variables, no random effect
pos.all.des0 <- glm(Desirable ~ Perc_dev_cum + AridityIndex_log + Treatment + 
                   PlotMix_Climate + MAT + MAP + Sand_content + Cum_precip_sqrt,
                 data = richcover, family = "poisson")
summary(pos.all.des0)
check_overdispersion(pos.all.des0) # overdispersion detected
check_zeroinflation(pos.all.des0) # zero-inflation detected
check_collinearity(pos.all.des0)

# All variables, with random effect
#   nested random effect takes care of disperion issue
pos.all.des <- glmmTMB(Desirable ~ Perc_dev_cum + AridityIndex_log + Treatment + 
                          PlotMix_Climate + MAT + MAP + Sand_content + Cum_precip_sqrt + 
                          (1 | Site / Plot),
                        data = richcover, family = genpois)
summary(pos.all.des)
r2(pos.all.des)
res.pos.all.des <- simulateResiduals(pos.all.des)
plotQQunif(pos.all.des)
plotResiduals(pos.all.des)
check_overdispersion(pos.all.des) # no overdispersion detected
check_zeroinflation(pos.all.des) # model is overfitting zeros
check_collinearity(pos.all.des)


## Zero-inflation negative binomial ---------------------------------------

# All variables, no random effect
zinb.all.des0 <- glmmTMB(Desirable ~ Perc_dev_cum + AridityIndex_log + Treatment + 
                       PlotMix_Climate + MAT + MAP + Sand_content + Cum_precip_sqrt,
                     data = richcover, family = nbinom2, ziformula = ~.)
summary(zinb.all.des0)
check_overdispersion(zinb.all.des0) # undispersion detected
check_zeroinflation(zinb.all.des0) # no zero-inflation detected


## Zero-inflated Poisson --------------------------------------------------

# All variables, no random effect
zip.all.des0 <- glmmTMB(Desirable ~ Perc_dev_cum + AridityIndex_log + Treatment + 
                 PlotMix_Climate + MAT + MAP + Sand_content + Cum_precip_sqrt,
               data = richcover, family = genpois, ziformula = ~.)
summary(zip.all.des0)
check_overdispersion(zip.all.des0) # no  overdispersion detected
check_zeroinflation(zip.all.des0) # no zero-inflation detected
check_collinearity(zip.all.des0)

# 01: Drop MAP & Cum_precip_sqrt (collinearity), no random effect
zip.all.des01 <- glmmTMB(Desirable ~ Perc_dev_cum + AridityIndex_log + Treatment + 
                          PlotMix_Climate + MAT + Sand_content,
                        data = richcover, family = genpois, ziformula = ~.)
summary(zip.all.des01)
check_overdispersion(zip.all.des01)
check_zeroinflation(zip.all.des01)
check_collinearity(zip.all.des01)

# All variables, nested random effect of Site/Plot: does not converge
zip.all.des <- glmmTMB(Desirable ~ Perc_dev_cum + AridityIndex_log + Treatment + 
                          PlotMix_Climate + MAT + MAP + Sand_content + Cum_precip_sqrt +
                         (1 | Site / Plot),
                        data = richcover, family = genpois, ziformula = ~.) # did not converge

# 1: Perc_dev_cum, AI_log, Treatment; nested random effect of Site/Plot: does not converge
zip.all.des1 <- glmmTMB(Desirable ~ Perc_dev_cum + AridityIndex + Treatment +
                         (1 | Site / Plot),
                       data = richcover, family = genpois, ziformula = ~.) # did not converge

# 1: Perc_dev_cum (quadratic), AI_log, Treatment; nested random effect of Site/Plot
zip.all.des2 <- glmmTMB(Desirable ~ I(Perc_dev_cum ^ 2) + AridityIndex + Treatment +
                          (1 | Site / Plot),
                        data = richcover, family = genpois, ziformula = ~.) 
summary(zip.all.des2)
check_overdispersion(zip.all.des2)
check_zeroinflation(zip.all.des2) # model is overfitting zeros
check_collinearity(zip.all.des2)


# Poisson with Perc_dev_cum as quadratic ----------------------------------

# All variables, with random effect
pos2.all.des <- glmmTMB(Desirable ~ I(Perc_dev_cum ^ 2) + AridityIndex_log + Treatment + 
                          PlotMix_Climate + MAT + MAP + Sand_content + Cum_precip_sqrt + 
                           (1 | Site / Plot),
                        data = richcover, family = genpois)
summary(pos2.all.des)
r2(pos2.all.des)
res.pos2.all.des <- simulateResiduals(pos2.all.des)
plotQQunif(pos2.all.des)
plotResiduals(pos2.all.des)
check_overdispersion(pos2.all.des) # no overdispersion detected
check_zeroinflation(pos2.all.des) # model is overfitting zeros
check_collinearity(pos2.all.des)


# nopellet, desirable richness --------------------------------------------

# I thought maybe the problem was unequal sample sizes with Pellet plots included,
#   but nopellet also does not converge.

## Zero-inflated Poisson --------------------------------------------------

# All variables, nested random effect of Site/Plot: did not converge
zip.nopellet.des <- glmmTMB(Desirable ~ Perc_dev_cum + AridityIndex_log + Treatment + 
                         PlotMix_Climate + MAT + MAP + Sand_content + Cum_precip_sqrt +
                         (1 | Site / Plot),
                       data = nopellet, family = genpois, ziformula = ~.) # does not converge

# 1:Perc_dev_cum, AI, Treatment; nested random effect of Site/Plot: did not converge
zip.nopellet.des1 <- glmmTMB(Desirable ~ Perc_dev_cum + AridityIndex + Treatment +
                          (1 | Site / Plot),
                        data = nopellet, family = genpois, ziformula = ~.) 
summary(zip.nopellet.des1)
