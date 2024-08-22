# Created: 2024-08-22
# Last updated: 2024-08-22

# Purpose: Add 0s to Count and Height to track all seeded species across plots.
#   Currently, 0 in Count and Height only exist in completely empty plots, and GLMs are
#   overfitting 0s. I am adding back in the omitted 0s, because we would expect seeded
#   species to grow in their plots (or at least would like to track if they did),
#   and right now all we have is what was present.

# Address each mix separately (Current vs. Projected). Because some sites have the same
#   mixes, they can be handled together.


library(tidyverse)
library(readxl)

# Load data ---------------------------------------------------------------

subplot <- read_csv("data/cleaned/04.1_subplot-data_clean.csv")
mix <- read_xlsx("data/raw/from-Master_seed-mix_LO.xlsx", sheet = "with-site_R (2)")
species.in <- read_csv("data/cleaned/01_species-list_location-independent_clean.csv")


# Data wrangling ----------------------------------------------------------

# Create replacement species list (info for rows to be added of seeded species)
species <- species.in |> 
  select(-CodeOriginal) |> 
  distinct(.keep_all = TRUE) |> 
  mutate(CodeOriginal = Code,
         SpeciesSeeded = "Yes",
         PlantSource = "Seeded",
         PlantSource2 = "Seeded",
         Weedy = "Desirable")
species <- species[, c(6, 1:5, 7:10)]
species <- species |> 
  filter(Code %in% mix$Code)

# Find species in mixes that never grew
setdiff(mix$Code, species$Code)
species.add <- data.frame(CodeOriginal = c("POFE", "BOBA3", "GAPU", "LEDU", "RACO3", "PHTA", 
                                           "ABVI", "HYSAS", "ATPO", "CAER", "HENE5"),
                          Code = c("POFE", "BOBA3", "GAPU", "LEDU", "RACO3", "PHTA", 
                                   "ABVI", "HYSAS", "ATPO", "CAER", "HENE5"),
                          Name = c("Poa fendleriana", "Bothriochloa barbinodis","Gaillardia pulchella",
                                   "Leptochloa dubia", "Ratibida columnifera", "Phacelia tanacetifolia",
                                   "Abronia villosa", "Ambrosia salsola", "Atriplex polycarpa",
                                   "Calliandra eriophylla", "Hesperostipa neomexicana"),
                          Native = rep("Native", 11),
                          Duration = c("Perennial", "Perennial", "Unknown", "Perennial", "Perennial",
                                       "Annual", "Annual", rep("Perennial", 4)),
                          Lifeform = c("Grass", "Grass", "Forb", "Grass", rep("Forb", 3),
                                       rep("Shrub", 3), "Grass"),
                          SpeciesSeeded = rep("Yes", 11),
                          PlantSource = rep("Seeded", 11),
                          PlantSource2 = rep("Seeded", 11),
                          Weedy = rep("Desirable", 11))

# Add to species list
species <- species |> 
  bind_rows(species.add)




# Chihuahuan --------------------------------------------------------------

# Mixes at Creosote and Mesquite were the same

# Separate out Chihuahuan plots
chi <- subplot |> 
  filter(Region == "Chihuahuan")


## Current -----------------------------------------------------------------

# Separate out Current mix plots and species codes
chiC <- chi |> 
  filter(PlotMix_Climate == "Current")
chi.current.code <- mix |> 
  filter(Site == "Creosote",
         PlotMix_Climate == "Current") |> 
  select(Code)

# Check if any seeded species grew in plots
chiC |> 
  filter(SpeciesSeeded == "Yes") |> 
  select(Name, Code)

# Create template (df of all monitoring events and seeded species)
chiC.monitor <- chiC[, 1:11]
chiC.monitor <- chiC.monitor |> 
  distinct(.keep_all = TRUE)

chiC.template <- chiC.monitor |> 
  expand_grid(Code = chi.current.code$Code)
chiC.template <- chiC.template |> 
  left_join(species)

# Create df of replacement data for seeded species
chiC.replace <- chiC.template |> 
  left_join(chiC) |> 
  mutate(Count = ifelse(is.na(Count), 0, Count),
         Height = ifelse(is.na(Height), 0, Height))

# Add back in observed data of non-seeded species
chiC.replace <- chiC.replace |> 
  bind_rows(chiC) |> 
  distinct(.keep_all = TRUE)



## Projected --------------------------------------------------------------

# Separate out Projected mix plots and species codes
chiP <- chi |> 
  filter(PlotMix_Climate == "Projected")
chi.projected.code <- mix |> 
  filter(Site == "Creosote",
         PlotMix_Climate == "Projected") |> 
  select(Code)

# Check if any seeded species grew in plots
chiP |> 
  filter(SpeciesSeeded == "Yes") |> 
  select(Name, Code)

# Create separate df of observed seeded species (identified to species level)
chiP.seeded <- chiP |> 
  filter(Code %in% chi.projected.code$Code)

# Create template (df of all monitoring events and seeded species)
chiP.monitor <- chiP[, 1:11]
chiP.monitor <- chiP.monitor |> 
  distinct(.keep_all = TRUE)

chiP.template <- chiP.monitor |> 
  expand_grid(Code = chi.projected.code$Code)
chiP.template <- chiP.template |> 
  left_join(species)

# Create df of replacement data for seeded species
chiP.replace <- chiP.template |> 
  left_join(chiP) |> 
  mutate(Count = ifelse(is.na(Count), 0, Count),
         Height = ifelse(is.na(Height), 0, Height))

# Add back in observed data of non-seeded species
chiP.replace <- chiP.replace |> 
  bind_rows(chiP) |> 
  distinct(.keep_all = TRUE)
#   Total rows: 1852 
#     template rows (1552) + observed rows (320) - seeded observed rows (22) = 1850
#   discrepancy is inherent to data: sometimes the same species got more than one row in the same plot



# Colorado Plateau: Med-Warm ----------------------------------------------

# Med-Warm mix is Current for AguaFria, MOWE, PEFO, Spiderweb,
#   and Projected for BarTBar, FlyingM, CRC, Salt_Desert

# Separate out data
copC1 <- subplot |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb"),
         PlotMix_Climate == "Current")

copP1 <- subplot |> 
  filter(Site %in% c("BarTBar", "FlyingM", "CRC", "Salt_Desert"),
         PlotMix_Climate == "Projected")

medwarmCOP <- bind_rows(copC1, copP1)
  
# Separate out species codes
medwarmCOP.code <- mix |> 
  filter(Site == "AguaFria",
         PlotMix_Climate == "Current") |> 
  select(Code)

# Create separate df of observed seeded species (identified to species level)
medwarmCOP.seeded <- medwarmCOP |> 
  filter(Code %in% medwarmCOP.code$Code)

# Create template (df of all monitoring events and seeded species)
medwarmCOP.monitor <- medwarmCOP[, 1:11]
medwarmCOP.monitor <- medwarmCOP.monitor |> 
  distinct(.keep_all = TRUE)

medwarmCOP.template <- medwarmCOP.monitor |> 
  expand_grid(Code = medwarmCOP.code$Code)
medwarmCOP.template <- medwarmCOP.template |> 
  left_join(species)

# Create df of replacement data for seeded species
medwarmCOP.replace <- medwarmCOP.template |> 
  left_join(medwarmCOP) |> 
  mutate(Count = ifelse(is.na(Count), 0, Count),
         Height = ifelse(is.na(Height), 0, Height))

# Add back in observed data of non-seeded species
medwarmCOP.replace <- medwarmCOP.replace |> 
  bind_rows(medwarmCOP) |> 
  distinct(.keep_all = TRUE)
#   Total rows: 14687
#     template rows (10864) + observed rows (4061) - seeded observed rows (274) = 14651
#   discrepancy is inherent to data: sometimes the same species got more than one row in the same plot



# Colorado Plateau: Warm --------------------------------------------------

# Warm mix is Projected for AguaFria, MOWE, PEFO, Spiderweb, TLE

# Separate out data and species codes
warmCOP <- subplot |> 
  filter(Site %in% c("AguaFria", "MOWE", "PEFO", "Spiderweb", "TLE"),
         PlotMix_Climate == "Projected")
warmCOP.code <- mix |> 
  filter(Site == "AguaFria",
         PlotMix_Climate == "Projected") |> 
  select(Code)

# Create separate df of observed seeded species (identified to species level)
warmCOP.seeded <- warmCOP |> 
  filter(Code %in% warmCOP.code$Code)

# Create template (df of all monitoring events and seeded species)
warmCOP.monitor <- warmCOP[, 1:11]
warmCOP.monitor <- warmCOP.monitor |> 
  distinct(.keep_all = TRUE)

warmCOP.template <- warmCOP.monitor |> 
  expand_grid(Code = warmCOP.code$Code)
warmCOP.template <- warmCOP.template |> 
  left_join(species)

# Create df of replacement data for seeded species
warmCOP.replace <- warmCOP.template |> 
  left_join(warmCOP) |> 
  mutate(Count = ifelse(is.na(Count), 0, Count),
         Height = ifelse(is.na(Height), 0, Height))

# Add back in observed data of non-seeded species
warmCOP.replace <- warmCOP.replace |> 
  bind_rows(warmCOP) |> 
  distinct(.keep_all = TRUE)
#   Total rows: 7485
#     template rows (5817) + observed rows (1982) - seeded observed rows (384) = 7415
#   discrepancy is inherent to data: sometimes the same species got more than one row in the same plot



# Colorado Plateau: Cool-Medium -------------------------------------------

# Cool-Med mix is Current for BarTBar, FlyingM, CRC, Salt_Desert,
#   and Projected for BabbittPJ, UtahPJ

# Separate out data
copC2 <- subplot |> 
  filter(Site %in% c("BarTBar", "FlyingM", "CRC", "Salt_Desert"),
         PlotMix_Climate == "Current")

copP2 <- subplot |> 
  filter(Site %in% c("BabbittPJ", "UtahPJ"),
         PlotMix_Climate == "Projected")

coolmedCOP <- bind_rows(copC2, copP2)

# Separate out species codes
coolmedCOP.code <- mix |> 
  filter(Site == "BarTBar",
         PlotMix_Climate == "Current") |> 
  select(Code)

# Create separate df of observed seeded species (identified to species level)
coolmedCOP.seeded <- coolmedCOP |> 
  filter(Code %in% coolmedCOP.code$Code)

# Create template (df of all monitoring events and seeded species)
coolmedCOP.monitor <- coolmedCOP[, 1:11]
coolmedCOP.monitor <- coolmedCOP.monitor |> 
  distinct(.keep_all = TRUE)

coolmedCOP.template <- coolmedCOP.monitor |> 
  expand_grid(Code = coolmedCOP.code$Code)
coolmedCOP.template <- coolmedCOP.template |> 
  left_join(species)

# Create df of replacement data for seeded species
coolmedCOP.replace <- coolmedCOP.template |> 
  left_join(coolmedCOP) |> 
  mutate(Count = ifelse(is.na(Count), 0, Count),
         Height = ifelse(is.na(Height), 0, Height))

# Add back in observed data of non-seeded species
coolmedCOP.replace <- coolmedCOP.replace |> 
  bind_rows(coolmedCOP) |> 
  distinct(.keep_all = TRUE)
#   Total rows: 11331
#     template rows (8183) + observed rows (4075) - seeded observed rows (1259) = 10999
#   discrepancy is inherent to data: sometimes the same species got more than one row in the same plot



# Colorado Plateau: Cool --------------------------------------------------

# Cool mix is Current for BabbittPJ, UtahPJ, TLE

# Separate out data and species codes
coolCOP <- subplot |> 
  filter(Site %in% c("BabbittPJ", "UtahPJ", "TLE"),
         PlotMix_Climate == "Current")
coolCOP.code <- mix |> 
  filter(Site == "BabbittPJ",
         PlotMix_Climate == "Current") |> 
  select(Code)

# Create separate df of observed seeded species (identified to species level)
coolCOP.seeded <- coolCOP |> 
  filter(Code %in% coolCOP.code$Code)

# Create template (df of all monitoring events and seeded species)
coolCOP.monitor <- coolCOP[, 1:11]
coolCOP.monitor <- coolCOP.monitor |> 
  distinct(.keep_all = TRUE)

coolCOP.template <- coolCOP.monitor |> 
  expand_grid(Code = coolCOP.code$Code)
coolCOP.template <- coolCOP.template |> 
  left_join(species)

# Create df of replacement data for seeded species
coolCOP.replace <- coolCOP.template |> 
  left_join(coolCOP) |> 
  mutate(Count = ifelse(is.na(Count), 0, Count),
         Height = ifelse(is.na(Height), 0, Height))

# Add back in observed data of non-seeded species
coolCOP.replace <- coolCOP.replace |> 
  bind_rows(coolCOP) |> 
  distinct(.keep_all = TRUE)
#   Total rows: 4219
#     template rows (3136) + observed rows (1257) - seeded observed rows (297) = 4096
#   discrepancy is inherent to data: sometimes the same species got more than one row in the same plot



# Mojave ------------------------------------------------------------------

# Mixes at 29_Palms and AVRCD were the same

# Separate out Mojave plots
moj <- subplot |> 
  filter(Region == "Mojave")


## Current -----------------------------------------------------------------

# Separate out Current mix plots and species codes
mojC <- moj |> 
  filter(PlotMix_Climate == "Current")
moj.current.code <- mix |> 
  filter(Site == "AVRCD",
         PlotMix_Climate == "Current") |> 
  select(Code)

# Create template (df of all monitoring events and seeded species)
mojC.monitor <- mojC[, 1:11]
mojC.monitor <- mojC.monitor |> 
  distinct(.keep_all = TRUE)

mojC.template <- mojC.monitor |> 
  expand_grid(Code = moj.current.code$Code)
mojC.template <- mojC.template |> 
  left_join(species)

# Create df of replacement data for seeded species
mojC.replace <- mojC.template |> 
  left_join(mojC) |> 
  mutate(Count = ifelse(is.na(Count), 0, Count),
         Height = ifelse(is.na(Height), 0, Height))

# Add back in observed data of non-seeded species
mojC.replace <- mojC.replace |> 
  bind_rows(mojC) |> 
  distinct(.keep_all = TRUE)



## Projected --------------------------------------------------------------

# Separate out Projected mix plots and species codes
mojP <- moj |> 
  filter(PlotMix_Climate == "Projected")
moj.projected.code <- mix |> 
  filter(Site == "AVRCD",
         PlotMix_Climate == "Projected") |> 
  select(Code)

# Create separate df of observed seeded species (identified to species level)
mojP.seeded <- mojP |> 
  filter(Code %in% moj.projected.code$Code)

# Create template (df of all monitoring events and seeded species)
mojP.monitor <- mojP[, 1:11]
mojP.monitor <- mojP.monitor |> 
  distinct(.keep_all = TRUE)

mojP.template <- mojP.monitor |> 
  expand_grid(Code = moj.projected.code$Code)
mojP.template <- mojP.template |> 
  left_join(species)

# Create df of replacement data for seeded species
mojP.replace <- mojP.template |> 
  left_join(mojP) |> 
  mutate(Count = ifelse(is.na(Count), 0, Count),
         Height = ifelse(is.na(Height), 0, Height))

# Add back in observed data of non-seeded species
mojP.replace <- mojP.replace |> 
  bind_rows(mojP) |> 
  distinct(.keep_all = TRUE)




# Sonoran Central ---------------------------------------------------------

# Mixes at Preserve, SCC, Roosevelt, Pleasant were the same

# Separate out Sonoran Central plots
socen <- subplot |> 
  filter(Region == "Sonoran Central")


## Current -----------------------------------------------------------------

# Separate out Current mix plots and species codes
socenC <- socen |> 
  filter(PlotMix_Climate == "Current")
socen.current.code <- mix |> 
  filter(Site == "Preserve",
         PlotMix_Climate == "Current") |> 
  select(Code)

# Create template (df of all monitoring events and seeded species)
socenC.monitor <- socenC[, 1:11]
socenC.monitor <- socenC.monitor |> 
  distinct(.keep_all = TRUE)

socenC.template <- socenC.monitor |> 
  expand_grid(Code = socen.current.code$Code)
socenC.template <- socenC.template |> 
  left_join(species)

# Create df of replacement data for seeded species
socenC.replace <- socenC.template |> 
  left_join(socenC) |> 
  mutate(Count = ifelse(is.na(Count), 0, Count),
         Height = ifelse(is.na(Height), 0, Height))

# Add back in observed data of non-seeded species
socenC.replace <- socenC.replace |> 
  bind_rows(socenC) |> 
  distinct(.keep_all = TRUE)



## Projected --------------------------------------------------------------

# Separate out Projected mix plots and species codes
socenP <- socen |> 
  filter(PlotMix_Climate == "Projected")
socen.projected.code <- mix |> 
  filter(Site == "Preserve",
         PlotMix_Climate == "Projected") |> 
  select(Code)

# Create separate df of observed seeded species (identified to species level)
socenP.seeded <- socenP |> 
  filter(Code %in% socen.projected.code$Code)

# Create template (df of all monitoring events and seeded species)
socenP.monitor <- socenP[, 1:11]
socenP.monitor <- socenP.monitor |> 
  distinct(.keep_all = TRUE)

socenP.template <- socenP.monitor |> 
  expand_grid(Code = socen.projected.code$Code)
socenP.template <- socenP.template |> 
  left_join(species)

# Create df of replacement data for seeded species
socenP.replace <- socenP.template |> 
  left_join(socenP) |> 
  mutate(Count = ifelse(is.na(Count), 0, Count),
         Height = ifelse(is.na(Height), 0, Height))

# Add back in observed data of non-seeded species
socenP.replace <- socenP.replace |> 
  bind_rows(socenP) |> 
  distinct(.keep_all = TRUE)



# Sonoran SE --------------------------------------------------------------

# Mixes at SRER, Patagonia were the same

# Separate out Sonoran SE plots
sose <- subplot |> 
  filter(Region == "Sonoran SE")


## Current -----------------------------------------------------------------

# Separate out Current mix plots and species codes
soseC <- sose |> 
  filter(PlotMix_Climate == "Current")
sose.current.code <- mix |> 
  filter(Site == "SRER",
         PlotMix_Climate == "Current") |> 
  select(Code)

# Create template (df of all monitoring events and seeded species)
soseC.monitor <- soseC[, 1:11]
soseC.monitor <- soseC.monitor |> 
  distinct(.keep_all = TRUE)

soseC.template <- soseC.monitor |> 
  expand_grid(Code = sose.current.code$Code)
soseC.template <- soseC.template |> 
  left_join(species)

# Create df of replacement data for seeded species
soseC.replace <- soseC.template |> 
  left_join(soseC) |> 
  mutate(Count = ifelse(is.na(Count), 0, Count),
         Height = ifelse(is.na(Height), 0, Height))

# Add back in observed data of non-seeded species
soseC.replace <- soseC.replace |> 
  bind_rows(soseC) |> 
  distinct(.keep_all = TRUE)



## Projected --------------------------------------------------------------

# Separate out Projected mix plots and species codes
soseP <- sose |> 
  filter(PlotMix_Climate == "Projected")
sose.projected.code <- mix |> 
  filter(Site == "SRER",
         PlotMix_Climate == "Projected") |> 
  select(Code)

# Create separate df of observed seeded species (identified to species level)
soseP.seeded <- soseP |> 
  filter(Code %in% sose.projected.code$Code)

# Create template (df of all monitoring events and seeded species)
soseP.monitor <- soseP[, 1:11]
soseP.monitor <- soseP.monitor |> 
  distinct(.keep_all = TRUE)

soseP.template <- soseP.monitor |> 
  expand_grid(Code = sose.projected.code$Code)
soseP.template <- soseP.template |> 
  left_join(species)

# Create df of replacement data for seeded species
soseP.replace <- soseP.template |> 
  left_join(soseP) |> 
  mutate(Count = ifelse(is.na(Count), 0, Count),
         Height = ifelse(is.na(Height), 0, Height))

# Add back in observed data of non-seeded species
soseP.replace <- soseP.replace |> 
  bind_rows(soseP) |> 
  distinct(.keep_all = TRUE)



# Combine all -------------------------------------------------------------

none.plots <- subplot |> 
  filter(PlotMix_Climate == "None")

all.replace <- bind_rows(chiC.replace, chiP.replace, warmCOP.replace, medwarmCOP.replace,
                         coolmedCOP.replace, coolCOP.replace, mojC.replace, mojP.replace,
                         socenC.replace, socenP.replace, soseC.replace, soseP.replace,
                         none.plots)
all.replace <- all.replace |> 
  arrange(SiteDatePlotID)



# Write to CSV ------------------------------------------------------------

write_csv(all.replace,
          file = "data/cleaned/04.15_subplot-data_clean-0-added.csv")


save.image("RData/04.15_data-wrangling_subplot_add-0s.RData")
