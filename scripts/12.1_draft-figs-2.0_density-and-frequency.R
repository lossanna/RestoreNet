# Created: 2024-09-25
# Last updated: 2024-10-28

# Purpose: Narrow down and improve figures from 09.1_draft-figs_precip-dev_subplot.R. In particular,
#   change Count to density (individuals per m2), and change N AZ Plateau to just Northern Arizona
#   (it contains both the Mountains and Plateau Level III ecoregions). Also frequency has changed a bit
#   since I updated (corrected) present_species. Update species of interest.

library(tidyverse)
library(scales)
library(viridis)
library(ggbreak)
library(ggpmisc)
library(ggpubr)

# Load data ---------------------------------------------------------------

subplot <- read_csv("data/cleaned/04.1_subplot-data_clean.csv")
prism.data <- read_csv("data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")
cum.pd <- read_csv("data/cleaned/03.3_cumulative-precip_percent-deviation-from-norm_clean.csv")
ai <- read_csv("data/cleaned/03.4_aridity-index-values_clean.csv")

sonoran.freq <- read_csv("data/cleaned/11.1_Sonoran-Desert_frequency_all_clean.csv")
sonoran.freq.interest.raw <- read_csv("data/cleaned/11.1_Sonoran-Desert_frequency_interest_clean.csv")
sonoran.freq.nativevolun <- read_csv("data/cleaned/11.1_Sonoran-Desert_frequency_native-volunteer_clean.csv")
sonoran.freq.weed <- read_csv("data/cleaned/11.1_Sonoran-Desert_frequency_weed_clean.csv")
sonoran.freq.current.raw <- read_csv("data/cleaned/11.1_Sonoran-Desert_frequency_seeded-current_clean.csv")
sonoran.freq.projected.raw <- read_csv("data/cleaned/11.1_Sonoran-Desert_frequency_seeded-projected_clean.csv")

naz.freq <- read_csv("data/cleaned/11.1_Northern-AZ_frequency_all_clean.csv")
naz.freq.interest.raw <- read_csv("data/cleaned/11.1_Northern-AZ_frequency_interest_clean.csv")
naz.freq.nativevolun <- read_csv("data/cleaned/11.1_Northern-AZ_frequency_native-volunteer_clean.csv")
naz.freq.weed <- read_csv("data/cleaned/11.1_Northern-AZ_frequency_weed_clean.csv")
naz.freq.current.raw <- read_csv("data/cleaned/11.1_Northern-AZ_frequency_seeded-current_clean.csv")
naz.freq.projected.raw <- read_csv("data/cleaned/11.1_Northern-AZ_frequency_seeded-projected_clean.csv")


# Data wrangling ----------------------------------------------------------

## Count -------------------------------------------------------------------

# Reorganize columns for left_join()
cum.pd.subplot <- cum.pd |> 
  select(Region, Site, SiteDateID, Date_Seeded, Date_Monitored, Perc_deviation, Deviation_mm) |> 
  rename(Perc_dev_cum = Perc_deviation,
         Dev_mm_cum = Deviation_mm)

# Combine all variables
dat <- subplot |> 
  left_join(prism.data) |> 
  left_join(ai) |> 
  left_join(cum.pd.subplot) 

# Check for NAs
apply(dat, 2, anyNA)

# Data without Infinity
dat <- dat |> 
  filter(Perc_dev_cum != Inf)

# Create Density column
dat <- dat |> 
  mutate(Density = Count / (0.25 * 0.25))

# Rename and reorder PlotMix_Climate
dat <- dat |> 
  mutate(PlotMix_Climate = case_when(
    PlotMix_Climate == "None" ~ "Not seeded",
    PlotMix_Climate == "Current" ~ "Current-adapted mix",
    PlotMix_Climate == "Projected" ~ "Projected-adapted mix"))
dat$PlotMix_Climate <- factor(dat$PlotMix_Climate,
                              levels = c("Not seeded", "Current-adapted mix", "Projected-adapted mix"))

# Rename weedy PlantSource2
dat <- dat |> 
  mutate(PlantSource2 = case_when(
    PlantSource2 == "Introduced/Invasive" ~ "Invasive",
    PlantSource2 == "Recruit" ~ "Unknown recruit",
    TRUE ~ PlantSource2))


# Separate out count of species of interest 
sonoran.count.interest <- dat |> 
  filter(Code == "SACO6" & PlotMix_Climate == "Current-adapted mix"| 
           Code == "LUSP2" & PlotMix_Climate == "Current-adapted mix" | 
           Code == "PLOV" & PlotMix_Climate == "Projected-adapted mix" & SpeciesSeeded == "Yes" | 
           Code == "SECO10" & PlotMix_Climate == "Projected-adapted mix" | 
           Code == "ARPU9" & PlotMix_Climate == "Projected-adapted mix" |
           Code %in% c("LOHU2", "LOAR12", "VUOC",
                       "ERCI6", "SCBA", "BRRU2"),
         Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  mutate(Code = factor(Code, levels = c("SACO6", "LUSP2", "PLOV", "SECO10", "ARPU9", 
                                        "LOHU2", "LOAR12", "VUOC", 
                                        "ERCI6", "SCBA", "BRRU2")))

naz.count.interest <- dat |> 
  filter(Code == "LECI4" & PlotMix_Climate == "Current-adapted mix"| 
           Code == "HEBO" & PlotMix_Climate == "Current-adapted mix" | 
           Code == "HECO26" & PlotMix_Climate == "Current-adapted mix" | 
           Code == "LILE3" & PlotMix_Climate == "Current-adapted mix" & SpeciesSeeded == "Yes" |
           Code == "BAMU" & PlotMix_Climate == "Projected-adapted mix" | 
           Code == "PASM" & PlotMix_Climate == "Projected-adapted mix" |
           Code == "ASTU" & PlotMix_Climate == "Projected-adapted mix" |
           Code %in% c("ATCO", "SOEL", "LEPA6", "SATR12"),
         Region == "Colorado Plateau") |> 
  mutate(Code = factor(Code, levels = c("LECI4", "HEBO", "HECO26", "LILE3", 
                                        "BAMU", "PASM", "ASTU", 
                                        "ATCO", "SOEL", "LEPA6", "SATR12")))


## Frequency --------------------------------------------------------------

# Attach Lifeform and Duration info to seeded species
seed.lifeform.duration <- dat |> 
  filter(Region %in% c("Colorado Plateau", "Sonoran Central", "Sonoran SE"),
         SpeciesSeeded == "Yes") |> 
  select(Code, Lifeform, Duration) |> 
  distinct(.keep_all = TRUE) |> 
  add_row(Code = "Empty", Lifeform = "Empty", Duration = "Empty") |> 
  add_row(Code = "ENFA", Lifeform = "Shrub", Duration = "Perennial") |> 
  add_row(Code = "PSSP6",  Lifeform = "Grass", Duration = "Perennial") |> 
  add_row(Code = "SPGR2", Lifeform = "Forb", Duration = "Perennial") |> 
  add_row(Code = "PLMU3", Lifeform = "Grass", Duration = "Perennial") |> 
  mutate(Duration = case_when(
    Duration == "Unknown" ~ "Both",
    TRUE ~ Duration))  

sonoran.freq.current <- left_join(sonoran.freq.current.raw, seed.lifeform.duration) |>
  mutate(Type2 = paste0(Lifeform, ", ", Plot)) 
sonoran.freq.projected <- left_join(sonoran.freq.projected.raw, seed.lifeform.duration) |>
  mutate(Type2 = paste0(Lifeform, ", ", Plot)) 
naz.freq.current <- left_join(naz.freq.current.raw, seed.lifeform.duration) |>
  mutate(Type2 = paste0(Lifeform, ", ", Plot)) 
naz.freq.projected <- left_join(naz.freq.projected.raw, seed.lifeform.duration) |>
  mutate(Type2 = paste0(Lifeform, ", ", Plot))   

# Reorder frequency for species of interest
sonoran.freq.interest <- sonoran.freq.interest.raw |> 
  mutate(Plant = factor(Plant, levels = c("Current mix", "Projected mix",
                                          "Native recruit", "Invasive",
                                          "Empty")),
         Code = factor(Code, levels = c("SACO6", "LUSP2", "PLOV", "SECO10", "ARPU9", 
                                        "LOHU2", "LOAR12", "VUOC", 
                                        "ERCI6", "SCBA", "BRRU2", "Empty"))) 

naz.freq.interest <- naz.freq.interest.raw |> 
  mutate(Plant = factor(Plant, levels = c("Current mix", "Projected mix",
                                          "Native recruit", "Invasive",
                                          "Empty")),
         Code = factor(Code, levels = c("LECI4", "HEBO", "HECO26", "LILE3",  
                                        "BAMU", "PASM", "ASTU", 
                                        "ATCO", "SOEL", "LEPA6", "SATR12", "Empty"))) 



# Sonoran Desert ----------------------------------------------------------

## Precip deviation -------------------------------------------------------

# Cumulative
cum.pd.sonoran <- cum.pd |> 
  filter(Perc_deviation != Inf) |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  mutate(Site = factor(Site, 
                       levels = c("SRER", "Patagonia", "Roosevelt", "Preserve", "Pleasant", "SCC"))) |> 
  ggplot(aes(x = Date_Monitored, y = Perc_deviation)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  ggtitle("Sonoran Desert precipitation conditions") +
  xlab(NULL) +
  ylab("Cumulative precip deviation from normals") +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black")) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) 
cum.pd.sonoran


## Count ------------------------------------------------------------------

# Desirable by PlotMix_Climate and PlantSource2 (native recruit outliers removed)
sonoran.des.count.plotmixclimate <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  filter(Density < 1000) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  labs(title = "Sonoran Desert desirable species",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
sonoran.des.count.plotmixclimate

# Weedy as single panel by PlantSource2
sonoran.weed.count <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  labs(title = "Sonoran Desert weedy species",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  theme(plot.margin = margin(0.1, 0.2, 0.2, 0.2, "in")) 
sonoran.weed.count


### SD: Seeded species & species of interest ------------------------------

# By seeded species: Current
sonoran.seed.count.current.species <- dat |> 
  filter(PlotMix_Climate == "Current-adapted mix",
         SpeciesSeeded == "Yes") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  mutate(Code = factor(Code, levels = c("SACO6", "LUSP2", "SPAM2", "BAMU", "MATA2", "HEMU3", 
                                        "DICA8", "SPCR", "ELEL5", "POSE", "BOGR2",
                                        "AMDE4"))) |> 
  mutate(Duration = case_when(
    Duration == "Unknown" ~ "Both",
    TRUE ~ Duration)) |> 
  mutate(Duration = factor(Duration, levels = c("Annual", "Perennial", "Both"))) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  labs(title = "Sonoran Desert seeded species (current-adapted mix)",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(17, 19, 15)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
sonoran.seed.count.current.species

# By seeded species: Current - alternate order
sonoran.seed.count.current.species.alt <- dat |> 
  filter(PlotMix_Climate == "Current-adapted mix",
         SpeciesSeeded == "Yes") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  mutate(Code = factor(Code, levels = c("SACO6", "LUSP2", "SPAM2", "BAMU", "MATA2", "DICA8",
                                        "AMDE4", "ELEL5", "POSE", "SPCR",  
                                        "BOGR2", "HEMU3"))) |> 
  mutate(Duration = case_when(
    Duration == "Unknown" ~ "Both",
    TRUE ~ Duration)) |> 
  mutate(Duration = factor(Duration, levels = c("Annual", "Perennial", "Both"))) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  labs(title = "Sonoran Desert seeded species (current-adapted mix)",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(17, 19, 15)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
sonoran.seed.count.current.species.alt

# By seeded species: Projected
sonoran.seed.count.projected.species <- dat |> 
  filter(PlotMix_Climate == "Projected-adapted mix",
         SpeciesSeeded == "Yes") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  mutate(Duration = case_when(
    Duration == "Unknown" ~ "Both",
    TRUE ~ Duration)) |> 
  mutate(Duration = factor(Duration, levels = c("Annual", "Perennial", "Both")),
         Code = factor(Code, levels = c("SECO10", "PLOV", "ASTU", "BAMU", "ARPU9", "BOCU", 
                                        "BORO2", "BOAR",
                                        "ENFA"))) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  labs(title = "Sonoran Desert seeded species (projected-adapted mix)",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(17, 19, 15)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
sonoran.seed.count.projected.species

# By seeded species: Projected - alternate order
sonoran.seed.count.projected.species.alt <- dat |> 
  filter(PlotMix_Climate == "Projected-adapted mix",
         SpeciesSeeded == "Yes") |> 
  filter(Region %in% c("Sonoran Central", "Sonoran SE")) |> 
  mutate(Duration = case_when(
    Duration == "Unknown" ~ "Both",
    TRUE ~ Duration)) |> 
  mutate(Duration = factor(Duration, levels = c("Annual", "Perennial", "Both")),
         Code = factor(Code, levels = c("PLOV", "SECO10", "ARPU9",  "BORO2", "BOCU",
                                        "ASTU", "BAMU", "ENFA", "BOAR"))) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  labs(title = "Sonoran Desert seeded species (projected-adapted mix)",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(17, 19, 15)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
sonoran.seed.count.projected.species.alt

# Species of interest
sonoran.species.count <- sonoran.count.interest |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = Lifeform),
             alpha = 0.7) +
  facet_wrap(~Code) +
  labs(title = "Sonoran Desert species of interest",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(19, 17)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) 
sonoran.species.count



## Frequency --------------------------------------------------------------

# Native recruit, all plots
sonoran.freq.nativevolun |> 
  filter(Plot == "total") |> 
  ggplot(aes(x = Code, y = Frequency)) +
  geom_bar(stat = "identity") +
  ggtitle("Sonoran Desert, native recruit, all plots") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))

# Native recruit, wetter/drier
sonoran.freq.nativevolun |> 
  filter(Plot %in% c("wetter", "drier")) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert, native recruit") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom")

# Native recruit, extremes
sonoran.freq.nativevolun |> 
  filter(Plot %in% c("very wet", "very dry")) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert, native recruit") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom")

# Weedy, all plots
sonoran.freq.weed |> 
  filter(Plot == "total") |> 
  ggplot(aes(x = Code, y = Frequency)) +
  geom_bar(stat = "identity") +
  ggtitle("Sonoran Desert, weeds, all plots") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) 

# Weedy, extremes
sonoran.freq.weed |> 
  filter(Plot %in% c("very wet", "very dry")) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert, weeds") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom")


### SD: Seeded species ----------------------------------------------------

# Current mix, all plots
sonoran.current.total <- sonoran.freq.current |> 
  filter(Plot == "total") |> 
  mutate(Lifeform = factor(Lifeform, levels = c("Forb", "Grass", "Shrub", "Empty")),
         Code = factor(Code, levels = c("SACO6", "LUSP2", "SPAM2", "BAMU", "MATA2", "HEMU3", 
                                        "DICA8", "SPCR", "ELEL5", "POSE", "BOGR2",
                                        "AMDE4", "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Lifeform)) +
  geom_bar(stat = "identity") +
  ggtitle("Sonoran Desert current-adapted mix, all conditions") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  scale_fill_manual(values = c("#8DA0CB", "#66C2A5", "#FC8D62", "#B3B3B3")) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35))  +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
sonoran.current.total

# Current mix, wetter/drier
sonoran.current.wetdry <- sonoran.freq.current |> 
  filter(Plot %in% c("wetter", "drier")) |> 
  add_row(Code = "HEMU3",
          Frequency = 0,
          Type2 = "Forb, drier") |> 
  add_row(Code = "ELEL5",
          Frequency = 0,
          Type2 = "Grass, drier") |> 
  add_row(Code = "BOGR2",
          Frequency = 0,
          Type2 = "Grass, drier") |> 
  mutate(Type2 = factor(Type2, levels = c("Forb, drier", "Forb, wetter", 
                                             "Grass, drier", "Grass, wetter",
                                             "Shrub, drier", "Shrub, wetter", 
                                             "Empty, drier", "Empty, wetter")),
         Code = factor(Code, levels = c("SACO6", "LUSP2", "SPAM2", "BAMU", "MATA2", "HEMU3", 
                                        "DICA8", "SPCR", "ELEL5", "POSE", "BOGR2",
                                        "AMDE4", "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type2)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert current-adapted mix, wetter vs. drier conditions") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  scale_fill_manual(values = c("#B3C8E8", "#7570B3", "#B3E2D6", "#1B9E77", "#FBB4AE", "#D95F02",
                               "#D9D9D9", "#666666")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom")
sonoran.current.wetdry

# Current mix, extremes
sonoran.current.ex <- sonoran.freq.current |> 
  filter(Plot %in% c("very wet", "very dry")) |> 
  add_row(Code = "MATA2",
          Frequency = 0,
          Type2 = "Forb, very dry") |> 
  add_row(Code = "HEMU3",
          Frequency = 0,
          Type2 = "Forb, very dry") |> 
  add_row(Code = "SPCR",
          Frequency = 0,
          Type2 = "Grass, very dry") |> 
  add_row(Code = "ELEL5",
          Frequency = 0,
          Type2 = "Grass, very dry") |> 
  add_row(Code = "POSE",
          Frequency = 0,
          Type2 = "Grass, very dry") |> 
  add_row(Code = "BOGR2",
          Frequency = 0,
          Type2 = "Grass, very dry") |> 
  add_row(Code = "BOGR2",
          Frequency = 0,
          Type2 = "Grass, very wet") |> 
  mutate(Type2 = factor(Type2, levels = c("Forb, very dry", "Forb, very wet", 
                                          "Grass, very dry", "Grass, very wet",
                                          "Shrub, very dry", "Shrub, very wet", 
                                          "Empty, very dry", "Empty, very wet")),
         Code = factor(Code, levels = c("SACO6", "LUSP2", "SPAM2", "BAMU", "MATA2", "HEMU3", 
                                        "DICA8", "SPCR", "ELEL5", "POSE", "BOGR2",
                                        "AMDE4", "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type2)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert current-adapted mix, highly variable precipitation") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#B3C8E8", "#7570B3", "#B3E2D6", "#1B9E77", "#FBB4AE", "#D95F02",
                               "#D9D9D9", "#666666")) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
sonoran.current.ex

# Current mix, all plots - alternate order
sonoran.current.total.alt <- sonoran.freq.current |> 
  filter(Plot == "total") |> 
  mutate(Lifeform = factor(Lifeform, levels = c("Forb", "Grass", "Shrub", "Empty")),
         Code = factor(Code, levels = c("SACO6", "LUSP2", "SPAM2", "BAMU", "MATA2", "DICA8",
                                        "AMDE4", "ELEL5", "POSE", "SPCR",  
                                        "BOGR2", "HEMU3",
                                         "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Lifeform)) +
  geom_bar(stat = "identity") +
  ggtitle("Sonoran Desert current-adapted mix, all conditions") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  scale_fill_manual(values = c("#8DA0CB", "#66C2A5", "#FC8D62", "#B3B3B3")) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35))  +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
sonoran.current.total.alt

# Current mix, extremes - alternate order
sonoran.current.ex.alt <- sonoran.freq.current |> 
  filter(Plot %in% c("very wet", "very dry")) |> 
  add_row(Code = "MATA2",
          Frequency = 0,
          Type2 = "Forb, very dry") |> 
  add_row(Code = "HEMU3",
          Frequency = 0,
          Type2 = "Forb, very dry") |> 
  add_row(Code = "SPCR",
          Frequency = 0,
          Type2 = "Grass, very dry") |> 
  add_row(Code = "ELEL5",
          Frequency = 0,
          Type2 = "Grass, very dry") |> 
  add_row(Code = "POSE",
          Frequency = 0,
          Type2 = "Grass, very dry") |> 
  add_row(Code = "BOGR2",
          Frequency = 0,
          Type2 = "Grass, very dry") |> 
  add_row(Code = "BOGR2",
          Frequency = 0,
          Type2 = "Grass, very wet") |> 
  mutate(Type2 = factor(Type2, levels = c("Forb, very dry", "Forb, very wet", 
                                          "Grass, very dry", "Grass, very wet",
                                          "Shrub, very dry", "Shrub, very wet", 
                                          "Empty, very dry", "Empty, very wet")),
         Code = factor(Code, levels = c("SACO6", "LUSP2", "SPAM2", "BAMU", "MATA2", "DICA8",
                                        "AMDE4", "ELEL5", "POSE", "SPCR",  
                                        "BOGR2", "HEMU3",
                                        "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type2)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert current-adapted mix, highly variable precipitation") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#B3C8E8", "#7570B3", "#B3E2D6", "#1B9E77", "#FBB4AE", "#D95F02",
                               "#D9D9D9", "#666666")) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
sonoran.current.ex.alt

# Projected mix, all plots
sonoran.projected.total <- sonoran.freq.projected |> 
  filter(Plot == "total") |> 
  mutate(Lifeform = factor(Lifeform, levels = c("Forb", "Grass", "Shrub", "Empty")),
         Code = factor(Code, levels = c("SECO10", "PLOV", "ASTU", "BAMU", "ARPU9", "BOCU", 
                                        "BORO2", "BOAR",
                                        "ENFA", "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Lifeform)) +
  geom_bar(stat = "identity") +
  ggtitle("Sonoran Desert projected-adapted mix, all conditions") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  scale_fill_manual(values = c("#8DA0CB", "#66C2A5", "#FC8D62", "#B3B3B3")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
sonoran.projected.total

# Projected mix, wetter/drier
sonoran.projected.wetdry <- sonoran.freq.projected |> 
  filter(Plot %in% c("wetter", "drier")) |> 
  add_row(Code = "BAMU",
          Frequency = 0,
          Type2 = "Forb, drier") |>
  add_row(Code = "BOAR",
          Frequency = 0,
          Type2 = "Grass, drier") |>
  mutate(Type2 = factor(Type2, levels = c("Forb, drier", "Forb, wetter", 
                                          "Grass, drier", "Grass, wetter",
                                          "Shrub, drier", "Shrub, wetter", 
                                          "Empty, drier", "Empty, wetter")),
         Code = factor(Code, levels = c("SECO10", "PLOV", "ASTU", "BAMU", "ARPU9", "BOCU", 
                                        "BORO2", "BOAR",
                                        "ENFA", "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type2)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert projected-adapted mix, wetter vs. drier conditions") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#B3C8E8", "#7570B3", "#B3E2D6", "#1B9E77", "#FBB4AE", "#D95F02",
                               "#D9D9D9", "#666666")) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom")
sonoran.projected.wetdry

# Projected mix, extremes
sonoran.projected.ex <- sonoran.freq.projected |> 
  filter(Plot %in% c("very wet", "very dry")) |> 
  add_row(Code = "ASTU",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "BAMU",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "BORO2",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  add_row(Code = "BOAR",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "BOAR",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  mutate(Type2 = factor(Type2, levels = c("Forb, very dry", "Forb, very wet", 
                                          "Grass, very dry", "Grass, very wet",
                                          "Shrub, very dry", "Shrub, very wet", 
                                          "Empty, very dry", "Empty, very wet")),
         Code = factor(Code, levels = c("SECO10", "PLOV", "ASTU", "BAMU", "ARPU9", "BOCU", 
                                        "BORO2", "BOAR",
                                        "ENFA", "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type2)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert projected-adapted mix, highly variable precipitation") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  scale_fill_manual(values = c("#B3C8E8", "#7570B3", "#B3E2D6", "#1B9E77", "#FBB4AE", "#D95F02",
                               "#D9D9D9", "#666666")) +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
sonoran.projected.ex

# Projected mix, all plots - alternate order
sonoran.projected.total.alt <- sonoran.freq.projected |> 
  filter(Plot == "total") |> 
  mutate(Lifeform = factor(Lifeform, levels = c("Forb", "Grass", "Shrub", "Empty")),
         Code = factor(Code, levels = c("PLOV", "SECO10", "ARPU9",  "BORO2", "BOCU",
                                        "ASTU", "BAMU", "ENFA", "BOAR",
                                         "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Lifeform)) +
  geom_bar(stat = "identity") +
  ggtitle("Sonoran Desert projected-adapted mix, all conditions") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  scale_fill_manual(values = c("#8DA0CB", "#66C2A5", "#FC8D62", "#B3B3B3")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
sonoran.projected.total.alt

# Projected mix, extremes - alternate order
sonoran.projected.ex.alt <- sonoran.freq.projected |> 
  filter(Plot %in% c("very wet", "very dry")) |> 
  add_row(Code = "ASTU",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "BAMU",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "BORO2",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  add_row(Code = "BOAR",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "BOAR",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  mutate(Type2 = factor(Type2, levels = c("Forb, very dry", "Forb, very wet", 
                                          "Grass, very dry", "Grass, very wet",
                                          "Shrub, very dry", "Shrub, very wet", 
                                          "Empty, very dry", "Empty, very wet")),
         Code = factor(Code, levels = c("PLOV", "SECO10", "ARPU9",  "BORO2", "BOCU",
                                        "ASTU", "BAMU", "ENFA", "BOAR",
                                        "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type2)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert projected-adapted mix, highly variable precipitation") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  scale_fill_manual(values = c("#B3C8E8", "#7570B3", "#B3E2D6", "#1B9E77", "#FBB4AE", "#D95F02",
                               "#D9D9D9", "#666666")) +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
sonoran.projected.ex.alt


### SD: Species of interest -----------------------------------------------

# Species of interest, all plots
sonoran.species.total <- sonoran.freq.interest |> 
  filter(Plot == "total") |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plant)) +
  geom_bar(stat = "identity") +
  ggtitle("Sonoran Desert species of interest, all conditions") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#E5C494", "#FC8D62", "#66C2A5", "#8DA0CB", "#B3B3B3")) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
sonoran.species.total

# Species of interest, paired wetter & drier
sonoran.species.wetdry <- sonoran.freq.interest |> 
  filter(Plot %in% c("wetter", "drier")) |> 
  mutate(Type = factor(Type, 
                       levels = c("Current mix, drier", "Current mix, wetter",
                                  "Projected mix, drier", "Projected mix, wetter",
                                  "Native recruit, drier", "Native recruit, wetter",
                                  "Invasive, drier", "Invasive, wetter",
                                  "Empty, drier", "Empty, wetter"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert species of interest, wetter vs. drier conditions") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#E5D4A7", "#A6761D", "#FBB4AE", "#D95F02",
                               "#B3E2D6", "#1B9E77", "#B3C8E8", "#7570B3",
                               "#D9D9D9", "#666666")) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom")
sonoran.species.wetdry

# Species of interest, very wet & very dry (extremes)
sonoran.species.ex <- sonoran.freq.interest |> 
  filter(Plot %in% c("very wet", "very dry")) |> 
  mutate(Type = factor(Type, 
                       levels = c("Current mix, very dry", "Current mix, very wet",
                                  "Projected mix, very dry", "Projected mix, very wet",
                                  "Native recruit, very dry", "Native recruit, very wet",
                                  "Invasive, very dry", "Invasive, very wet",
                                  "Empty, very dry", "Empty, very wet"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Sonoran Desert species of interest, highly variable precipitation") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#E5D4A7", "#A6761D", "#FBB4AE", "#D95F02",
                               "#B3E2D6", "#1B9E77", "#B3C8E8", "#7570B3",
                               "#D9D9D9", "#666666")) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom")  +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
sonoran.species.ex



# Northern Arizona --------------------------------------------------------

## Precip deviation -------------------------------------------------------

# Cumulative
vlines <- data.frame(
  Site = factor(c("FlyingM", "BabbittPJ", "BarTBar", "MOWE", "PEFO", "Spiderweb")),
  Date_Monitored = as.Date(c("2020-07-17", "2020-07-16", "2020-07-20",
                             "2020-07-22", "2020-07-23", "2020-07-21"))
)
cum.pd.naz <- cum.pd |> 
  filter(Perc_deviation != Inf) |> 
  filter(Region == "Colorado Plateau") |> 
  mutate(Site = factor(Site,
                       levels = c("FlyingM", "BabbittPJ", "BarTBar", "AguaFria",
                                  "MOWE", "PEFO", "Spiderweb", "TLE"))) |> 
  ggplot(aes(x = as.Date(Date_Monitored), y = Perc_deviation)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  ggtitle("Northern Arizona precipitation conditions") +
  xlab(NULL) +
  ylab("Cumulative precip deviation from normals") +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(data = vlines,
             aes(xintercept = Date_Monitored),
             linetype = "dashed",
             color = "blue") +
  theme(axis.text.x = element_text(color = "black")) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) 
cum.pd.naz


## Count ------------------------------------------------------------------

# Desirable by PlotMix_Climate and PlantSource2 (native recruit outliers removed)
naz.des.count.plotmixclimate <- dat |> 
  filter(Weedy != "Weedy") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Density < 500) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  facet_wrap(~PlotMix_Climate) +
  labs(title = "Northern Arizona desirable species",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(20, 17, 15)) +
  scale_color_manual(values = c("#8DA0CB", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)  +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
naz.des.count.plotmixclimate

# Weedy as single panel by PlantSource2 (outliers removed)
naz.weed.count <- dat |> 
  filter(Weedy != "Desirable") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(Density < 1000) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = PlantSource2),
             alpha = 0.7) +
  geom_smooth() +
  labs(title = "Northern Arizona weedy species",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(20, 15, 17)) +
  scale_color_manual(values = c("#8DA0CB", "#D95F02", "#1B9E77")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  theme(plot.margin = margin(0.1, 0.2, 0.2, 0.2, "in")) 
naz.weed.count

# By known native recruits: narrow down contenders for best under var precip
dat |> 
  filter(Code %in% c("LEPA6", "CHEN.BabbittPJ", "CHAL11", "PHNE3", "DAPU7", "TOIN", "PLPA2", "SCMU6",
                     "HESP.BabbittPJ", "SPSP.BarTBar")) |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau, native recruits") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 17)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 

# By known weeds: narrow down contenders for best under var precip
dat |> 
  filter(Code %in% c("UNFO1.FlyingM", "UNGR1.MOWE", "SATR12", "BRNI", "UNFO8.BarTBar", 
                     "UNGR.BarTBar", "UNGR.FlyingM", "HAGL", "BRRU2")) |> 
  filter(Region == "Colorado Plateau") |> 
  ggplot(aes(x = Perc_dev_cum, y = Count)) +
  geom_point(aes(color = Lifeform,
                 shape = Lifeform),
             alpha = 0.7) +
  facet_wrap(~Code) +
  ggtitle("Northern Arizona Plateau, native recruits") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(19, 17)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 


### N AZ: Seeded species & species of interest -----------------------------

# By seeded species: Current
naz.seed.count.current.species <- dat |> 
  filter(PlotMix_Climate == "Current-adapted mix",
         SpeciesSeeded == "Yes") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(!str_detect(Code, "SPP|SUNGR|UNFO|UNGR")) |> 
  mutate(Code = factor(Code, levels = c("LILE3", "DACA7", "HEBO", "ACMI2", "MATA2", "PEPA8", 
                                        "SPGR2", "HEMU3", "PASM", "LECI4", "HECO26", "ELEL5", "ELTR7",
                                        "ELWA2", "BOGR2", "SPCR", "POSE", "PLJA", "BOER4", "PSSP6",
                                        "KRLA2"))) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  labs(title = "Northern Arizona seeded species (current-adapted mix)",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(17, 19, 15)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
naz.seed.count.current.species

# By seeded species: Current - alternate order
naz.seed.count.current.species.alt <- dat |> 
  filter(PlotMix_Climate == "Current-adapted mix",
         SpeciesSeeded == "Yes") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(!str_detect(Code, "SPP|SUNGR|UNFO|UNGR")) |> 
  mutate(Code = factor(Code, levels = c("LECI4", "HEBO", "HECO26", "LILE3", "PASM",
                                        "DACA7", "ELEL5","ACMI2", "ELTR7", "ELWA2", 
                                        "BOGR2", "MATA2", "PEPA8", "SPCR", "POSE",
                                        "KRLA2", "PLJA", "SPGR2", "BOER4",     
                                        "PSSP6", "HEMU3"))) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  labs(title = "Northern Arizona seeded species (current-adapted mix)",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(17, 19, 15)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
naz.seed.count.current.species.alt

# By seeded species: Projected
naz.seed.count.projected.species <- dat |> 
  filter(PlotMix_Climate == "Projected-adapted mix",
         SpeciesSeeded == "Yes") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(!str_detect(Code, "SPP|SUNGR|UNFO|UNGR")) |> 
  mutate(Duration = case_when(
    Duration == "Unknown" ~ "Both",
    TRUE ~ Duration)) |> 
  mutate(Duration = factor(Duration, levels = c("Annual", "Perennial", "Both"))) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  labs(title = "Northern Arizona seeded species (projected-adapted mix)",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(17, 19, 15)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) 
naz.seed.count.projected.species

# By seeded species: Projected - alternate order
naz.seed.count.projected.species.alt <- dat |> 
  filter(PlotMix_Climate == "Projected-adapted mix",
         SpeciesSeeded == "Yes") |> 
  filter(Region == "Colorado Plateau") |> 
  filter(!str_detect(Code, "SPP|SUNGR|UNFO|UNGR")) |> 
  mutate(Duration = case_when(
    Duration == "Unknown" ~ "Both",
    TRUE ~ Duration)) |> 
  mutate(Duration = factor(Duration, levels = c("Annual", "Perennial", "Both"))) |> 
  mutate(Code = factor(Code, levels = c("BAMU", "PASM", "ASTU", "ACHY", "LILE3", "SECO10",
                                        "ELEL5", "BOER4", "ACMI2", "POSE", "KRLA2",
                                        "PLJA", "BOCU", "ARPU9", "HEMU3", "BOGR2",
                                        "SPCR", "DACA7", "PEPA8", "MATA2",
                                        "PLMU3"))) |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = Lifeform,
                 shape = Duration),
             alpha = 0.7) +
  facet_wrap(~Code) +
  labs(title = "Northern Arizona seeded species (projected-adapted mix)",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Cumulative precip deviation from normals") +
  scale_shape_manual(values = c(17, 19, 15)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) 
naz.seed.count.projected.species.alt


# Species of interest
naz.species.count <- naz.count.interest |> 
  ggplot(aes(x = Perc_dev_cum, y = Density)) +
  geom_point(aes(color = PlantSource2,
                 shape = Lifeform),
             alpha = 0.7) +
  facet_wrap(~Code) +
  labs(title = "Northern Arizona species of interest",
       x = "Cumulative precipitation deviation from normals",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(19, 17, 15)) +
  scale_color_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) 
naz.species.count


## Frequency --------------------------------------------------------------

# Native recruit, all plots
naz.freq.nativevolun |> 
  filter(Plot == "total") |> 
  ggplot(aes(x = Code, y = Frequency)) +
  geom_bar(stat = "identity") +
  ggtitle("Northern Arizona, top native recruits, all plots") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))

# Native recruit, wetter/drier
naz.freq.nativevolun |> 
  filter(Plot %in% c("wetter", "drier")) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona, native recruit") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))

# Native recruit, extremes
naz.freq.nativevolun |> 
  filter(Plot %in% c("very wet", "very dry")) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona, native recruit") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))

# Weedy, all plots
naz.freq.weed |> 
  filter(Plot == "total") |> 
  filter(Code != "Empty") |> 
  ggplot(aes(x = Code, y = Frequency)) +
  geom_bar(stat = "identity") +
  ggtitle("Northern Arizona, weeds, all plots") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))

# Weedy, wetter/drier
naz.freq.weed |> 
  filter(Plot %in% c("wetter", "drier")) |> 
  filter(Code != "Empty") |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona, weeds") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))

# Weedy, extremes
naz.freq.weed |> 
  filter(Plot %in% c("very wet", "very dry")) |> 
  filter(Code != "Empty") |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona, weeds") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(axis.text.x = element_text(color = "black"))


### N AZ: Seeded species --------------------------------------------------

# Current mix, all plots
naz.current.total <- naz.freq.current |> 
  filter(Plot == "total") |> 
  mutate(Lifeform = factor(Lifeform, levels = c("Forb", "Grass", "Shrub", "Empty")),
         Code = factor(Code, levels = c("LILE3", "DACA7", "HEBO", "ACMI2", "MATA2", "PEPA8", 
                                        "SPGR2", "HEMU3", "PASM", "LECI4", "HECO26", "ELEL5", "ELTR7",
                                        "ELWA2", "BOGR2", "SPCR", "POSE", "PLJA", "BOER4", "PSSP6",
                                        "KRLA2", "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Lifeform)) +
  geom_bar(stat = "identity") +
  ggtitle("Northern Arizona current-adapted mix, all conditions") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  scale_fill_manual(values = c("#8DA0CB", "#66C2A5", "#FC8D62", "#B3B3B3")) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35))  +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in"))
naz.current.total

# Current mix, wetter/drier
naz.current.wetdry <- naz.freq.current |> 
  filter(Plot %in% c("wetter", "drier")) |> 
  add_row(Code = "SPGR2",
          Frequency = 0,
          Type2 = "Forb, wetter") |>
  add_row(Code = "HEMU3",
          Frequency = 0,
          Type2 = "Forb, wetter") |>
  add_row(Code = "PSSP6",
          Frequency = 0,
          Type2 = "Grass, drier") |>
  mutate(Type2 = factor(Type2, levels = c("Forb, drier", "Forb, wetter", 
                                          "Grass, drier", "Grass, wetter",
                                          "Shrub, drier", "Shrub, wetter", 
                                          "Empty, drier", "Empty, wetter")),
         Code = factor(Code, levels = c("LILE3", "DACA7", "HEBO", "ACMI2", "MATA2", "PEPA8", 
                                        "SPGR2", "HEMU3", "PASM", "LECI4", "HECO26", "ELEL5", "ELTR7",
                                        "ELWA2", "BOGR2", "SPCR", "POSE", "PLJA", "BOER4", "PSSP6",
                                        "KRLA2", "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type2)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona current-adapted mix, wetter vs. drier conditions") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  scale_fill_manual(values = c("#B3C8E8", "#7570B3", "#B3E2D6", "#1B9E77", "#FBB4AE", "#D95F02",
                               "#D9D9D9", "#666666")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom")
naz.current.wetdry

# Current mix, extremes
naz.current.ex <- naz.freq.current |> 
  filter(Plot %in% c("very wet", "very dry")) |> 
  add_row(Code = "LILE3",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "ACMI2",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "MATA2",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "SPGR2",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "SPGR2",
          Frequency = 0,
          Type2 = "Forb, very wet") |>
  add_row(Code = "HEMU3",
          Frequency = 0,
          Type2 = "Forb, very wet") |>
  add_row(Code = "ELTR7",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "ELWA2",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "BOGR2",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "SPCR",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "PLJA",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "PLJA",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  add_row(Code = "BOER4",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  add_row(Code = "PSSP6",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "PSSP6",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  mutate(Type2 = factor(Type2, levels = c("Forb, very dry", "Forb, very wet", 
                                          "Grass, very dry", "Grass, very wet",
                                          "Shrub, very dry", "Shrub, very wet", 
                                          "Empty, very dry", "Empty, very wet")),
         Code = factor(Code, levels = c("LILE3", "DACA7", "HEBO", "ACMI2", "MATA2", "PEPA8", 
                                        "SPGR2", "HEMU3", "PASM", "LECI4", "HECO26", "ELEL5", "ELTR7",
                                        "ELWA2", "BOGR2", "SPCR", "POSE", "PLJA", "BOER4", "PSSP6",
                                        "KRLA2", "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type2)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona current-adapted mix, highly variable precipitation") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#B3C8E8", "#7570B3", "#B3E2D6", "#1B9E77", "#FBB4AE", "#D95F02",
                               "#D9D9D9", "#666666")) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in"))
naz.current.ex

# Current mix, all plots - alternate order
naz.current.total.alt <- naz.freq.current |> 
  filter(Plot == "total") |> 
  mutate(Lifeform = factor(Lifeform, levels = c("Forb", "Grass", "Shrub", "Empty")),
         Code = factor(Code, levels = c("LECI4", "HEBO", "HECO26", "LILE3", "PASM",
                                        "DACA7", "ELEL5","ACMI2", "ELTR7", "ELWA2", 
                                        "BOGR2", "MATA2", "PEPA8", "SPCR", "POSE",
                                        "KRLA2", "PLJA", "SPGR2", "BOER4",     
                                         "PSSP6", "HEMU3", "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Lifeform)) +
  geom_bar(stat = "identity") +
  ggtitle("Northern Arizona current-adapted mix, all conditions") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  scale_fill_manual(values = c("#8DA0CB", "#66C2A5", "#FC8D62", "#B3B3B3")) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35))  +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in"))
naz.current.total.alt

# Current mix, extremes - alternate order
naz.current.ex.alt <- naz.freq.current |> 
  filter(Plot %in% c("very wet", "very dry")) |> 
  add_row(Code = "LILE3",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "ACMI2",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "MATA2",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "SPGR2",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "SPGR2",
          Frequency = 0,
          Type2 = "Forb, very wet") |>
  add_row(Code = "HEMU3",
          Frequency = 0,
          Type2 = "Forb, very wet") |>
  add_row(Code = "ELTR7",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "ELWA2",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "BOGR2",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "SPCR",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "PLJA",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "PLJA",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  add_row(Code = "BOER4",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  add_row(Code = "PSSP6",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "PSSP6",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  mutate(Type2 = factor(Type2, levels = c("Forb, very dry", "Forb, very wet", 
                                          "Grass, very dry", "Grass, very wet",
                                          "Shrub, very dry", "Shrub, very wet", 
                                          "Empty, very dry", "Empty, very wet")),
         Code = factor(Code, levels = c("LECI4", "HEBO", "HECO26", "LILE3", "PASM",
                                        "DACA7", "ELEL5","ACMI2", "ELTR7", "ELWA2", 
                                        "BOGR2", "MATA2", "PEPA8", "SPCR", "POSE",
                                        "KRLA2", "PLJA", "SPGR2", "BOER4",     
                                        "PSSP6", "HEMU3", "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type2)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona current-adapted mix, highly variable precipitation") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#B3C8E8", "#7570B3", "#B3E2D6", "#1B9E77", "#FBB4AE", "#D95F02",
                               "#D9D9D9", "#666666")) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in"))
naz.current.ex.alt

# Projected mix, all plots
naz.projected.total <- naz.freq.projected |> 
  filter(Plot == "total") |> 
  mutate(Lifeform = factor(Lifeform, levels = c("Forb", "Grass", "Shrub", "Empty")),
         Code = factor(Code, levels = c("BAMU", "ASTU", "SECO10", "LILE3", "ACMI2", "HEMU3", 
                                        "DACA7", "MATA2", "PEPA8", "PASM", "ACHY", "ELEL5",
                                        "BOER4", "POSE", "PLJA", "BOCU", "ARPU9", "BOGR2",
                                        "SPCR", "PLMU3", "KRLA2", "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Lifeform)) +
  geom_bar(stat = "identity") +
  ggtitle("Northern Arizona projected-adapted mix, all conditions") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  scale_fill_manual(values = c("#8DA0CB", "#66C2A5", "#FC8D62", "#B3B3B3")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in"))
naz.projected.total

# Projected mix, wetter/drier
naz.projected.wetdry <- naz.freq.projected |> 
  filter(Plot %in% c("wetter", "drier")) |> 
  add_row(Code = "ACMI2",
          Frequency = 0,
          Type2 = "Forb, drier") |>
  add_row(Code = "HEMU3",
          Frequency = 0,
          Type2 = "Forb, wetter") |>
  add_row(Code = "DACA7",
          Frequency = 0,
          Type2 = "Forb, drier") |>
  add_row(Code = "MATA2",
          Frequency = 0,
          Type2 = "Forb, drier") |>
  add_row(Code = "BOER4",
          Frequency = 0,
          Type2 = "Grass, drier") |>
  add_row(Code = "BOGR2",
          Frequency = 0,
          Type2 = "Grass, drier") |>
  add_row(Code = "PLMU3",
          Frequency = 0,
          Type2 = "Grass, wetter") |>
  mutate(Type2 = factor(Type2, levels = c("Forb, drier", "Forb, wetter", 
                                          "Grass, drier", "Grass, wetter",
                                          "Shrub, drier", "Shrub, wetter", 
                                          "Empty, drier", "Empty, wetter")),
         Code = factor(Code, levels = c("BAMU", "ASTU", "SECO10", "LILE3", "ACMI2", "HEMU3", 
                                        "DACA7", "MATA2", "PEPA8", "PASM", "ACHY", "ELEL5",
                                        "BOER4", "POSE", "PLJA", "BOCU", "ARPU9", "BOGR2",
                                        "SPCR", "PLMU3", "KRLA2", "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type2)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona projected-adapted mix, wetter vs. drier conditions") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#B3C8E8", "#7570B3", "#B3E2D6", "#1B9E77", "#FBB4AE", "#D95F02",
                               "#D9D9D9", "#666666")) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom")
naz.projected.wetdry

# Projected mix, extremes
naz.projected.ex <- naz.freq.projected |> 
  filter(Plot %in% c("very wet", "very dry")) |> 
  add_row(Code = "ASTU",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "SECO10",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "ACMI2",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "HEMU3",
          Frequency = 0,
          Type2 = "Forb, very wet") |>
  add_row(Code = "DACA7",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "MATA2",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "PEPA8",
          Frequency = 0,
          Type2 = "Forb, very wet") |>
  add_row(Code = "BOER4",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "POSE",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  add_row(Code = "PLJA",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  add_row(Code = "BOCU",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "ARPU9",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "BOGR2",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "SPCR",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  add_row(Code = "PLMU3",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "PLMU3",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  add_row(Code = "KRLA2",
          Frequency = 0,
          Type2 = "Shrub, very wet") |>
  mutate(Type2 = factor(Type2, levels = c("Forb, very dry", "Forb, very wet", 
                                          "Grass, very dry", "Grass, very wet",
                                          "Shrub, very dry",  "Shrub, very wet",
                                          "Empty, very dry", "Empty, very wet")),
         Code = factor(Code, levels = c("BAMU", "ASTU", "SECO10", "LILE3", "ACMI2", "HEMU3", 
                                        "DACA7", "MATA2", "PEPA8", "PASM", "ACHY", "ELEL5",
                                        "BOER4", "POSE", "PLJA", "BOCU", "ARPU9", "BOGR2",
                                        "SPCR", "PLMU3", "KRLA2", "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type2)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona projected-adapted mix, highly variable precipitation") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  scale_fill_manual(values = c("#B3C8E8", "#7570B3", "#B3E2D6", "#1B9E77", "#FBB4AE", "#D95F02",
                               "#D9D9D9", "#666666")) +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in"))
naz.projected.ex

# Projected mix, all plots - alternate order
naz.projected.total.alt <- naz.freq.projected |> 
  filter(Plot == "total") |> 
  mutate(Lifeform = factor(Lifeform, levels = c("Forb", "Grass", "Shrub", "Empty")),
         Code = factor(Code, levels = c("BAMU", "PASM", "ASTU", "ACHY", "LILE3", "SECO10",
                                        "ELEL5", "BOER4", "ACMI2", "POSE", "KRLA2",
                                        "PLJA", "BOCU", "ARPU9", "HEMU3", "BOGR2",
                                        "SPCR", "DACA7", "PEPA8", "MATA2",
                                         "PLMU3",  "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Lifeform)) +
  geom_bar(stat = "identity") +
  ggtitle("Northern Arizona projected-adapted mix, all conditions") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  scale_fill_manual(values = c("#8DA0CB", "#66C2A5", "#FC8D62", "#B3B3B3")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in"))
naz.projected.total.alt

# Projected mix, extremes - alternate order
naz.projected.ex.alt <- naz.freq.projected |> 
  filter(Plot %in% c("very wet", "very dry")) |> 
  add_row(Code = "ASTU",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "SECO10",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "ACMI2",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "HEMU3",
          Frequency = 0,
          Type2 = "Forb, very wet") |>
  add_row(Code = "DACA7",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "MATA2",
          Frequency = 0,
          Type2 = "Forb, very dry") |>
  add_row(Code = "PEPA8",
          Frequency = 0,
          Type2 = "Forb, very wet") |>
  add_row(Code = "BOER4",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "POSE",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  add_row(Code = "PLJA",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  add_row(Code = "BOCU",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "ARPU9",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "BOGR2",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "SPCR",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  add_row(Code = "PLMU3",
          Frequency = 0,
          Type2 = "Grass, very dry") |>
  add_row(Code = "PLMU3",
          Frequency = 0,
          Type2 = "Grass, very wet") |>
  add_row(Code = "KRLA2",
          Frequency = 0,
          Type2 = "Shrub, very wet") |>
  mutate(Type2 = factor(Type2, levels = c("Forb, very dry", "Forb, very wet", 
                                          "Grass, very dry", "Grass, very wet",
                                          "Shrub, very dry",  "Shrub, very wet",
                                          "Empty, very dry", "Empty, very wet")),
         Code = factor(Code, levels = c("BAMU", "PASM", "ASTU", "ACHY", "LILE3", "SECO10",
                                        "ELEL5", "BOER4", "ACMI2", "POSE", "KRLA2",
                                        "PLJA", "BOCU", "ARPU9", "HEMU3", "BOGR2",
                                        "SPCR", "DACA7", "PEPA8", "MATA2",
                                        "PLMU3",  "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type2)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona projected-adapted mix, highly variable precipitation") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  scale_fill_manual(values = c("#B3C8E8", "#7570B3", "#B3E2D6", "#1B9E77", "#FBB4AE", "#D95F02",
                               "#D9D9D9", "#666666")) +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in"))
naz.projected.ex.alt


### N AZ: Species of interest ---------------------------------------------

# Species of interest, all plots
naz.species.total <- naz.freq.interest |> 
  filter(Plot == "total") |> 
  ggplot(aes(x = Code, y = Frequency, fill = Plant)) +
  geom_bar(stat = "identity") +
  ggtitle("Northern Arizona species of interest, all conditions") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#E5C494", "#FC8D62", "#66C2A5", "#8DA0CB", "#B3B3B3")) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
naz.species.total

# Species of interest, paired wetter & drier
naz.species.wetdry <- naz.freq.interest |> 
  filter(Plot %in% c("wetter", "drier")) |> 
  add_row(Code = "LEPA6",
          Frequency = 0,
          Type = "Native recruit, drier") |>
  mutate(Type = factor(Type, 
                       levels = c("Current mix, drier", "Current mix, wetter",
                                  "Projected mix, drier", "Projected mix, wetter",
                                  "Native recruit, drier", "Native recruit, wetter",
                                  "Invasive, drier", "Invasive, wetter",
                                  "Empty, drier", "Empty, wetter")),
         Code = factor(Code, levels = c("LECI4", "HEBO", "HECO26", "LILE3", 
                                        "BAMU", "PASM", "ASTU", 
                                        "ATCO", "SOEL", "LEPA6", "SATR12", "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona species of interest, wetter vs. drier conditions") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#E5D4A7", "#A6761D", "#FBB4AE", "#D95F02",
                               "#B3E2D6", "#1B9E77", "#B3C8E8", "#7570B3",
                               "#D9D9D9", "#666666")) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom")
naz.species.wetdry

# Species of interest, very wet & very dry (extremes)
naz.species.ex <- naz.freq.interest |> 
  filter(Plot %in% c("very wet", "very dry")) |> 
  add_row(Code = "LILE3",
          Frequency = 0,
          Type = "Current mix, very dry") |>
  add_row(Code = "ASTU",
          Frequency = 0,
          Type = "Projected mix, very dry") |>
  add_row(Code = "LEPA6",
          Frequency = 0,
          Type = "Native recruit, very dry") |>
  mutate(Type = factor(Type, 
                       levels = c("Current mix, very dry", "Current mix, very wet",
                                  "Projected mix, very dry", "Projected mix, very wet",
                                  "Native recruit, very dry", "Native recruit, very wet",
                                  "Invasive, very dry", "Invasive, very wet",
                                  "Empty, very dry", "Empty, very wet")),
         Code = factor(Code, levels = c("LECI4", "HEBO", "HECO26", "LILE3", 
                                        "BAMU", "PASM", "ASTU", 
                                        "ATCO", "SOEL", "LEPA6", "SATR12", "Empty"))) |> 
  ggplot(aes(x = Code, y = Frequency, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Northern Arizona species of interest, highly variable precipitation") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  xlab(NULL) +
  ylab("Frequency (presence in plots)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#E5D4A7", "#A6761D", "#FBB4AE", "#D95F02",
                               "#B3E2D6", "#1B9E77", "#B3C8E8", "#7570B3",
                               "#D9D9D9", "#666666")) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.x = element_text(angle = 35)) +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
naz.species.ex




# Write out draft figures -------------------------------------------------

## Sonoran Desert ---------------------------------------------------------

# Sonoran Desert: Precip deviation
tiff("figures/2024-09_draft-figures-2.0/Sonoran_percent-deviation_cumulative-precip.tiff", units = "in", height = 5, width = 7, res = 150)
cum.pd.sonoran
dev.off()

# Sonoran Desert: Count
tiff("figures/2024-09_draft-figures-2.0/Sonoran_desirable_Count-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.des.count.plotmixclimate
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Sonoran_weedy_Count-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
sonoran.weed.count
dev.off()

tiff("figures/2024-09_draft-figures-2.0/Sonoran_seeded_Count-by-seeded-species_Current.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.seed.count.current.species
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Sonoran_seeded_Count-by-seeded-species_Projected.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.seed.count.projected.species
dev.off()

# Sonoran Desert: Frequency
tiff("figures/2024-09_draft-figures-2.0/Sonoran_seeded_frequency-total_Current.tiff", units = "in", height = 5, width = 8, res = 150)
sonoran.current.total
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Sonoran_seeded_frequency-wet-dry_Current.tiff", units = "in", height = 5, width = 8, res = 150)
sonoran.current.wetdry
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Sonoran_seeded_frequency-extremes_Current.tiff", units = "in", height = 5, width = 8, res = 150)
sonoran.current.ex
dev.off()

tiff("figures/2024-09_draft-figures-2.0/Sonoran_seeded_frequency-total_Projected.tiff", units = "in", height = 5, width = 8, res = 150)
sonoran.projected.total
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Sonoran_seeded_frequency-wet-dry_Projected.tiff", units = "in", height = 5, width = 8, res = 150)
sonoran.projected.wetdry
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Sonoran_seeded_frequency-extremes_Projected.tiff", units = "in", height = 5, width = 8, res = 150)
sonoran.projected.ex
dev.off()

# Sonoran Desert: Species of interest
tiff("figures/2024-09_draft-figures-2.0/Sonoran_species-of-interest_Count.tiff", units = "in", height = 5, width = 7, res = 150)
sonoran.species.count
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Sonoran_species-of-interest_frequency-total.tiff", units = "in", height = 5, width = 8, res = 150)
sonoran.species.total
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Sonoran_species-of-interest_frequency-wet-dry.tiff", units = "in", height = 5, width = 8, res = 150)
sonoran.species.wetdry
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Sonoran_species-of-interest_frequency-extremes.tiff", units = "in", height = 5, width = 8, res = 150)
sonoran.species.ex
dev.off()



## Northern Arizona Plateau -----------------------------------------------

# Northern Arizona: Precip deviation
tiff("figures/2024-09_draft-figures-2.0/Northern-AZ_percent-deviation_cumulative-precip.tiff", units = "in", height = 5, width = 7, res = 150)
cum.pd.naz
dev.off()

# Northern Arizona: Count
tiff("figures/2024-09_draft-figures-2.0/Northern-AZ_desirable_Count-by-PlotMix_Climate-and-PlantSource2.tiff", units = "in", height = 5, width = 7, res = 150)
naz.des.count.plotmixclimate
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Northern-AZ_weedy_Count-single-by-PlantSource2.tiff", units = "in", height = 4, width = 5, res = 150)
naz.weed.count
dev.off()

tiff("figures/2024-09_draft-figures-2.0/Northern-AZ_seeded_Count-by-seeded-species_Current.tiff", units = "in", height = 5, width = 7, res = 150)
naz.seed.count.current.species
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Northern-AZ_seeded_Count-by-seeded-species_Projected.tiff", units = "in", height = 5, width = 7, res = 150)
naz.seed.count.projected.species
dev.off()

# Northern Arizona: Frequency
tiff("figures/2024-09_draft-figures-2.0/Northern-AZ_seeded_frequency-total_Current.tiff", units = "in", height = 5, width = 8, res = 150)
naz.current.total
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Northern-AZ_seeded_frequency-wet-dry_Current.tiff", units = "in", height = 5, width = 8, res = 150)
naz.current.wetdry
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Northern-AZ_seeded_frequency-extremes_Current.tiff", units = "in", height = 5, width = 8, res = 150)
naz.current.ex
dev.off()

tiff("figures/2024-09_draft-figures-2.0/Northern-AZ_seeded_frequency-total_Projected.tiff", units = "in", height = 5, width = 8, res = 150)
naz.projected.total
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Northern-AZ_seeded_frequency-wet-dry_Projected.tiff", units = "in", height = 5, width = 8, res = 150)
naz.projected.wetdry
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Northern-AZ_seeded_frequency-extremes_Projected.tiff", units = "in", height = 5, width = 8, res = 150)
naz.projected.ex
dev.off()

# Northern Arizona: Species of interest
tiff("figures/2024-09_draft-figures-2.0/Northern-AZ_species-of-interest_Count.tiff", units = "in", height = 5, width = 7, res = 150)
naz.species.count
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Northern-AZ_species-of-interest_frequency-total.tiff", units = "in", height = 5, width = 8, res = 150)
naz.species.total
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Northern-AZ_species-of-interest_frequency-wet-dry.tiff", units = "in", height = 5, width = 8, res = 150)
naz.species.wetdry
dev.off()
tiff("figures/2024-09_draft-figures-2.0/Northern-AZ_species-of-interest_frequency-extremes.tiff", units = "in", height = 5, width = 8, res = 150)
naz.species.ex
dev.off()



## Figure 1: Desirable & weedy density ------------------------------------

tiff("figures/2024-09_draft-figures-2.0/Fig1_desirable-weedy-count.tiff", units = "in", height = 9, width = 13, res = 150)
ggarrange(sonoran.des.count.plotmixclimate, sonoran.weed.count,
          naz.des.count.plotmixclimate, naz.weed.count,
          ncol = 2, nrow = 2,
          labels = c("(A)", "(B)", "(C)", "(D)")) 
dev.off()


## Figure 2: SD species of interest, frequency ----------------------------

tiff("figures/2024-09_draft-figures-2.0/Fig2_Sonoran-species-of-interest-frequency.tiff", units = "in", height = 10, width = 9, res = 150)
ggarrange(sonoran.species.total, sonoran.species.ex,
          ncol = 1, nrow = 2,
          labels = c("(A)", "(B)")) 
dev.off()


## Figure 3: NAZ species of interest, frequency ---------------------------

tiff("figures/2024-09_draft-figures-2.0/Fig3_Northern-AZ-species-of-interest-frequency.tiff", units = "in", height = 10, width = 9, res = 150)
ggarrange(naz.species.total, naz.species.ex,
          ncol = 1, nrow = 2,
          labels = c("(A)", "(B)")) 
dev.off()




## Figure S1: Precip deviation conditions ---------------------------------

tiff("figures/2024-09_draft-figures-2.0/FigS01_percent-deviation_cumulative-precip.tiff", units = "in", height = 5, width = 14, res = 150)
ggarrange(cum.pd.sonoran, cum.pd.naz,
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)")) 
dev.off()


## Figure S3: Species of interest, density --------------------------------

tiff("figures/2024-09_draft-figures-2.0/FigS03_species-of-interest-density.tiff", units = "in", height = 5, width = 14, res = 150)
ggarrange(sonoran.species.count, naz.species.count,
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)"),
          common.legend = TRUE, legend = "bottom") 
dev.off()


## Figure S4: SD current mix, frequency -----------------------------------

tiff("figures/2024-09_draft-figures-2.0/FigS04_Sonoran-current-frequency.tiff", units = "in", height = 10, width = 9, res = 150)
ggarrange(sonoran.current.total, sonoran.current.ex,
          ncol = 1, nrow = 2,
          labels = c("(A)", "(B)")) 
dev.off()

tiff("figures/2024-09_draft-figures-2.0/FigS04_Sonoran-current-frequency-alt-order.tiff", units = "in", height = 10, width = 9, res = 150)
ggarrange(sonoran.current.total.alt, sonoran.current.ex.alt,
          ncol = 1, nrow = 2,
          labels = c("(A)", "(B)")) 
dev.off()


## Figure S5: SD projected mix, frequency ---------------------------------

tiff("figures/2024-09_draft-figures-2.0/FigS05_Sonoran-projected-frequency.tiff", units = "in", height = 10, width = 9, res = 150)
ggarrange(sonoran.projected.total, sonoran.projected.ex,
          ncol = 1, nrow = 2,
          labels = c("(A)", "(B)")) 
dev.off()

tiff("figures/2024-09_draft-figures-2.0/FigS05_Sonoran-projected-frequency-alt-order.tiff", units = "in", height = 10, width = 9, res = 150)
ggarrange(sonoran.projected.total.alt, sonoran.projected.ex.alt,
          ncol = 1, nrow = 2,
          labels = c("(A)", "(B)")) 
dev.off()


## Figure S6: SD seeded species, density ----------------------------------

tiff("figures/2024-09_draft-figures-2.0/FigS06_Sonoran-seeded-species-density.tiff", units = "in", height = 5, width = 14, res = 150)
ggarrange(sonoran.seed.count.current.species, sonoran.seed.count.projected.species,
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)"),
          common.legend = TRUE, legend = "bottom") 
dev.off()

tiff("figures/2024-09_draft-figures-2.0/FigS06_Sonoran-seeded-species-density-alt-order.tiff", units = "in", height = 5, width = 14, res = 150)
ggarrange(sonoran.seed.count.current.species.alt, sonoran.seed.count.projected.species.alt,
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)"),
          common.legend = TRUE, legend = "bottom") 
dev.off()


## Figure S7: NAZ current mix, frequency ----------------------------------

tiff("figures/2024-09_draft-figures-2.0/FigS07_Nothern-AZ-current-frequency.tiff", units = "in", height = 10, width = 9, res = 150)
ggarrange(naz.current.total, naz.current.ex,
          ncol = 1, nrow = 2,
          labels = c("(A)", "(B)")) 
dev.off()

tiff("figures/2024-09_draft-figures-2.0/FigS07_Nothern-AZ-current-frequency-alt-order.tiff", units = "in", height = 10, width = 9, res = 150)
ggarrange(naz.current.total.alt, naz.current.ex.alt,
          ncol = 1, nrow = 2,
          labels = c("(A)", "(B)")) 
dev.off()


## Figure S8: NAZ projected mix, frequency --------------------------------

tiff("figures/2024-09_draft-figures-2.0/FigS08_Northern-AZ-projected-frequency.tiff", units = "in", height = 10, width = 9, res = 150)
ggarrange(naz.projected.total, naz.projected.ex,
          ncol = 1, nrow = 2,
          labels = c("(A)", "(B)")) 
dev.off()

tiff("figures/2024-09_draft-figures-2.0/FigS08_Northern-AZ-projected-frequency-alt-order.tiff", units = "in", height = 10, width = 9, res = 150)
ggarrange(naz.projected.total.alt, naz.projected.ex.alt,
          ncol = 1, nrow = 2,
          labels = c("(A)", "(B)")) 
dev.off()


## Figure S9: NAZ seeded species, density ---------------------------------

tiff("figures/2024-09_draft-figures-2.0/FigS09_Northern-AZ-seeded-species-density.tiff", units = "in", height = 5, width = 14, res = 150)
ggarrange(naz.seed.count.current.species, naz.seed.count.projected.species,
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)"),
          common.legend = TRUE, legend = "bottom") 
dev.off()

tiff("figures/2024-09_draft-figures-2.0/FigS09_Northern-AZ-seeded-species-density-alt-order.tiff", units = "in", height = 5, width = 14, res = 150)
ggarrange(naz.seed.count.current.species.alt, naz.seed.count.projected.species.alt,
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)"),
          common.legend = TRUE, legend = "bottom") 
dev.off()


save.image("RData/12.1_draft-figs-2.0_density-and-frequency.RData")
