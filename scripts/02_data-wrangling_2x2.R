library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

plot.2x2.raw <- read_xlsx("data/raw/Master Germination Data 2022.xlsx", sheet = "AllPlotData")
species.all.in <- read_csv("data/cleaned/species-list_all_location-independent.csv")
species.all.de <- read_csv("data/cleaned/species-list_all_location-dependent.csv")
p2x2.codes.dup <- read_csv("data/raw/2x2-codes_need-duplicate-rows.csv")
p2x2.codes.dup.count <- read_csv("data/raw/2x2-codes_need-duplicate-rows_count.csv")
mix <- read_xlsx("data/raw/master-seed-mix.xlsx")
