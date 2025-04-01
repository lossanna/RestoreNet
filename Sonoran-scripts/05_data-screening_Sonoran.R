# Created: 2025-04-01
# Last updated: 2025-04-01

# Purpose: Examine distributions, outliers, and variable relationships for Count response variable.

# Determine if the relationship between Count and precip variables is linear.

library(tidyverse)
library(GGally)

# Load data ---------------------------------------------------------------

subplot.raw <- read_xlsx("Sonoran-data/cleaned/04.1_subplot-data_clean.csv")
prism.data <- read_csv("Sonoran-data/cleaned/03.1_monitoring-events-with-PRISM-climate-data_clean.csv")
since.pd <- read_csv("Sonoran-data/cleaned/")