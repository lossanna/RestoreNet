# Created: 2024-12-11
# Last updated: 2024-12-11

# Purpose: Add Days_since_last col, the number of days since the last monitoring period, and add
#   Monitor_season col, the season of the monitoring event (Spring or Fall).

library(tidyverse)
library(readxl)

# Load data ---------------------------------------------------------------

sonoran.subplot <- read_csv("data/cleaned/14.1_Sonoran-Desert_subplot_with-seasonality_clean.csv")
sonoran.monitor.prev <- read_xlsx("data/data-wrangling-intermediate/14.2_SiteDateID-with-previous-monitoring-date.xlsx")

# Notes about manual edits ------------------------------------------------

# For manual edits to CSVs, the CSV is written from R, copied and named a new name, edited,
#   and new file is read into R.

# Files in the format "output_xx.csv" are ones written from R.
# Files in the format "edited_xx.csv" are manually edited and read back in as new objects,
#   but then usually used to alter existing objects.
#   See README_rawdata.md for more details.

# Add Days_elapsed col ----------------------------------------------------

sonoran.monitor <- sonoran.monitor.prev |> 
  mutate(Days_elapsed = difftime(Date_Monitored, Date_Seeded)) |> 
  mutate(Days_elapsed = as.numeric(Days_elapsed)) |> 
  mutate(Days_since_last = difftime(Date_Monitored, Monitored_Previous, units = "days")) |> 
  mutate(Days_since_last = as.numeric(Days_since_last))

# Convert to date (remove time)
sonoran.monitor <- sonoran.monitor |> 
  mutate(Date_Seeded = as.Date(Date_Seeded),
         Date_Monitored = as.Date(Date_Monitored),
         Monitored_Previous = as.Date(Monitored_Previous))


# Mark event as spring or fall --------------------------------------------

# OUTPUT: list of Sonoran Desert monitoring events
write_csv(sonoran.monitor,
          file = "data/data-wrangling-intermediate/14.2a_output1_Sonoran-monitoring-events.csv")

# EDITED: list of Sonoran Desert monitoring events with season of event added (Spring or Fall)
sonoran.monitor <- read_xlsx("data/data-wrangling-intermediate/14.2b_edited1_Sonoran-monitoring-events-with-monitor-season.xlsx")



# Write to csv ------------------------------------------------------------

write_csv(sonoran.monitor,
          file = "data/cleaned/14.2_Sonoran-Desert_monitoring-events.csv")
