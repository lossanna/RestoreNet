# Created: 2023-09-27
# Last updated: 2023-09-27

# Purpose: Compile climate data gathered from PRISM and compare to 
#   Farrell 2023 data (specifically cumulative precipitation and precipitation
#   since last monitoring event).

library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

prism.files.raw <- read_xlsx("data/data-wrangling-intermediate/03.2_monitoring-events-with-Farrell-climate-data-and-PRISM-csv-file-name.xlsx",
                             sheet = "daily")


# Add complete path to prism.files columns --------------------------------

# Drop null ID 145 and add complete path to file names
prism.files <- prism.files.raw |> 
  filter(!is.na(cum_file)) |> 
  mutate(cum_file = paste0(path_beginning, "/", cum_file),
         since_last_file = paste0(path_beginning, "/", since_last_file))



# Add Cum_precip and Since_last_precip cols based on PRISM ----------------

# Add columns of cumulative precip and precip since last monitoring events based
#   on PRISM data. I downloaded a file from PRISM based on the relevant specifications,
#   and it can be used to calculate the precip values for each MonitorSiteID.
# Write a function to process the CSVs, and then iterative over for each row.


# Write function to process CSVs
process_csv <- function(file.names) {
  
  # Read the CSV file
  data <- read.csv(file.names, skip = 11)
  
  # Rename columns
  colnames(data) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
  
  # Calculate the sum of the desired column
  sum_value <- sum(data$ppt_mm)
  
  return(sum_value)
}

# Use purrr::map to apply the function to each row of df
prism.files <- prism.files %>%
  mutate(Cum_precip = map_dbl(cum_file, process_csv)) %>%
  mutate(Since_last_precip = map_dbl(since_last_file, process_csv))


# Compare precip values ---------------------------------------------------

compare.ppt <- prism.files |> 
  select(Region, Site, Date_Seeded, Date_Monitored, MonitorSiteID, Farrell_cum_precip,
         Cum_precip, Farrell_precip_since_monitor, Since_last_precip) |> 
  mutate(Cum_difference = Farrell_cum_precip - Cum_precip,
         Since_difference = Farrell_precip_since_monitor - Since_last_precip)

# Inspect differences
summary(compare.ppt$Cum_difference)
summary(compare.ppt$Since_difference)

compare.ppt.inspect <- compare.ppt |> 
  filter(abs(Cum_difference) > 10 | abs(Since_difference) > 10)
#   There are only 18 instances where my PRISM data differs by more than 10 mm of precip
#     when compared with Hannah's.
#   They overall seem similar, so I will proceed with my values.

compare.ppt.inspect2 <- compare.ppt |> 
  filter(abs(Cum_difference) > 40 | abs(Since_difference) > 40)
