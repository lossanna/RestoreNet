# Created: 2025-01-23
# Last updated: 2025-01-23

# Purpose: Compile climate data gathered from PRISM for Sonoran Desert sites.
# Used 4km for daily values, and used 800m 1991-2020 normals.


library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

prism.daily.raw <- read_xlsx("Sonoran-data/data-wrangling-intermediate/03.1_monitoring-events-with-Farrell-climate-data-and-PRISM-csv-file-name.xlsx",
                             sheet = "daily")
prism.normals.raw <- read_xlsx("Sonoran-data/data-wrangling-intermediate/03.1_monitoring-events-with-Farrell-climate-data-and-PRISM-csv-file-name.xlsx",
                               sheet = "normals")

monitor.info <- read_csv("Sonoran-data/cleaned/02_corrected-monitoring-info_clean.csv")
monitor.site <- read_csv("Sonoran-data/cleaned/02_SiteDateID_clean.csv")



# Add complete path to prism.daily columns --------------------------------

# Add complete path to file names
prism.daily <- prism.daily.raw |> 
  mutate(cum_file = paste0(path_beginning, "/", cum_file),
         since_last_file = paste0(path_beginning, "/", since_last_file))

# Add complete path for normals
prism.normals <- prism.normals.raw |> 
  mutate(normals_file = paste0(path_beginning, "/", normals_file))


# Add Cum_precip and Since_last_precip cols based on PRISM ----------------

# Add columns of cumulative precip and precip since last monitoring events based
#   on PRISM data. I downloaded a file from PRISM based on the relevant specifications,
#   and it can be used to calculate the precip values for each SiteDateID.
# Write a function to process the CSVs, and then iterative over for each row.


# Write function to process CSVs
process_csv <- function(file.names) {
  
  # Read the CSV file
  data <- read.csv(file.names, skip = 10)
  
  # Rename columns
  colnames(data) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
  
  # Calculate the sum of the desired column
  sum_value <- sum(data$ppt_mm)
  
  return(sum_value)
}

# Use purrr::map to apply the function to each row of df
prism.daily <- prism.daily %>%
  mutate(Cum_precip = map_dbl(cum_file, process_csv)) %>%
  mutate(Since_last_precip = map_dbl(since_last_file, process_csv))



# Compare precip values ---------------------------------------------------

compare.ppt <- prism.daily |> 
  select(Region, Site, Date_Seeded, Date_Monitored, SiteDateID, Farrell_cum_precip,
         Cum_precip, Farrell_precip_since_monitor, Since_last_precip) |> 
  mutate(Cum_difference = Farrell_cum_precip - Cum_precip,
         Since_difference = Farrell_precip_since_monitor - Since_last_precip)

# Inspect differences
summary(compare.ppt$Cum_difference)
summary(compare.ppt$Since_difference)

compare.ppt.inspect <- compare.ppt |> 
  filter(abs(Cum_difference) > 20 | abs(Since_difference) > 20)
#   There are only 22 instances where my PRISM data differs by more than 20 mm of precip
#     when compared with Hannah's.
#   They overall seem similar, so I will proceed with my values.

compare.ppt.inspect2 <- compare.ppt |> 
  filter(abs(Cum_difference) > 40 | abs(Since_difference) > 40)



# Extract 30 year normals for MAT & MAP -----------------------------------

# Write function to process CSVs
# MAP
ppt_normals <- function(file.names) {
  
  # Read the CSV file
  data <- read.csv(file.names, skip = 10)
  
  # Rename columns
  colnames(data) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
  
  # Extract annual value
  annual <- data$ppt[13]
  
  return(annual)
}

# MAT
temp_normals <- function(file.names) {
  
  # Read the CSV file
  data <- read.csv(file.names, skip = 10)
  
  # Rename columns
  colnames(data) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
  
  # Extract annual value
  annual <- data$tmean[13]
  
  return(annual)
}

# Use purrr::map to apply the function to each row of df
prism.normals <- prism.normals %>%
  mutate(MAP = map_dbl(normals_file, ppt_normals)) %>%
  mutate(MAT = map_dbl(normals_file, temp_normals))


# Compare normals ---------------------------------------------------------

compare.normals <- prism.normals |> 
  select(Region, Site, Farrell_MAP, MAP, Farrell_MAT, MAT) |> 
  mutate(MAP_difference = Farrell_MAP - MAP,
         MAT_difference = Farrell_MAT - MAT)
#   1991-2020 normals (mine) are slightly hotter than 1981-2010 (Hannah's),
#     which is to be expected. MAP differences are all less than 10 cm.



# Examine monthly precip normals ------------------------------------------

# Extract monthly normals for each site
#   Pleasant
month.normal.Pleasant <- read.csv(prism.normals$normals_file[1], skip = 10)
colnames(month.normal.Pleasant) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.Pleasant$Site <- "Pleasant"
month.normal.Pleasant$Region <- "Sonoran Central"

#   Preserve
month.normal.Preserve <- read.csv(prism.normals$normals_file[2], skip = 10)
colnames(month.normal.Preserve) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.Preserve$Site <- "Preserve"
month.normal.Preserve$Region <- "Sonoran Central"

#   Roosevelt
month.normal.Roosevelt <- read.csv(prism.normals$normals_file[3], skip = 10)
colnames(month.normal.Roosevelt) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.Roosevelt$Site <- "Roosevelt"
month.normal.Roosevelt$Region <- "Sonoran Central"

#   SCC
month.normal.SCC <- read.csv(prism.normals$normals_file[4], skip = 10)
colnames(month.normal.SCC) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.SCC$Site <- "SCC"
month.normal.SCC$Region <- "Sonoran Central"

#   Patagonia
month.normal.Patagonia <- read.csv(prism.normals$normals_file[5], skip = 10)
colnames(month.normal.Patagonia) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.Patagonia$Site <- "Patagonia"
month.normal.Patagonia$Region <- "Sonoran SE"

#   SRER
month.normal.SRER <- read.csv(prism.normals$normals_file[6], skip = 10)
colnames(month.normal.SRER) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.SRER$Site <- "SRER"
month.normal.SRER$Region <- "Sonoran SE"



# Combine all sites
month.normal <- bind_rows(month.normal.Pleasant, month.normal.Preserve,
                          month.normal.Roosevelt, month.normal.SCC, 
                          month.normal.Patagonia, month.normal.SRER) |> 
  arrange(Site) |> 
  arrange(Region)



# Compile PRISM ppt, MAP, MAT with monitor events -------------------------

# Narrow columns
monitor.site.info <- prism.daily |> 
  select(Region, Site, Date_Seeded, Date_Monitored, SiteDateID,
         Cum_precip, Since_last_precip) |> 
  filter(!is.na(SiteDateID)) # remove last line which is not actual event but contains path to cumulative 29 Palms precip

# Site-specific info, independent of date 
site.info <- prism.normals |> 
  select(-path_beginning, -normals_file, -Farrell_MAP, -Farrell_MAT) |> 
  rename(Latitude = Farrell_lat,
         Longitude = Farrell_long,
         Elevation_ft = Farrell_elev,
         Sand_content = Farrell_sand,
         Clay_content = Farrell_clay)

# Add site-specific info to list with dates
monitor.site.info <- monitor.site.info |> 
  left_join(site.info) |> 
  select(Region, Site, Date_Seeded, Date_Monitored, SiteDateID, Latitude,
         Longitude, Elevation_ft, Sand_content, Clay_content,
         MAP, MAT, Cum_precip, Since_last_precip)

# Test to make sure monitoring events are not conflicting
monitor.info |> 
  left_join(monitor.site.info) |> 
  filter(is.na(SiteDatePlotID)) # no conflicts



# Compile daily values for each site --------------------------------------

# Make a table of precip data from PRISM for every day of the experiment:
#   Compile total of all precip values used without duplicates
#   (the separate PRISM files have a lot of overlap).

# For sites that were not reseeded (all Sonoran sites), the file of cumulative precipitation of the
#   last monitoring event contains all the days. 

# Sites not reseeded
#   Pleasant
daily.Pleasant <- read.csv(prism.daily$cum_file[6], skip = 10)
colnames(daily.Pleasant) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.Pleasant$Site <- "Pleasant"
daily.Pleasant$Region <- "Sonoran Central"

#   Preserve
daily.Preserve <- read.csv(prism.daily$cum_file[11], skip = 10)
colnames(daily.Preserve) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.Preserve$Site <- "Preserve"
daily.Preserve$Region <- "Sonoran Central"

#   Roosevelt
daily.Roosevelt <- read.csv(prism.daily$cum_file[16], skip = 10)
colnames(daily.Roosevelt) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.Roosevelt$Site <- "Roosevelt"
daily.Roosevelt$Region <- "Sonoran Central"

#   SCC
daily.SCC <- read.csv(prism.daily$cum_file[20], skip = 10)
colnames(daily.SCC) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.SCC$Site <- "SCC"
daily.SCC$Region <- "Sonoran Central"

#   Patagonia
daily.Patagonia <- read.csv(prism.daily$cum_file[29], skip = 10)
colnames(daily.Patagonia) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.Patagonia$Site <- "Patagonia"
daily.Patagonia$Region <- "Sonoran SE"

#   SRER
daily.SRER <- read.csv(prism.daily$cum_file[41], skip = 10)
colnames(daily.SRER) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.SRER$Site <- "SRER"
daily.SRER$Region <- "Sonoran SE"


# Combine all sites
daily <- bind_rows(daily.Pleasant, daily.Preserve,
                   daily.Roosevelt, daily.SCC, 
                   daily.Patagonia, daily.SRER) |> 
  arrange(Site) |> 
  arrange(Region)




# Write clean climate data to CSV -----------------------------------------

# Monitor info with climate data
write_csv(monitor.site.info,
          file = "Sonoran-data/cleaned/03.1_monitoring-events-with-PRISM-climate-data_clean.csv")


# Monthly normals
write_csv(month.normal,
          file = "Sonoran-data/cleaned/03.1_PRISM-month-normals-all-sites_clean.csv")


# Daily values
write_csv(daily,
          file = "Sonoran-data/cleaned/03.1_PRISM-daily-all-sites_clean.csv")



save.image("Sonoran-RData/03.1_PRISM-data-wrangling.RData")
