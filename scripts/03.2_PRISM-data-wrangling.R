# Created: 2023-09-27
# Last updated: 2023-10-20

# Purpose: Compile climate data gathered from PRISM and compare to 
#   Farrell 2023 data (specifically cumulative precipitation, precipitation
#   since last monitoring event, MAT, and MAP). 
# Farrell used 800m resolution data for daily values, and I used 4km 
#   (800m not freely available); Farrell used 1981-2010 normals and I used
#   1991-2020 normals (both at 800m).


library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

prism.daily.raw <- read_xlsx("data/data-wrangling-intermediate/03.2_monitoring-events-with-Farrell-climate-data-and-PRISM-csv-file-name.xlsx",
                             sheet = "daily")
prism.normals.raw <- read_xlsx("data/data-wrangling-intermediate/03.2_monitoring-events-with-Farrell-climate-data-and-PRISM-csv-file-name.xlsx",
                           sheet = "normals")

monitor.info <- read_csv("data/cleaned/02_corrected-monitoring-info_clean.csv")
monitor.site <- read_csv("data/cleaned/02_corrected-monitoring-info-by-date-and-site_clean.csv")



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
#   Creosote
month.normal.Creosote <- read.csv(prism.normals$normals_file[1], skip = 10)
colnames(month.normal.Creosote) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.Creosote$Site <- "Creosote"
month.normal.Creosote$Region <- "Chihuahuan"

#   Mesquite
month.normal.Mesquite <- read.csv(prism.normals$normals_file[2], skip = 10)
colnames(month.normal.Mesquite) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.Mesquite$Site <- "Mesquite"
month.normal.Mesquite$Region <- "Chihuahuan"

#   AguaFria
month.normal.AguaFria <- read.csv(prism.normals$normals_file[3], skip = 10)
colnames(month.normal.AguaFria) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.AguaFria$Site <- "AguaFria"
month.normal.AguaFria$Region <- "Colorado Plateau"

#   BabbittPJ
month.normal.BabbittPJ <- read.csv(prism.normals$normals_file[4], skip = 10)
colnames(month.normal.BabbittPJ) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.BabbittPJ$Site <- "BabbittPJ"
month.normal.BabbittPJ$Region <- "Colorado Plateau"

#   BarTBar
month.normal.BarTBar <- read.csv(prism.normals$normals_file[5], skip = 10)
colnames(month.normal.BarTBar) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.BarTBar$Site <- "BarTBar"
month.normal.BarTBar$Region <- "Colorado Plateau"

#   FlyingM
month.normal.FlyingM <- read.csv(prism.normals$normals_file[6], skip = 10)
colnames(month.normal.FlyingM) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.FlyingM$Site <- "FlyingM"
month.normal.FlyingM$Region <- "Colorado Plateau"

#   MOWE
month.normal.MOWE <- read.csv(prism.normals$normals_file[7], skip = 10)
colnames(month.normal.MOWE) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.MOWE$Site <- "MOWE"
month.normal.MOWE$Region <- "Colorado Plateau"

#   PEFO
month.normal.PEFO <- read.csv(prism.normals$normals_file[8], skip = 10)
colnames(month.normal.PEFO) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.PEFO$Site <- "PEFO"
month.normal.PEFO$Region <- "Colorado Plateau"

#   Spiderweb
month.normal.Spiderweb <- read.csv(prism.normals$normals_file[9], skip = 10)
colnames(month.normal.Spiderweb) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.Spiderweb$Site <- "Spiderweb"
month.normal.Spiderweb$Region <- "Colorado Plateau"

#   TLE
month.normal.TLE <- read.csv(prism.normals$normals_file[10], skip = 10)
colnames(month.normal.TLE) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.TLE$Site <- "TLE"
month.normal.TLE$Region <- "Colorado Plateau"

#   29_Palms
month.normal.29_Palms <- read.csv(prism.normals$normals_file[11], skip = 10)
colnames(month.normal.29_Palms) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.29_Palms$Site <- "29_Palms"
month.normal.29_Palms$Region <- "Mojave"

#   AVRCD
month.normal.AVRCD <- read.csv(prism.normals$normals_file[12], skip = 10)
colnames(month.normal.AVRCD) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.AVRCD$Site <- "AVRCD"
month.normal.AVRCD$Region <- "Mojave"

#   Pleasant
month.normal.Pleasant <- read.csv(prism.normals$normals_file[13], skip = 10)
colnames(month.normal.Pleasant) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.Pleasant$Site <- "Pleasant"
month.normal.Pleasant$Region <- "Sonoran Central"

#   Preserve
month.normal.Preserve <- read.csv(prism.normals$normals_file[14], skip = 10)
colnames(month.normal.Preserve) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.Preserve$Site <- "Preserve"
month.normal.Preserve$Region <- "Sonoran Central"

#   Roosevelt
month.normal.Roosevelt <- read.csv(prism.normals$normals_file[15], skip = 10)
colnames(month.normal.Roosevelt) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.Roosevelt$Site <- "Roosevelt"
month.normal.Roosevelt$Region <- "Sonoran Central"

#   SCC
month.normal.SCC <- read.csv(prism.normals$normals_file[16], skip = 10)
colnames(month.normal.SCC) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.SCC$Site <- "SCC"
month.normal.SCC$Region <- "Sonoran Central"

#   Patagonia
month.normal.Patagonia <- read.csv(prism.normals$normals_file[17], skip = 10)
colnames(month.normal.Patagonia) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.Patagonia$Site <- "Patagonia"
month.normal.Patagonia$Region <- "Sonoran SE"

#   SRER
month.normal.SRER <- read.csv(prism.normals$normals_file[18], skip = 10)
colnames(month.normal.SRER) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.SRER$Site <- "SRER"
month.normal.SRER$Region <- "Sonoran SE"

#   CRC
month.normal.CRC <- read.csv(prism.normals$normals_file[19], skip = 10)
colnames(month.normal.CRC) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.CRC$Site <- "CRC"
month.normal.CRC$Region <- "Utah"

#   Salt_Desert
month.normal.Salt_Desert <- read.csv(prism.normals$normals_file[20], skip = 10)
colnames(month.normal.Salt_Desert) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.Salt_Desert$Site <- "Salt_Desert"
month.normal.Salt_Desert$Region <- "Utah"

#   UtahPJ
month.normal.UtahPJ <- read.csv(prism.normals$normals_file[21], skip = 10)
colnames(month.normal.UtahPJ) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
month.normal.UtahPJ$Site <- "UtahPJ"
month.normal.UtahPJ$Region <- "Utah"


# Combine all sites
month.normal <- bind_rows(month.normal.29_Palms, month.normal.AguaFria,
                          month.normal.AVRCD, month.normal.BabbittPJ,
                          month.normal.BarTBar, month.normal.CRC,
                          month.normal.Creosote, month.normal.FlyingM,
                          month.normal.Mesquite, month.normal.MOWE,
                          month.normal.Patagonia, month.normal.PEFO,
                          month.normal.Pleasant, month.normal.Preserve,
                          month.normal.Roosevelt, month.normal.Salt_Desert,
                          month.normal.SCC, month.normal.Spiderweb,
                          month.normal.SRER,
                          month.normal.TLE, month.normal.UtahPJ) |> 
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

# For sites that were not reseeded, the file of cumulative precipitation of the
#   last monitoring event contains all the days. 
# For the Mojave plots that were reseeded at AVRCD, the last monitoring event still contains
#   all of the days, because some plots were not reseeded.
# For the Mojave plots that were reseeded at 29 Palms, there is a large gap between
#   reseeding and next monitoring event, but I already have a file that contains all
#   the days (back when I thought only some plots were reseeded).
# For the 6 CO Plateau sites that were reseeded, some sites have no gap in time 
#   (sites were monitored and then reseeded in the same day), 
#   and some sites have a small gap in time (a few days missing from when 
#   it was monitored, and then reseeded a few days later):
#     No gap: MOWE, PEFO
#     Gap of 2 days: BabbittPJ, FlyingM
#     Gap of 5 days: BarTBar
#     Gap of 7 days: Spiderweb
#   Even a 1-week gap is pretty small, and I just want to use this continuous data
#     for visualizing trends, so having small gaps is okay. 
# Therefore, for sites that were reseeded, combining the files of cumulative precipitation of
#   the last monitoring events after both seeding dates will contain all of the
#   days (minus the gaps mentioned above).


# Sites not reseeded
#   Creosote
daily.Creosote <- read.csv(prism.daily$cum_file[7], skip = 10)
colnames(daily.Creosote) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.Creosote$Site <- "Creosote"
daily.Creosote$Region <- "Chihuahuan"

#   Mesquite
daily.Mesquite <- read.csv(prism.daily$cum_file[13], skip = 10)
colnames(daily.Mesquite) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.Mesquite$Site <- "Mesquite"
daily.Mesquite$Region <- "Chihuahuan"

#   AguaFria
daily.AguaFria <- read.csv(prism.daily$cum_file[20], skip = 10)
colnames(daily.AguaFria) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.AguaFria$Site <- "AguaFria"
daily.AguaFria$Region <- "Colorado Plateau"

#   TLE
daily.TLE <- read.csv(prism.daily$cum_file[111], skip = 10)
colnames(daily.TLE) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.TLE$Site <- "TLE"
daily.TLE$Region <- "Colorado Plateau"

#   29_Palms
daily.29_Palms <- read.csv(prism.daily$cum_file[188], skip = 10)
colnames(daily.29_Palms) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.29_Palms$Site <- "29_Palms"
daily.29_Palms$Region <- "Mojave"

#   AVRCD
daily.AVRCD <- read.csv(prism.daily$cum_file[119], skip = 10)
colnames(daily.AVRCD) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.AVRCD$Site <- "AVRCD"
daily.AVRCD$Region <- "Mojave"

#   Pleasant
daily.Pleasant <- read.csv(prism.daily$cum_file[125], skip = 10)
colnames(daily.Pleasant) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.Pleasant$Site <- "Pleasant"
daily.Pleasant$Region <- "Sonoran Central"

#   Preserve
daily.Preserve <- read.csv(prism.daily$cum_file[130], skip = 10)
colnames(daily.Preserve) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.Preserve$Site <- "Preserve"
daily.Preserve$Region <- "Sonoran Central"

#   Roosevelt
daily.Roosevelt <- read.csv(prism.daily$cum_file[135], skip = 10)
colnames(daily.Roosevelt) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.Roosevelt$Site <- "Roosevelt"
daily.Roosevelt$Region <- "Sonoran Central"

#   SCC
daily.SCC <- read.csv(prism.daily$cum_file[139], skip = 10)
colnames(daily.SCC) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.SCC$Site <- "SCC"
daily.SCC$Region <- "Sonoran Central"

#   Patagonia
daily.Patagonia <- read.csv(prism.daily$cum_file[148], skip = 10)
colnames(daily.Patagonia) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.Patagonia$Site <- "Patagonia"
daily.Patagonia$Region <- "Sonoran SE"

#   SRER
daily.SRER <- read.csv(prism.daily$cum_file[160], skip = 10)
colnames(daily.SRER) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.SRER$Site <- "SRER"
daily.SRER$Region <- "Sonoran SE"

#   CRC
daily.CRC <- read.csv(prism.daily$cum_file[170], skip = 10)
colnames(daily.CRC) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.CRC$Site <- "CRC"
daily.CRC$Region <- "Utah"

#   Salt_Desert
daily.Salt_Desert <- read.csv(prism.daily$cum_file[179], skip = 10)
colnames(daily.Salt_Desert) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.Salt_Desert$Site <- "Salt_Desert"
daily.Salt_Desert$Region <- "Utah"

#   UtahPJ
daily.UtahPJ <- read.csv(prism.daily$cum_file[187], skip = 10)
colnames(daily.UtahPJ) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.UtahPJ$Site <- "UtahPJ"
daily.UtahPJ$Region <- "Utah"


# Sites that were reseeded
#   BabbittPJ
daily.BabbittPJ <- read.csv(prism.daily$cum_file[32], skip = 10) |> 
  bind_rows(read.csv(prism.daily$cum_file[36], skip = 10))
colnames(daily.BabbittPJ) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.BabbittPJ$Site <- "BabbittPJ"
daily.BabbittPJ$Region <- "Colorado Plateau"

#   BarTBar
daily.BarTBar <- read.csv(prism.daily$cum_file[48], skip = 10) |> 
  bind_rows(read.csv(prism.daily$cum_file[51], skip = 10))
colnames(daily.BarTBar) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.BarTBar$Site <- "BarTBar"
daily.BarTBar$Region <- "Colorado Plateau"

#   FlyingM
daily.FlyingM <- read.csv(prism.daily$cum_file[62], skip = 10) |> 
  bind_rows(read.csv(prism.daily$cum_file[66], skip = 10))
colnames(daily.FlyingM) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.FlyingM$Site <- "FlyingM"
daily.FlyingM$Region <- "Colorado Plateau"

#   MOWE
daily.MOWE <- read.csv(prism.daily$cum_file[76], skip = 10) |> 
  bind_rows(read.csv(prism.daily$cum_file[79], skip = 10)) |> 
  distinct(.keep_all = TRUE)
colnames(daily.MOWE) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.MOWE$Site <- "MOWE"
daily.MOWE$Region <- "Colorado Plateau"

#   PEFO
daily.PEFO <- read.csv(prism.daily$cum_file[90], skip = 10) |> 
  bind_rows(read.csv(prism.daily$cum_file[93], skip = 10)) |> 
  distinct(.keep_all = TRUE)
colnames(daily.PEFO) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.PEFO$Site <- "PEFO"
daily.PEFO$Region <- "Colorado Plateau"

#   Spiderweb
daily.Spiderweb <- read.csv(prism.daily$cum_file[103], skip = 10) |> 
  bind_rows(read.csv(prism.daily$cum_file[107], skip = 10))
colnames(daily.Spiderweb) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
daily.Spiderweb$Site <- "Spiderweb"
daily.Spiderweb$Region <- "Colorado Plateau"


# Combine all sites
daily <- bind_rows(daily.29_Palms, daily.AguaFria,
                   daily.AVRCD, daily.BabbittPJ,
                   daily.BarTBar, daily.CRC,
                   daily.Creosote, daily.FlyingM,
                   daily.Mesquite, daily.MOWE,
                   daily.Patagonia, daily.PEFO,
                   daily.Pleasant, daily.Preserve,
                   daily.Roosevelt, daily.Salt_Desert,
                   daily.SCC, daily.Spiderweb,
                   daily.SRER,
                   daily.TLE, daily.UtahPJ) |> 
  arrange(Site) |> 
  arrange(Region)




# Write clean climate data to CSV -----------------------------------------

# Monitor info with climate data
write_csv(monitor.site.info,
          file = "data/cleaned/03.2_monitoring-events-with-PRISM-climate-data_clean.csv")


# Monthly normals
write_csv(month.normal,
          file = "data/cleaned/03.2_PRISM-month-normals-all-sites_clean.csv")


# Daily values
write_csv(daily,
          file = "data/cleaned/03.2_PRISM-daily-all-sites_clean.csv")



save.image("RData/03.2_PRISM-data-wrangling.RData")
