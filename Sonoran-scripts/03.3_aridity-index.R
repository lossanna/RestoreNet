# Created: 2025-01-31
# Last updated: 2025-10-10

# Purpose: Extract aridity index values (based on annual averages) from the Global Aridity Index
#   and Potential Evapotranspiration Database for each site.

# Publication about database: https://doi.org/10.1038/s41597-022-01493-1
# Database download: https://figshare.com/articles/dataset/Global_Aridity_Index_and_Potential_Evapotranspiration_ET0_Climate_Database_v2/7504448/6
#   DOI: https://doi.org/10.6084/m9.figshare.7504448.v5 (DOI links to V5, but V6 was available for download)
#   From there, download Global-AI_ET0_annual_v3 file (do not need to download all files)

# Note: do not load tidyverse, it will interfere with extract() function.


library(raster)
library(sp)


# Load data ---------------------------------------------------------------

prism.dat <- read.csv("Sonoran-data/cleaned/03.1_monitoring-events-with-PRISM-climate-data_clean.csv")


# Import raster -----------------------------------------------------------

# Import raster for Aridity Index data (file is called ai_v3_yr.tif)
#   Set working directory to where database is located
ai_raster <- raster(x = "C:/Users/lossanna/OneDrive - New Mexico State University/Documents/02_RestoreNet/RestoreNet/Sonoran-data/Global-AI_ET0_annual_v3/ai_v3_yr.tif")


# Create table of GPS coordinates -----------------------------------------

sites.gps <- prism.dat[, c(1, 2, 7, 6)]
sites.gps <- sites.gps[!duplicated(sites.gps), ]

# GPS coordinates should be in WGS84 format
coords <- data.frame(x = sites.gps$Longitude,
                     y = sites.gps$Latitude)

# Change rasters to spatial points
ai_points <- SpatialPoints(coords, proj4string = ai_raster@crs)



# Extract values ----------------------------------------------------------

# Extract points
ai_values <- extract(ai_raster, ai_points)

# Multiply by 0.0001 to convert to correct units (as explained in database README)
AridityIndex <- ai_values * 0.0001

# Attach AI values to coordinates
ai <- cbind.data.frame(sites.gps, AridityIndex)



# Write to CSV ------------------------------------------------------------

write.csv(ai,
          file = "Sonoran-data/cleaned/03.3_aridity-index-values_clean.csv",
          row.names = FALSE)
