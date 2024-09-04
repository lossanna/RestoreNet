Created: 2023-10-20  
Last updated: 2024-09-04
  
Notes about cleaned data for RAMPS RestoreNet 1.0 project.


# Directory

## From `01_curate-species-list.R`
Species lists were used to compile correct information about plants recorded during data wrangling for `subplot` and `2x2` data. Information included standardized spelling and USDA Plants codes, the codes I developed, lifeform, duration, and native status.
#### `01_2x2_species-list_location-dependent_clean.csv`
#### `01_2x2_species-list_location-independent_clean.csv`
#### `01_species-list_location-dependent_clean.csv`
#### `01_species-list_location-independent_clean.csv`
#### `01_subplot_species-list_location-dependent_clean.csv`
#### `01_subplot_species-list_location-independent_clean.csv`

## From `02_correct-monitoring-info.R`
Corrected monitoring info was used during data wrangling for `subplot` and `2x2` data.
#### `02_corrected-monitoring-info_clean.csv`
- Columns: `Region`, `Site`, `Date_Seeded`, `Date_Monitored`, `Plot`, `Treatment`, `PlotMix`, `SiteDatePlotID`.
#### `02_SiteDateID_clean.csv`
#### `02_SitePlotID_clean.csv`

## From `03.2_PRISM-data-wrangling.R`
#### `03.2_monitoring-events-with-PRISM-climate-data_clean.csv`
- Columns: `Region`, `Site`, `Date_Seeded`, `Date_Monitored`, `SiteDateID`, `Latitude`, `Longitude`, `Elevation_ft`, `Sand_content`, `Clay_content`, `MAP`, `MAT`, `Cum_precip`, `Since_last_precip`.
#### `03.2_PRISM-daily-all-sites_clean.csv`
- All of the daily PRISM data used to calculate precipitation since last monitoring event, and cumulative precipitation since seeding (the two ways we are measuring precip here, as Farrell did). This is to have the data all in one place, rather than a million separate files that I originally downloaded (in `data/prism-dat/`).
#### `03.2_PRISM-month-normals-all-sites_clean.csv`
- 30-year normals of precip, max temp, min, temp, and average temp for all sites. Normals are given as monthly values, and an annual value.
- Original files I downloaded from PRISM were for each site individually; this is a compilation of all normals.

## From `03.3_explore-precip-trends.R`
#### `03.3_cumulative-precip_CV_clean.csv`
-  Deprecated.
#### `03.3_cumulative-precip_percent-deviation-from-norm_clean.csv`
- Percent deviation from normals of cumulative precipitation was ultimately used as the measure of precipitation variability.
#### `03.3_since-last-precip_CV_clean.csv`
- Deprecated.
#### `03.3_since-last-precip_percent-deviation-from-norm_clean.csv`
- Deprecated.

## From `03.4_aridity-index.R`
#### `03.4_aridity-index-values_clean.csv`
- Columns: `Region`, `Site`, `Latitude`, `Longitude`, `AridityIndex`.

## From `04.1_data-wrangling_subplot.R`
#### `04.1_subplot-data_clean.csv`
- Columns: `Region`, `Site`, `Date_Seeded`, `Date_Monitored`, `SiteDateID`, `Plot`, `Treatment`, `PlotMix`, `PlotMix_Climate`, `SitePlotID`, `SiteDatePlotID`, `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`,  `SpeciesSeeded`, `PlantSource`, `PlantSource2`, `Weedy`, `Count`, `Height`, `raw.row`.
- `Count` and `Height` are the response variables.

## From `04.2_data-wrangling_2x2.R`
#### `04.2_2x2-richness-cover_clean.csv`
- Columns:  `Region`, `Site`, `Date_Seeded`, `Date_Monitored`, `SiteDateID`, `Plot`, `Treatment`, `PlotMix`, `SiteDatePlotID`, `SitePlotID`, `raw.row`, `Richness`, `Seeded`, `Native_recruit`, `LikelyNative_recruit`, `Unknown_recruit`, `Invasive`, `Desirable_recruit`, `Weedy`, `Desirable`, `Seeded_Cover`, `Total_Veg_Cover`, `Not_Seeded_Cover`.
#### `04.2_2x2-species-present_clean.csv`
- Columns: `Region`, `Site`, `Date_Seeded`, `Date_Monitored`, `Plot`, `Treatment`, `PlotMix`, `SiteDatePlotID`, `SiteDateID`, `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`,  `SpeciesSeeded`, `ObsSource`, `PlantSource`, `PlantSource2`, `Weedy`, `PlotMix_Climate`, `SitePlotID`.

## From `04.15_data-wrangling_subplot_add-0s.R`
#### `04.15_subplot-data_clean-0-added.csv`
- Deprecated.
- Added 0s to track the absence of all seeded species across plots (versus only the presence); was an attempt to deal with GLMs overfitting zeros.