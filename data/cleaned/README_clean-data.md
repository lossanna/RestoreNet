Created: 2023-10-20  
Last updated: 2023-10-20
  
Notes about cleaned data for RAMPS RestoreNet 1.0 project.


# Directory

## From `01_curate-species-list.R`
#### `01_2x2_species-list_location-dependent_clean.csv`


#### `01_2x2_species-list_location-independent_clean.csv`

#### `01_species-list_location-dependent_clean.csv`

#### `01_species-list_location-independent_clean.csv`

#### `01_subplot_species-list_location-dependent_clean.csv`

#### `01_subplot_species-list_location-independent_clean.csv`


## From `02_correct-monitoring-info.R`
#### `02_corrected-monitoring-info_clean.csv`

#### `02_SiteDateID_clean.csv`

#### `02_SitePlotID_clean.csv`


## From `03.2_PRISM-data-wrangling.R`
#### `03.2_monitoring-events-with-PRISM-climate-data_clean.csv`

#### `03.2_PRISM-daily-all-sites_clean.csv`
- All of the daily PRISM data used to calculate precipitation since last monitoring event, and cumulative precipitation since seeding (the two ways we are measuring precip here, as Farrell did). This is to have the data all in one place, rather than a million separate files that I originally downloaded (in `data/prism-dat/`).

#### `03.2_PRISM-month-normals-all-sites_clean.csv`
- 30-year normals of precip, max temp, min, temp, and average temp for all sites. Normals are given as monthly values, and an annual value.
- Original files I downloaded from PRISM were for each site individually; this is a compilation of all normals.

## From `04.1_data-wrangling_subplot.R`
#### `04.1_subplot-data_clean.csv`