Created: 2023-09-18  
Last updated: 2025-01-21
  
Notes about raw data for RAMPS RestoreNet 1.0 project (original/dissertation analysis).

Data source:
- Data all comes either directly from a tab in `2023-09-15_Master Germination Data_raw.xlsx` (`Master.xlsx` for short), or adapted from a tab.
    - The exception is suplot data from 29 Palms in Spring 2022, which is missing from the master spreadsheet but was collected and exists in a different file (`29Palms_Spr22.xlsx`).
- Species list: include columns `CodeOriginal`, `Name`, and `Native` (native/introduced, derived from USDA Plants database). 
- `subplot` data: monitoring observations from the 25 x 25 cm subplots. Measurements are seedling density and average height by species. Raw data from `AllSubplotData` tab of `Master.xlsx`.
- `2x2` data: monitoring observations from the 2 x 2 m plots. Measurements are seeded cover and total vegetation cover, and names of additional species in plot (in addition to what was present in the subplot and already recorded). Raw data from `AllPlotData` tab of `Master.xlsx`.


# Directory
`2023-09-15_Master Germination Data_raw.xlsx`
- Received from Laura Shriver via email on 2023-09-15. Includes all of RestoreNet 1.0 data, from 2018 to 2023 (after that, RestoreNet 2.0 began).
- File will not be modified in any way.  

`from-Master_seed-mix_LO.xlsx`
- Five tabs: `from-Master.xlsx`, `with-site`, `with-site_R`, `with-site_R (2)`, and `Sonoran & N AZ`.
- `from-Master.xlsx` tab is directly from the `Seed Mixes` tab of `Master.xlsx`, but codes have been alphabetized within each region. It was read into `01_curate-species-list.R` script to compare codes (`CodeOriginal`) with those from `from-Master_species-list-with-native-status_LO.xlsx` and `subplot` data.
- `with-site` tab has been edited to correct seed mixes for each specific site, as confirmed by Farrell 2023, Havrilla 2020, monitoring event data, and what was originally listed in `Master.xlsx`. This lis is used during manual check and corrections of the `SpeciesSeeded` column for the `subplot` data in `03.1_data-wrangling_subplot.R`.
- `with-site_R` tab is the same as `with-site` tab, but each site is listed in its own row, rather than multiple sites listed in the same row (this is easier for a human to read).
- `with-site_R (2)` tab is the same as `with-site_R` tab, except `PlotMix_Climate` column has also been added.
- `Sonoran & N AZ` tab is formatted to be Table S1 in dissertation.
 
`from-Master_species-list-with-native-status_LO.xlsx`
- Adapted from the `Species_Definitions` tab of `Master.xlsx`.
- Manually added the `Native` column based on USDA Plants database, and did small corrections to spelling of scientific names.
- I did not change any species codes, as they relate directly to the `Master` data (the original `subplot` and `2x2` data).
- Used to be read into `01.R`. Will be replaced by other species lists made from `01.R` for data wrangling.

`29Palms_Spr22.xlsx`
- Subplot data from 29 Palms in Spring 2022. Observations from 2x2 plots are included in `Master.xlsx`, but not subplot data. I emailed about this and got this separate spreadsheet that shows the subplots were monitored, but all had 0 recruitment.
