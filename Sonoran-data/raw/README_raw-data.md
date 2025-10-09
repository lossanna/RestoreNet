Created: 2025-01-21  
Last updated: 2025-10-09  
  
Notes about raw data for RAMPS RestoreNet 1.0 project, updated analysis for publication with Sonoran Desert sites only.

# Notes
Data source:
- Data all comes adapted versions of tabs from  `2023-09-15_Master Germination Data_raw.xlsx` (`Master.xlsx` for short), which is what I received from Laura Shriver (USGS) on 2023-09-15.
    - This version is the same as the one in the `data/raw/` folder used for original/dissertation analysis.
- Species list: include columns `CodeOriginal`, `Name`, and `Native` (native/introduced, derived from USDA Plants database). 
- `subplot` data: monitoring observations from the 25 x 25 cm subplots. Measurements are seedling density and average height by species.
- `2x2` data: monitoring observations from the 2 x 2 m plots. Measurements are seeded cover and total vegetation cover, and names of additional species in plot (in addition to what was present in the subplot and already recorded). 

Adapted files:
- Tabs were adapted and manually edited to incorporate information from the `Notes` columns into the `subplot` and `2x2` data for the Sonoran sites in 2025-10, so the original unchanged `Master.xlsx` is no longer used moving forward. 
    - Instead, it has been replaced by `Sonoran_2023-09-15_Master 1.0 Germination Data_LO.xlsx` (`Sonoran-Master.xlsx` for short).
- The species list was adapted from the original `Master.xlsx` in a new file, `from-Master_species-list-with-native-status_LO.xlsx`.
- The seed mix was also adapted from the original `Master.xlsx` in a new file, `from-Master_seed-mix_LO_Sonoran.xlsx`.




# Directory
### `2023-09-15_Master Germination Data_raw.xlsx`
- Received from Laura Shriver via email on 2023-09-15. Includes all of RestoreNet 1.0 data, from 2018 to 2023 (after that, RestoreNet 2.0 began).
- File will not be modified in any way.  
- Same as file in `data/raw/` folder for original/dissertation analysis.

### `from-Master_seed-mix_LO.xlsx`
- Five tabs: `from-Master.xlsx`, `with-site`, `with-site_R`, `with-site_R (2)`, and `Sonoran & N AZ`.
- `from-Master.xlsx` tab is directly from the `Seed Mixes` tab of `Master.xlsx`, but codes have been alphabetized within each region. It was read into `01_curate-species-list.R` script to compare codes (`CodeOriginal`) with those from `from-Master_species-list-with-native-status_LO.xlsx` and `subplot` data.
- `with-site` tab has been edited to correct seed mixes for each specific site, as confirmed by Farrell 2023, Havrilla 2020, monitoring event data, and what was originally listed in `Master.xlsx`. This lis is used during manual check and corrections of the `SpeciesSeeded` column for the `subplot` data in `03.1_data-wrangling_subplot.R`.
- `with-site_R` tab is the same as `with-site` tab, but each site is listed in its own row, rather than multiple sites listed in the same row (this is easier for a human to read).
- `with-site_R (2)` tab is the same as `with-site_R` tab, except `PlotMix_Climate` column has also been added.
- `Sonoran` tab is formatted to be possible table in publication.
 
### `from-Master_species-list-with-native-status_LO.xlsx`
- Adapted from the `Species_Definitions` tab of `Master.xlsx`.
- Manually added the `Native` column based on USDA Plants database, and did small corrections to spelling of scientific names.
- I did not change any species codes, as they relate directly to the `Master` data (the original `subplot` and `2x2` data).
- Used to be read into `01.R`. Will be replaced by other species lists made from `01.R` for data wrangling.
- Same as file in `data/raw/` folder for original/dissertation analysis.

### `Sonoran_2023-09-15_Master 1.0 Germination Data_LO.xlsx`
- Adapted from the four tabs with `subplot` and `2x2` data from the Sonoran sites in `Master.xlsx`:
    1. `SonoranSE_Subplots`
    2. `SonoranSE_Plots`
    3. `SonoranCentral_Subplots`
    4. `SonoranCentral_Plots`
-  No longer using `AllSubplotData`/`AllPlotData` tabs from `Master.xlsx` due to manual edits required in Excel to integrate information fom `Notes` columns.
- 10 tabs: 
    - 2 info tabs:
        1. `README` with bulleted list of notes
        2. `Data_Info` of column metadata, copied directly from original `Master.xlsx`.
    - The same four data tabs copied directly from  original `Master.xlsx` and not changed.
    - Four adapted data tabs (changed cells highlighted in yellow):
        1. `SonoranSE_Subplots_LO`
        2. `SonoranSE_Plots_LO`
        3. `SonoranCentral_Subplots_LO`
        4. `SonoranCentral_Plots_LO`
- These adapted `_LO` data tabs will be used for reading in raw data.