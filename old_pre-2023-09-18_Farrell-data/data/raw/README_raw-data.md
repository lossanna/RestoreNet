Created: 2022-11-28  
Last updated: 2023-09-18
  
Notes about raw data for RAMPS RestoreNet project.

Data source:
- Data all comes either directly from a tab in `Master Germination Data 2022.xlsx` (`Master.xlsx` for short), or adapted from a tab.
- Species list: include columns `CodeOriginal`, `Name`, and `Native` (native/introduced, derived from USDA Plants database). 
- `subplot` data: monitoring observations from the 25 x 25 cm subplots. Measurements are seedling density and average height by species. Raw data from `AllSubplotData` tab of `Master Germination Data 2022.xlsx`.
- `2x2` data: monitoring observations from the 2 x 2 m plots. Measurements are seeded cover and total vegetation cover, and names of additional species in plot (in addition to what was present in the subplot and already recorded). Raw data from `AllPlotData` tab of `Master.xlsx`.


# Directory
`from-Master_seed-mix_LO.xlsx`
- Two tabs: `from-Master.xlsx` and `with-site`.
- `from-Master.xlsx` is directly from the `Seed Mixes` tab of `Master.xlsx`, but codes have been alphabetized within each region. It was read into `01_curate-species-list.R` script to compare codes (`CodeOriginal`) with those from `from-Master_species-list-with-native-status_LO.xlsx` and `subplot` data.
- `with-site` tab has been edited to correct seed mixes for each specific site, as confirmed by Farrell 2023, Havrilla 2020, monitoring event data, and what was originally listed in `Master.xlsx`. This lis is used during manual check and corrections of the `SpeciesSeeded` column for the `subplot` data.

`from-Master_species-list-with-native-status_LO.xlsx`
- Adapted from the `Species_Definitions` tab of `Master.xlsx`.
- Manually added the `Native` column based on USDA Plants database, and did small corrections to spelling of scientific names.
- I did not change any species codes, as they relate directly to the `Master` data (the original `subplot` and `2x2` data).
- Used to be read into `01.R`.

`Master Germination Data 2022.xlsx`
- Received from Hannah Farrell via email. I believe this is what she used to write her paper, which is currently in review.
- File will not be modified in any way.  
