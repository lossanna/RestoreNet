Created: 2022-11-28  
Last updated: 2022-11-29  
  
Notes about raw data for RAMPS RestoreNet project:

# Directory
`edited_codes-missing.csv`
- List of codes that were present on subplot data sheets but were not included in origina species list from `Master.xlsx`.
- Manually edited to add native status, lifeform, and duration.  

`edited_lifeform-na.csv`
- Manually edited version of `output_lifeform-na.csv`, created to assign lifeform (functional group) to plant species based on existing labels from subplot data from `Master.xlsx`.
- Lifeform according to USDA Plants.

`edited_species-unique_native-lifeform-duration.csv`
- Manually edited list to resolve conflicting lifeform assignments, and added plant duration based on USDA Plants.

`Master Germination Data 2022.xlsx`
- Received from Hannah Farrell via email. I believe this is what she used to write her paper, which is currently in review.
- File will not be modified in any way.  

`output_codes-missing/csv`
- List of codes that were included in the subplot data (`AllSubplotData` tab of `Master.xlsx`) but not included in the original species list (plant-species_native status, adapted from `Species_Definitions` tab of `Master.xlsx`).
- Output written directly from R and edited in a new file.

`output_lifeform-na.csv`
- List of species without lifeform (functional group) assignment from the species mix data (`seed-mix.xlsx`) or fromthe subplot data from `Master.xlsx`.
- Output written directly from R and edited in a new file.

`output_species-unique_native-lifeform.csv`
- List of unique species codes (although there are some duplicates when there are conflicting lifeforms or names for the same code).
- Output written directly from R and edited in a new file.


`plant-species_native-status.xlsx`
- Adapted from the `Species_Definitions` tab of `Master.xlsx`.
- I added the `Native` column based on USDA Plants databse, and did small corrections to spelling of scientific names.
- I did not change any species codes, as they relate directly to the `Master` data.

`seed-mix.xlsx`
- Adapted from the `Seed Mixes` tab of `Master.xlsx`.
- Added native status column (all seeded species were native).



