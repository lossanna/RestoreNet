Created: 2022-11-28  
Last updated: 2022-11-30  
  
Notes about raw data for RAMPS RestoreNet project.
- Intermediate CSVs required to curate species lists are saved here.
- Subplot species lists are separate from 2x2m plot species lists, and location-independent lists are separate from location-dependent ones.
- Curated species lists:
	+ Location-dependent (unknowns) codes/species for the subplot data, with name, native status, duration, lifeform, and Site (plus Region).
	+ Location-independent codes/species for the subplot data with name, native status, duration, and  lifeform.

Abbreviations:
- `Master Germination Data 2022.xlsx` is abbreviated as `Master.xlsx` and is never directly edited.

File naming notes:
- `master-` indicates the spreadsheet was adapted from a tab in `Master.xlsx`.
- `-species#` indicates the CSV is an intermediate step in curating a complete species list. Numbers correspond to edited files
- `output-` indicates the CSV was written from R. Numbers correspond to edited files, and sequentially mark workflow.
- `edited-` indicates the corresponding CSV written from R was manually edited.
- `xlsx_` indicates the species list includes only codes from  `master-species_native.xlsx`, with unknowns removed.
- `_native-lifeform-duration` indicates if the respective columns are present in the species list.

# Directory
`edited-species1_subplot-codes-missing.csv`
- List of codes included in the subplot data (`AllSubplotData` tab of `Master.xlsx`), but are missing from the the original master species list (`master-species_native.xlsx`).
- Manually edited to add plant name, duration, and lifeform based on USDA Plants.
- Not divided by location dependence, but ones that will be location-dependent (unknowns) have sites added to the name.

`edited-species2_xlsx_lifeform-na.csv`
- List of species originally without lifeform information. Subset of codes is taken only codes from the `master-species_native.xlsx`
	- Lifeform information would have come from `AllSubplotData` tab from `Master.xlsx`.
- Manually edited to assign missing lifeform (functional group) information.
- Lifeform according to USDA Plants.
- Not a complete species list.

`edited-species3_xlsx_native-lifeform-duration.csv`
- List of codes taken from `master-species_native.xlsx`. Not yet a complete species list.
- Manually edited to add plant duration, based on USDA Plants.
- Manually edited to resolve conflicting lifeform assignments or misspelled names to remove duplicates.  Codes are not changed, and different codes for the same species are maintained because they connect to the subplot data.
- Manually add a row of all 0s to mark observations of plots that had no plants.
- Not yet a complete species list.

`edited-species4_location-dependent_native-duration-lifeform.csv`
- List of location-dependent species (unknows), listed first by the ones from `master-species_native.unk` that lack site data, and then by location-dependent codes from the subplot data (`AllSubplotData` tab from `Master.xlsx`), whose site information was already manually added in `edited-species1_subplot-codes-missing_native-duration-lifeform`.
- Manually added site information for the unknowns from the master list (first section) based on site information in the subplot data, which sometimes included adding multiple rows because the same code occured at different sites. Codes have not been changed, and will be changed in the cleaned subplot data through data wrangling (`02_data-wrangling.R`).
	+ Sometimes codes were listed in the master species list that didn't occur in the subplot data, and therefore didn't have any site information. These codes were removed because they will serve no purpose, anyway.

`edited-species5_codes-missing-2x2plot.csv`
- List of codes that were in the 2x2 plot data (`AllPlotData` tab in `Master.xlsx`), but not yet in species list.
- Most of the codes are unknowns or descriptions.

`Master Germination Data 2022.xlsx`
- Received from Hannah Farrell via email. I believe this is what she used to write her paper, which is currently in review.
- File will not be modified in any way.  

`master-seed-mix.xlsx`
- Adapted from the `Seed Mixes` tab of `Master.xlsx`.
- Manually added native status column (all seeded species were native).

`master-species_native.xlsx`
- Adapted from the `Species_Definitions` tab of `Master.xlsx`.
- Manually added the `Native` column based on USDA Plants databse, and did small corrections to spelling of scientific names.
- I did not change any species codes, as they relate directly to the `Master` data.

`output-species1_subplot-codes-missing.csv`
- List of codes included in the subplot data (`AllSubplotData` tab of `Master.xlsx`), but are missing from the the original master species list (`master-species_native.xlsx`).
- Output written directly from R and edited in a new file.

`output-species2_xlsx_lifeform-na.csv`
- List of species without lifeform information. Subset of codes is taken only codes from the `master-species_native.xlsx`.
 	- Lifeform information would have come from `AllSubplotData` tab from `Master.xlsx`.
- Output written directly from R and edited in a new file.

`output-species3_xlsx_native-lifeform.csv`
- List of unique species codes (although there are some duplicates when there are conflicting lifeforms or names for the same code).
- Output written directly from R and edited in a new file.

`output-species4_codes-missing-2x2plot.csv`
- List of codes that were in the 2x2 plot data (`AllPlotData` tab in `Master.xlsx`), but not yet in species list.
- Most of the codes are unknowns or descriptions.








