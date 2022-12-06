Created: 2022-11-28  
Last updated: 2022-12-02  
  
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
- Edited list is the same length as output list.

`edited-species2_xlsx_lifeform-na.csv`
- List of species originally without lifeform information. Subset of codes is taken only codes from the `master-species_native.xlsx`
	- Lifeform information would have come from `AllSubplotData` tab from `Master.xlsx`.
- Manually edited to assign missing lifeform (functional group) information.
- Lifeform according to USDA Plants.
- Not a complete species list.
- Edited list is the same length as output list.

`edited-species3_xlsx_native-lifeform-duration.csv`
- List of codes taken from `master-species_native.xlsx`. Not yet a complete species list.
- Manually edited to add plant duration, based on USDA Plants.
- Manually edited to resolve conflicting lifeform assignments or misspelled names to remove duplicates.  Codes are not changed, and different codes for the same species are maintained because they connect to the subplot data.
- Manually add a row of all 0s to mark observations of plots that had no plants.
- Not yet a complete species list.
- Edited list is not the same length as output list (rows deleted to resolve conflicts, so edited < output).

`edited-species4_location-dependent_native-duration-lifeform.csv`
- List of location-dependent species (unknowns), listed first by the ones from `master-species_native.unk` that lack site data, and then by location-dependent codes from the subplot data (`AllSubplotData` tab from `Master.xlsx`), whose native/duration/lifeform information was already manually added in `edited-species1_subplot-codes-missing_native-duration-lifeform`.
- Manually added site information for the unknowns from the master list (first section) based on site information in the subplot data, which sometimes included adding multiple rows because the same code occured at different sites. Codes have not been changed, and will be changed in the cleaned subplot data through data wrangling (`02_data-wrangling.R`).
	+ Sometimes codes were listed in the master species list that didn't occur in the subplot data, and therefore didn't have any site information. They will be included in the 2x2m plot species lists.
- Manually added assigned codes for subplot observations that were missing codes, as noted in `02_data-wrangling.R` (there is just one in row 12166).
- Edited list is not the same length as either output list, because some codes in `4.1.csv` weren't in the subplot data, so they weren't included, and some codes had multiple locations according to `4.2.csv`.

`edited-species5_codes-missing-2x2plot.csv`
- List of codes that were in the 2x2 plot data (`AllPlotData` tab in `Master.xlsx`), but not yet in the species lists for the subplot data, which have already been generated.
- Most of the codes are unknowns or descriptions; long codes that are basically descriptions and mention multiple species have multiple rows for the same code, and a column marks if there is a duplicate that is needed. All of these codes came from the Sonoran SE sites. Most other sites had a standard USDA code marked in multiple columns all named `Additional_Species_In_Plot` in the original raw 2x2 plot data.
	+ The Sonoran SE plots were difficult to identify, so longer explanations were needed.
	+ `NeedsItsDuplicate` = `Yes`: more than one species mentioned in a single code, so the same code has multiple lines with different species.
	+ `LocationDependence` = `dependent`: Site needs to be added to code to make `Code.Site` column. Unknowns are location-dependent.
	
`Farrell_2020_EcologicalApplications_table1.xlsx`
- Adapted from Table 1 of H. Farrell's manuscript (in review), with coordinates taken from the `Site_Information` tab of `Master.xlsx`

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

`output-species4.1_location-dependent.csv`
- List of codes for location-dependent species in subplot data. Site, Duration, and Lifeform information is filled out for the second part of the list, which came from codes missing from the `master-species_native.xlsx` list, and had been manually input into `edited-species1_subplot-codes-missing_native-duration-lifeform.csv`.
- Site, Duration, and Lifeform information is not yet entered for codes that were included in the `master-species_native.xlsx` list.
- Output written directly from R and edited in a new file, in combination with `output-species4.2.csv`.

`output-species4.2_location-dependent_xlsx_sites`
- List of codes from the `master-species_native.xlsx` list, with their Site and Region information.
- Output written directly from R and edited in a new file, in combination with `output-species4.1.csv`.

`output-species5_codes-missing-2x2plot.csv`
- List of codes that were in the 2x2 plot data (`AllPlotData` tab in `Master.xlsx`), but not yet in the species lists for the subplot data, which have already been generated.
- Most of the codes are unknowns or descriptions.
- Output written directly from R and edited in a new file.
