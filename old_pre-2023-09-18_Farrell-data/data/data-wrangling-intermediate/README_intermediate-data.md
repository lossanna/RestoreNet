Created: 2023-09-18  
Last updated: 2023-09-18
  
Notes about `output` and `edited` intermediate data files created in data cleaning for RAMPS RestoreNet project.

Types of data:
- Species lists: include columns `Code`, `Name`, and information like `Native` (native/introduced), `Duration` (annual/perennial), and `Lifeform` (grass/forb/shrub). 
	- Location-dependent (unknowns or plants not identified to species level) codes/species are separate from location-independent (identified to species level) codes/species because known species will have the same information (native status, lifeform, duration) regardless of location, but unknowns can have the same codes across sites, but  refer to different plants.
- `subplot` data: monitoring observations from the 25 x 25 cm subplots. Measurements are seedling density and average height by species. Raw data from `AllSubplotData` tab of `Master Germination Data 2022.xlsx`.
- `2x2` data: monitoring observations from the 2 x 2 m plots. Measurements are seeded cover and total vegetation cover, and names of additional species in plot (in addition to what was present in the subplot and already recorded). Raw data from `AllPlotData` tab of `Master.xlsx`.

File naming notes:
- First number corresponds to the R script of the same number. `a` and `b` are so `output` files are listed before `edited` ones.
- `_output-` indicates the CSV was written directly from R and not manually edited.
- `_edited-` indicates the corresponding CSV written from R was then manually edited, and then read back into script.
- `-species#_` indicates the CSV is an intermediate step in curating a complete species list, from `01_curate-species-list.R`. Numbers correspond between output and edited files, and sequentially mark workflow.
- `01-dependency_` indicates the CSV is an intermediate step in correcting native status of seeded species, based on information from `subplot` data; produced from `01-dependency_assign-seeded-species-native-status.R` or `01_curate-species-list.R`.
- `_xlsx_` indicates the species list includes only codes from  `master-species_native.xlsx`, with unknowns removed.
- `master-` indicates the spreadsheet was adapted from a tab in `Master Germination Data 2022.xlsx`.
- `_native-lifeform-duration` indicates if the respective columns are present in the species list.
- `-monitor_` indicates CSV is an intermediate step in curating complete correct monitoring information, from `02_correct-monitoring-info.R`. No number because only one output/edited needed from script.
- `-wrangling-2x2-` indicates the CSV is an intermediate step in data wrangling for `2x2` m plot data, from `02.2_data-wrangling_2x2`.



# Directory

## From `01.R`

`01a_output-species1_subplot-codes-missing.csv`
- List of codes included in the `subplot` data, but are missing from the the original master species list (`master-species_native.xlsx`).

`01a_output-species2_subplot-lifeform-info.csv`
- List of codes and accompanying lifeform information based on the `subplot` data, where a code and the functional type was recorded.

`01a_output-species3_xlsx_lifeform-na.csv`
- List of species without lifeform information. Subset of codes is taken only codes from the `from-Master-species-list-with-native-status_LO.xlsx`.
 	- Lifeform information would have come from `AllSubplotData` tab from `Master.xlsx`.

`01a_output-species4_xlsx_native-lifeform.csv`
- List of unique species codes (although there are some duplicates when there are conflicting names for the same code).

`01a_output-species5.1_location-dependent.csv`
- List of codes for location-dependent species in subplot data. Site, Duration, and Lifeform information is filled out for the second part of the list, which came from codes missing from the `master-species_native.xlsx` list, and had been manually input into `01a_edited-species1_subplot-codes-missing_native-duration-lifeform.csv`.
- Site, Duration, and Lifeform information is not yet entered for codes that were included in the `master-species_native.xlsx` list.
- Output written directly from R and edited in a new file, in combination with `01a_output-species4.2.csv`.

`01a_output-species5.2_location-dependent_xlsx_sites`
- List of codes from the `master-species_native.xlsx` list, with their Site and Region information.
- Output written directly from R and edited in a new file, in combination with `01a_output-species4.1.csv`.

`01a_output-species6_codes-missing-2x2plot.csv`
- List of codes that were in the `2x2` data, but not yet in the species lists for the subplot data, which have already been generated/have species info associated with them.
- Most of the codes are unknowns or descriptions.

`01a_output-species7_location-independent-final-check.csv`
- Final manual check of codes for location-independent species list. Changed values are highlighted and explained in comment.

`01a_output-species8_location-dependent-final-check.csv`
- Final manual check of codes for location-dependent species list. Changed values are highlighted and explained in comment.

`01a_output-species9_p2x2-location-independent-need-duplicate-number.csv`
- List of location-independent codes that need duplicate rows from `2x2` with corrected/finalized species info (created in `edited-species7.csv`). List needs `DuplicateNum` column added, which is easiest to do manually (rather than trying to write a vector in R, which I initially tried to do but it didn't work lol).
- Duplicate rows needed because a single original code mentions multiple species; hence, the same code will need multiple rows to describe all the information. All duplicate rows needed are included in this table. `subplot` data doesn't need any duplicate rows.
- Location-dependent rows that needed duplicates were handled solely in R (there were only 5).


`01b_edited-species1_subplot-codes-missing.csv`
- List of codes included in the raw `subplot` data, but are missing from the the original master species list (`master-species_native.xlsx`).
- Manually edited to add plant name, duration, and lifeform based on USDA Plants.
- Not divided by location dependence, but ones that will be location-dependent (unknowns) have sites added to the name.
- Edited list is the same length as output list.

`01b_edited-species2_subplot-lifeform-info-corrected.csv`
- List of codes and their lifeform information, as taken from the `subplot` data. I deleted rows with incorrect information, so there would be only one assignment per `CodeOriginal`.

`01b_edited-species3_xlsx_lifeform-na.csv`
- List of species originally without lifeform information. Subset of codes is taken only codes from the `master-species_native.xlsx`
	- Lifeform information would have come from raw `subplot` data (`AllSubplotData` tab from `Master.xlsx`).
- Manually edited to assign missing lifeform (functional group) information.
- Lifeform according to USDA Plants.
- Not a complete species list.
- Edited list is the same length as output list.

`01b_edited-species4_xlsx_native-lifeform-duration.csv`
- List of codes taken from `master-species_native.xlsx`. Not yet a complete species list.
- Manually edited to add plant duration, based on USDA Plants.
- Manually edited to resolve conflicting/misspelled names for BOAR and SATR12 to remove duplicates.
- Manually add a row of all 0s to mark observations of plots that had no plants.
- Edited list is not the same length as output list (rows deleted to resolve conflicts, so edited < output).

`01b_edited-species5_location-dependent_native-duration-lifeform.csv`
- List of location-dependent species (unknowns), listed first by the ones from master species list that lack site data, and then by location-dependent codes from the raw `subplot` data, whose native/duration/lifeform information was already manually added in `01b_edited-species1_subplot-codes-missing_native-duration-lifeform.csv`.
- Manually added site information for the unknowns from the master list (first section) based on site information in the subplot data, which sometimes included adding multiple rows because the same code occurred at different sites. Codes have not been changed, and will be changed in the cleaned data through data wrangling (`03.1_data-wrangling_subplot.R` and `03.2_data-wrangling_2x2.R`).
	+ Sometimes codes were listed in the master species list that didn't occur in the `subplot` data, and therefore didn't have any site information. They are not included here, but are included later when looking at `2x2` codes in `species6.csv`.
- Manually added assigned codes for subplot observations that were missing codes, as noted in `03.1_data-wrangling_subplot.R` (there is just one in row 12166).
- Edited list is not the same length as either output list, because some codes in `5.1.csv` weren't in the subplot data, so they weren't included, and some codes had multiple locations according to `5.2.csv`.

`01b_edited-species6_codes-missing-2x2plot.csv`
- List of codes that were in the `2x2` plot data (`AllPlotData` tab in `Master.xlsx`), but not yet in the species lists for the subplot data, which have already been generated.
- Most of the codes are unknowns or descriptions; long codes that are basically descriptions and mention multiple species have multiple rows for the same code, and a column marks if there is a duplicate that is needed. All of these codes came from the Sonoran SE sites. Most other sites had a standard USDA code marked in multiple columns all named `Additional_Species_In_Plot` in the original raw `2x2` data.
	+ The Sonoran SE plots were difficult to identify, so longer explanations were needed.
	+ `NeedsItsDuplicate` = `Yes`: more than one species mentioned in a single code, so the same code has multiple lines with different species.
	+ `DuplicateNum` = `1`, `2`, `3`, etc.: row number for duplicate rows when more than one species is mentioned in a single code. Marked `0` when `NeedsItsDuplicate` = `No`.
	+ `LocationDependence` = `dependent`: Site needs to be added to code to make `Code` column. Unknowns are location-dependent.
- Unknowns that are also duplicates are technically site-specific, but the `CodeOriginal` is usually long and descriptive and never used at another site, so the `CodeOriginal` is already location-independent just by being unique. Thus, what must be addressed is the fact they are duplicates.

`01b_edited-species7_location-independent-final-fix.xlsx`
- Final fixes to  location-independent species list. All that was changed was the `Code` of a couple of species (ELEL5 and SPAM2) because they had wrong numbers for some reason.

`01b_edited-species8_location-dependent-final-fix.xlsx`
- Final fixes to  location-dependent species list. Removed duplicates so there would only be one row per `Code`, unless the `CodeOriginal` was different. Changes are listed in a textbook in the Excel file.

`01b_edited-species9_p2x2-location-independent-duplicate-number-added.csv`
- List of location-independent codes that need duplicate rows from `2x2` with corrected/finalized species info (created in `edited-species7.csv`). Manually added `DuplicateNum` column.
- Duplicate rows needed because a single original code mentions multiple species; hence, the same code will need multiple rows to describe all the information. All duplicate rows needed are included in this table. `subplot` data doesn't need any duplicate rows.
- Location-dependent rows that needed duplicates were handled solely in R (there were only 5).


## From `01-dependency.R`
`01-dependency_seeded-species-to-be-marked-native.csv`
- List of seeded species marked as seeded in the `subplot` data, and are therefore native, but were not marked as such in `master-species_native.xlsx`. 
- Produced in `01-dependency_assign-seeded-species-native-status.R` and needed for `01_curate-species-list.R`.
	- CSV is an intermediate step in `01_curate-species-list.R`, and must be created in order to run the script all the way through, but the dependency script `01-dependency.R` needs also CSVs from `01_curate.R`. Thus, a dependency, because neither can be run all the way through alone if never run before and no CSVs made yet.
	- Separate script created so there could remain a single script that produces curated species lists and can be run from start to finish without overwriting files or breaking things.

`01-dependency_species-list_location-dependent.csv`
- Table of location-dependent species before native status of select seeded species was fixed. Produced in `01.R` and needed for `01-dependency.R`.

`01-dependency_species-list_location-independent.csv`
- Table of location-independent species before native status of select seeded species was fixed. Produced in `01.R` and needed for `01-dependency.R`.





## From `03.1_subplot.R`
`03.1a_output-species-seeded1_seeded-not-in-mix_subplot.csv`
- List of species from `subplot` data originally marked as seeded but do not appear on seed mix list, as matched by `CodeOriginal`.

`03.1a_output-species-seeded2_seeded-in-mix_subplot.csv`
- List of species from `subplot` data marked seeded and in at least one of the seed mixes. Need to look at each manually because seed mixes are site-specific.

`03.1a_output-species-seeded3_unk_subplot.csv`
- List of species from `subplot` data originally marked as either unknown or NA for seeding status.

`03.1a_output-species-seeded4_no_subplot.csv`
- List of species from `subplot` data originally marked as not seeded.

`03.1a_output-species-seeded5_conflicting-SpeciesSeeded.csv`
- After having in theory compiled lists based off of all possible original values from `SpeciesSeeded` column, there were still some conflicts, creating duplicate rows (only `SpeciesSeeded` was conflicting). I couldn't figure out a way to extract all of the duplicate rows, not just half of them, so I filtered the entire list based on `Code`. This also brought in some rows that weren't actually conflicting duplicates, but were codes from a different mix. All of them were unknowns, and the conflict occurred because in some rows in the raw `subplot` data they were marked as seeded, but sometimes they were marked as not seeded.



`03.1b_edited-species-seeded1_corrected-seeded-not-in-mix_subplot.xlsx`
- `SpeciesSeeded` column corrected based on seed mixes listed in `from-Master_seed-mix_LO.xlsx`. 
- Unknowns originally marked as seeded remained marked as seeded.
- Only changed status if the plant was identified to genus level. Cells that are changed are highlighted.

`03.1b_edited-species-seeded2_corrected-seeded-in-mix_subplot.xlsx`
- `SpeciesSeeded` column corrected based on site-specific seed mixes. Usually discrepancies are because the `PlotMix` column is conflicting (species weren't included in both warm and cool mixes, so there can only be one for any of them per site).

`03.1b_edited-species-seeded3_unk-corrected_subplot.xlsx`
- Entire `SpeciesSeeded` column edited to either `No`, `Yes`, or `0`.
- `0` assigned if `Code` was `0`, which indicates there was no plant for observation. Unknowns marked as not seeded.

`03.1b_edited-species-seeded4_corrected-not-seeded_subplot.xlsx`
- `SpeciesSeeded` column corrected based on site-specific seed mixes. This spreadsheet has 1077 rows and I honestly just went through all of them (most of them did not need to be changed, though).

`03.1b_edited-species-seeded5_conflicting-SpeciesSeeded-fixed.xlsx`
- This list contained rows with conflicting `SpeciesSeeded` information, and a few rows that were not conflicts but was just the same `Code` in a different `PlotMix`. I manually made a new column `Retain` to manually mark which conflicting duplicate rows should be dropped. Because they were all unknowns, I marked them all as not seeded, removing the conflicting duplicate row that said they were seeded.



## From `03.2_2x2.R`
`03.2a_output-species-seeded1_in-mix-need-assignment.csv`
- List of `2x2` species (from site-specific plot mixes) not assigned a `SpeciesSeeded` status from the `subplot` data that existed in at least one seed mix. All species not in a seed mix were assigned not seeded, but I also manually looked over the codes and retained a few from SRER that referenced possibly seeded species.

`03.2b_edited-species-seeded1_SpeciesSeeded-in-mix-assigned.xlsx`
- Manually edited to assign `SpeciesSeeded` based on site-specific seed mixes.
