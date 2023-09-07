Created: 2022-11-28  
Last updated: 2023-09-07
  
Notes about raw data for RAMPS RestoreNet project.

Types of data:
- Species lists: include columns `Code`, `Name`, and information like `Native` (native/introduced), `Duration` (annual/perennial), and `Lifeform` (grass/forb/shrub). 
	- Location-dependent (unknowns or plants not identified to species level) codes/species are separate from location-independent (identified to species level) codes/species because known species will have the same information (native status, lifeform, duration) regardless of location, but unknowns can have the same codes across sites, but  refer to different plants.
- `subplot` data: monitoring observations from the 25 x 25 cm subplots. Measurements are seedling density and average height by species. Raw data from `AllSubplotData` tab of `Master Germination Data 2022.xlsx`.
- `2x2` data: monitoring observations from the 2 x 2 m plots. Measurements are seeded cover and total vegetation cover, and names of additional species in plot (in addition to what was present in the subplot and already recorded). Raw data from `AllPlotData` tab of `Master.xlsx`.

File naming notes:
- First number corresponds to the R script of the same number. `a` and `b` are so `output` files are listed before `edited` ones.
- `_output-` indicates the CSV was written from R.
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
- Output written directly from R and edited in a new file.

`01a_output-species2_xlsx_lifeform-na.csv`
- List of species without lifeform information. Subset of codes is taken only codes from the `master-species_native.xlsx`.
 	- Lifeform information would have come from `AllSubplotData` tab from `Master.xlsx`.
- Output written directly from R and edited in a new file.

`01a_output-species3_xlsx_native-lifeform.csv`
- List of unique species codes (although there are some duplicates when there are conflicting lifeforms or names for the same code).
- Output written directly from R and edited in a new file.

`01a_output-species4.1_location-dependent.csv`
- List of codes for location-dependent species in subplot data. Site, Duration, and Lifeform information is filled out for the second part of the list, which came from codes missing from the `master-species_native.xlsx` list, and had been manually input into `01a_edited-species1_subplot-codes-missing_native-duration-lifeform.csv`.
- Site, Duration, and Lifeform information is not yet entered for codes that were included in the `master-species_native.xlsx` list.
- Output written directly from R and edited in a new file, in combination with `01a_output-species4.2.csv`.

`01a_output-species4.2_location-dependent_xlsx_sites`
- List of codes from the `master-species_native.xlsx` list, with their Site and Region information.
- Output written directly from R and edited in a new file, in combination with `01a_output-species4.1.csv`.

`01a_output-species5_codes-missing-2x2plot.csv`
- List of codes that were in the `2x2` data, but not yet in the species lists for the subplot data, which have already been generated/have species info associated with them.
- Most of the codes are unknowns or descriptions.
- Output written directly from R and edited in a new file.

`01a_output-species6_2x2-codes_need-duplicate-rows.csv`
- List of codes from `2x2` data that need duplicate rows because a single original code mentions multiple species; hence, the same code will need multiple rows to describe all the information. All duplicate rows needed are included in this table.
- Codes from `01b_edited-species5_codes-missing-2x2plot.csv` that did not need duplicate rows were added to comprehensive location-dependent/location-independent species lists that include `subplot` and `2x2` codes:
	+ `data/cleaned/species-list_location-dependent_clean.csv`
	+ `data/cleaned/species-list_location-independent_clean.csv`
- Codes that need duplicate rows must be dealt with separate in the `2x2` data and species information cannot be added with just a `left_join()`.
- There is no "edited" version of this file.


`01b_edited-species1_subplot-codes-missing.csv`
- List of codes included in the raw `subplot` data, but are missing from the the original master species list (`master-species_native.xlsx`).
- Manually edited to add plant name, duration, and lifeform based on USDA Plants.
- Not divided by location dependence, but ones that will be location-dependent (unknowns) have sites added to the name.
- Edited list is the same length as output list.

`01b_edited-species2_xlsx_lifeform-na.csv`
- List of species originally without lifeform information. Subset of codes is taken only codes from the `master-species_native.xlsx`
	- Lifeform information would have come from raw `subplot` data (`AllSubplotData` tab from `Master.xlsx`).
- Manually edited to assign missing lifeform (functional group) information.
- Lifeform according to USDA Plants.
- Not a complete species list.
- Edited list is the same length as output list.

`01b_edited-species3_xlsx_native-lifeform-duration.csv`
- List of codes taken from `master-species_native.xlsx`. Not yet a complete species list.
- Manually edited to add plant duration, based on USDA Plants.
- Manually edited to resolve conflicting lifeform assignments or misspelled names to remove duplicates.  Codes are not changed, and different codes for the same species are maintained because they connect to the subplot data.
- Manually add a row of all 0s to mark observations of plots that had no plants.
- Edited list is not the same length as output list (rows deleted to resolve conflicts, so edited < output).

`01b_edited-species4_location-dependent_native-duration-lifeform.csv`
- List of location-dependent species (unknowns), listed first by the ones from `master-species_native.unk` that lack site data, and then by location-dependent codes from the raw `subplot` data, whose native/duration/lifeform information was already manually added in `01b_edited-species1_subplot-codes-missing_native-duration-lifeform.csv`.
- Manually added site information for the unknowns from the master list (first section) based on site information in the subplot data, which sometimes included adding multiple rows because the same code occurred at different sites. Codes have not been changed, and will be changed in the cleaned data through data wrangling (`03.1_data-wrangling_subplot.R` and `03.2_data-wrangling_2x2.R`).
	+ Sometimes codes were listed in the master species list that didn't occur in the `subplot` data, and therefore didn't have any site information. They are not included here, but are included later when looking at `2x2` codes in `species5.csv`.
- Manually added assigned codes for subplot observations that were missing codes, as noted in `03.1_data-wrangling_subplot.R` (there is just one in row 12166).
- Edited list is not the same length as either output list, because some codes in `4.1.csv` weren't in the subplot data, so they weren't included, and some codes had multiple locations according to `4.2.csv`.

`01b_edited-species5_codes-missing-2x2plot.csv`
- List of codes that were in the `2x2` plot data (`AllPlotData` tab in `Master.xlsx`), but not yet in the species lists for the subplot data, which have already been generated.
- Most of the codes are unknowns or descriptions; long codes that are basically descriptions and mention multiple species have multiple rows for the same code, and a column marks if there is a duplicate that is needed. All of these codes came from the Sonoran SE sites. Most other sites had a standard USDA code marked in multiple columns all named `Additional_Species_In_Plot` in the original raw `2x2` data.
	+ The Sonoran SE plots were difficult to identify, so longer explanations were needed.
	+ `NeedsItsDuplicate` = `Yes`: more than one species mentioned in a single code, so the same code has multiple lines with different species.
	+ `DuplicateNum` = `1`, `2`, `3`, etc.: row number for duplicate rows when more than one species is mentioned in a single code. Marked `0` when `NeedsItsDuplicate` = `No`.
	+ `LocationDependence` = `dependent`: Site needs to be added to code to make `Code` column. Unknowns are location-dependent.
	

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

## From `02.R`
`02a_output-monitor_subplot-2x2-conflicting-monitoring-info.csv`
- Table of original monitoring information from `subplot` data, with no corrections made yet.

`02b_edited-monitor_conflicting-monitoring-info-resolved.xlsx`
- Spreadsheet of monitoring events with conflicting information between the `subplot` and `2x2` data. Monitoring information refers to the columns `Site`, `Date_Seeded`, `Date_Monitored`, `Plot`, `Treatment`, and `PlotMix`. Each monitoring event (for each plot) is given an ID number (`MonitorID`), unrelated to the data actually collected from the subplot or 2x2 m plot.
- Converted to an Excel file to manually edit so I could highlight changes and add comments.
- `comparison` tab manually edited to highlight correct values in green, and wrong values in yellow. Comments inserted to give brief explanation of how this decision was made. Justifications also written in comments in `02_correct-monitoring-info.R` script.
- `corrected` tab manually created to have table of only correct information to read back in. 	

## From `03.1_subplot.R`
`03.1a_output-species-seeded1_seeded-not-in-mix_subplot.csv`
- List of species from `subplot` data originally marked as seeded but do not appear on seed mix list, as matched by `CodeOriginal`.

`03.1a_output-species-seeded2_unk_subplot.csv`
- List of species from `subplot` data originally marked as either unknown or NA for seeding status.

`03.1a_output-species-seeded3_no_subplot.csv`
- List of species from `subplot` data originally marked as not seeded.

`03.1b_output-species-seeded1_corrected-seeded-not-in-mix_subplot.xlsx`
- `SpeciesSeeded` column corrected based on seed mixes listed in `from-Master_seed-mix_LO.xlsx`. 
- Unknowns originally marked as seeded remained marked as seeded.
- Only changed status if the plant was identified to genus level. Cells that are changed are highlighted.

`03.1b_edited-species-seeded2_unk-corrected_subplot.xlsx`
- Entire `SpeciesSeeded` column edited to either `No`, `Yes`, or `0`.
- `0` assigned if `Code` was `0`, which indicates there was no plant for observation.

## From `03.2_2x2.R`
`03.2a_output-wrangling-2x2_1monitor-info-to-be-fixed.csv`
- Table of monitoring events from `2x2` data with information that conflicts with `subplot` data and must be corrected manually.
- Output written directly from R and edited in a new file.

`03.2b_edited-wrangling-2x2_1monitor-info-fixed.xlsx`
- Shortened version of the `corrected` tab of `02b_edited-monitor_conflicting-monitoring-info-resolved.xlsx`; includes only events that need to be fixed for 2x2 data.

## Need to be dealt with
`output-wrangling-2x2_2need-duplicate-rows.csv`
- A subset of the `2x2` data for codes that need duplicate rows because the code refers to more than one species.
- Output written directly from R and edited in a new file.

`edited-wrangling-2x2_2duplicate-rows-added.csv`
- A subset of the `2x2` data for original codes that need duplicate rows because the code refers to more than one species.
- Manually added extra rows based on what was needed, and added a new Code that connects to species lists, based on definitions and species information listed in `output-species6_2x2-codes_need-duplicate-rows.csv`.

## From Farrell papers	
`Farrell_2020_EcologicalApplications_table1.xlsx`
- Adapted from Table 1 of H. Farrell's manuscript (in review), with coordinates taken from the `Site_Information` tab of `Master.xlsx`

## Master data
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