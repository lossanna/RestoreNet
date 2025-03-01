Created: 2023-09-18  
Last updated: 2024-04-29
  
Notes about `output` and `edited` intermediate data files created in data cleaning for RAMPS RestoreNet project (original/dissertation analysis).

# Notes

## Raw data
- `29Palms_Spr22.xlsx`
	- Data that was missing from `Master.xlsx` that I emailed to get.
- `2023-09-15_Master 1.0 Germination Data_raw.xlsx`
	- Shortened name: `Master.xlsx`.
	- Contains `AllSubplotData` tab (raw `subplot` data), and `AllPlotData` (raw `2x2` plot data).
- `from-Master_seed-mix_LO.xlsx`
	- Usually referred to as "seed mix".
	- Seed mix adapted from `Master.xlsx`. Used in `01.R` to compare codes for standardization and to make sure species marked as seeded were also marked as native.

- `from-Master_species-list-with-native-status_LO.xlsx` 
	- Usually referred to as "master species list".
	- Adapted from `Master.xlsx` and was the starting list in building species lists.

## Types of data
- Species lists: include columns `Code`, `Name`, and information like `Native` (native/introduced), `Duration` (annual/perennial), and `Lifeform` (grass/forb/shrub). 
	- Location-dependent (unknowns or plants not identified to species level) codes/species are separate from location-independent (identified to species level) codes/species because known species will have the same information (native status, lifeform, duration) regardless of location, but unknowns can have the same codes across sites, but  refer to different plants.
- `subplot` data: monitoring observations from the 25 x 25 cm subplots. Measurements are seedling density and average height by species. Raw data from `AllSubplotData` tab of `2023-09-15_Master 1.0 Germination Data_raw.xlsx`.
- `2x2` data: monitoring observations from the 2 x 2 m plots. Measurements are seeded cover and total vegetation cover, and names of additional species in plot (in addition to what was present in the subplot and already recorded). Raw data from `AllPlotData` tab of `Master.xlsx`.

## File naming notes
- First number corresponds to the R script of the same number. `a` and `b` are so `output` files are listed before `edited` ones.
- `_output-` indicates the CSV was written directly from R and not manually edited.
- `_edited-` indicates the corresponding CSV written from R was then manually edited, and then read back into script.
- `-species#_` indicates the CSV is an intermediate step in curating a complete species list, from `01_curate-species-list.R`. Numbers correspond between output and edited files, and sequentially mark workflow.
- `01-dependency_` indicates the CSV is an intermediate step in correcting native status of seeded species, based on information from `subplot` data; produced from `01-dependency_assign-seeded-species-native-status.R` or `01_curate-species-list.R`.
- `_xlsx_` indicates the species list includes only codes from  the master species list, with location-dependent unknowns removed.
- `master-` indicates the spreadsheet was adapted from a tab in `Master.xlsx`.


## `Code` vs. `CodeOriginal`
- `CodeOriginal` refers to the codes originally used in `Master.xlsx`. However, in `Master.xlsx`, the column is called `Species_Code`, which I change to `CodeOriginal` when reading in the data.
- `Code` is the code that I assign, which is according to USDA Plants codes, and includes site information for location-dependent species.



# Directory

## From `01_curate-species-list.R`
### Output
#### `01a_output1_subplot-codes-missing.csv`
- List of codes included in the `subplot` data, but are missing from the the original master species list.
- Columns: `Region`, `Site`, `CodeOriginal`.

#### `01a_output2_subplot-lifeform-info.csv`
- List of codes and accompanying lifeform information based on the `subplot` data, where a code and the functional type was recorded.
- Columns: `CodeOriginal`, `Lifeform`.

#### `01a_output3_xlsx_lifeform-na.csv`
- List of species without lifeform information from the master species list.
 	- Lifeform information would have come from `AllSubplotData` tab from `Master.xlsx`.
- Columns: `CodeOriginal`, `Name`, `Lifeform` (all NAs).

#### `01a_output4_xlsx_native-lifeform.csv`
- List of unique location-independent species codes from master species list. All need duration information added.
- Columns: `CodeOriginal`, `Name`, `Native`, `Lifeform`.

#### `01a_output5.1_location-dependent.csv`
- List of codes for location-dependent species in `subplot` data. First part is from master species list, and is missing site, duration, and lifeform information. Second part is from codes in the `subplot` data missing from the master species list. Site, duration, and lifeform information is already filled out for these (had been manually input into `edited1.csv`).
- `output5.1.csv` is used as a skeleton to fill in the missing information for the species in the master list.
- Columns: `CodeOriginal`, `Name`, `Native`, `Region`, `Site` (partially filled out), `Duration` (partially filled out), `Lifeform` (partially filled out).

#### `01a_output5.2_location-dependent_xlsx_sites`
- List of codes from the master species list, with their site and region information. Used in conjunction with `output5.1.csv` to fill in missing site data.
- Columns: `CodeOriginal`, `Region`, `Site`.

#### `01a_output6_codes-missing-2x2plot.csv`
- List of codes that were in the `2x2` data, but not yet in the species lists for the `subplot` data, which have already been generated/have species info associated with them.
- Most of the codes are unknowns or descriptions.
- Columns: `Region`, `Site`, `CodeOriginal`.

#### `01a_output7_location-independent-final-check.csv`
- Final manual check of codes for location-independent species list. 
- Columns: `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`.

#### `01a_output8_location-dependent-final-check.csv`
- Final manual check of codes for location-dependent species list. Changed values are highlighted and explained in comment.
- Columns: `Region`, `Site`, `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`.

#### `01a_output9_2x2-location-independent-need-duplicate-number.csv`
- List of location-independent codes that need duplicate rows from `2x2` with corrected/finalized species info (created in `edited7.csv`). List needs `DuplicateNum` column added, which is easiest to do manually (rather than trying to write a vector in R, which I initially tried to do but it didn't work lol).
- Duplicate rows needed because a single original code mentions multiple species; hence, the same code will need multiple rows to describe all the information. All duplicate rows needed are included in this table. `subplot` data doesn't need any duplicate rows.
- Location-dependent rows that needed duplicates were handled solely in R (there were only 4).
- Columns: `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`. `NeedsItsDuplicate`.

### Edited
#### `01b_edited1_subplot-codes-missing.csv`
- List of codes included in the raw `subplot` data, but are missing from the the original master species list (`from-Master-species-list-with-native-status_LO.xlsx`).
- Manually edited to add plant name, duration, and lifeform based on USDA Plants.
- Not divided by location dependence, but ones that will be location-dependent (unknowns) have sites added to the name.
- Row length: edited list is the same length as output list.
- Columns: `Region`, `Site`, `CodeOriginal`, `Name` (added), `Native` (added), `Duration` (added), `Lifeform` (added).

#### `01b_edited2_subplot-lifeform-info-corrected.csv`
- List of codes and their lifeform information, as taken from the `subplot` data. I deleted rows with incorrect information, so there would be only one assignment per `CodeOriginal` (this includes standardized spelling of Grass/Forb/Shrub).
- Row length: edited is shorter than output, because output includes duplicates with wrong information.
- Columns: `CodeOriginal`, `Lifeform` (edited).

#### `01b_edited3_xlsx_lifeform-na.csv`
- List of species originally without lifeform information. Subset of codes is taken only codes from the master species list.
	- Lifeform information would have come from raw `subplot` data (`AllSubplotData` tab from `Master.xlsx`).
- Manually edited to assign missing lifeform (functional group) information.
- Lifeform according to USDA Plants.
- Not a complete species list.
- Row length: edited list is the same length as output list.
- Columns: `CodeOriginal`, `Name`, `Lifeform` (edited).

#### `01b_edited4_xlsx_native-lifeform-duration.csv`
- List of codes taken from master species list. Species have native and lifeform information, but need duration information added.
- Manually edited to add plant duration, based on USDA Plants.
- Manually edited to resolve conflicting/misspelled names for BOAR and SATR12 to remove duplicates.
- Row length: edited list is 1 row less than output list (2 rows removed, 1 row of 0s added).
- Columns: `CodeOriginal`, `Name`, `Native`, `Duration` (added), `Lifeform`.

#### `01b_edited5_location-dependent_native-duration-lifeform.csv`
- List of `subplot` location-dependent codes.
	- Unknowns from master species list lack site, duration, and lifeform information. `Duration` and `Lifeform` are assigned according to USDA Plants, and `Site` comes from `output5.2.csv`.
	- Unknowns from the subplot data that were not in the master species list contain all information because all information was added in `edited1.csv`.
- Manually added a row for a subplot observation that has an NA code but is an actual observation of a plant (there is just one in row 12576).
- Add rows when there are species with the same code and name at different sites within the same region. 
- Delete duplicate rows of the same species with the same codes at the same sites but with different names and retain the more specific name (this happens because there were were multiple entries in the master species list).
	- Unkcrypt and Unksalsola at Creosote
	- Unksporob at Mesquite
	- UNGRS1 at SRER is a different situation because name descriptions are very different, but can be linked to year; will deal with in data wrangling.
- Row length: edited is longer than either output, because rows needed to be added for species with the same code and name, but at different sites within the same region.
- Columns: `CodeOriginal`, `Name`, `Native`, `Region`, `Site` (edited), `Duration` (edited), `Lifeform` (edited).

#### `01b_edited6_codes-missing-2x2plot.csv`
- List of codes that were in the `2x2` plot data, but not yet in the species lists for the `subplot` data, which have already been generated.
- Most of the codes are unknowns or descriptions; long codes that are basically descriptions and mention multiple species have multiple rows for the same code, and a column marks if there is a duplicate that is needed. All of these codes came from the Sonoran SE sites. Most other sites had a standard USDA code marked in multiple columns all named `Additional_Species_In_Plot` in the original raw `2x2` data.
	+ The Sonoran SE plots were difficult to identify, so longer explanations were needed.
	+ `NeedsItsDuplicate` = `Yes`: more than one species mentioned in a single code, so the same code has multiple lines with different species.
	+ `DuplicateNum` = `1`, `2`, `3`, etc.: row number for duplicate rows when more than one species is mentioned in a single code. Marked `0` when `NeedsItsDuplicate` = `No`.
	+ `LocationDependence` = `dependent`: Site needs to be added to code to make `Code` column. Unknowns are location-dependent.
- Unknowns that are also duplicates are technically site-specific, but the `CodeOriginal` is usually long and descriptive and never used at another site, so the `CodeOriginal` is already location-independent just by being unique. Thus, what must be addressed is the fact they are duplicates.
- Some codes can be cross-referenced with the master species list to see what the name is.
- Lifeform, native status, and duration are according to USDA Plants.
- Cross referenced ambiguous codes with master species list and notes from raw `2x2` data.
- Row length: the edited version is much longer than the output, because of necessary duplicate rows.
- Columns: `Region`, `Site`, `CodeOriginal`, `NeedsItsDuplicate` (added), `DuplicateNum` (added), `Code` (added), `Name` (added), `Native` (added), `Duration` (added), `Lifeform` (added), `LocationDependence` (added).

#### `01b_edited7_location-independent-final-fix.xlsx`
- Final fixes to  location-independent species list. Changes are noted as comments in Excel sheet (standardized codes to be the same as USDA Plants ones).
- Row length: edited is the same length as output.
- Columns: `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`.

#### `01b_edited8_location-dependent-final-fix.xlsx`
- Final fixes to  location-dependent species list. Changed native status for a few unknowns after cross-referencing master species list.
- Columns: `Region`, `Site`, `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`.

#### `01b_edited9_2x2-location-independent-duplicate-number-added.csv`
- List of location-independent codes that need duplicate rows from `2x2` with corrected/finalized species info (created in `edited7.csv`). Manually added `DuplicateNum` column.
- Duplicate rows needed because a single original code mentions multiple species; hence, the same code will need multiple rows to describe all the information. All duplicate rows needed are included in this table. `subplot` data doesn't need any duplicate rows.
- Location-dependent rows that needed duplicates were handled solely in R (there were only 4).
- Row length: edited list has the same number of rows as output.
- Columns: `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`. `NeedsItsDuplicate`, `DuplicateNum` (added).


## From `01-dependency.R`
### Written out
#### `01-dependency_seeded-species-to-be-marked-native.csv`
- List of seeded species marked as seeded in the `subplot` data, and are therefore native, but were not marked as such in `master-species_native.xlsx`. 
- Produced in `01-dependency_assign-seeded-species-native-status.R` and needed for `01_curate-species-list.R`.
	- CSV is an intermediate step in `01_curate-species-list.R`, and must be created in order to run the script all the way through, but the dependency script `01-dependency.R` needs also CSVs from `01_curate.R`. Thus, a dependency, because neither can be run all the way through alone if never run before and no CSVs made yet.
	- Separate script created so there could remain a single script that produces curated species lists and can be run from start to finish without overwriting files or breaking things.

#### `01-dependency_species-list_location-dependent.csv`
- Table of location-dependent species before native status of select seeded species was fixed. Produced in `01.R` and needed for `01-dependency.R`.

#### `01-dependency_species-list_location-independent.csv`
- Table of location-independent species before native status of select seeded species was fixed. Produced in `01.R` and needed for `01-dependency.R`.



## From `02_correct-monitoring-info.R`
 - No output/edited pairs.
 - Used in `04.1_data-wrangling_subplot.R` and `04.2_data-wrangling_2x2.R`.
 - Lists of wrong events are used to match with `raw.row` of either `subplot` or `2x2` data, to know which rows to remove and replace.
 - Corrected events are linked to the `raw.row` because the lists are in identical order (the fix is the same row as the wrong event).
 - Lastly, a few SiteDatePlotID values were rendered null because they were duplicates of others that had correct monitoring info, but the wrong/old SiteDatePlotID is needed to link the wrong and fixed rows to each other. After all the monitoring info is correct (Region, Site, CodeOriginal, Code, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix), then the SiteDatePlotID can be corrected.

### Written out
`02_2x2-wrong-monitor-events.csv`  
`02_2x2-wrong-monitor-events-corrected.csv`  
`02_SiteDatePlotID-replacements.csv`
`02_subplot-wrong-monitor-events.csv`  
`02_subplot-wrong-monitor-events-corrected.csv`


## From `03.1_monitor-info-comparison.R`
- No output/edited pair.

### Written out
#### `03.1_monitoring-events-with-Farrell-climate-data.csv`
- Table of monitoring information by site and date with climate/abiotic information from Farrell (2023) *Ecological Applications* data added. 
- Used to make `03.2_monitoring-events-with-Farrell-climate-data-and-PRISM-csv-file-name.xlsx`.



## From `03.2_PRISM-data-wrangling.R`
- No output/edited pairs.

### Read in
#### `03.2_monitoring-events-with-Farrell-climate-data-and-PRISM-csv-file-name.xlsx`
- Excel file that I started from the `03.2.R` intermediate csv, and then added the columns `path_beginning`, `cum_file`, and `since_file`. Spreadsheet has two tabs, `daily` (actual precip experienced during the  experiment) and `normals` (30-year normal averages). This is used to connect the correct file path name to the Region/Site/Date_Seeded/Date_Monitored information.
- Two individual PRISM files were downloaded using the Explorer tool and the coordinates Farrell provided for every date monitoring occurred. One file covers the time interval of the previous to current monitoring date (`since_file`), and one file covers the time interval from the time of seeding to the current monitoring date (`cum_file`).
- See the textbox in the Excel file for more details.


## From `03.3_explore-precip-trends`
- No output/edited pair.

### Read in
#### `03.3_months-to-include-for-precip-normals-comparison.xlsx`
- Excel file of monitoring information, and which months to include when creating normals of comparable interval to the interval between monitoring events, or since seeding. See `README` tab for more details.
- Separate tabs for intervals between monitoring events (`since_last`), and since seeding (`cum`).
- Started out with information from `02_corrected-monitoring-info-by-date-and-site_clean.csv` (Region, Site, Date_Seeded, Date_Monitored, SiteDateID), and then I added columns to "round" the dates to the beginning, middle or end of the month to make comparable intervals, since normals were given at a monthly resolution.


## From `04.1_data-wrangling_subplot.R`
### Output
#### `04.1a_output-seeded1_seeded-not-in-mix_subplot.csv`
- List of species from `subplot` data originally marked as seeded but do not appear on the seed mix list, as matched by `CodeOriginal`.
- Columns: `Site`, `Region`, `PlotMix`, `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`, `SpeciesSeeded`.

#### `04.1a_output-seeded2_seeded-in-mix_subplot.csv`
- List of species from `subplot` data marked seeded and in at least one of the seed mixes. Need to look at each manually because seed mixes are site-specific.
- Columns: `Site`, `Region`, `PlotMix`, `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`, `SpeciesSeeded`.

#### `04.1a_output-seeded3_unk_subplot.csv`
- List of species from `subplot` data originally marked as unknown for seeding status.
- Columns: `Site`, `Region`, `PlotMix`, `CodeOriginal`, `Code`, `Name`, `SpeciesSeeded`.

#### `04.1a_output-seeded4_NA_subplot.csv`
- List of species from `subplot` data originally marked as NA for seeding status.
- Columns: `Site`, `Region`, `PlotMix`, `CodeOriginal`, `Code`, `Name`, `SpeciesSeeded`.

#### `04.1a_output-seeded5_no_subplot.csv`
- List of species from `subplot` data originally marked as not seeded.
- Columns: `Site`, `Region`, `PlotMix`, `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`, `SpeciesSeeded`.

#### `04.1a_output-seeded6_conflicting-SpeciesSeeded.csv`
- After having in theory compiled lists based off of all possible original values from `SpeciesSeeded` column, there were still some conflicts, creating duplicate rows (only `SpeciesSeeded` was conflicting). I couldn't figure out a way to extract all of the duplicate rows, not just half of them, so I filtered the entire list based on `Code`. This also brought in some rows that weren't actually conflicting duplicates, but were codes from a different mix. All of them were unknowns, and the conflict occurred because in some rows in the raw `subplot` data they were marked as seeded, but sometimes they were marked as not seeded.
- Columns: `Site`, `Region`, `PlotMix`, `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`, `SpeciesSeeded`.


### Edited
#### `04.1b_edited-seeded1_corrected-seeded-not-in-mix_subplot.xlsx`
- `SpeciesSeeded` column corrected based on seed mixes listed in `from-Master_seed-mix_LO.xlsx`. 
- Unknowns originally marked as seeded remained marked as seeded.
- Only changed status if the plant was identified to genus level. Cells that are changed are highlighted.
- Row length: edited has the same number of rows as output.
- Columns: `Site`, `Region`, `PlotMix`, `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`, `SpeciesSeeded` (edited).

#### `04.1b_edited-seeded2_corrected-seeded-in-mix_subplot.xlsx`
- `SpeciesSeeded` column corrected based on site-specific seed mixes. Usually discrepancies are because the `PlotMix` column is conflicting (species weren't included in both warm and cool mixes, so there can only be one for any of them per site, or they were marked seeded in a control plot that didn't receive any seeding).
- Also standardized `SpeciesSeeded` response to be only "Yes", which created some duplicate rows, but these are retained and dealt with in R.
- Row length: edited has the same number of rows as output.
- Columns: `Site`, `Region`, `PlotMix`, `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`, `SpeciesSeeded` (edited).

#### `04.1b_edited-seeded3_unk-corrected_subplot.xlsx`
- Entire `SpeciesSeeded` column edited to either `No`, `Yes`, or `0`.
- Unknowns marked as not seeded (`No`).
- Row length: edited has the same number of rows as output.
- Columns: `Site`, `Region`, `PlotMix`, `CodeOriginal`, `Code`, `Name`, `SpeciesSeeded` (edited).

#### `04.1b_edited-seeded4_NA-corrected_subplot.xlsx`
- Entire `SpeciesSeeded` column edited to either `No`, `Yes`, or `0`.
- `0` assigned if `Code` was `0`, which indicates there was no plant for observation. Unknowns marked as not seeded (`No`).
- `Yes` assigned to a few identified to species level if they were in that plot's seed mix.
- Row length: edited has the same number of rows as output.
- Columns: `Site`, `Region`, `PlotMix`, `CodeOriginal`, `Code`, `Name`, `SpeciesSeeded` (edited).

#### `04.1b_edited-seeded5_corrected-not-seeded_subplot.xlsx`
- `SpeciesSeeded` column corrected based on site-specific seed mixes. This spreadsheet has 1222 rows and I honestly just went through all of them (most of them did not need to be changed, though).
- Row length: edited has the same number of rows as output.
- Columns: `Site`, `Region`, `PlotMix`, `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`, `SpeciesSeeded` (edited).

#### `04.1b_edited-seeded6_conflicting-SpeciesSeeded-fixed.xlsx`
- This list contained rows with conflicting `SpeciesSeeded` information, and a few rows that were not conflicts but was just the same `Code` in a different `PlotMix`. I manually made a new column `Retain` to manually mark which conflicting duplicate rows should be dropped. Because they were all unknowns, I marked them all as not seeded, removing the conflicting duplicate row that said they were seeded.
- Row length: edited has the same number of rows as output.
- Columns: `Site`, `Region`, `PlotMix`, `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`, `SpeciesSeeded`, `Retain` (added).



## From `04.2_data-wrangling_2x2.R`
### Output
#### `04.2a_output1_SpeciesSeeded-in-mix-need-assignment.csv`
- List of `2x2` species (from site-specific plot mixes) not assigned a `SpeciesSeeded` status from the `subplot` data that existed in at least one seed mix. All species not in a seed mix were assigned not seeded, but I also manually looked over the codes and retained a few from SRER that referenced possibly seeded species.
- Columns: `Region`, `Site`, `PlotMix`, `CodeOriginal`, `Code`, `Name`, `SpeciesSeeded` (all NAs).

### Edited
#### `04.2b_edited1_SpeciesSeeded-in-mix-assigned.xlsx`
- Manually edited to assign `SpeciesSeeded` based on site-specific seed mixes (all assigned either `No` or `Yes`).
- Row length: edited has the same number of rows as output.
- Columns: `Region`, `Site`, `PlotMix`, `CodeOriginal`, `Code`, `Name`, `SpeciesSeeded` (edited).
