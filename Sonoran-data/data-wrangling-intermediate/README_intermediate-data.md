Created: 2025-01-21  
Last updated: 2025-04-01
  
Notes about `output` and `edited` intermediate data files created in data cleaning for RAMPS RestoreNet project (updated analysis for publication with Sonoran sites only).

# Notes

## Raw data
- `2023-09-15_Master 1.0 Germination Data_raw.xlsx`
	- Shortened name: `Master.xlsx`.
	- Contains `AllSubplotData` tab (raw `subplot` data).
- `from-Master_seed-mix_LO_Sonoran.xlsx`
	- Usually referred to as "seed mix".
	- Seed mix adapted from `Master.xlsx`. Used in `01.R` to compare codes for standardization and to make sure species marked as seeded were also marked as native.
- `from-Master_species-list-with-native-status_LO.xlsx` 
	- Usually referred to as "master species list".
	- Adapted from `Master.xlsx` and was the starting list in building species lists.

## Types of data
- Species lists: include columns `Code`, `Name`, and information like `Native` (native/introduced), `Duration` (annual/perennial), and `Lifeform` (grass/forb/shrub). 
	- Location-dependent (unknowns or plants not identified to species level) codes/species are separate from location-independent (identified to species level) codes/species because known species will have the same information (native status, lifeform, duration) regardless of location, but unknowns can have the same codes across sites, but  refer to different plants.
- `subplot` data: monitoring observations from the 25 x 25 cm subplots. Measurements are seedling density and average height by species. Raw data from `AllSubplotData` tab of `2023-09-15_Master 1.0 Germination Data_raw.xlsx`.

## File naming notes
- First number corresponds to the R script of the same number. `a` and `b` are so `output` files are listed before `edited` ones.
- `_output#-` indicates the CSV was written directly from R and not manually edited. Numbers correspond between output and edited files, and sequentially mark workflow.
- `_edited#-` indicates the corresponding CSV written from R was then manually edited, and then read back into script. Numbers correspond between output and edited files, and sequentially mark workflow.
- `_xlsx_` indicates the species list includes only codes from  the master species list, with location-dependent unknowns removed.


## `Code` vs. `CodeOriginal`
- `CodeOriginal` refers to the codes originally used in `Master.xlsx`. However, in `Master.xlsx`, the column is called `Species_Code`, which I change to `CodeOriginal` when reading in the data.
- `Code` is the code that I assign, which is according to USDA Plants codes, and includes site information for location-dependent species.



# Directory

## From `01_curate-species-list.R`
### Output
#### `01a_output1_subplot-codes-missing.csv`
- List of codes included in the `subplot` data, but are missing from the the original master species list (`Master.xlsx`).
- Columns: `Region`, `Site`, `CodeOriginal`.

#### `01a_output2_subplot-lifeform-info.csv`
- List of codes for location-independent knowns and accompanying lifeform information based on the `subplot` data, where a code and the functional type was recorded.
- Columns: `CodeOriginal`, `Lifeform`.

#### `01a_output3_xlsx_lifeform-na.csv`
- List of species (location-independent knowns) without lifeform information from the master species list.
 	- Lifeform information would have come from `AllSubplotData` tab from `Master.xlsx`.
- Columns: `CodeOriginal`, `Name`, `Lifeform` (all NAs).

#### `01a_output4_xlsx_native-lifeform.csv`
- List of unique location-independent species codes from master species list. All need duration information added.
- Columns: `CodeOriginal`, `Name`, `Native`, `Lifeform`.

#### `01a_output5.1_location-dependent.csv`
- List of codes for location-dependent species (unknowns) in `subplot` data. First part is from master species list, and is missing site, duration, and lifeform information. Second part is from codes in the `subplot` data missing from the master species list. Site, duration, and lifeform information is already filled out for these (had been manually input into `edited1.csv`).
- `output5.1.csv` is used as a skeleton to fill in the missing information for the species in the master list.
- Columns: `CodeOriginal`, `Name`, `Native`, `Region`, `Site` (partially filled out), `Duration` (partially filled out), `Lifeform` (partially filled out).

#### `01a_output5.2_location-dependent_xlsx_sites`
- List of codes from the master species list, with their site and region information. Used in conjunction with `output5.1.csv` to fill in missing site data.
- Columns: `CodeOriginal`, `Region`, `Site`.

#### `01a_output6_location-independent-final-check.csv`
- List of location-independent species (knowns), intended for final manual check with USDA Plants info.
- Columns: `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`.

#### `01a_output7_location-dependent-final-check.csv`
- List of location-dependent species (unknowns), intended for final manual check of names.
- Columns: `CodeOriginal`, `Code`, `Name`, `Native`, `Duration`, `Lifeform`.


### Edited
#### `01b_edited1_subplot-codes-missing.csv`
- List of codes included in the raw `subplot` data, but are missing from the the original master species list (`from-Master-species-list-with-native-status_LO.xlsx`).
- Manually edited to add plant name, duration, and lifeform based on USDA Plants.
- Not divided by location dependence, but ones that will be location-dependent (unknowns) have sites added to the name.
- Row length: edited list is the same length as output list.
- Columns: `Region`, `Site`, `CodeOriginal`, `Name` (added), `Native` (added), `Duration` (added), `Lifeform` (added).

#### `01b_edited2_subplot-lifeform-info-corrected.csv`
- List of codes for location-independent knowns and their lifeform information, as taken from the `subplot` data. I deleted rows with incorrect information, so there would be only one assignment per `CodeOriginal` (this includes standardized spelling of Grass/Forb/Shrub).
- Row length: edited is shorter than output, because output includes duplicates with wrong information.
- Columns: `CodeOriginal`, `Lifeform` (edited).

#### `01b_edited3_xlsx_lifeform-na.csv`
- List of species (location-independent knowns) originally without lifeform information. Subset of codes is taken only codes from the master species list tht are location-independent knowns.
	- Lifeform information would have come from raw `subplot` data (`AllSubplotData` tab from `Master.xlsx`).
- Manually edited to assign missing lifeform (functional group) information.
- Lifeform according to USDA Plants.
- Not a complete species list.
- Row length: edited list is the same length as output list.
- Columns: `CodeOriginal`, `Name`, `Lifeform` (edited).

#### `01b_edited4_xlsx_native-lifeform-duration.csv`
- List of location-independent codes taken from master species list. Species have native and lifeform information, but need duration information added.
- Manually edited to add plant duration, based on USDA Plants.
- Manually edited to resolve conflicting/misspelled names for BOAR and SATR12, and removed duplicates.
- Manually edited to add one row of all 0s, for completely empty plots.
- Row length: edited list is 1 row less than output list (2 rows removed, 1 row of 0s added).
- Columns: `CodeOriginal`, `Name`, `Native`, `Duration` (added), `Lifeform`.

#### `01b_edited5_location-dependent_native-duration-lifeform.csv`
- List of `subplot` location-dependent codes.
	- Unknowns from master species list lack site, duration, and lifeform information. `Duration` and `Lifeform` are assigned according to USDA Plants, and `Site` comes from `output5.2.csv`.
	- Unknowns from the `subplot` data that were not in the master species list contain all information because all information was added in `edited1.csv`.
- Add rows when there are species with the same code and name at different sites within the same region. 
- Row length: edited is longer than either output, because rows needed to be added for species with the same code and name, but at different sites within the same region.
- Columns: `CodeOriginal`, `Name`, `Native`, `Region`, `Site` (edited), `Duration` (edited), `Lifeform` (edited).

#### `01b_edited6_location-independent-final-fix.xlsx`
- Final complete list of location-independent species, standardized with USDA Plants data. 
- Manually edited a few instances of `Code` and `Name`, where `CodeOriginal` was an old version that had since been updated. Changed cells were highlighted in yellow with comment added to note fix.
Row length: edited list is the same length as output list.
- Columns: `CodeOriginal`, `Code` (edited), `Name` (edited), `Native`, `Duration`, `Lifeform`.

#### `01b_edited7_location-dependent-final-fix.xlsx`
- Final complete list of location-dependent species.
- Manually edited one instance of `Native` status based on additional details in the species name.
- Manually edited two `Name` values to change the site specified in the name, because it did not match the `Site` column.
- Columns: `CodeOriginal`, `Code`, `Name` (edited), `Native` (edited), `Duration`, `Lifeform`.



## From `02_correct-monitoring-info.R`
 - No output/edited pairs.
 - Used in `04_data-wrangling_subplot.R`. 
 - Lists of wrong events are used to match with `raw.row` of either `subplot` or `2x2` data, to know which rows to remove and replace.
 - Corrected events are linked to the `raw.row` because the lists are in identical order (the fix is the same row as the wrong event).
 - Lastly, a few SiteDatePlotID values were rendered null because they were duplicates of others that had correct monitoring info, but the wrong/old SiteDatePlotID is needed to link the wrong and fixed rows to each other. After all the monitoring info is correct (Region, Site, CodeOriginal, Code, Date_Seeded, Date_Monitored, Plot, Treatment, PlotMix), then the SiteDatePlotID can be corrected.

### Written out
`02_2x2-wrong-monitor-events.csv`  
`02_2x2-wrong-monitor-events-corrected.csv`  
`02_SiteDatePlotID-replacements.csv`
`02_subplot-wrong-monitor-events.csv`  
`02_subplot-wrong-monitor-events-corrected.csv`



## From `03.2_PRISM-data-wrangling.R`
- No output/edited pairs.

### Read in
#### `03.1_monitoring-events-with-Farrell-climate-data-and-PRISM-csv-file-name.xlsx`
- Excel file with Farrell climate data, with added the columns `path_beginning`, `cum_file`, and `since_file`. Spreadsheet has two tabs, `daily` (actual precip experienced during the  experiment) and `normals` (30-year normal averages). This is used to connect the correct file path name to the Region/Site/Date_Seeded/Date_Monitored information.
- Two individual PRISM files were downloaded using the Explorer tool and the coordinates Farrell provided for every date monitoring occurred. One file covers the time interval of the previous to current monitoring date (`since_file`), and one file covers the time interval from the time of seeding to the current monitoring date (`cum_file`).
- See the textbox in the Excel file for more details.


## From `03.2_precip-percent-deviation-from-normals.R`
- No output/edited pair.

### Read in
#### `03.2_months-to-include-for-precip-normals-comparison.xlsx`
- Excel file of monitoring information, and which months to include when creating normals of comparable interval to the interval between monitoring events, or since seeding. See `README` tab for more details.
- Separate tabs for intervals between monitoring events (`since_last`), and since seeding (`cum`).
- Started out with information from `02_corrected-monitoring-info.csv` (Region, Site, Date_Seeded, Date_Monitored, SiteDateID), and then I added columns to "round" the dates to the beginning, middle or end of the month to make comparable intervals, since normals were given at a monthly resolution.


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
