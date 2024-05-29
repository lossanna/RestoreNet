# RestoreNet
Created: 2022-11-15  
Last updated: 2024-05-29
  
## Description  
Complete data analysis on response to variable precipitation using the RestoreNet 1.0 data, which was the first iteration of [RAMPS RestoreNet](https://www.usgs.gov/centers/southwest-biological-science-center/science/restorenet-distributed-field-trial-network). For other analysis on the RestoreNet 1.0 data, see [Farrell et al. 2023, *Ecological Applications*](https://doi.org/10.1002/eap.2834).

  
## Author
Contact: Lia Ossanna, lossanna@arizona.edu

## Data
There are two main datasets, referred to in script names by `_subplot` and `_2x2`.
- `_subplot` is the data collected from 0.25 x 0.25 m subplots, where seedlings were identified by species, density counted, and height measured.
- `_2x2` is the data collected from the 2 x 2 m plots, where percent cover of seeded species and percent cover of total vegetation was collected, as well as the names of all species present in the plot.

# Workflow for current analysis
Scripts should be run in order of number in name.
1. **Data wrangling**:
- `01_curate-species-list.R` and `01-dependency_assign-seeded-species-native-status.R`
- `02_correct-monitoring-info.R`
- `03.1_monitor-info-comparison-with-Farrell-2023.R`
- `03.2_PRISM-data-wrangling.R`
- `03.3_explore-precip-trends.R`
- `03.4_aridity-index.R`
- `04.1_data-wrangling_subplot.R`
- `04.2_data-wrangling_2x2.R`

2. **Data screening**:
- `05.1_data-screening_subplot.R`
- `05.2_data-screening_2x2.R`
- `05.3_data-screening_continuous-explanatory-variables.R`

3. **Analysis**:
- `06.1_exploratory-graphs_subplot.R`
- `06.2_exploratory-graphs_2x2.R`
- `07.1_linear-models_subplot.R`

# Directory
- `data/`
- `figures/`
- `old_pre-2023-09-18_Farrell-data/`
- `RData/`
    - `.RData` files not pushed to GitHub.
- `scripts/`