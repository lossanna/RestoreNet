# RestoreNet
Created: 2022-11-15  
Last updated: 2024-09-04
  
## Description  
Complete data analysis on response to variable precipitation using the RestoreNet 1.0 data, which was the first iteration of [RAMPS RestoreNet](https://www.usgs.gov/centers/southwest-biological-science-center/science/restorenet-distributed-field-trial-network). For other analysis on the RestoreNet 1.0 data, see [Farrell et al. 2023, *Ecological Applications*](https://doi.org/10.1002/eap.2834).

  
## Author
Contact: Lia Ossanna, lossanna@arizona.edu

## Data
There are two main datasets, referred to in script names by `_subplot` and `_2x2`.
- `_subplot` is the data collected from 0.25 x 0.25 m subplots, where seedlings were identified by species, density counted, and height measured.
- `_2x2` is the data collected from the 2 x 2 m plots, where percent cover of seeded species and percent cover of total vegetation was collected, as well as the names of all species present in the plot.  

To understand more about data wrangling, see folder-specific READMEs.

## Scripts
Scripts include date of creation and last update, as well as a purpose statement, and any major takeaways.

# Workflow for current analysis
Scripts should be run in order of number in name.  
Exploratory scripts not necessary for producing final results, but explain some of the analysis discoveries and decisions along the way.
1. **Data wrangling**:
- `01_curate-species-list.R` and `01-dependency_assign-seeded-species-native-status.R`
- `02_correct-monitoring-info.R`
- `03.1_monitor-info-comparison-with-Farrell-2023.R` (sub-exploratory)
- `03.2_PRISM-data-wrangling.R`
- `03.3_explore-precip-trends.R`
- `03.4_aridity-index.R`
- `04.1_data-wrangling_subplot.R`
- `04.2_data-wrangling_2x2.R`
- `04.15_data-wrangling_subplot_add-0s.R` (deprecated)

2. **Data screening**:
- `05.1_data-screening_subplot.R`
- `05.2_data-screening_2x2.R`
- `05.3_data-screening_continuous-explanatory-variables.R`

3. **Exploratory graphs**:
- `06.1_exploratory-graphs_precip-dev_subplot.R`
- `06.2_exploratory-graphs_2x2.R`
- `06.3_exploratory-graphs_cum-precip_subplot.R` (exploratory)
- `06.4_exploratory-graphs_precip-dev-abs_subplot.R` (exploratory)
- `06.5_exploratory-graphs_since-last-precip_subplot.R` (exploratory)
- `06.15_exploratory-graphs_subplot-0-added.R` (deprecated)

4. **Generalized linear models**
- `07.1_generalized-linear-models_subplot-Count.R` (exploratory)
- `07.2_generalized-linear-models_subplot-Count.R` (exploratory)
- `07.3_generalized-linear-models_2x2-richness.R`
- `07.15_generalized-linear-models_subplot-0-added-Count.R` (deprecated)
- `08.1_generalized-linear-models-2.0_subplot-Count.R`
- `08.2_generalized-linear-models-2.0_subplot-Height.R`
- `08.15_generalized-linear-models-split-pos-neg_subplot-Count.R` (deprecated)

# Directory
- `data/`
    - `cleaned/`
        - `README_clean-data.md`
    - `data-wrangling-intermediate/`
        - `README_intermediate-data.md`
    - `Farrell_2023_EcologicalApplications_supp_RestoreNetsubpl/` (sub-exploratory)
    - `Global-AI_ET0_annual_v3/`
    - `prism-dat/`
    - `raw/`
        - `README_raw-data.md`
- `figures/`
    - `2024-03_draft-figures/`
    - `2024-04_ALVSCE/`
        - Files not pushed to GitHub due to size.
    - `2024-04_NCER/`
        - Files not pushed to GitHub due to size.
    - `2024-08_draft-figures/`
- `old_pre-2023-09-18_Farrell-data/` (deprecated)
    - `data/`
    - `RData/`
    - `scripts/`
    - `README_old_pre-2023-09-18.md`
- `RData/`
    - `.RData` files not pushed to GitHub.
- `scripts/`