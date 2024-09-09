# RestoreNet
Created: 2022-11-15  
Last updated: 2024-09-09
  
## Description  
Complete data analysis on response to intra- and inter-annual precipitation variability using the RestoreNet 1.0 data, which was the first iteration of [RAMPS RestoreNet](https://www.usgs.gov/centers/southwest-biological-science-center/science/restorenet-distributed-field-trial-network). For other analysis on the RestoreNet 1.0 data, see [Farrell et al. 2023, *Ecological Applications*](https://doi.org/10.1002/eap.2834).

  
## Author
Contact: Lia Ossanna, lossanna@arizona.edu

## Data
There are two main datasets, referred to in script names by `_subplot` and `_2x2`.
- `_subplot` is the data collected from 0.25 x 0.25 m subplots, where seedlings were identified by species, density counted, and height measured.
- `_2x2` is the data collected from the 2 x 2 m plots, where percent cover of seeded species and percent cover of total vegetation was collected, as well as the names of all species present in the plot.  

To understand more about data wrangling, see folder-specific READMEs.

## Scripts
Scripts include date of creation and last update, as well as a purpose statement, and any major takeaways.  

Scripts were numbered approximately according to creation date and workflow of analysis. 
- Top-level folder has scripts needed for final analysis.
- `exploratory/` subfolder has scripts not used for final analysis but explain some of the analysis discoveries and decisions along the way.
- `not-pursued/` subfolder has scripts whose analysis routes were discontinued and not pursued.

The complete workflow for current analysis is:  
1. Data wrangling: scripts numbered `01`, `02`, `03`, `04`.  
2. Data screening: scripts numbered `05`.
3. Exploratory graphs: scripts numbered `06`.
4. Exploratory generalized linear models: scripts numbered `07` and `08`.
5. Draft figures: scripts numbered `09`.
6. Generalized linear models: scripts numbered `10`.


# Workflow for final analysis
**1. Data wrangling**:
- `01_curate-species-list.R` and `01-dependency_assign-seeded-species-native-status.R`
- `02_correct-monitoring-info.R`
- `03.2_PRISM-data-wrangling.R`
- `03.3_explore-precip-trends.R`
- `03.4_aridity-index.R`
- `04.1_data-wrangling_subplot.R`
- `04.2_data-wrangling_2x2.R`

**2. Data screening**:
- `05.1_data-screening_subplot.R`
- `05.2_data-screening_2x2.R`
- `05.3_data-screening_continuous-explanatory-variables.R`

**3. Draft figures**:
- `09.1_draft-figs_precip-dev_subplot.R`


**4. Generalized linear models**
- `10.1_generalized-linear-models_subplot-Count.R`
- `10.2_generalized-linear-models_subplot-Height.R`


# Directory
- `data/`
    - `cleaned/`
        - `README_clean-data.md`
    - `data-wrangling-intermediate/`
        - `README_intermediate-data.md`
    - `Farrell_2023_EcologicalApplications_supp_RestoreNetsubpl/` (sub-exploratory)
    - `Global-AI_ET0_annual_v3/`
        - Data used to extract aridity index values for sites. 
        - Not pushed to GitHub.
    - `prism-dat/`
        - Climate data downloaded from PRISM for each site. Used for MAP, MAT, and quantifying precipitation variability.
    - `raw/`
        - `README_raw-data.md`
- `figures/`
    - `2024-03_draft-figures/`
        - Produced from exploratory scripts.
    - `2024-04_ALVSCE/`
        - Files not pushed to GitHub due to size.
    - `2024-04_NCER/`
        - Files not pushed to GitHub due to size.
    - `2024-08_draft-figures/`
        - Produced from exploratory scripts.
- `old_pre-2023-09-18_Farrell-data/` (do not run; used older dataset)
    - `data/`
    - `RData/`
    - `scripts/`
    - `README_old_pre-2023-09-18.md`
- `RData/`
    - `.RData` files not pushed to GitHub.
- `scripts/`
    - See "Workflow for final analysis" for top folder.
    - `exploratory/`
        - `03.1_monitor-info-comparison-with-Farrell-2023.R`
        - `06.1_exploratory-graphs_precip-dev_subplot.R` 
        - `06.2_exploratory-graphs_2x2.R` 
        - `06.3_exploratory-graphs_cum-precip_subplot.R` 
        - `06.4_exploratory-graphs_precip-dev-abs_subplot.R`
        - `06.5_exploratory-graphs_since-last-precip_subplot.R` 
        - `07.1_generalized-linear-models_subplot-Count.R` 
        - `07.2_generalized-linear-models_subplot-Count.R` 
        - `07.3_generalized-linear-models_2x2-richness.R` 
        - `08.1_generalized-linear-models-2.0_subplot-Count.R` 
        - `08.2_generalized-linear-models-2.0_subplot-Height.R`
    - `not-pursued/`
        - `04.15_data-wrangling_subplot_add-0s.R`
        - `06.15_exploratory-graphs_subplot-0-added.R`
        - `07.15_generalized-linear-models_subplot-0-added-Count.R` 
        - `08.15_generalized-linear-models-split-pos-neg_subplot-Count.R`
