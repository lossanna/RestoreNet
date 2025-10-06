# RestoreNet
Created: 2022-11-15  
Last updated: 2025-10-06
  
## Author
Contact: Lia Ossanna, lossanna@arizona.edu


## Description  
### Updated (for publication): Sonoran only
Complete data analysis on seedling response to very wet and very dry  conditions in the Sonoran Desert using the RestoreNet 1.0 data, which was the first iteration of [RAMPS RestoreNet](https://www.usgs.gov/centers/southwest-biological-science-center/science/restorenet-distributed-field-trial-network). For other analysis on the RestoreNet 1.0 data, see [Farrell et al. 2023, *Ecological Applications*](https://doi.org/10.1002/eap.2834) and [Havrilla et al. 2020, *Journal of Applied Ecology*]( https://doi.org/10.1111/1365-2664.13715).

### Original (for dissertation): all sites
Complete data analysis on response to intra- and inter-annual precipitation variability using the RestoreNet 1.0 data, which was the first iteration of [RAMPS RestoreNet](https://www.usgs.gov/centers/southwest-biological-science-center/science/restorenet-distributed-field-trial-network). For other analysis on the RestoreNet 1.0 data, see [Farrell et al. 2023, *Ecological Applications*](https://doi.org/10.1002/eap.2834) and [Havrilla et al. 2020, *Journal of Applied Ecology*]( https://doi.org/10.1111/1365-2664.13715).

## Updated versus original analysis  
Original analysis looked at all the RestoreNet sites, and later narrowed down to just the sites in Arizona. Data are divided by desirable vs. weedy species, and cumulative precipitation deviation from average is used to measure "precipitation variability". This version was used for PhD dissertation.

Updated analysis looks at only the sites in the Sonoran Desert and the data are divided by cool vs. warm season species, and precipitation since the last monitoring event was used to quantify how wet or dry the period was, compared to historic averages.


## Data
There are two main datasets, referred to in script names by `_subplot` and `_2x2`.
- `_subplot` is the data collected from 0.25 x 0.25 m subplots, where seedlings were identified by species, density counted, and height measured.
- `_2x2` is the data collected from the 2 x 2 m plots, where percent cover of seeded species and percent cover of total vegetation was collected, as well as the names of all species present in the plot.  

To understand more about data wrangling, see folder-specific READMEs (which exist for both original/dissertation and publication analysis folders).

Updated analysis of Sonoran sites only uses the subplot data only.


## Sonoran Desert analysis
Located in folders that start with "Sonoran":
- `Sonoran-data/`
- `Sonoran-RData/`
- `Sonoran-scripts/`
- `Sonoran-UA-HPC`
    - High performance computing required for model selection and model averaging.

## Scripts
Scripts include date of creation and last update, as well as a purpose statement, and any major takeaways.  

Scripts were numbered approximately according to creation date and workflow of analysis. 
- Top-level `scripts/` folder has scripts needed for original dissertation analysis.
- `scripts/exploratory/` subfolder has scripts not used for final analysis but explain some of the analysis discoveries and decisions along the way.
- `scripts/not-pursued/` subfolder has scripts whose analysis routes were discontinued and not pursued.
- `scripts/poster-figs/` subfolder has scripts to make figures for posters.

### Original/dissertation analysis
The workflow for original/dissertation analysis is:  
1. Data wrangling: scripts numbered `01`, `02`, `03`, `04`.  
2. Data screening: scripts numbered `05`.
3. Conditions of Arizona sites: script `09.3`.
4. Examination of frequency and count of individual species: scripts numbered `11`.
5. Draft figures: scripts numbered `12`.
6. Generalized linear models: scripts numbered `13`.

Scripts numbered `14` through `16` are initial attempts at looking at Sonoran sites only (this analysis did not end up in dissertation, but I decided I wanted to pursue it for published version).

### Publication analysis
The workflow for publication (Sonoran sites only) analysis is:
1. Data wrangling: scripts numbered `01`, `02`, `03`, `04`.
2. Data screening: script numbered `05`.




## Workflow for original/dissertation analysis
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

**3. Draft figures and species of interest**:
- `09.3_characterizing-SD-and-NAZ-by-explanatory-variables.R`
- `11.1_calculate-species-frequency-by-species.R`
- `11.2_examine-species-by-Count.R`
- `12.1_draft-figs-2.0_density-and-frequency.R`
- `12.2_draft-figs-2.0_seeded-cover.R`

**4. Generalized linear models**:
- `13.1_generalized-linear-models_subplot-Count_2.0.R`
- `13.2_generalized-linear-models_2x2-Seeded-Cover_2.0.R`

**5. Initial Sonoran-only analysis:**
- `14.1_assign-seasonality-to-Sonoran-species.R`
- `14.2_assign-seasonality-to-Sonoran-monitoring-events.R`
- `14.3_explore-Sonoran-precip-trends.R`
- `14.4_data-screening_Sonoran-seasonality.R`
- `15_draft-figs_Sonoran-seasonality.R`
- `16_generalized-linear-models_Sonoran-seasonality.R`

## Directory
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
    - `2024-09_draft-figures/`
        - Produced from `09.1_draft-figs_precip-dev_subplot.R`.
    - `2024-09_draft-figures-2.0/`
        - Produced from `12.1_draft-figs-2.0_density-and-frequency.R` and `12.2_draft-figs-2.0_seeded-cover.R`.
    - `2024-12_draft-figures/`
        - Produced from `15_draft-figs_Sonoran-seasonality.R`, part of initial attempt to look at Sonoran sites only and match precipitation with plant seasonality.
- `old_pre-2023-09-18_Farrell-data/` (do not run; used older dataset)
    - `data/`
    - `RData/`
    - `scripts/`
    - `README_old_pre-2023-09-18.md`
- `RData/`
    - `.RData` files not pushed to GitHub.
- `RMarkdown/`
    - `Table-S3-through-S10.docx`
    - `Table-S3-through-S10.pdf`
    - `Table-S3-through-S10.Rmd`
- `scripts/`
    - See "Workflow for original/dissertation analysis" (previous section) for list of scripts in top-level folder; subfolders explained below.
    - `exploratory/`
        - Exploratory scripts might not be able to be re-run and reproduced exactly, because cleaned data might have changed slightly (`present_species` table in particular).
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
        - `09.1_draft-figs_precip-dev_subplot.R`
        - `09.1_identify-species-of-interest.R`
        - `09.2_draft-figs_precip-dev_2x2.R`
        - `10.1_generalized-linear-models_subplot-Count.R`
        - `10.2_generalized-linear-models_subplot-Height.R`
        - `10.3_generalized-linear-models_2x2-Seeded-Cover.R`
    - `not-pursued/`
        - `04.15_data-wrangling_subplot_add-0s.R`
        - `06.15_exploratory-graphs_subplot-0-added.R`
        - `07.15_generalized-linear-models_subplot-0-added-Count.R` 
        - `08.15_generalized-linear-models-split-pos-neg_subplot-Count.R`
