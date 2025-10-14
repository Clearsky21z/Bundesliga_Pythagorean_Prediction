# Bundesliga Pythagorean Prediction

## Overview

This repository contains the data, scripts, and outputs for estimating **Bundesliga-specific Pythagorean coefficients** and producing **Points-in-Table (PiT)** forecasts for the 2024/25 season.
The pipeline:

1. cleans match CSVs,
2. builds season tables and a pooled team–season dataset (2010/11–2023/24),
3. fits a four-parameter Pythagorean model by minimizing MAE, validates via LOSO, and
4. forecasts 2024/25 end-of-season points after rounds 9, 18, and 27.

Final figures/tables are saved in `output/` and the paper is rendered from `paper/`.

## File Structure

```
Bundesliga_Pythagorean_Prediction/
├─ data/
│  ├─ raw_data/            # source CSVs (e.g., D1_2024_2025.csv)
│  ├─ cleaned_data/        # standardized match files (from script/01)
│  └─ analysis_data/       # per-season EoS tables + pooled stack (from script/02)
├─ output/                 # final analysis artifacts (from scripts 03–04)
│  ├─ BL_2024_25_PiT_round_09.png
│  ├─ BL_2024_25_PiT_round_18.png
│  ├─ BL_2024_25_PiT_round_27.png
│  ├─ BL_2024_25_PiT_summary.csv
│  ├─ BL_2024_25_team_diffs_round27.csv
│  ├─ bundesliga_coefs_pooled.csv
│  ├─ bundesliga_in_sample_by_season.csv
│  ├─ bundesliga_loso_cv.csv
│  └─ bundesliga_pool_with_pred.csv
├─ paper/
│  ├─ paper.qmd            # Quarto source
│  ├─ paper.pdf            # compiled paper
│  └─ reference.bib        # bibliography
└─ script/
   ├─ 01-data_cleaning.R          # clean/standardize raw CSVs
   ├─ 02-data_aggregating.R       # build EoS tables + pooled dataset
   ├─ 03-fitting_coefficients.R   # fit (a,b,c,d), LOSO, write outputs to /output
   └─ 04-pit_testing_2024_25.R    # PiT forecasts (R9/R18/R27), plots + summaries
```

**How to run (from project root):**

```r
source("script/01-data_cleaning.R")
source("script/02-data_aggregating.R")
source("script/03-fitting_coefficients.R")
source("script/04-pit_testing_2024_25.R")
# optional: render the paper
quarto::render("paper/paper.qmd")
```
