# Serve Profile Analysis

This repository contains analysis of tennis serving profiles from Grand Slams to explore serving archetypes and predict point outcomes.

## Directory Structure

```
.
├── archived/
│   ├── code/
│   │   ├── new/
│   │   └── old/
│   ├── data/
│   │   ├── output/
│   │   │   ├── model-testing-results/
│   │   │   ├── new-model-results/
│   │   │   ├── out-data/
│   │   │   └── scaled-results/
│   │   ├── rankings/
│   │   └── sackmann/
│   └── figures/
│       ├── new/
│       │   ├── all-serves/
│       │   │   ├── us-open/
│       │   │   └── wimbledon/
│       │   ├── correlations/
│       │   ├── second-serves-in/
│       │   │   ├── us-open/
│       │   │   └── wimbledon/
│       │   └── variable-importance/
│       │       ├── general/
│       │       ├── us-open/
│       │       └── wimbledon/
│       ├── old/
│       │   ├── first-serves/
│       │   ├── linear-examples/
│       │   ├── second-serves/
│       │   └── winning-serves/
│       └── player-effects/
│           ├── 2-sds-away/
│           │   ├── usopen/
│           │   └── wimbledon/
│           ├── by_year/
│           │   ├── usopen/
│           │   └── wimbledon/
│           └── top-bottom-10/
│               ├── usopen/
│               └── wimbledon/
├── code/
├──  data/
│   ├── raw/
│   ├── processed/
│   │   ├── scaled/
│   └── results/
└── README.md
```

## Data Processing Pipeline

The analysis follows this workflow:

1. **`01_get_data.R`** - Combines raw match and points data, removes invalid serves
2. **`02_welo.R`** - Adds Welo ratings and speed ratios for player analysis
3. **`03_combine_years.R`** - Creates training/testing splits for model development
4. **`04_elapsedtime_standardize.R`** - Fixes time gaps and creates standardized versions
5. **`05_server_pattern_exploration.R`** - Performs server clustering analysis

## Key Features

- **Welo ratings** for player strength assessment
- **Speed ratios** for serve analysis
- **Time standardization** to fix data gaps
- **Z-score standardization** for key variables
- **Server pattern clustering** analysis
- **Training/testing splits** for model validation

## Usage

Run scripts from the `code/` directory:

```bash
cd code
Rscript 01_get_data.R
Rscript 02_welo.R
Rscript 03_combine_years.R
Rscript 04_elapsedtime_standardize.R
Rscript 05_server_pattern_exploration.R
``` 