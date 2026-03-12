# Server Quality

This is the code and data for “A Unified Server Quality Metric for Tennis,” which constructs a serve-quality score from Grand Slam point data and evaluates it against wElo and task-aligned serving baselines.

## Directory Structure

```
.
├── code/
│   ├── 01_get-data.R
│   ├── 02_welo.R
│   ├── 03_split-year.R
│   ├── 04_fix-time.R
│   ├── 05_sqs.R
│   ├── 06_oos-eval.R
│   └── 07_temporal-validation.R
├── data/
│   ├── raw/
│   ├── processed/
│   │   ├── combined/
│   │   ├── subset/
│   │   └── splits/
│   └── results/
│       ├── importance/
│       └── {us_men,us_women,wimb_men,wimb_women}/
│           ├── evaluation/
│           └── rankings/
├── paper/
├── presentations/
└── serving.Rproj
```

## Data Processing Pipeline

The analysis follows this workflow (run from repo root):

1. **`code/01_get-data.R`** - Combines raw match and points data, removes invalid serves
2. **`code/02_welo.R`** - Adds pre-match wElo values and speed ratios for player analysis
3. **`code/03_split-year.R`** - Creates match-level 80/20 splits within each year and writes train/test files
4. **`code/04_fix-time.R`** - Optional: fixes elapsed-time gaps in the train/test files
5. **`code/05_sqs.R`** - Fits first/second-serve SQS models and saves outputs
6. **`code/06_oos-eval.R`** - Evaluates first/second-serve SQS with wElo, serve-stat, random-effects-only, and fixed-effects-only baselines
7. **`code/07_temporal-validation.R`** - Repeats model fitting/evaluation with an out-of-time split (train 2018--2022, test 2023--2024)

## Key Features

- **Pre-match wElo ratings** for player strength assessment
- **Speed ratios** for serve analysis
- **Elapsed-time gap correction** at the match level
- **Match-level train/test splits**
- **Separate first-serve and second-serve SQS models**
- **Task-aligned baselines** (ace rate, unreturned rate proxy, first-serve points won, first-serve-in%)
- **Model ablations** (random-effects-only and fixed-effects-only)

## Usage

Run scripts from the repo root:

```zsh
Rscript code/01_get-data.R
Rscript code/02_welo.R
Rscript code/03_split-year.R
Rscript code/04_fix-time.R  # optional
Rscript code/05_sqs.R
Rscript code/06_oos-eval.R
Rscript code/07_temporal-validation.R
```
