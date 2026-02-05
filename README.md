# Server Quality

This is the code and data for “A Unified Server Quality Metric for Tennis,” which constructs a serve-quality score from Grand Slam point data and evaluates it against wElo baselines.

## Directory Structure

```
.
├── code/
│   ├── 01_get-data.R
│   ├── 02_welo.R
│   ├── 03_split-year.R
│   ├── 04_fix-time.R
│   ├── 05_sqs.R
│   └── 06_oos-eval.R
├── data/
│   ├── raw/
│   ├── processed/
│   │   ├── combined/
│   │   ├── subset/
│   │   └── splits/
│   └── results/
│       └── importance/
├── paper/
│   └── main.pdf
├── presentations/
│   ├── cmsac2025.pdf
│   └── lab.pdf
└── serving.Rproj
```

## Data Processing Pipeline

The analysis follows this workflow (run from repo root):

1. **`code/01_get-data.R`** - Combines raw match and points data, removes invalid serves
2. **`code/02_welo.R`** - Adds pre-match wElo values and speed ratios for player analysis
3. **`code/03_split-year.R`** - Creates match-level 80/20 splits within each year and writes train/test files
4. **`code/04_fix-time.R`** - Optional: fixes elapsed-time gaps in the train/test files
5. **`code/05_sqs.R`** - Fits first/second-serve SQS models and saves outputs
6. **`code/06_oos-eval.R`** - Evaluates first/second-serve SQS in testing data

## Key Features

- **Pre-match wElo ratings** for player strength assessment
- **Speed ratios** for serve analysis
- **Elapsed-time gap correction** at the match level
- **Match-level train/test splits**
- **Separate first-serve and second-serve SQS models**

## Usage

Run scripts from the repo root:

```zsh
Rscript code/01_get-data.R
Rscript code/02_welo.R
Rscript code/03_split-year.R
Rscript code/04_fix-time.R  # optional
Rscript code/05_sqs.R
Rscript code/06_oos-eval.R
```