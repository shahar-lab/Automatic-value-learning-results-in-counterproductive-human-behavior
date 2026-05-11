# Reanalysis - Feher Da Silva & Hare (2023)

Reanalysis of data from Feher Da Silva & Hare (2023), who used the **two-step task** with a story vs. abstract instruction manipulation similar to ours. We apply a model-agnostic regression to test whether their data show the same OIL signature reported in our experiments.

## Data

Data from the original publication, formatted for our regression pipeline.

`beh_noslow.csv` - download from the original repository: https://github.com/carolfs/fmri_magic_carpet/blob/main/code/analysis/beh_noslow.csv

## Code

- `regression_FigureS3.R` - Model-agnostic regression of stay behavior on previous outcome in story instruction condition (Figure S3)

## Output

Populated when scripts are run. Figures saved as `.pdf`.
