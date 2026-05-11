# Experiment 3

The irrelevant feature was changed from **spatial location** to **visual color**, so participants selected between stimuli differing in shape rather than position. This tests whether the OIL effect generalizes when the outcome-irrelevant dimension is visual rather than spatial. No WM measurement was included.

## Data

**Raw/** - Trial-level RL data extracted from task output files.

**Filtered/** - Data after applying preregistered exclusion criteria.

**Analysis/df/** - `df.csv`: trial-level dataset with estimated model parameters.

**Analysis/standata/** - JSON file formatted for Stan model fitting.

## Code

**Preprocessing/**
- `create_RL_raw_visual.R` - Extract and format raw data
- `create_RL_filtered_visual.R` - Apply exclusion criteria

**Computational_model/**
- `fit_stan.R` - Fits the full model via cmdstanr
- `parameter_estimation_Figure4B.R` - Extracts and plots λ posterior (Figure 4B)

**Regression/**
- `model_agnostic_visual_Figure4C_D.R` - Model-agnostic OIL signature for the visual condition (Figures 4C–D)

## Output

Populated when scripts are run. Model fits saved as `.rds` figures saved as `.pdf`.
