# Experiment 4B

Similar to Experiment 3 (visual irrelevant feature), but **without choice feedback**. This allows us to disentangle whether the OIL effect requires feedback about which option was chosen, in the visual variant of the task. No WM measurement was included.

## Data

**Raw/** - Trial-level RL data extracted from task output files.

**Filtered/** - Data after applying preregistered exclusion criteria.

**Analysis/df/** - `df.csv`: trial-level dataset with estimated model parameters.

**Analysis/standata/** - JSON file formatted for Stan model fitting.

## Code

**Preprocessing/**
- `create_RL_raw_visual_pavlovian.R` - Extract and format raw data
- `create_RL_filtered_visual_pavlovian.R` - Apply exclusion criteria

**Computational_model/**
- `fit_stan.R` - Fits the full model via cmdstanr
- `parameter_estimation_Figure5D.R` - Extracts and plots λ posterior (Figure 5D)

**Regression/**
- `model_agnostic_Figure5F_G.R` - Model-agnostic OIL signature for visual no-feedback condition (Figures 5F–G)

## Output

Populated when scripts are run. Model fits saved as `.rds` figures saved as `.pdf`.
