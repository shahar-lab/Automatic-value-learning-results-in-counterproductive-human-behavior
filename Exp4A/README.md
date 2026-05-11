# Experiment 4A

Participants completed the original story version of the task but **without choice feedback** — they did not see which option they had selected after making their choice. This tests whether visual feedback about the chosen option is necessary for the OIL effect. No WM measurement was included.

## Data

**Raw/** — Trial-level RL data extracted from task output files.

**Filtered/** — Data after applying preregistered exclusion criteria.

**Analysis/df/** — `df.csv`: trial-level dataset with estimated model parameters.

**Analysis/standata/** — JSON file formatted for Stan model fitting.

## Code

**Preprocessing/**
- `create_RL_raw_no_ch_feedback.R` — Extract and format raw data
- `create_RL_filtered_no_ch_feedback.R` — Apply exclusion criteria

**Computational_model/**
- `fit_stan.R` — Fits the full model via cmdstanr
- `parameter_estimation_Figure5C.R` — Extracts and plots λ posterior (Figure 5C)

**Regression/**
- `model_agnostic_Figure5D_E.R` — Model-agnostic OIL signature without choice feedback (Figures 5D–E)

## Output

Populated when scripts are run. Model fits saved as `.rds`; figures saved as `.pdf`.
