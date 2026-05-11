# Experiment 2

Participants completed the bandit task across **three sessions** using the story instruction version, allowing us to test the stability of λ over time. WM was measured once. No model comparison was performed the full model was fit to each session separately. Intraclass correlation of λ across sessions was computed to assess reliability.

## Data

**Raw/** - Trial-level RL data and WM data extracted from task output files, one file per participant per session.

**Filtered/** - RL and WM data after applying preregistered exclusion criteria.

**Analysis/df/** - `df.csv`: trial-level dataset integrated with WM scores and estimated model parameters per session.

**Analysis/standata/** - JSON files formatted for Stan, one per session.

## Code

**Preprocessing/**
- `create_RL_raw.R` / `create_WM_raw.R` - Extract and format raw data
- `create_RL_filtered.R` / `create_WM_filtered.R` - Apply exclusion criteria

**Computational_model/**
- `fit_stan.R` - Fits the full model to each session via cmdstanr
- `lambda_posterior_session_Figure3B.R` - Plots λ posteriors per session (Figure 3B)
- `intraclass_correlation_Figure6A.R` - Computes and plots ICC of λ across sessions (Figure 6A)
- `all_params_TableS1.R` - Extracts all parameter estimates for Table S1

**Regression/**
- `model_agnostic_session_Figure3C.R` - Model-agnostic OIL signature per session (Figure 3C)

## Output

Populated when scripts are run. Model fits saved as `.rds` figures saved as `.pdf`.
