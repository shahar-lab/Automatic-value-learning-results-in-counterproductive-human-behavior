# Experiments 1 & 2 Combined

Analyses that pool data from Experiments 1 and 2 to examine relationships between λ, WM capacity, payoff, and the OIL signature across a larger sample.

## Data

**Data/df_combined_exp1_2.csv** - Trial-level dataset combining Exp1 and Exp2, including model parameters and WM scores, with an `experiment` column identifying the source.

## Code

**Regression/**
- `accuracy_lambda_Figure1H.R` - Accuracy as a function of individual-level λ (Figure 1H)
- `payoff_lambda_FigureS1.R` - Mean payoff as a function of λ, by experiment (Figure S1)
- `model_agnostic_lambda_Figure2D.R` - OIL signature (ΔP stay) as a function of λ (Figure 2D)
- `model_agnostic_lambda_high_beta_FigureS8.R` - OIL signature as a function of λ, restricted to high-β participants (Figure S8)
- `location_switching_parameter_Figure2H.R` - Location switching as a function of model parameters (Figure 2H)
- `wm_lambda_Figure6C.R` - WM capacity as a function of λ (Figure 6C)
- `wm_model_agnostic_Figure6D.R` - Model-agnostic OIL signature as a function of WM (Figure 6D)
- `wm_lambda_covariates_Figure7D.R` - WM–λ relationship with covariates (Figure 7D)
- `model_agnostic_lambda_beta_Figure7B_C.R` - OIL signature as a function of λ and β (Figures 7B–C)
- `empirical_correlations_FigureS10.R` - Correlations among estimated parameters (Figure S10)

## Output

Populated when scripts are run. Model fits saved as `.rds` figures saved as `.pdf`.
