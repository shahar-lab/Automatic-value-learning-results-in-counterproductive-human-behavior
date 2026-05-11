# Experiment 1

Participants completed a four-armed bandit task in either a **story** or **abstract** instruction condition, allowing us to test whether framing affects the degree to which participants rely on card vs. location values (λ). This experiment also included a working memory (WM) measurement, enabling us to test whether WM capacity predicts λ. A model comparison between the full model (with λ) and a reduced model (without λ) was conducted for each condition.

## Data

**Raw/** - Initial trial-level RL data and WM data extracted from task output files, one file per participant.

**Filtered/** - RL and WM data after applying preregistered exclusion criteria (trial omissions, inattention, key repetitions chance-level WM performance).

**Analysis/df/** - `df.csv`: trial-level dataset integrated with WM scores and estimated model parameters (α, β, λ, ρ, η). Used for all regression analyses.

**Analysis/standata/** - JSON files (`data_for_stan_story.json`, `data_for_stan_abstract.json`) formatted for Stan model fitting.

## Code

**Preprocessing/**
- `create_RL_raw.R` / `create_WM_raw.R` - Extract and format raw data from task output
- `create_RL_filtered.R` / `create_WM_filtered.R` - Apply exclusion criteria to produce filtered data

**Computational_model/**
- `fit_stan.R` - Fits the full and reduced model to story and abstract samples via cmdstanr
- `model_comparison.R` - LOO-CV model comparison between full and reduced models
- `parameter_estimation_Figure1D.R` - Extracts and plots population- and individual-level λ posteriors (Figure 1D)
- `pairs_plot_FigureS9.R` - Posterior pairs plot for model parameters (Figure S9)

**Regression/**
- `Qdiff_Figure1E_F.R` - Regression of choices on card and location Q-value differences (Figures 1E–F)
- `easy_trials_FigureS4.R` - Stay behavior on easy trials filtered by |ΔEV| and |ΔQ_color| (Figure S4)
- `Q_diff_distributrions_FigureS5.R` - Distributions of |ΔQ_card| and |ΔQ_key| (Figure S5)
- `model_agnostic_by_sample_Figure2B.R` - Model-agnostic OIL signature by instruction condition (Figure 2B)
- `one_two_three_back_Figure2E.R` - Effect of 1-, 2-, and 3-back outcomes on stay behavior (Figure 2E)
- `cumulative_reward_Figure2F.R` - Cumulative reward analysis (Figure 2F)
- `location_switching_Figure2G.R` - Location switching analysis (Figure 2G)

## Output

Populated when scripts are run. Model fits saved as `.rds` figures saved as `.pdf`.
