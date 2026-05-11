# Simulation

Scripts for simulating artificial agents performing the bandit task, used to validate the computational model and demonstrate key behavioral signatures.

## Data

**Data/Parameter_recovery/** — Simulated data from 200 agents with parameters drawn from the estimated population distribution. Used to verify that the model recovers individual-level parameters.

**Data/Lambda_1_recovery/** — Simulated data from agents with λ fixed to 1 (pure card-following). Used to verify that the model correctly identifies λ = 1.

**Data/Random_responding/** — Simulated data from agents that randomly ignore β on a subset of trials (inattentive agents).
- `Signature/` — Data used to characterize the behavioral signature of random responding (Figure 7A)
- `Recovery/` — Data used to test whether the model recovers parameters when some agents are inattentive (Figure S7)

## Code

**Simulation_scripts/** — Functions for simulating agents under the full model, reduced model, and random-responding variant.

**Stan_models/** — `full_model.stan` and `reduced_model.stan`, shared with the empirical experiments.

**Figure scripts:**
- `accuracy_lambda_simulation_Figure1G.R` — Accuracy as a function of λ in simulated agents (Figure 1G)
- `model_agnostic_lambda_simulated_Figure2C.R` — OIL signature as a function of λ in simulated agents (Figure 2C)
- `two_three_back_simulation_FigureS2.R` — 2- and 3-back reward effects on stay behavior in simulation (Figure S2)
- `lambda_one_simulation_FigureS6.R` — Fits the full model to λ = 1 simulated data and plots the recovered λ posterior (Figure S6)
- `recovery_inattentive_FigureS7.R` — Fits the full model to random-responding simulated data and plots the recovered λ posterior (Figure S7)
- `parameter_recovery_Figure8.R` — Full parameter recovery: population-level posteriors and individual true vs. recovered scatterplots (Figure 8)
- `random_responding_signature_Figure7A.R` — OIL signature in simulated inattentive agents (Figure 7A)

## Output

Populated when scripts are run. Model fits saved as `.rds`; figures saved as `.pdf`.
