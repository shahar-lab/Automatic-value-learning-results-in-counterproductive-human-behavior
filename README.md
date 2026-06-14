# Automatic value learning results in counterproductive human behavior

This repository contains the code and data needed to reproduce the analyses and plots reported in the paper. It includes Stan models for computational model fitting, brms regression analyses, and model comparison with LOO-CV. Outputs are saved under each experiment's `Output/` folder. All figures can be reproduced as PDFs by running the scripts.

Preprint: https://osf.io/preprints/psyarxiv/73d5t_v3

## Repository structure

Each experiment has its own folder (`Exp1`, `Exp2`, `Exp1_2_combined`, `Exp3`, `Exp4A`, `Exp4B`) with the following layout:

```
ExpN/
├── Data/
│   ├── Raw/           # Initial data extracted from task output files
│   ├── Filtered/      # Data after excluding trials and subjects per preregistered criteria
│   └── Analysis/      # Integrated dataset with WM capacity (where applicable) and estimated model parameters
│       ├── standata/  # Data formatted as JSON for Stan model fitting
│       └── df/        # Trial-level data as CSV, ready for regression analyses
├── Code/
│   ├── Preprocessing/        # Scripts to produce Raw and Filtered data from task output
│   ├── Computational_model/  # Stan model files, fitting scripts, model comparison, and parameter estimation
│   └── Regression/           # Brms regression scripts producing all regression figures
└── Output/                   # Placeholder directories, for when scripts are run
    ├── computational_model/
    │   └── plots/
    └── regression/
        └── plots/
```

## Requirements

- R (v4.3.1)
- CmdStan (via `cmdstanr`)

**R packages:**

| Package | Version |
|---|---|
| brms | 2.22 |
| cmdstanr | 0.8.1 |
| loo | 2.8 |
| tidyverse | 2.0 |
| bayestestR | 0.15 |
| ggdist | 3.3 |
| tidybayes | 3.0 |
| emmeans | 1.10 |
| patchwork | 1.3 |
| corrplot | 0.92 |
| bayesplot | 1.11 |

## Reproducibility note

All scripts are self-contained and set to a reduced iteration count by default (`warmup = 2`, `iter = 4`) for demonstration purposes. Comments in each script indicate the values used for the reported analyses (`warmup = 2000`, `iter = 4000`). Full Stan model fits may take up to ~48 hours depending on hardware.

## Contact

For questions, contact me (Ido Ben-Artzi) at idobenartzi@mail.tau.ac.il.
