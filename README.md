
# Automatic-value-learning-results-in-counterproductive-human-behavior

This repository contains R code, Stan models, and empirical data to reproduce the regression and Bayesian analyses reported in "Automatic value learning results in counterproductive human behavior." It includes pipelines for brms regression analyses, model fitting with cmdstanr, and model comparison with loo.

## 1. System requirements

- **Software dependencies:**
	- R (>= 4.0.0)
	- R packages: `cmdstanr`, `brms`, `tidyverse`, `bayestestR`, `loo`
	- CmdStan (for `cmdstanr` backend)

## 2. Installation guide

1. Install [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/).
2. Install CmdStan using the `cmdstanr` package:
	 ```r
	 install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
	 cmdstanr::install_cmdstan()
	 ```
3. Install required R packages (will be installed automatically if missing when running scripts):
	 - `brms`, `tidyverse`, `bayestestR`, `loo`
4. Clone or download this repository and open the project in RStudio.

**Typical install time:** 10-20 minutes (including CmdStan build)

## 3. Instructions for use

To run a demo analysis on the provided data:

1. Open the R scripts (`fit_stan.R`, `regression.R`, `model_comparison.R`) in RStudio.
2. Run each script section by section, or source the entire script.
3. Output files (model fits, regression results, LOO comparisons) will be saved in the `output/` folder.

**Expected output:**
	- Stan model fit objects (`.rds` files)
	- Brms regression fits
	- Model comparison results (LOO)

**Expected run time:**
	- Demo settings (as in scripts): ~5 minutes per script
	- Full analyses (increase `iter` and `warmup` as suggested): ~48 hours, depending on hardware

---
For questions or issues, please contact Ido Ben-Artzi at idobenartzi@mail.tau.ac.il.
