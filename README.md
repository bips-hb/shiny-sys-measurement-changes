
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Shiny application for “Systematic measurment changes: A simulation study”

<!-- badges: start -->

<!-- badges: end -->

This repository contains the source code for the Shiny application
accompanying the paper  
*“Assessing systematic changes in measurements across a data collection:
A simulation study”*.

## 🚀 Try the app

The interactive shiny app is available online at:
<https://sys-measurement-changes.bips.eu/>.

The app allows users to explore results from a range of simulation
settings across different sample sizes using LOESS-based visualisations
and boxplots. All resulting figures can be downloaded directly from the
application, and brief usage instructions are provided within the app
interface.

## 📖 About the app

This application enables interactively visualisation of results from a
large simulation study investigating systematic changes in measurements.
The study compares seven statistical methods for quantifying systematic
changes in measurements across different settings:  
- Autoregressive integrated moving average (ARIMA)  
- Fused lasso signal approximator (FLSA)  
- Generalized additive model (GAM)  
- Locally weighted scatterplot smoothing (LOWESS)  
- Moving average (MA)  
- Pruned exact linear time (PELT)  
- Piecewise regression (PR)

Users can explore how these methods behave under different
distributions, signal-to-noise ratios, and systematic change patterns.

## 📁 Repository structure

- `data/`: Contains the results of the simulation study.
- `renv/`: Contains project-specific environment configuration files.
- `output_shiny_application.pdf`: All figures from the Shiny
  application. This corresponds to Supplementary data 3 of the paper.
- `renv.lock`: Records package versions.
- `server.R`: Shiny server logic.
- `ui.R`: Shiny user interface definition.

## 📚 R environment (for local development)

This project uses the `renv` R package to manage package versions for
reproducibility. Developers who wish to run the app locally can restore
the exact package versions recorded in [renv.lock](renv.lock) using:

``` r
install.packages("renv")
renv::restore()
```

> Note: This step is **not required** to use the [hosted
> version](https://sys-measurement-changes.bips.eu/) of the app.
