# SeasonDx: A Shiny Application for Seasonality Diagnostics in Time Series

> **Supplementary material for:**
> *SeasonDx: a web tool for detecting and characterising seasonality in health time series*
>
> **Authors:** Jaime Pinilla, Christian González-Martel, Francisco Estupiñán-Romero
>
> **Status:** manuscript in preparation/under review
> **Software DOI:** assigned by Zenodo on release (see the badge/citation once published)

---

## Description

SeasonDx is a web application for the analysis of seasonality in health-related time series. It provides an interactive interface to detect, quantify and characterise seasonal structure from multiple complementary perspectives.

A live instance of the application is available at:
**https://chrglez.shinyapps.io/SeasonDx/**

---

## Analytical workflow

The application organises the analysis into three blocks:

1. **Decomposition & seasonal indices** — The series is decomposed into trend-cycle, seasonal and irregular components under either an additive or multiplicative specification, yielding seasonal indices together with autocorrelation-based diagnostics and the Friedman rank-sum test.

2. **Autoregressive seasonality** — An autoregressive approach based on automatic ARIMA model selection, combining Welch and Kruskal–Wallis seasonality tests with an autoregressive R² measure of seasonal strength and frequency-domain diagnostics (Bartlett's Kolmogorov–Smirnov and Fisher's Kappa tests).

3. **Two-sample difference tests** — The estimated seasonal profile is compared with a user-specified theoretical distribution through the two-sample Kolmogorov–Smirnov and Kuiper tests.

---

## Running locally

### Requirements

R (≥ 4.1) with the following packages:

```r
install.packages(c(
  "shiny", "bslib", "dygraphs", "DT", "plotly", "ggplot2",
  "forecast", "seastests", "readxl", "shinycssloaders",
  "shinyWidgets", "shinyjs", "nlme", "tseries", "KSgeneral",
  "writexl", "rmarkdown", "tinytex", "mirai", "promises"
))
```

### Launch

```r
shiny::runApp()
```

---

## Input data format

- File formats accepted: `.xlsx`, `.xls`, `.csv`
- **A header row is required** (first row must contain column names)
- First column: time index — supported formats:
  - `2014Q1`, `2014Q3` (quarter format)
  - `2014:1`, `2014:3` (colon separated)
  - `2014-1`, `2014-3` (dash separated)
  - `2014-01-01` (date format)
- Second column: numeric values
- Only **monthly** (frequency = 12) and **quarterly** (frequency = 4) series are supported
- Minimum 24 observations for monthly data; minimum 8 for quarterly data

An example dataset is included in `data/example_data.xlsx`.

---

## Repository structure

```
app.R                        # Main application entry point
R/
  config.R                   # Theme and global configuration
  modules/
    mod_upload.R             # Data upload module
    mod_visualization.R      # Visualisation module
  utils/
    data_processing.R        # Data processing utilities
    stat_functions.R         # Statistical functions
    plot_helpers.R           # Plot helper functions
data/
  example_data.xlsx          # Example monthly time series
www/
  styles.css                 # Custom styles
  js/custom.js               # Custom JavaScript
  img/                       # Logo assets
```

---

## License

MIT — see [LICENSE](LICENSE).
