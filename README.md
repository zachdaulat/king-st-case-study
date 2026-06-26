# Evaluating the King Street Transit Pilot

## Project Summary

The King Street Transit Priority Corridor was launched as a pilot on 12 November 2017 and made permanent on 16 April 2019, restricts private through-traffic on King Street to prioritize streetcar movement. The city evaluated it with descriptive before/after comparisons of average travel times and reliability, without a formal counterfactual, uncertainty quantification, nor a time-varying treatment.

This project performs a causal inference evaluation of the pilot's *transit performance* outcomes, using disaggregated datasetes the pilot itself produced. It addresses three questions: (1) whether the pilot improved transit speed and reliability; (2) whether any improvement came at the expense of nearby streets through spillover effects for example; and (3) whether effects differed between peak and off-peak periods.

The planned approach has three major components:

- An **L2-regularized distributional synthetic control** (extending Gunsilius, 2023) to construct counterfactual *distributions* of travel times rather than just means, and to address high multicollinearity in the synthetic control donor pool.
- A **Bayesian distributional regression** with a flexible four-parameter response, modelling multiple parameters as functions of covariates jointly so that conditional speed and reliability can be estimated with posterior uncertainty.
- A **network-aware spillover component** to evaluate effects on nearby segments under spatial interference

The analysis is implemented in R, with the distributional synthetic control method implemented in the [`statz`](https://github.com/zachdaulat/statz) package.

## Dataset Access

The raw datasets used in this analysis are large and intentionally excluded from version control. To reproduce this analysis, please download the following datasets directly from the City of Toronto Open Data Portal and place them in the `data/raw/` directory. Unzip where relevant. The Disaggregate Travel Time dataset is downloaded as an `.xlsx` files, and should be converted to a `.csv` file first. The other two are provided as compressed `.gz` files, and do not need to be decompressed manually as this is handled by `readr`.

**King Street Pilot Datasets:**

- [King Street Pilot - Disaggregate Headway and Travel Time](https://open.toronto.ca/dataset/king-street-pilot-disaggregate-headway-and-travel-time/)
- [King Street Pilot - Detailed Bluetooth Travel Time](https://open.toronto.ca/dataset/king-st-transit-pilot-detailed-bluetooth-travel-time/)
- [King Street Pilot - Detailed Traffic & Pedestrian Volumes](https://open.toronto.ca/dataset/king-st-transit-pilot-detailed-traffic-pedestrian-volumes/)

**Geospatial Datasets:**

Currently, the Bluetooth segments is the only spatial dataset used so far. Download the shapefile (SHP), which will be in WGS84 CRS.

- [King Street Pilot - Bluetooth Travel Time Segments](https://open.toronto.ca/dataset/king-st-transit-pilot-bluetooth-travel-time-segments/)

The street network and intersections geospatial datasets might be used in a future stage. They can be downloaded as GeoPackage (`.gpkg`) files in MTM10 CRS.

- [Toronto Centreline (TCL)](https://open.toronto.ca/dataset/toronto-centreline-tcl/)
- [Intersection File - City of Toronto](https://open.toronto.ca/dataset/intersection-file-city-of-toronto/)

## Repository Structure

### Directories

- `/data`: Contains raw and processed data directories used in the analysis
- `/notebooks`: contains Quarto notebooks as `.qmd` files for exploratory data analysis (EDA), data profiling, and spatial matrix prototyping
- `/reports`: Contains the rendered PDF/HTML report outputs
- `/R`: Contains the R scripts used for the data preparation and analysis

### Notable files

- **Rendered Milestone 2 report:** [`/reports/Milestone_2.pdf`](reports/Milestone_2.pdf)
  — contains the data profiling, exploratory analysis, distribution screening, and
  methodological rationale.
- **Milestone 2 Source:** [`/notebooks/Milestone-2/Milestone_2.qmd`](notebooks/Milestone-2/Milestone_2.qmd)
- **Distribution screening script:** [`/R/fit_distributions.R`](R/fit_distributions.R)
  — runs the `gamlss::fitDist` family screening and writes results to
  [`/data/processed/dist_fits_bic.csv`](data/processed/dist_fits_bic.csv). Separated from the Quarto source file because it is
  computationally expensive; the cached results are loaded at render time.

This project uses R and Quarto rather than Python and Jupyter. The `.qmd` source document combines regular prose and code blocks to perform the analyses that are executed when rendering into a target file type, in this case a PDF document.

## Dependencies and Installation

Core packages used across the analysis:

- **Data handling:** `tidyr`, `dplyr`, `readr`, `lubridate`, `stringr`,`purrr`
- **Spatial:** `sf`, `spdep`
- **Distribution fitting:** `gamlss`, `e1071`
- **Visualization:** `ggplot2`, `ggcorrplot`, `patchwork`, `ggrepel`, `knitr`, `kableExtra`
- **Modelling:** `bamlss`

This project requires R (≥ 4.0) and relies on several R packages for spatial routing and Bayesian modelling.

The Distributional Synthetic Control weight generation with L2 regularization is implemented in my own R package with a compiled Rust engine called [`statz`](https://github.com/zachdaulat/statz).

To install the `statz` package from source, you will need the Rust toolchain and standard R build tools installed on your system. You can then compile and install it directly from GitHub using `pak` or `devtools`.

- **R** (≥ 4.0)
- **Rust toolchain**: Install via [rustup.rs](https://rust-lang.org/tools/install/). Verify with `rustc --version` and `cargo --version`.
- **C/C++ Build Tools**: 
  - **Windows**: Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) matching your R version.
  - **macOS**: Run `xcode-select --install` in the terminal.
  - **Linux**: Install standard R development tools (e.g., `sudo apt install r-base-dev` on Ubuntu).

```r
# Using pak (recommended)
# install.packages("pak")
pak::pkg_install("zachdaulat/statz")

# Or using devtools
# install.packages("devtools")
devtools::install_github("zachdaulat/statz")
```
