# Equilibrium_conundrum_code

This repository walks through an example of how equilibria could be identified from timeseries data, specifically in a case where system dynamics oscillate regularly, a type of equilibrium that is only apparent when a system is observed over longer timescales. This code is referenced in the manuscript 'The Equilibrium Conundrum' in the section "What can theoreticians do?" and also in the supplement, "An approach to identify the scale of equilibrium from time-series data".

# Repository contents

-   **extracting equilibrium across scales.R** - generates datasets using 'deSolve' and analyzes them to allow for visual identification of equilibria scales. 

-   **.gitignore** - Default version created when generating the R project.

-   **EquilibriumScales.Rproj -** R project file for organizing the code in Rstudio.

-   **README.md -** Summary of GitHub repository.

# R packages and their versions used (R version 4.3.1)

-   deSolve 1.40
-   tidyverse 2.0.0
-   cowplot 1.1.3
