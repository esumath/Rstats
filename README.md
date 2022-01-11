
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rstats

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The web app is deployed on <https://esumath.shinyapps.io/rstats/>

## About

The goal of Rstats is to to promote students’ interest in [**R
programming**](https://cran.r-project.org/) while learning
**introductory statistics**. Installation and configuration are not
needed as a web-based application. Students interested in coding can
copy the **R code** in the app and paste to the R workspace installed on
their PC or Mac to **reproduce** the data analysis results. However, the
functionality is point-and-click so those uninterested in coding will
not be intimidated by the features. The app was intended for students in
all PASSHE ([**PA State System of Higher
Education**](https://www.passhe.edu)) schools to use. It is expected to
be adopted by more undergraduate universities. Please contact me
**<xzhang2@esu.edu>** if you have any suggestions or ideas to improve
this web app.

## Installation

Currently, Rstats is not available on CRAN, but can be installed
directly from github using the **devtools** package.

    if (!require("devtools")) install.packages("devtools")
    devtools::install_github("esumath/Rstats")

## Usage

Use the following code to run the app.

    pkgload::load_all()
    Rstats::run_app()

## Code of Conduct

Please note that the Rstats project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
