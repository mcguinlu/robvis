
<!-- README.md is generated from README.Rmd. Please edit that file -->

# robvis <img src="man/figures/robvis_hex_box.png" align="right" width="18%" height="18%" />

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN
Badge.](https://www.r-pkg.org/badges/version-ago/robvis)](https://cran.r-project.org/web/packages/robvis/index.html)
[![CRAN
Downloads.](https://cranlogs.r-pkg.org/badges/last-month/robvis)](https://cran.r-project.org/web/packages/robvis/index.html)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3552342.svg)](https://doi.org/10.5281/zenodo.3552342)
<br> [![Build
Status](https://img.shields.io/travis/mcguinlu/robvis.svg?label=build&logo=travis&branch=master)](https://travis-ci.com/mcguinlu/robvis)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/mcguinlu/robvis?branch=master&svg=true)](https://ci.appveyor.com/project/mcguinlu/robvis)
[![Codecov test
coverage](https://codecov.io/gh/mcguinlu/robvis/branch/master/graph/badge.svg)](https://codecov.io/gh/mcguinlu/robvis?branch=master)
<br> [![metaverse
Identifier](https://img.shields.io/static/v1.svg?label=Part%20of%20the&message=metaverse&color=informational)](https://www.github.com/rmetaverse/metaverse)

**UPDATE**: `robvis` now exists as a
[web-app](https://mcguinlu.shinyapps.io/robvis), aimed at those who are
not familiar with R or who want to explore the package’s functionality
before installing it locally.

## Description

The `robvis` package takes the summary table from risk-of-bias
assessments and produces plots formatted according to the assessment
tool used.

## Getting started

Install the CRAN version:

``` r
install.packages("robvis")
```

Alternatively, the development version can be installed using:

``` r
install.packages("devtools")
devtools::install_github("mcguinlu/robvis")
```

To update the package, run the `install_github("mcguinlu/robvis")`
command again.

### Load data

To load your own data from a .csv file:

``` r
mydata <- read.csv("path/to/mydata.csv", header = TRUE)
```

To help users explore `robvis`, we have included example datasets in the
package, one for each of the tool templates that currently exist within
the package. The `data_rob2` dataset ([view it
here](https://github.com/mcguinlu/robvis/blob/master/data_raw/data_rob2.csv)),
which contains example risk-of-bias assessments performed using the
RoB2.0 tool for randomized controlled trials, is used to create the
plots in subsequent sections.

### Create plots

The package contains two plotting functions:

#### 1\. rob\_summary()

Returns a ggplot object displaying a weighted barchart of the risk of
bias of included studies across the domains of the specified tool.

``` r
summary_rob <- rob_summary(data = data_rob2, tool = "ROB2")
summary_rob
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="90%" />

#### 2\. rob\_traffic\_light()

Returns a ggplot object displaying a [“traffic light
plot”](https://handbook-5-1.cochrane.org/chapter_8/figure_8_6_c_example_of_a_risk_of_bias_summary_figure.htm),
displaying the risk of bias judgment in each domain for each study.

``` r
trafficlight_rob <- rob_traffic_light(data = data_rob2, tool = "ROB2")
trafficlight_rob
```

<div style="text-align:center">

<img src="man/figures/rob2trafficlight.png" width="70%" height="70%"/>

</div>

### Other functions

#### rob\_tools()

Outputs a list of the risk of bias assessment tools for which a template
currently exists in rob\_summary(). We expect this list to be updated in
the near future to include tools such as ROBIS (tool for assessing risk
of bias in systematic reviews).

    rob_tools()
    [1] "ROB2"
    [1] "ROBINS-I"
    [1] "QUADAS-2"
    [1] "ROB1"

## Advanced usage

### Change the colour scheme

The `colour` argument of both plotting functions allows users to select
from two predefined colour schemes (“cochrane” or “colourblind”) or to
define their own palette by providing a vector of hex codes.

For example, to use the predefined “colourblind” palette:

``` r
summary_rob <- rob_summary(data = data_rob2, tool = "ROB2", colour = "colourblind")
summary_rob
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="90%" />

And to define your own colour scheme:

``` r
summary_rob <- rob_summary(data = data_rob2, tool = "ROB2", colour = c("#f442c8","#bef441","#000000"))
summary_rob
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="90%" />

### Created an unweighted summary barplot

By default, the `rob_summary()` function creates a barplot weighted by
some measure of a study’s precision. This can be prevented using the
“weighted” argument. For example, compare the following two plots:

``` r
summary_rob <- rob_summary(data = data_rob2, tool = "ROB2")
summary_rob
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="90%" />

``` r
summary_rob <- rob_summary(data = data_rob2, tool = "ROB2", weighted = FALSE)
summary_rob
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="90%" />

### Editing the plots

Finally, because the output (`summary_rob` and `trafficlight_rob` in the
examples above) is a ggplot2 object, it is easy to adjust the plot to
your own preferences.

For example, to add a title to the unweighted RoB2.0 plot created above:

``` r
library(ggplot2)

summary_rob +
  ggtitle("Summary of RoB2.0 assessments")
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="90%" />

## Examples of `robvis` in published papers

  - Tanneru, Karthik, et al. “[Meta-analysis and systematic review of
    intermediate-term follow-up of prostatic urethral lift for benign
    prostatic
    hyperplasia.](https://link.springer.com/article/10.1007/s11255-020-02408-y)”
    International Urology and Nephrology (2020): 1-10.
  - Veloso, Andreia, et al.. “[Effectiveness of Cognitive Training for
    School-Aged Children and Adolescents With Attention
    Deficit/Hyperactivity Disorder: A Systematic
    Review.](https://www.frontiersin.org/articles/10.3389/fpsyg.2019.02983/full)”
    Frontiers in Psychology 10 (2020): 2983.
  - Simillis, Constantinos, et al. “[Postoperative chemotherapy improves
    survival in patients with resected high‐risk stage II colorectal
    cancer: results of a systematic review and
    meta‐analysis.](https://onlinelibrary.wiley.com/doi/pdf/10.1111/codi.14994)”
    Colorectal Disease (2020).
  - Habadi, Mohammed Ibrahim, et al. “[Prevalence of Panic Disorders in
    the Primary Health Care Setting: A Systematic Review and
    Meta-Analysis.](https://www.ecronicon.com/ecmi/pdf/prevalence-of-panic-disorders-in-the-primary-health-care-setting-a-systematic-review-and-meta-analysis.pdf)”
    EC Microbiology 16.1 (2019): 01-09.
  - Gibb, Kate, et al. “[The consistent burden in published estimates of
    delirium occurrence in medical inpatients over four decades: a
    systematic review and meta-analysis
    study.](https://www.medrxiv.org/content/medrxiv/early/2019/08/29/19005165.full.pdf)”
    medRxiv (2019): 19005165.

## Code of conduct

Please note that the ‘robvis’ project is released with a [Contributor
Code of
Conduct](https://github.com/mcguinlu/robvis/blob/master/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## License

This project is licensed under the MIT License - see the
[LICENSE.md](https://github.com/mcguinlu/robvis/blob/master/LICENSE)
file for details.

## Acknowledgments

  - The `rob_summary()` function was based on code forwarded by a
    colleague. I recently discovered that this code was adapted from
    that presented in the wonderful “[Doing Meta-Analysis in
    R](https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/plotting-the-summary.html)”
    guide, so I would like to acknowledge the authors here.
  - [Emily Kothe](https://github.com/ekothe) for help in fixing
    `ggplot2` coding issues.
  - [Eliza Grames](https://github.com/elizagrames) for creating the
    `robvis` hex sticker.
