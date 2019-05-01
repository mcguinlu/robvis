
<!-- README.md is generated from README.Rmd. Please edit that file -->

# robvis <img src="man/figures/robvis_hex_box.png" align="right" width="18%" height="18%" />

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Build
Status](https://travis-ci.org/mcguinlu/robvis.svg?branch=master)](https://travis-ci.org/mcguinlu/robvis)

Part of the [metaverse](https://github.com/rmetaverse/metaverse)

## Description

**UPDATE**: `robvis` now exists as a
[web-app](https://mcguinlu.shinyapps.io/robvis), aimed at those who are
not confident with R/those who want to explore the package’s
functionality.

The `robvis` package takes the summary table from risk-of-bias
assessments, converts it to tidy data, produces a summary plot
(incorporating some measure of weighting for each study), and formats
the plot according to the assessment tool used.

The package contains three functions:

### rob\_summary()

Returns a ggplot object displaying a weighted barchart of the risk of
bias of included studies across the domains of the specified tool.

### rob\_traffic\_light()

Returns a ggplot object displaying a [“traffic light
plot”](https://handbook-5-1.cochrane.org/chapter_8/figure_8_6_c_example_of_a_risk_of_bias_summary_figure.htm),
displaying the risk of bias judgement made in each domain for each
study.

### rob\_tools()

Outputs a list of the risk of bias assessment tools for which a template
currently exists in rob\_summary(). Users can currently produce summary
plots for three commonly used tools: ROB2.0, ROBINS-I and QUADAS-2. We
expect this list to be updated in the near future to inlcude tools such
as ROBIS (tool for assessing risk of bias in systematic reviews).

    rob_tools()
    [1] "ROB2"
    [1] "ROBINS-I"
    [1] "QUADAS-2"

## Getting started

### Install the `robvis` R package

First ensure you have the `devtools` package installed:

``` r
install.packages("devtools")
library(devtools)
```

Then, to install:

``` r
install_github("mcguinlu/robvis")
library(robvis)
```

To update the package, run the `install_github("mcguinlu/robvis")`
command again.

### Load data

Load your own data (mostly likely from a .csv)

``` r
mydata <- read.csv("path/to/mydata.csv", header = TRUE)
```

To familiarise users with the package, we have included three example
datasets, one for each fo the tool templates that currently exist within
`rob_summary()`. These datasets are used to create the example plots
below.

### Create basic plots

#### RoB2.0 tool for randomized controlled trials:

``` r
plot_rob <- rob_summary(data = data_rob, tool = "ROB2")
plot_rob
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="90%" />

``` r
trafficlight_rob <- rob_traffic_light(data = data_rob, tool = "ROB2")
trafficlight_rob
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="90%" />

#### ROBINS-I tool for non-randomised studies of interventions:\*\*

``` r
plot_robins <- rob_summary(data = data_robins, tool = "ROBINS-I")
plot_robins
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

#### QUADAS-2 tool for diagnostic accuracy studies:\*\*

``` r
plot_quadas <- rob_summary(data = data_quadas, tool = "QUADAS-2")
plot_quadas
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

## Additional usage

Because the output (`plot_tool` in the above examples) is a ggplot2
object, it is easy to adjust the plot to your own preferences.

For example, to add a title to the RoB2.0 plot created above:

``` r
library(ggplot2)

plot_rob +
  ggtitle("Summary of RoB2.0 assessments")
```

![](man/figures/README-unnamed-chunk-9-1.png)<!-- -->

## Planned updates

  - Templates for additional risk of bias tools, including:
      - Cochrane risk of bias tool for randomised controlled trials (RoB
        Version 1)
      - PROBAST
      - ROBIS
  - Ability to specify file type extension when saving image. Priority
    being Window
  - Additional functionality to specify the element of QUADAS-2 that you
    want to visualise (applicability or risk of bias)
  - Choice of colour schemes (or ability to specify your own colours,
    have not decided yet)
  - Improved text processing to allow for imperfect matching of
    judgements, primarily to allow for differences in cases (e.g. “low”
    will match with “Low”" and for minor spelling errors (e.g. “loq”
    will match with “Low”)
  - A planned function to provide a brief draft paragraph summarising
    the risk-of-bias assessment results, which authors can then
    edit/elaborate on as needed. This element of the project was
    inspired by the reporter function in `metafor`

## License

This project is licensed under the MIT License - see the
[LICENSE.md](https://github.com/mcguinlu/robvis/blob/traffic_light_plot/LICENSE)
file for details.

## Acknowledgments

  - [Emily Kothe](https://github.com/ekothe) for help in fixing
    `ggplot2` coding issues.
  - [Eliza Grames](https://github.com/elizagrames) for creating the
    `robvis` hex sticker.
