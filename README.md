
<!--
  README.md is generated from README.Rmd, so you should edit that file.
-->
rnoaa <img src="man/figures/logo.png" align="right" width="120"/>
=================================================================

[![Travis Build Status](https://travis-ci.org/Cesar-Urteaga/rnoaa.svg?branch=master)](https://travis-ci.org/Cesar-Urteaga/rnoaa) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/Cesar-Urteaga/rnoaa?branch=master&svg=true)](https://ci.appveyor.com/project/Cesar-Urteaga/rnoaa)

Overview
--------

`rnoaa` is an [R](https://www.r-project.org/) package with a set of functions that makes easier to analyze the earthquake data provided by the US National Oceanic and Atmospheric Administration (NOAA).

Installation
------------

``` r
# Install the package from GitHub without the vignette:
devtools::install_github("Cesar-Urteaga/rnoaa")

# Or you can include it:
devtools::install_github("Cesar-Urteaga/rnoaa", build_vignettes = TRUE)
```

Usage
-----

This package allows you to get and clean the latest earthquake data from [the NOAA's Webpage](https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1) so as to prepare it for analysis.

``` r
library(rnoaa)
library(dplyr)

# GETTING THE DATA
#   In case you do not have internet access, you can use the get_earthquake_data
#   function, which is a snapshot of the quake's data on September 10, 2017:
#   raw_data <- get_earthquake_data()
raw_data <- download_earthquake_data()

# TIDYING THE DATA UP
#   Before the data has been processed:
raw_data %>%
  select(YEAR, MONTH, DAY, COUNTRY, LOCATION_NAME) %>%
  head()
# A tibble: 6 x 5
   YEAR MONTH   DAY      COUNTRY                     LOCATION_NAME
  <int> <int> <int>        <chr>                             <chr>
1 -2150    NA    NA       JORDAN     JORDAN:  BAB-A-DARAA,AL-KARAK
2 -2000    NA    NA TURKMENISTAN                  TURKMENISTAN:  W
3 -2000    NA    NA        SYRIA                    SYRIA:  UGARIT
4 -1610    NA    NA       GREECE GREECE:  THERA ISLAND (SANTORINI)
5 -1566    NA    NA       ISRAEL          ISRAEL:  ARIHA (JERICHO)
6 -1450    NA    NA        ITALY              ITALY:  LACUS CIMINI
#   We use the two rnoaa's functions to clean the data.
clean_data <- raw_data %>%
  eq_clean_data() %>%
  eq_location_clean()
#   After the data has been processed (note that the DATE variable has been
#   created and the country has been removed for the LOCATION_NAME variable):
clean_data %>%
  select(YEAR, MONTH, DAY, DATE, COUNTRY, LOCATION_NAME) %>%
  head()
# A tibble: 6 x 6
   YEAR MONTH   DAY        DATE      COUNTRY            LOCATION_NAME
  <int> <int> <int>      <date>        <chr>                    <chr>
1 -2150    NA    NA -2150-07-02       JORDAN    Bab-A-Daraa, Al-Karak
2 -2000    NA    NA -2000-07-02 TURKMENISTAN                        W
3 -2000    NA    NA -2000-07-02        SYRIA                   Ugarit
4 -1610    NA    NA -1610-07-02       GREECE Thera Island (Santorini)
5 -1566    NA    NA -1566-07-02       ISRAEL          Ariha (Jericho)
6 -1450    NA    NA -1450-07-02        ITALY             Lacus Cimini
```

Once the data was tidied, `rnoaa` includes two ggplot2's geoms to visualize the timeline in which the quakes have ocurred and label the ones with the greater magnitude:

``` r
library(ggplot2)
clean_data %>%
  filter(COUNTRY %in% c("CANADA", "USA", "MEXICO",
                        "CHINA", "JAPAN", "INDIA"),
         !is.na(EQ_PRIMARY),
         YEAR %in% 2000:2016) %>%
  ggplot(mapping = aes(x = DATE,
                       y = COUNTRY,
                       size = EQ_PRIMARY,
                       color = TOTAL_DEATHS / 1000,
                       label = LOCATION_NAME)
         ) +
  geom_timeline() +
  geom_timeline_label(# We want to show the label for at most the two highest
                      # earthquakes by size.
                      n_max = 2,
                      line_height = 1 / 4,
                      angle = 10,
                      fontsize = 2.5) +
  labs(size = "Richter scale value",
       color = "# deaths in thousands",
       y = "") +
  guides(size = FALSE) +
  theme_timeline()
```

<img src="man/figures/README-TimelineGeom-1.png" style="display: block; margin: auto;" />

Furthermore, it provides with functions to display the epicenters in an interactive R leaflet map:

``` r
clean_data %>%
  dplyr::filter(COUNTRY == "JAPAN" & lubridate::year(DATE) >= 2000) %>%
  eq_map(annot_col = "DATE")
```

![](./man/figures/README-LeafletMap-1.png?raw=true)

Also, it assists you to display the quake's traits from the data using popup text labels:

``` r
clean_data %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) == 2017) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = "popup_text")
```

![](./man/figures/README-LeafletMap-2.png?raw=true)

Documentation
-------------

Please check the [package's vignette](./vignettes/using-rnoaa.Rmd) to see how the package works or review the examples given in the package's documentation (use `?function_name`); you can run them with the function `example` (e.g., `example("geom_timeline_label")`).

Unit Testing
------------

In order to increase the quality of the package, a [set of tests](./tests/testthat) were carried out for each function using the `testthat` R package.

Development Workflow
--------------------

The workflow that was used to develop this package is described [here](https://github.com/Cesar-Urteaga/rfars#workflow).
