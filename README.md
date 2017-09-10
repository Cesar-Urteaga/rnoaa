
<!--
  README.md is generated from README.Rmd, so you should edit that file.
-->
rnoaa <img src="man/figures/logo.png" align="right" width="120"/>
=================================================================

Overview
--------

`rnoaa` is an [R](https://www.r-project.org/) package with a set of functions that makes easier to analyze the earthquake data provided from the US National Oceanic and Atmospheric Administration (NOAA).

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
# In case you do not have internet access, you can use the get_earthquake_data
# function, which is a snapshot of the quake's data on September 10, 2017:
# raw_data <- get_earthquake_data()
raw_data <- download_earthquake_data()
# Basically, converts some important variables into the right format.
clean_data <- eq_clean_data(raw_data)
# Tidies the variable with the quake's location.
clean_data <- eq_location_clean(clean_data)
```

Once the data was tidied, `rnoaa` includes two ggplot2's geoms to visualize the timeline in which the quakes have ocurred and label the ones with the greater magnitude:

``` r
library(dplyr)
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

Furthermore, it provides with functions to display the epicenters in an interactive R Leaflet map:

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
