
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
raw_data <- download_earthquake_data()
clean_data <- eq_clean_data(raw_data)
clean_data <- eq_location_clean(clean_data)
```

Once the data was tidied, `rnoaa` includes two gplot2's geoms to visualize the timeline in which the quakes have ocurred and label the ones with the greater magnitude:

``` r
library(dplyr)
library(ggplot2)
clean_data %>%
  filter(COUNTRY == "MEXICO", 
         !is.na(EQ_PRIMARY),
         YEAR %in% 2000:2016) %>% 
  ggplot(mapping = aes(x = DATE,
                       #y = COUNTRY,
                       size = EQ_PRIMARY,
                       color = TOTAL_DEATHS,
                       label = LOCATION_NAME)
         ) +
  geom_timeline() +
  geom_timeline_label(line_height = 2/3, angle = 10) +
  labs(size = "Richter scale value",
       color = "# deaths", 
       y = "") +
  guides(size = FALSE) +
  theme_timeline()
```

![](man/figures/README-TimelineGeom-1.png)

Furthermore, it provides with functions to display the epicenters in an interactive R Leaflet map:

``` r
clean_data %>%
  dplyr::filter(COUNTRY == "JAPAN" & lubridate::year(DATE) >= 2000) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = "popup_text")
```

![](./man/figures/README-LeafletMap-1.png?raw=true)
