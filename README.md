# shipr

## Install Package

```r
# if missing package devtools
install.packages(devtools) 

# install this R package
devtools::install_github("mvisalli/shipr")
```

## Use Package

- See R package vignette [Basic Ship Reporting • shipr](https://mvisalli.github.io/shipr/articles/intro.html)
- See [website](http://ecoquants.github.io/ship-cards/) and [code](https://github.com/ecoquants/ship-cards) demonstration of making ship cards using [parameterized](https://bookdown.org/yihui/rmarkdown/parameterized-reports.html) [flexdashboard](https://rmarkdown.rstudio.com/flexdashboard/).

## Package Development

For reference, see:

- cheat sheet: [package development](https://github.com/rstudio/cheatsheets/raw/master/package-development.pdf)
- book: [R packages](http://r-pkgs.had.co.nz/) by Hadley Wickham

### setup 

Setup the skeleton of needed files in an existing directory:

```r
devtools::setup()
```

### functions

Muliple functions can live in a single *.R script inside the `R/` folder:

- [`read.R`](https://github.com/mvisalli/shipr/blob/master/R/read.R):

    - `read_ais_txt()`: Read AIS text file
  
- [`analyze.R`](https://github.com/mvisalli/shipr/blob/master/R/analyze.R):

    - `ship_limits()`: Get breakdown of distance and time above/below speed limit
    
    - `ship_segments()`: Create line segments with speed for specific ship from AIS data

- [`viz.R`](https://github.com/mvisalli/shipr/blob/master/R/viz.R):

    - `map_ship_limits()`: Interactive map of ship limits
    
    - `map_ship_segments()`: Interactive map of ship segments

    - `ship_gauge()`: Show gauge of ship performance
    
    - `ship_box()`: Show value box of ship performance

### documentation

Generate the R documentation from roxygen2 comments above the R functions to be placed into the manual folder `/man`:

```r
devtools::document()
```
    
### data

Use raw data:

```r
devtools::use_data_raw()
```

Move data files into `data-raw/`, then `sbais.R` to generate `sbais.csv` and load into `data/` with:

```r
usethis::use_data(sbais, overwrite = TRUE)
```

### vignettes

```r
devtools::use_vignette("intro")
```

### packages

Create dependencies on packages so these get installed if missing. Updates the DESCRIPTION file:

```r
usethis::use_package("dplyr")
usethis::use_package("leaflet")
usethis::use_package("lubridate")
usethis::use_package("purrr")
usethis::use_package("reader")
usethis::use_package("readr")
usethis::use_package("sf")
usethis::use_package("stringr")
usethis::use_package("units")
usethis::use_package("yaml")
usethis::use_package("DBI")
usethis::use_package("RPostgres")
usethis::use_package("glue")
usethis::use_package("here")
usethis::use_package("xml2")
```

### website

Build website into `docs/` folder with [`pkgdown`](https://pkgdown.r-lib.org), to be hosted using [Github Pages](https://pages.github.com).

```r
devtools::document()
pkgdown::build_site()
```

Structure reference listing with `inst/_pkgdown.yml` per:

* [Introduction • pkgdown](https://pkgdown.r-lib.org/articles/pkgdown.html#configuration)
* [`build_reference()` • pkgdown](https://pkgdown.r-lib.org/reference/build_reference.html)

```r
usethis::use_github_links()
```

```
✔ Setting URL field in DESCRIPTION to 'https://github.com/mvisalli/shipr'
✔ Setting BugReports field in DESCRIPTION to 'https://github.com/mvisalli/shipr/issues'
```

```r
usethis::use_news_md()
```

```
✔ Writing 'NEWS.md'
```
