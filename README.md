# shipr

## Package creation

### setup 
```r
library(devtools)
devtools::setup()
```

### data

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
```


### documentation

```r
devtools::document()
pkgdown::build_site()
```


