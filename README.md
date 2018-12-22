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

### documentation

```r
pkgdown::build_site()
```


