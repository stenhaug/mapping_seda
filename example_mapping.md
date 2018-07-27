example\_transition
================
Ben Stenhaug

# Read in shape files

``` r
shape <- readOGR(dsn = "SEDA_shapefiles_v20", layer = "2013_Unified_Elementary_SD") %>%
  st_as_sf()
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "/Users/benastenhaug/Desktop/mapping/SEDA_shapefiles_v20", layer: "2013_Unified_Elementary_SD"
    ## with 14285 features
    ## It has 29 fields
    ## Integer64 fields read as strings:  ALAND AWATER

# Read in scores by district

``` r
scores <- read_csv("SEDA_geodist_long_NAEP_v21.csv") %>%
  filter(year == 2010, subject == "math", grade == 3)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   leaidC = col_character(),
    ##   leaname = col_character(),
    ##   fips = col_character(),
    ##   stateabb = col_character(),
    ##   grade = col_integer(),
    ##   year = col_integer(),
    ##   subject = col_character(),
    ##   totgyb_all = col_integer(),
    ##   totgyb_asn = col_integer(),
    ##   totgyb_blk = col_integer(),
    ##   totgyb_fem = col_integer(),
    ##   totgyb_hsp = col_integer(),
    ##   totgyb_mal = col_integer(),
    ##   totgyb_wht = col_integer()
    ## )

    ## See spec(...) for full column specifications.

# Simulate random student locations before and after

``` r
n_students <- 10000

coordinates_before <- data.frame(
    lon = runif(n_students, -120, -80), 
    lat = runif(n_students, 33, 47)
)

coordinates_after <- data.frame(
    lon = runif(n_students, -120, -80), 
    lat = runif(n_students, 33, 47)
)
```

# Write a function that takes coordinates and give district test score

``` r
coordinates_to_district_score <- function(coordinates, shape, suffix) {
  sf <- st_as_sf(coordinates, coords = c("lon", "lat"), crs = st_crs(shape))

  school_district <- st_join(sf, shape, largest = TRUE) %>% select(STATEFP:NAME) 

  scores_df <- school_district %>% 
      left_join(scores %>% 
                    select(leaidC, mn_all), by = c("GEOID" = "leaidC"))

  tibble(
    district = scores_df$NAME, geoid = scores_df$GEOID,
    district_avg_score = scores_df$mn_all,
    district_percent = percent_rank(scores_df$mn_all)
  ) %>%
    set_names(paste0(names(.), "_", suffix))
}
```

# Apply that function to get before and after

``` r
before <- coordinates_to_district_score(coordinates_before, shape, "before")
```

    ## although coordinates are longitude/latitude, st_intersection assumes that they are planar

    ## Warning: attribute variables are assumed to be spatially constant
    ## throughout all geometries

    ## Warning: Column `GEOID`/`leaidC` joining factor and character vector,
    ## coercing into character vector

``` r
after <- coordinates_to_district_score(coordinates_after, shape, "after")
```

    ## although coordinates are longitude/latitude, st_intersection assumes that they are planar

    ## Warning: attribute variables are assumed to be spatially constant
    ## throughout all geometries
    
    ## Warning: Column `GEOID`/`leaidC` joining factor and character vector,
    ## coercing into character vector

# Now we can generate transition matrix

``` r
get_decile <- function(x){
    floor(x * 10) / 10
}

transition <- tibble(
    before = before$district_percent_before,
    after = after$district_percent_after
) %>% 
    mutate_all(get_decile)

transition %>% 
    na.omit() %>% 
    count(before, after) %>% 
    mutate(n = n / sum(n)) %>% 
    spread(after, n)
```

    ## # A tibble: 11 x 12
    ##    before      `0`   `0.1`    `0.2`    `0.3`    `0.4`    `0.5`    `0.6`
    ##     <dbl>    <dbl>   <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ##  1    0    0.0102  9.23e-3  0.0116   0.0116   0.0125   0.0109   0.00758
    ##  2    0.1  0.00971 8.29e-3  0.0107   0.00923  0.0102   0.00900  0.0109 
    ##  3    0.2  0.0114  9.94e-3  0.0111   0.00971  0.00947  0.00971  0.0104 
    ##  4    0.3  0.0116  1.11e-2  0.00781  0.00900  0.0109   0.0114   0.00852
    ##  5    0.4  0.0111  9.23e-3  0.00971  0.00947  0.0107   0.00947  0.0102 
    ##  6    0.5  0.00994 6.87e-3  0.0109   0.0111   0.0111   0.00994  0.00781
    ##  7    0.6  0.0133  1.21e-2  0.0125   0.00852  0.00710  0.00994  0.0123 
    ##  8    0.7  0.00947 9.47e-3  0.00805  0.0128   0.0104   0.00900  0.0111 
    ##  9    0.8  0.00852 9.47e-3  0.0116   0.00805  0.0104   0.0109   0.00947
    ## 10    0.9  0.00734 1.02e-2  0.0102   0.00923  0.00805  0.0102   0.0107 
    ## 11    1   NA       2.37e-4 NA       NA       NA       NA       NA      
    ## # ... with 4 more variables: `0.7` <dbl>, `0.8` <dbl>, `0.9` <dbl>,
    ## #   `1` <dbl>
