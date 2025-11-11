# Assign seasons to months

Assign seasons to months

## Usage

``` r
season(x, lang = c("en", "es"))

seasonally(x)

is.full_season(x)
```

## Arguments

- x:

  A vector of dates (alternative a numeric vector of months, for
  `season()`)

- lang:

  Language to use.

## Value

`season()` returns a factor vector of the same length as `x` with the
trimester of each month. `seasonaly()` returns a date vector of the same
length as `x` with the date "rounded" up to the centre month of each
season. `is.full_season()` returns a logical vector of the same length
as `x` that is true only if the 3 months of each season for each year
(December counts for the following year) are present in the dataset.

## Examples

``` r
season(1, lang = "en")
#> [1] DJF
#> Levels: DJF MAM JJA SON
season(as.Date("2017-01-01"))
#> [1] DJF
#> Levels: DJF MAM JJA SON

seasonally(as.Date(c("2017-12-01", "2018-01-01", "2018-02-01")))
#> [1] "2018-01-15" "2018-01-15" "2018-01-15"

is.full_season(as.Date(c("2017-12-01", "2018-01-01", "2018-02-01", "2018-03-01")))
#> [1]  TRUE  TRUE  TRUE FALSE
```
