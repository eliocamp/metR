# Get Meteorological data This function is defunct.

Get Meteorological data This function is defunct.

## Usage

``` r
GetSMNData(
  date,
  type = c("hourly", "daily", "radiation"),
  bar = FALSE,
  cache = TRUE,
  file.dir = tempdir()
)
```

## Arguments

- date:

  date vector of dates to fetch data

- type:

  type of data to retrieve

- bar:

  logical object indicating whether to show a progress bar

- cache:

  logical indicating if the results should be saved on disk

- file.dir:

  optional directory where to save and/or retrieve data

## Value

Nothing
