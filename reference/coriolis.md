# Effects of the Earth's rotation

Coriolis and beta parameters by latitude.

## Usage

``` r
coriolis(lat)

f(lat)

coriolis.dy(lat, a = 6371000)

f.dy(lat, a = 6371000)
```

## Arguments

- lat:

  latitude in degrees

- a:

  radius of the earth

## Details

All functions use the correct sidereal day (24hs 56mins 4.091s) instead
of the incorrect solar day (24hs) for 0.3\\ pedantry.
