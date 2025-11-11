# Computes Eliassen-Palm fluxes.

Computes Eliassen-Palm fluxes.

## Usage

``` r
EPflux(lon, lat, lev, t, u, v)
```

## Arguments

- lon:

  longitudes in degrees.

- lat:

  latitudes in degrees.

- lev:

  pressure levels.

- t:

  temperature in Kelvin.

- u:

  zonal wind in m/s.

- v:

  meridional wind in m/s.

## Value

A data.table with columns `Flon`, `Flat` and `Flev` giving the zonal,
meridional and vertical components of the EP Fluxes at each longitude,
latitude and level.

## References

Plumb, R. A. (1985). On the Three-Dimensional Propagation of Stationary
Waves. Journal of the Atmospheric Sciences, 42(3), 217–229.
[doi:10.1175/1520-0469(1985)042\<0217:OTTDPO\>2.0.CO;2](https://doi.org/10.1175/1520-0469%281985%29042%3C0217%3AOTTDPO%3E2.0.CO%3B2)
Cohen, J., Barlow, M., Kushner, P. J., & Saito, K. (2007).
Stratosphere–Troposphere Coupling and Links with Eurasian Land Surface
Variability. Journal of Climate, 20(21), 5335–5343.
[doi:10.1175/2007JCLI1725.1](https://doi.org/10.1175/2007JCLI1725.1)
