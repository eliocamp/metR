# Calculate wave-activity flux

Calculate wave-activity flux

## Usage

``` r
WaveFlux(gh, u, v, lon, lat, lev, g = 9.81, a = 6371000)
```

## Arguments

- gh:

  geopotential height

- u:

  mean zonal velocity

- v:

  mean meridional velocity

- lon:

  longitude (in degrees)

- lat:

  latitude (in degrees)

- lev:

  pressure level (in hPa)

- g:

  acceleration of gravity

- a:

  Earth's radius

## Value

A list with elements: longitude, latitude, and the two horizontal
components of the wave activity flux.

## Details

Calculates Plum-like wave activity fluxes

## References

Takaya, K. and H. Nakamura, 2001: A Formulation of a Phase-Independent
Wave-Activity Flux for Stationary and Migratory Quasigeostrophic Eddies
on a Zonally Varying Basic Flow. J. Atmos. Sci., 58, 608–627,
[doi:10.1175/1520-0469(2001)058\<0608:AFOAPI\>2.0.CO;2](https://doi.org/10.1175/1520-0469%282001%29058%3C0608%3AAFOAPI%3E2.0.CO%3B2)  
Adapted from
<https://github.com/marisolosman/Reunion_Clima/blob/master/WAF/Calculo_WAF.ipynb>

## See also

Other meteorology functions:
[`Derivate()`](https://eliocamp.github.io/metR/reference/Derivate.md),
[`EOF()`](https://eliocamp.github.io/metR/reference/EOF.md),
[`GeostrophicWind()`](https://eliocamp.github.io/metR/reference/GeostrophicWind.md),
[`thermodynamics`](https://eliocamp.github.io/metR/reference/thermodynamics.md),
[`waves`](https://eliocamp.github.io/metR/reference/waves.md)
