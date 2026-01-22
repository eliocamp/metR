# Calculate wave-activity flux

Calculate wave-activity flux

## Usage

``` r
WaveFlux(gh, u, v, lon, lat, lev, g = 9.81, a = 6371000)
```

## Arguments

- gh:

  Geopotential Height Anomaly (unit: gpm or m). The deviation of the
  geopotential height from the climatological mean (\\z' = z -
  \bar{z}\\). It is used to compute the perturbation geostrophic
  streamfunction: \\\psi' = \frac{g}{f}z'\\.

- u:

  Mean Zonal Wind (unit: m/s). The background basic flow (\\\bar{u}\\),
  usually representing the long-term climatological mean zonal velocity.

- v:

  Mean Meridional Wind (unit: m/s). The background basic flow
  (\\\bar{v}\\), usually representing the long-term climatological mean
  meridional velocity.

- lon:

  Numeric vector of longitudes (unit: degrees).

- lat:

  Numeric vector of latitudes (unit: degrees).

- lev:

  The pressure level of the data (unit: hPa). Used for the vertical
  scaling factor \\p/1000\\ in the T-N flux formula.

- g:

  Standard gravity acceleration (default: \\9.81 m/s^2\\).

- a:

  Earth's mean radius (default: 6,371,000 m).

## Value

A `data.table` containing the calculated wave activity flux components:

- w.x:

  Zonal component of the Wave Activity Flux.

- w.y:

  Meridional component of the Wave Activity Flux.

## Details

The function computes the horizontal components (\\W_x, W_y\\) of the
phase-independent wave activity flux.

**Note on Units:** Ensure that `gh` is provided as **Geopotential
Height** (in meters or gpm). If your input data is **Geopotential**
(\\\Phi\\, in \\m^2/s^2\\, common in ERA5 raw data), you must divide it
by gravity (g) before passing it to this function.

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
