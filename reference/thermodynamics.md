# Thermodynamics

Functions related to common atmospheric thermodynamic relationships.

## Usage

``` r
IdealGas(p, t, rho, R = 287.058)

Adiabat(p, t, theta, p0 = 1e+05, kappa = 2/7)

VirtualTemperature(p, t, e, tv, epsilon = 0.622)

MixingRatio(p, e, w, epsilon = 0.622)

ClausiusClapeyron(t, es)

DewPoint(p, ws, td, epsilon = 0.622)
```

## Arguments

- p:

  pressure

- t:

  temperature

- rho:

  density

- R:

  gas constant for air

- theta:

  potential temperature

- p0:

  reference pressure

- kappa:

  ratio of dry air constant and specific heat capacity at constant
  pressure

- e:

  vapour partial pressure

- tv:

  virtual temperature

- epsilon:

  ratio of dry air constant and vapour constant

- w:

  mixing ratio

- es:

  saturation vapour partial pressure

- ws:

  saturation mixing ratio

- td:

  dewpoint

## Value

Each function returns the value of the missing state variable.

## Details

`IdealGas` computes pressure, temperature or density of air according to
the ideal gas law \\P=\rho R T\\.

`Adiabat` computes pressure, temperature or potential temperature
according to the adiabatic relationship \\\theta = T (P0/P)^\kappa\\.

`VirtualTemperature` computes pressure, temperature, vapour partial
pressure or virtual temperature according to the virtual temperature
definition \\T(1 - e/P(1 - \epsilon))^{-1}\\.

`MixingRatio` computes pressure, vapour partial temperature, or mixing
ratio according to \\w = \epsilon e/(P - e)\\.

`ClausiusClapeyron` computes saturation pressure or temperature
according to the August-Roche-Magnus formula \\es = a exp{bT/(T + c)}\\
with temperature in Kelvin and saturation pressure in Pa.

`DewPoint` computes pressure, saturation mixing ration or dew point from
the relationship \\ws = \epsilon es(Td)/(p - es(Td))\\. Note that the
computation of dew point is approximated.

Is important to take note of the units in which each variable is
provided. With the default values, pressure should be passed in Pascals,
temperature and potential temperature in Kelvins, and density in
\\kg/m^3\\. `ClausiusClayperon` and `DewPoint` require and return values
in those units.

The defaults value of the `R` and `kappa` parameters are correct for dry
air, for the case of moist air, use the virtual temperature instead of
the actual temperature.

## References

http://www.atmo.arizona.edu/students/courselinks/fall11/atmo551a/ATMO_451a_551a_files/WaterVapor.pdf

## See also

Other meteorology functions:
[`Derivate()`](https://eliocamp.github.io/metR/reference/Derivate.md),
[`EOF()`](https://eliocamp.github.io/metR/reference/EOF.md),
[`GeostrophicWind()`](https://eliocamp.github.io/metR/reference/GeostrophicWind.md),
[`WaveFlux()`](https://eliocamp.github.io/metR/reference/WaveFlux.md),
[`waves`](https://eliocamp.github.io/metR/reference/waves.md)

## Examples

``` r
IdealGas(1013*100, 20 + 273.15)
#> [1] 1.203788
IdealGas(1013*100, rho = 1.15) - 273.15
#> [1] 33.71118

(theta <- Adiabat(70000, 20 + 273.15))
#> [1] 324.5993
Adiabat(70000, theta = theta) - 273.15
#> [1] 20

# Relative humidity from T and Td
t <- 25 + 273.15
td <- 20 + 273.15
p <- 1000000
(rh <- ClausiusClapeyron(td)/ClausiusClapeyron(t))
#> [1] 0.7380251

# Mixing ratio
ws <- MixingRatio(p, ClausiusClapeyron(t))
w <- ws*rh
DewPoint(p, w) - 273.15    # Recover Td
#> [1] 20.01339
```
