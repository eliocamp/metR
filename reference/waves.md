# Fourier transform functions

Use [`fft()`](https://rdrr.io/r/stats/fft.html) to fit, filter and
reconstruct signals in the frequency domain, as well as to compute the
wave envelope.

## Usage

``` r
FitWave(y, k = 1)

BuildWave(
  x,
  amplitude,
  phase,
  k,
  wave = list(amplitude = amplitude, phase = phase, k = k),
  sum = TRUE
)

FilterWave(y, k, action = sign(k[k != 0][1]))

WaveEnvelope(y)
```

## Arguments

- y:

  numeric vector to transform

- k:

  numeric vector of wave numbers

- x:

  numeric vector of locations (in radians)

- amplitude:

  numeric vector of amplitudes

- phase:

  numeric vector of phases

- wave:

  optional list output from `FitWave`

- sum:

  whether to perform the sum or not (see Details)

- action:

  integer to disambiguate action for k = 0 (see Details)

## Value

`FitWaves` returns a a named list with components

- k:

  wavenumbers

- amplitude:

  amplitude of each wavenumber

- phase:

  phase of each wavenumber in radians

- r2:

  explained variance of each wavenumber

`BuildWave` returns a vector of the same length of x with the
reconstructed vector if `sum` is `TRUE` or, instead, a list with
components

- k:

  wavenumbers

- x:

  the vector of locations

- y:

  the reconstructed signal of each wavenumber

`FilterWave` and `WaveEnvelope` return a vector of the same length as
`y` \`

## Details

`FitWave` performs a fourier transform of the input vector and returns a
list of parameters for each wave number kept. The amplitude (A), phase
(\\\phi\\) and wave number (k) satisfy: \$\$y = \sum A cos((x -
\phi)k)\$\$ The phase is calculated so that it lies between 0 and
\\2\pi/k\\ so it represents the location (in radians) of the first
maximum of each wave number. For the case of k = 0 (the mean), phase is
arbitrarily set to 0.

`BuildWave` is `FitWave`'s inverse. It reconstructs the original data
for selected wavenumbers. If `sum` is `TRUE` (the default) it performs
the above mentioned sum and returns a single vector. If is `FALSE`, then
it returns a list of k vectors consisting of the reconstructed signal of
each wavenumber.

`FilterWave` filters or removes wavenumbers specified in `k`. If `k` is
positive, then the result is the reconstructed signal of `y` only for
wavenumbers specified in `k`, if it's negative, is the signal of `y`
minus the wavenumbers specified in `k`. The argument `action` must be be
manually set to `-1` or `+1` if `k=0`.

`WaveEnvelope` computes the wave envelope of `y` following Zimin (2003).
To compute the envelope of only a restricted band, first filter it with
`FilterWave`.

## References

Zimin, A.V., I. Szunyogh, D.J. Patil, B.R. Hunt, and E. Ott, 2003:
Extracting Envelopes of Rossby Wave Packets. Mon. Wea. Rev., 131,
1011–1017,
[doi:10.1175/1520-0493(2003)131\<1011:EEORWP\>2.0.CO;2](https://doi.org/10.1175/1520-0493%282003%29131%3C1011%3AEEORWP%3E2.0.CO%3B2)

## See also

Other meteorology functions:
[`Derivate()`](https://eliocamp.github.io/metR/reference/Derivate.md),
[`EOF()`](https://eliocamp.github.io/metR/reference/EOF.md),
[`GeostrophicWind()`](https://eliocamp.github.io/metR/reference/GeostrophicWind.md),
[`WaveFlux()`](https://eliocamp.github.io/metR/reference/WaveFlux.md),
[`thermodynamics`](https://eliocamp.github.io/metR/reference/thermodynamics.md)

## Examples

``` r
# Build a wave with specific wavenumber profile
waves <- list(k = 1:10,
              amplitude = rnorm(10)^2,
              phase = runif(10, 0, 2*pi/(1:10)))
x <- BuildWave(seq(0, 2*pi, length.out = 60)[-1], wave = waves)

# Just fancy FFT
FitWave(x, k = 1:10)
#> $amplitude
#>  [1] 0.031639811 0.222183229 2.275286190 0.020037645 0.521286930 0.006932046
#>  [7] 0.261543066 0.011468912 1.948771665 0.241891010
#> 
#> $phase
#>  [1] 4.80636512 0.23540576 1.96704693 0.10808491 0.36202644 0.26751500
#>  [7] 0.13034289 0.70324492 0.05378108 0.20742644
#> 
#> $k
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> $r2
#>  [1] 1.062237e-04 5.238128e-03 5.493203e-01 4.260366e-05 2.883416e-02
#>  [6] 5.098900e-06 7.258385e-03 1.395719e-05 4.029726e-01 6.208591e-03
#> 

# Extract only specific wave components
plot(FilterWave(x,  1), type = "l")

plot(FilterWave(x,  2), type = "l")

plot(FilterWave(x,  1:4), type = "l")


# Remove components from the signal
plot(FilterWave(x,  -4:-1), type = "l")


# The sum of the two above is the original signal (minus floating point errors)
all.equal(x, FilterWave(x,  1:4) + FilterWave(x,  -4:-1))
#> [1] TRUE

# The Wave envelopes shows where the signal is the most "wavy".
plot(x, type = "l", col = "grey")
lines(WaveEnvelope(x), add = TRUE)
#> Warning: "add" is not a graphical parameter


# Examples with real data
data(geopotential)
library(data.table)
# January mean of geopotential height
jan <- geopotential[month(date) == 1, .(gh = mean(gh)), by = .(lon, lat)]

# Stationary waves for each latitude
jan.waves <- jan[, FitWave(gh, 1:4), by = .(lat)]
library(ggplot2)
ggplot(jan.waves, aes(lat, amplitude, color = factor(k))) +
    geom_line()


# Build field of wavenumber 1
jan[, gh.1 := BuildWave(lon*pi/180, wave = FitWave(gh, 1)), by = .(lat)]
#>         lon   lat       gh     gh.1
#>       <num> <num>    <num>    <num>
#>    1:   0.0 -22.5 3167.817 11.73417
#>    2:   2.5 -22.5 3166.253 11.84884
#>    3:   5.0 -22.5 3165.204 11.94097
#>    4:   7.5 -22.5 3164.823 12.01036
#>    5:  10.0 -22.5 3165.247 12.05689
#>   ---                              
#> 4028: 347.5 -90.0 2701.129  0.00000
#> 4029: 350.0 -90.0 2701.129  0.00000
#> 4030: 352.5 -90.0 2701.129  0.00000
#> 4031: 355.0 -90.0 2701.129  0.00000
#> 4032: 357.5 -90.0 2701.129  0.00000
ggplot(jan, aes(lon, lat)) +
    geom_contour(aes(z = gh.1, color = after_stat(level))) +
    coord_polar()


# Build fields of wavenumber 1 and 2
waves <- jan[, BuildWave(lon*pi/180, wave = FitWave(gh, 1:2), sum = FALSE), by = .(lat)]
waves[, lon := x*180/pi]
#>         lat     k          x        y   lon
#>       <num> <num>      <num>    <num> <num>
#>    1: -22.5     1 0.00000000 11.73417   0.0
#>    2: -22.5     1 0.04363323 11.84884   2.5
#>    3: -22.5     1 0.08726646 11.94097   5.0
#>    4: -22.5     1 0.13089969 12.01036   7.5
#>    5: -22.5     1 0.17453293 12.05689  10.0
#>   ---                                      
#> 8060: -90.0     2 6.06501915  0.00000 347.5
#> 8061: -90.0     2 6.10865238  0.00000 350.0
#> 8062: -90.0     2 6.15228561  0.00000 352.5
#> 8063: -90.0     2 6.19591884  0.00000 355.0
#> 8064: -90.0     2 6.23955208  0.00000 357.5
ggplot(waves, aes(lon, lat)) +
    geom_contour(aes(z = y, color = after_stat(level))) +
    facet_wrap(~k) +
    coord_polar()


# Field with waves 0 to 2 filtered
jan[, gh.no12 := gh - BuildWave(lon*pi/180, wave = FitWave(gh, 0:2)), by = .(lat)]
#>         lon   lat       gh     gh.1  gh.no12
#>       <num> <num>    <num>    <num>    <num>
#>    1:   0.0 -22.5 3167.817 11.73417 5.858149
#>    2:   2.5 -22.5 3166.253 11.84884 4.006104
#>    3:   5.0 -22.5 3165.204 11.94097 2.689758
#>    4:   7.5 -22.5 3164.823 12.01036 2.061137
#>    5:  10.0 -22.5 3165.247 12.05689 2.261535
#>   ---                                       
#> 4028: 347.5 -90.0 2701.129  0.00000 0.000000
#> 4029: 350.0 -90.0 2701.129  0.00000 0.000000
#> 4030: 352.5 -90.0 2701.129  0.00000 0.000000
#> 4031: 355.0 -90.0 2701.129  0.00000 0.000000
#> 4032: 357.5 -90.0 2701.129  0.00000 0.000000
ggplot(jan, aes(lon, lat)) +
    geom_contour(aes(z = gh.no12, color = after_stat(level))) +
    coord_polar()


# Much faster
jan[, gh.no12 := FilterWave(gh, -2:0), by = .(lat)]
#>         lon   lat       gh     gh.1  gh.no12
#>       <num> <num>    <num>    <num>    <num>
#>    1:   0.0 -22.5 3167.817 11.73417 5.858149
#>    2:   2.5 -22.5 3166.253 11.84884 4.006104
#>    3:   5.0 -22.5 3165.204 11.94097 2.689758
#>    4:   7.5 -22.5 3164.823 12.01036 2.061137
#>    5:  10.0 -22.5 3165.247 12.05689 2.261535
#>   ---                                       
#> 4028: 347.5 -90.0 2701.129  0.00000 0.000000
#> 4029: 350.0 -90.0 2701.129  0.00000 0.000000
#> 4030: 352.5 -90.0 2701.129  0.00000 0.000000
#> 4031: 355.0 -90.0 2701.129  0.00000 0.000000
#> 4032: 357.5 -90.0 2701.129  0.00000 0.000000
ggplot(jan, aes(lon, lat)) +
    geom_contour(aes(z = gh.no12, color = after_stat(level))) +
    coord_polar()


# Using positive numbers returns the field
jan[, gh.only12 := FilterWave(gh, 2:1), by = .(lat)]
#>         lon   lat       gh     gh.1  gh.no12 gh.only12
#>       <num> <num>    <num>    <num>    <num>     <num>
#>    1:   0.0 -22.5 3167.817 11.73417 5.858149  11.17110
#>    2:   2.5 -22.5 3166.253 11.84884 4.006104  11.45865
#>    3:   5.0 -22.5 3165.204 11.94097 2.689758  11.72661
#>    4:   7.5 -22.5 3164.823 12.01036 2.061137  11.97348
#>    5:  10.0 -22.5 3165.247 12.05689 2.261535  12.19776
#>   ---                                                 
#> 4028: 347.5 -90.0 2701.129  0.00000 0.000000   0.00000
#> 4029: 350.0 -90.0 2701.129  0.00000 0.000000   0.00000
#> 4030: 352.5 -90.0 2701.129  0.00000 0.000000   0.00000
#> 4031: 355.0 -90.0 2701.129  0.00000 0.000000   0.00000
#> 4032: 357.5 -90.0 2701.129  0.00000 0.000000   0.00000
ggplot(jan, aes(lon, lat)) +
    geom_contour(aes(z = gh.only12, color = after_stat(level))) +
    coord_polar()


# Compute the envelope of the geopotential
jan[, envelope := WaveEnvelope(gh.no12), by = .(lat)]
#>         lon   lat       gh     gh.1  gh.no12 gh.only12  envelope
#>       <num> <num>    <num>    <num>    <num>     <num>     <num>
#>    1:   0.0 -22.5 3167.817 11.73417 5.858149  11.17110 10.834194
#>    2:   2.5 -22.5 3166.253 11.84884 4.006104  11.45865  9.544124
#>    3:   5.0 -22.5 3165.204 11.94097 2.689758  11.72661  8.173016
#>    4:   7.5 -22.5 3164.823 12.01036 2.061137  11.97348  6.906214
#>    5:  10.0 -22.5 3165.247 12.05689 2.261535  12.19776  5.958197
#>   ---                                                           
#> 4028: 347.5 -90.0 2701.129  0.00000 0.000000   0.00000  0.000000
#> 4029: 350.0 -90.0 2701.129  0.00000 0.000000   0.00000  0.000000
#> 4030: 352.5 -90.0 2701.129  0.00000 0.000000   0.00000  0.000000
#> 4031: 355.0 -90.0 2701.129  0.00000 0.000000   0.00000  0.000000
#> 4032: 357.5 -90.0 2701.129  0.00000 0.000000   0.00000  0.000000
ggplot(jan[lat == -60], aes(lon, gh.no12)) +
    geom_line() +
    geom_line(aes(y = envelope), color = "red")

```
