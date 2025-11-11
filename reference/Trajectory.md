# Compute trajectories

Computes trajectories of particles in a time-varying velocity field.

## Usage

``` r
Trajectory(formula, x0, y0, cyclical = FALSE, data = NULL, res = 2)
```

## Arguments

- formula:

  a formula indicating dependent and independent variables in the form
  of dx + dy ~ x + y + t.

- x0, y0:

  starting coordinates of the particles.

- cyclical:

  logical vector of boundary condition for x and y.

- data:

  optional data.frame containing the variables.

- res:

  resolution parameter (higher numbers increases the resolution)
