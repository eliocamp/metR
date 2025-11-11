# Functions to place contour labels

These functions compute the position of contour labels.

## Usage

``` r
label_placer_fraction(
  frac = 0.5,
  rot_adjuster = isoband::angle_halfcircle_bottom()
)

label_placement_fraction(
  frac = 0.5,
  rot_adjuster = isoband::angle_halfcircle_bottom()
)

label_placer_n(n = 2, rot_adjuster = isoband::angle_halfcircle_bottom())

label_placement_n(n = 2, rot_adjuster = isoband::angle_halfcircle_bottom())

label_placer_random(
  seed = 42,
  n = 1,
  rot_adjuster = isoband::angle_halfcircle_bottom()
)

label_placement_random(
  seed = 42,
  n = 1,
  rot_adjuster = isoband::angle_halfcircle_bottom()
)

label_placer_all(rot_adjuster = isoband::angle_halfcircle_bottom())

label_placement_all(rot_adjuster = isoband::angle_halfcircle_bottom())

label_placer_flattest(
  ref_angle = 0,
  rot_adjuster = isoband::angle_halfcircle_bottom()
)

label_placement_flattest(
  ref_angle = 0,
  rot_adjuster = isoband::angle_halfcircle_bottom()
)

label_placer_minmax(
  direction = c("vertical", "horizontal"),
  rot_adjuster = isoband::angle_halfcircle_bottom()
)

label_placement_minmax(
  direction = c("vertical", "horizontal"),
  rot_adjuster = isoband::angle_halfcircle_bottom()
)
```

## Arguments

- frac:

  A numeric vector with values between 0 and 1 representing where in the
  contour to put labels (i.e. `frac = 0.5` puts labels at the midpoint).

- rot_adjuster:

  A function that standardizes the rotation angles of the labels. See
  e.g.
  [isoband::angle_halfcircle_bottom](https://isoband.r-lib.org/reference/angle_halfcircle_bottom.html).

- n:

  Number of labels to put.

- seed:

  Seed to use for randomly choosing where to put labels.

- ref_angle:

  Angle (in degrees counter-clockwise from East) to try to approximate
  labels.

- direction:

  Direction in which to compute the maximum and minimum.
