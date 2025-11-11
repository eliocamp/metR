# Denormalise eof matrices

The matrices returned by
[`EOF()`](https://eliocamp.github.io/metR/reference/EOF.md) are
normalized. This function multiplies the left or right matrix by the
diagonal matrix to return it to proper units.

## Usage

``` r
denormalise(eof, which = c("left", "right"))

denormalize(eof, which = c("left", "right"))
```

## Arguments

- eof:

  an `eof` object.

- which:

  which side of the eof decomposition to denormalise
