
# We use `<<-` below to modify the package's namespace.
# It doesn't modify the global environment.
# We do this to prevent build time dependencies on {memoise} and {ratelimitr},
# as recommended in <http://memoise.r-lib.org/reference/memoise.html#details>.
# Cf. <https://github.com/r-lib/memoise/issues/76> for further details.
fast.unit.c <- grid::unit.c

.onLoad <- function(libname, pkgname) {
    .impute_data <<- memoise::memoise(.impute_data)
    fast.unit.c <<- memoise::memoise(grid::unit.c)
    .cast_shadow <<- memoise::memoise(.cast_shadow)
    streamline.f <<- memoise::memoise(streamline.f)
    .order_contour <- memoise::memoise(.order_contour)
}
