
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

    ggplot2::register_theme_elements(
        palette.label_colour.continuous = scales::pal_seq_gradient("#132B43", "#56B1F7"),
        element_tree = list(
            palette.label_colour.continuous = ggplot2::el_def(c("character", "numeric", "integer", "function"))
        )
    )

    ggplot2::register_theme_elements(
        palette.stroke.colour.continuous = scales::pal_seq_gradient("#132B43", "#56B1F7"),
        element_tree = list(
            palette.stroke.colour.continuous = ggplot2::el_def(c("character", "numeric", "integer", "function"))
        )
    )
}

.onAttach <- function(...) {
    if (!interactive()) return()

    # get semirandom number without messing with the seed
    trigger <- floor(as.numeric(Sys.time()) %% 100) <= 1

    if (trigger) packageStartupMessage("Trans rights are human rights.")
}
