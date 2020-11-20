
get_middle <- function(x) {
    if (is.numeric(x)) {
        return(x)
    }
    breaks <- levels(x)
    splitted <- strsplit(gsub("[\\(\\[\\)\\]]", "", as.character(breaks), perl = TRUE), ",")


    low <- vapply(splitted, function(x) min(as.numeric(x)), FUN.VALUE =  numeric(1))
    high <- vapply(splitted, function(x) max(as.numeric(x)), FUN.VALUE =  numeric(1))

    if (low[1] == -Inf) {
        low[1] <- high[1] - (high[2] - low[2])
    }
    n <- length(breaks)
    if (high[n] == Inf) {
        high[n] <- high[n-1] + (high[n-1] - low[n-1])
    }

    middle <- (high + low)/2
    names(middle) <- breaks
    middle <- middle[x]
    middle
}


uncut <- function(x, squash_infinite = TRUE) {

    splitted <- strsplit(gsub("[\\(\\[\\)\\]]", "", as.character(x), perl = TRUE), ",")

    low <- vapply(splitted, function(x) as.numeric(x[1]), FUN.VALUE =  numeric(1))
    high <- vapply(splitted, function(x) as.numeric(x[2]), FUN.VALUE =  numeric(1))

    if (squash_infinite) {
        if (low[1] == -Inf) {
            low[1] <- high[1] - (high[2] - low[2])
        }
        n <- length(x)
        if (high[n] == Inf) {
            high[n] <- high[n-1] + (high[n-1] - low[n-1])
        }
    }

    sort(c(low, high))

}


mid_rescaler <- function(mid) {
    function(x, from) {
        scales::rescale_mid(x, to = c(0, 1), from = from, mid)
    }
}



#' @importFrom ggplot2 scale_type
#' @export
scale_type.metR_discretised <- function(x) {
  c("discretised", "ordinal")
}

as.discretised <- function(x) {
  new_x <- get_middle(x)
  if (anyNA(is.na(new_x))) {
    stop('Breaks not formatted correctly for a bin legend. Use `(<lower>, <upper>]` format to indicate bins')
  }

  class(x) <- c("metR_discretised", class(x))
  x
}

#' Discretised scale
#'
#' This scale allows ggplot to understand data that has been discretised with
#' some procedure akin to `cut` and access the underlying continuous values.
#' For a scale that does the opposite (take continuous data and treat them as
#' discrete) see [ggplot2::binned_scale()].
#'
#' This scale makes it very easy to synchronise the breaks of filled contours
#' and the breaks shown no the colour guide. Bear in mind that when using
#' [geom_contour_fill()], the default fill aesthetic (`level_mid`) is **not**
#' discretised. To use this scale with that geom, you need to set
#' `aes(fill = stat(level))`.
#'
#' @examples
#' library(ggplot2)
#'
#' # Using the `level` compute aesthetic from `geom_contour_fill()`
#' # (or ggplot2::geom_contour_filled()), the default scale is discrete.
#' # This means that you cannot map colours to the underying numbers.
#' v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
#' v + geom_contour_fill(aes(fill = stat(level)))
#'
#' v + geom_contour_fill(aes(fill = stat(level))) +
#'   scale_fill_discretised()
#'
#' # The scale can be customised the same as any continuous colour scale
#' v + geom_contour_fill(aes(fill = stat(level))) +
#'   scale_fill_discretised(low = "#a62100", high = "#fff394")
#'
#'
#' v + geom_contour_fill(aes(fill = stat(level))) +
#'   scale_fill_divergent_discretised(midpoint = 0.02)
#'
#'
#' # Existing continous scales can be "retrofitted" by changing the `super`
#' # and `guide` arguments.
#' v + geom_contour_fill(aes(fill = stat(level))) +
#'     scale_fill_distiller(super = ScaleDiscretised, guide = guide_colorsteps())
#'
#' # Unequal breaks will, by default, map to unequal spacing in the guide
#' v + geom_contour_fill(aes(fill = stat(level)), breaks = c(0, 0.005, 0.01, 0.02, 0.04)) +
#'   scale_fill_discretised()
#'
#' # You can change that by the `even.steps` argument on ggplot2::guide_colorsteps()
#' v + geom_contour_fill(aes(fill = stat(level)), breaks = c(0, 0.005, 0.01, 0.02, 0.04)) +
#'   scale_fill_discretised(guide = guide_colorsteps(even.steps = TRUE, show.limits = TRUE))
#'
#'
#' @inheritParams ggplot2::scale_fill_gradient
#' @rdname discretised_scale
#' @export
scale_fill_discretised <- function (..., low = "#132B43", high = "#56B1F7", space = "Lab",
                                 na.value = "grey50", guide = ggplot2::guide_colorsteps(even.steps = FALSE, show.limits = TRUE),
                                 aesthetics = "fill") {

    discretised_scale(aesthetics, "gradient",
                   scales::seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, super = ScaleDiscretised, ...)
}

#' @rdname discretised_scale
#' @inheritParams scale_fill_divergent
#' @export
scale_fill_divergent_discretised <- function(..., low = scales::muted("blue"),
                                          mid = "white",
                                          high = scales::muted("red"),
                                          midpoint = 0,
                                          space = "Lab",
                                          na.value = "grey50",
                                          guide = ggplot2::guide_colorsteps(even.steps = FALSE, show.limits = TRUE)) {

    discretised_scale("fill", "brewer", palette = scales::div_gradient_pal(low, mid, high, space),
                   rescaler = mid_rescaler(mid = midpoint),
                   super = ScaleDiscretised, na.value = na.value, guide = guide, ...)

}

#' @param drop 	Should unused factor levels be omitted from the scale? The default, TRUE, uses the
#' levels that appear in the data; FALSE uses all the levels in the factor.
#' @inheritParams ggplot2::continuous_scale
#' @rdname discretised_scale
#' @export
discretised_scale <- function(aesthetics, scale_name, palette, name = ggplot2::waiver(),
                              breaks = ggplot2::waiver(), labels = ggplot2::waiver(),
                              limits = NULL,
                              trans = scales::identity_trans(),
                              na.value = NA, drop = FALSE,
                              guide = ggplot2::guide_colorsteps(FALSE), position = "left",
                              rescaler = scales::rescale,
                              oob = scales::squish,
                              super = ScaleDiscretised) {
    aesthetics <- ggplot2::standardise_aes_names(aesthetics)

    check_breaks_labels(breaks, labels)

    position <- match.arg(position, c("left", "right", "top", "bottom"))

    # If the scale is non-positional, break = NULL means removing the guide
    if (is.null(breaks) && all(!is_position_aes(aesthetics))) {
        guide <- "none"
    }

    trans <- scales::as.trans(trans)

    ggplot2::ggproto(NULL, super,
                     call = match.call(),

                     aesthetics = aesthetics,
                     scale_name = scale_name,
                     palette = palette,
                     oob = oob,
                     range =  ggplot2::ggproto(NULL, RangeContinuous),
                     limits = limits,
                     na.value = na.value,

                     # expand = c(0, 0),
                     rescaler = rescaler,
                     trans = trans,

                     name = name,
                     breaks = breaks,
                     labels = labels,
                     drop = drop,
                     guide = guide,
                     position = position
    )
}


#' @rdname discretised_scale
#' @format NULL
#' @usage NULL
#' @export
ScaleDiscretised <-  ggplot2::ggproto("ScaleDiscretised", ggplot2::ScaleBinned,
   transform = function(self, x) {
       if (is.numeric(x)) {
           stop("Discretised scales only support discrete data")
       }

       new_x <- get_middle(x)
       new_x <- self$trans$transform(new_x)
       breaks <- unique(uncut(levels(x), squash_infinite = TRUE))
       breaks <- self$trans$transform(breaks)

       self$breaks <- breaks
       self$range$range <- range(breaks)

       limits <- range(self$get_limits())

       limits <- cut(limits[1], c(-Inf, breaks, Inf),  ordered_result = TRUE)

       if (is.numeric(self$get_limits())) {
           self$scale_limits <- range(self$get_limits())
       } else {
           self$scale_limits <- range(get_middle(limits))
       }

       self$limits <- self$scale_limits
       self$breaks <- setdiff(self$breaks, self$limits)

       axis <- if ("x" %in% self$aesthetics) "x" else "y"
       ggplot2:::check_transformation(x, new_x, self$scale_name, axis)
       new_x

   },
   map = function(self, x, limits = self$get_limits()) {
       limits <-  self$scale_limits
       ggplot2::ggproto_parent(ggplot2::ScaleBinned,
                               self)$map(x, limits)
   }
)

# From ggplot2
is_position_aes <- function (vars) {
    aes_to_scale(vars) %in% c("x", "y")
}

aes_to_scale <- function (var) {
    var[var %in% c("x", "xmin", "xmax", "xend", "xintercept")] <- "x"
    var[var %in% c("y", "ymin", "ymax", "yend", "yintercept")] <- "y"
    var
}


check_breaks_labels <- function (breaks, labels) {
    if (is.null(breaks)) {
        return(TRUE)
    }
    if (is.null(labels)) {
        return(TRUE)
    }
    bad_labels <- is.atomic(breaks) && is.atomic(labels) &&
        length(breaks) != length(labels)
    if (bad_labels) {
        stop("`breaks` and `labels` must have the same length")
    }
    TRUE
}


Range <- ggplot2::ggproto("Range", NULL,
   range = NULL,
   reset = function(self) {
       self$range <- NULL
       }
)

RangeContinuous <- ggplot2::ggproto("RangeContinuous", Range,
                           train = function(self, x) {
                             self$range <- scales::train_continuous(x, self$range)
                           }
)


