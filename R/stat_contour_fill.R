#' @rdname geom_contour_fill
#' @export
stat_contour_fill <- function(mapping = NULL, data = NULL,
                              geom = "polygon", position = "identity",
                              ...,
                              breaks = MakeBreaks(),
                              bins = NULL,
                              binwidth = NULL,
                              global.breaks = TRUE,
                              kriging = FALSE,
                              na.fill = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
    .check_wrap_param(list(...))
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = StatContourFill,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = FALSE,
            na.fill = na.fill,
            breaks = breaks,
            bins = bins,
            binwidth = binwidth,
            global.breaks = global.breaks,
            kriging = kriging,
            ...
        )
    )
}

#' @rdname geom_contour_fill
#' @usage NULL
#' @format NULL
#' @export
StatContourFill <- ggplot2::ggproto("StatContourFill", ggplot2::Stat,
    required_aes = c("x", "y", "z"),
    default_aes = ggplot2::aes(fill = ..level_mid.., order = ..level..),
    dropped_aes = "z",
    setup_params = function(data, params) {
        if (is.null(params$global) || isTRUE(params$global.breaks)) {
            params$breaks <- setup_breaks(data,
                                          breaks = params$breaks,
                                          bins = params$bins,
                                          binwidth = params$binwidth)
        }
        return(params)
    },
    compute_layer = function(self, data, params, layout) {
        ggplot2:::check_required_aesthetics(
            self$required_aes,
            c(names(data), names(params)),
            ggplot2:::snake_class(self)
            )

        # Trim off extra parameters
        params <- params[intersect(names(params), self$parameters())]

        args <- c(list(data = quote(data), scales = quote(scales)), params)
        data <- plyr::ddply(data, "PANEL", function(data) {
            scales <- layout$get_scales(data$PANEL[1])
            tryCatch(do.call(self$compute_panel, args), error = function(e) {
                warningf("Computation failed in `%s()`:\n %s",
                                 ggplot2:::snake_class(self), e$message,
                        call. = FALSE)
                data.frame()
            })
        })

        if (nrow(data) > 0) {
            data$level_d <- data$level
            class(data$level_d) <- c("metR_discretised", class(data$level_d))
        }

        data
    },
    compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                             breaks = scales::fullseq, complete = TRUE,
                             na.rm = FALSE, xwrap = NULL,
                             ywrap = NULL, na.fill = FALSE, global.breaks = TRUE,
                             proj = NULL, kriging = FALSE) {
        data.table::setDT(data)

        if (isFALSE(global.breaks)) {
            breaks <- setup_breaks(data,
                                   breaks = breaks,
                                   bins = bins,
                                   binwidth = binwidth)
        }

        data <- data[!(is.na(y) | is.na(x)), ]

        if (!isFALSE(na.fill)) {
            # Check if is a complete grid
            complete.grid <- with(data, .is.regular_grid(x, y))
            if (complete.grid == FALSE) {
                if (complete == FALSE) {
                    warningf("The data must be a complete regular grid.", call. = FALSE)
                    return(data.frame())
                } else {
                    # data <- data.table::setDT(tidyr::complete(data, x, y, fill = list(z = NA)))
                    data <- .complete(data, x, y)
                }
            }

            data <- .impute_data(data, na.fill)
        } else {
            data <- data[!is.na(z), ]
        }

        if (kriging) {
            check_packages("kriging", "kriging")

            pixels <- 40
            data <- try(with(data, setNames(kriging::kriging(x, y, z, pixels = pixels)$map,
                                            c("x", "y", "z"))), silent = TRUE)
            if (inherits(data, "try-error")) {
                warningf("kriging failed. Perhaps the number of points is too small.")
                return(data.frame())
            }

            data.table::setDT(data)
        }

        if (!is.null(xwrap)) {
            data <- suppressWarnings(WrapCircular(data, "x", xwrap))
        }
        if (!is.null(ywrap)) {
            data <- suppressWarnings(WrapCircular(data, "y", ywrap))
        }


        # Make contours
        cont <- data.table::setDT(.contour_bands(data, breaks, complete = complete))

        cont[, int.level := (level_high + level_low)/2]
        cont[, level_mid := int.level]
        cont[, nlevel := level_high/max(level_high)]

        if (!is.null(proj)) {
            if (is.function(proj)) {
                cont <- proj(cont)
            } else {
                if (is.character(proj)) {
                    if (!requireNamespace("proj4", quietly = TRUE)) {
                        stopf("Projection requires the proj4 package. Install it with 'install.packages(\"proj4\")'.")
                    }
                    cont <- data.table::copy(cont)[, c("x", "y") := proj4::project(list(x, y), proj,
                                                                                           inverse = TRUE)][]

                }
            }
        }


        cont

        }
)


.contour_bands <- function(data, breaks, complete = FALSE) {
    band <- level_high <- level_low <- NULL

    # From ggplot2
    x_pos <- as.integer(factor(data$x, levels = sort(unique(data$x))))
    y_pos <- as.integer(factor(data$y, levels = sort(unique(data$y))))

    nrow <- max(y_pos)
    ncol <- max(x_pos)

    z <- matrix(NA_real_, nrow = nrow, ncol = ncol)
    z[cbind(y_pos, x_pos)] <- data$z



    cl <- isoband::isobands(x = sort(unique(data$x)),
                            y = sort(unique(data$y)),
                            z = z,
                            levels_low = breaks[-length(breaks)],
                            levels_high = breaks[-1])


    if (length(cl) == 0) {
        warningf("Not possible to generate contour data.", call. = FALSE)
        return(data.frame())
    }

    # Convert list of lists into single data frame

    bands <- pretty_isoband_levels(names(cl))
    cont <- data.table::rbindlist(lapply(cl, data.table::as.data.table), idcol = "band")


    cont[, c("level_low", "level_high") := data.table::tstrsplit(band, ":")]
    cont[, `:=`(level_low = as.numeric(level_low), level_high = as.numeric(level_high))]

    cont[, level := ordered(pretty_isoband_levels(band), bands)]
    # cont[, level := factor(level, ordered = TRUE)]

    cont[, piece := as.numeric(interaction(band))]
    cont[, group := factor(paste(data$group[1], sprintf("%03d", piece), sep = "-"))]


    cont[, .(level = level, level_low, level_high, x, y, piece, group, subgroup = id)]

}

#  from ggplot2
pretty_isoband_levels <- function(isoband_levels, dig.lab = 3) {
    interval_low <- gsub(":.*$", "", isoband_levels)
    interval_high <- gsub("^[^:]*:", "", isoband_levels)

    label_low <- format(as.numeric(interval_low), digits = dig.lab, trim = TRUE)
    label_high <- format(as.numeric(interval_high), digits = dig.lab, trim = TRUE)

    # from the isoband::isobands() docs:
    # the intervals specifying isobands are closed at their lower boundary
    # and open at their upper boundary
    sprintf("(%s, %s]", label_low, label_high)
}
