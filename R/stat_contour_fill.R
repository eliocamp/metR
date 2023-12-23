#' @rdname geom_contour_fill
#' @export
stat_contour_fill <- function(mapping = NULL, data = NULL,
                              geom = "polygon", position = "identity",
                              ...,
                              breaks = MakeBreaks(),
                              bins = NULL,
                              binwidth = NULL,
                              global.breaks = TRUE,
                              proj = NULL,
                              clip = NULL,
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
            proj = proj,
            clip = clip,
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
    default_aes = ggplot2::aes(fill = ggplot2::after_stat(level_mid), order = ggplot2::after_stat(level)),
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
                             proj = NULL, kriging = FALSE, clip = NULL) {
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
                    data <- .complete(data, x, y)
                }
            }

            data <- .impute_data(data, na.fill)
        } else {
            data <- data[!is.na(z), ]
        }

        if (!isFALSE(kriging)) {
            check_packages("kriging", "kriging")

            if (isTRUE(kriging)) {
                kriging <- 40
            }
            data <- try(with(data, setNames(kriging::kriging(x, y, z, pixels = kriging)$map,
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
        dec <- getOption("OutDec")
        options(OutDec = ".")
        on.exit(options(OutDec = dec))
        cont <- data.table::setDT(.contour_bands(data, breaks, complete = complete, clip = clip, proj = proj))

        cont[, int.level := (level_high + level_low)/2]
        cont[, level_mid := int.level]
        cont[, nlevel := level_high/max(level_high)]


        cont
        }
)


.contour_bands <- function(data, breaks, complete = FALSE, proj = NULL, clip = NULL) {
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

    if (!is.null(proj)) {
        cl_class <- class(cl)
        if (is.function(proj)) {
            cl <- proj(cl)
        } else {
            if (is.character(proj)) {
                if (!requireNamespace("proj4", quietly = TRUE)) {
                    stopf("Projection requires the proj4 package. Install it with 'install.packages(\"proj4\")'.")
                }
                cl <- lapply(cl, function(x) {
                    x[c("x", "y")] <- proj4::project(list(x$x, x$y), proj, inverse = TRUE)
                    return(x)
                })
            }
        }
        class(cl) <- cl_class
    }

    if (!is.null(clip)) {
        s2 <- suppressMessages(sf::sf_use_s2(FALSE))
        on.exit(suppressMessages(sf::sf_use_s2(s2)))
        clip <- suppressMessages(sf::st_union(sf::st_make_valid(clip)))

        if (!is.na(sf::st_crs(clip))) {
            sf::st_crs(clip) <- NA
        }

        cl <- clip_iso(cl, clip, "POLYGON")
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


clip_contours <- function(x, y, clip, type = "POLYGON") {

    if (type == "POLYGON" & length(x) < 4) {
        return(NULL)
    }

    xy <- sf::st_linestring(x = matrix(c(x, y), ncol = 2))
    xy <- sf::st_cast(xy, type)
    xy <- sf::st_make_valid(xy)
    xy <- sf::st_intersection(xy, clip)

    if (length(xy) == 0) {
        return(NULL)
    }

    # browser(expr = inherits(xy, "GEOMETRYCOLLECTION"))

    if (inherits(xy, "GEOMETRYCOLLECTION"))  {
        xy <- sf::st_collection_extract(xy, type)
    }

    xy <- sf::st_coordinates(xy)

    # Annoying st_coordinates that returns variable columns!!!
    if (ncol(xy) > 2) {
        L <- do.call(interaction, lapply(seq(3, ncol(xy)), function(i) xy[, i]))
    } else {
        L <- factor("1")
    }

    list(x = xy[, 1],
         y = xy[, 2],
         L = L)
}



get_sf_coords <- function(x, type = "POLYGON")  {
    if (inherits(x, "GEOMETRYCOLLECTION"))  {
        x <- sf::st_collection_extract(x, type)
    }

    x <- sf::st_coordinates(x)

    # Annoying st_coordinates that returns variable columns!!!
    if (ncol(x) > 2) {
        L <- do.call(interaction, lapply(seq(3, ncol(x)), function(i) x[, i]))
    } else {
        L <- factor("1")
    }

    list(x = x[, 1],
         y = x[, 2],
         id = L)
}


clip_iso  <- function(iso, clip, type = "POLYGON") {
    iso <- isoband::iso_to_sfg(iso)
    iso <- lapply(iso, function(x) {
        # stopifnot(!sf::st_is_valid(x))

        result <- sf::st_intersection(sf::st_make_valid(x), clip)
        if (sf::st_is_empty(result)) {
            return(NULL)
        }
        get_sf_coords(result, type)
    })
    iso <- iso[!vapply(iso, is.null, TRUE)]

    class(iso) <- c("isobands", "iso")
    return(iso)
}

