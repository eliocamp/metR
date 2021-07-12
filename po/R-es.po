#' @section Overview:
#' Conceptually it's divided into *visualization tools* and *data tools*.
#' The former are geoms, stats and scales that help with plotting using
#' [ggplot2], such as [stat_contour_fill] or [scale_y_level], while the
#' later are functions for common data processing tools in the atmospheric
#' sciences, such as [Derivate] or [EOF]; these are implemented to work in the
#' [data.table] paradigm, but also work with regular data frames.
#'
#' To get started, check the vignettes:
#' * Visualization Tools: `vignette("Visualization-tools", package = "metR")`
#' * Working with Data: `vignette("Working-with-data", package = "metR")`
#'
#' @name metR
#' @docType package
"_PACKAGE"
