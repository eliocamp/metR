# # Switch between longitude conventions
# #
# # Transform longitude coordinates from and to [0, 360) and [-180, 180). Similar
# # to [ConverLongitude] but suitable for using in [ggplot2::scale_x_continuous] or,
# # better still, [scale_x_longitude].
# #
# # @param to convention to transform to.
# #
# # @examples
# # library(ggplot2)
# #
# # (g <- ggplot(geopotential[date == date[1]], aes(lon, lat)) +
# #     geom_contour2(aes(z = gh)))
# #
# # g + scale_x_longitude(trans = lon_trans(180))
# #
# # # Same as
# # g + scale_x_longitude(trans = "lon180")
# #
# # # @export
# # @importFrom scales trans_new
# lon_trans <- function(to = c(180, 360)) {
#     if (to[1] == 180) {
#         scales::trans_new("lon180",
#                           lon180,
#                           lon360)
#     } else if (to[1] == 360) {
#         scales::trans_new("lon360",
#                           lon360,
#                           lon180)
#     } else {
#         stopf("From must be 360 or 180.")
#     }
# }
#
# # #' @export
# # #' @rdname lon_trans
# lon180_trans <- function() {
#     lon_trans(180)
# }
#
# # #' @export
# # #' @rdname lon_trans
# lon360_trans <- function() {
#     lon_trans(360)
# }
#
# # #' @export
# # #' @rdname lon_trans
# lon180 <- function(lon) {
#     lower <- lon[is.finite(lon)] == -180
#     lon[!lower] <- lon[!lower] %% 360
#     trans <- lon > 180 & is.finite(lon)
#     lon[trans] <- lon[trans] - 360
#     lon
# }
#
# # #' @export
# # #' @rdname lon_trans
# lon360 <- function(lon) {
#     upper <- lon[is.finite(lon)] == 360
#     lon[!upper] <- (lon[!upper] + 180) %% 360 - 180
#     trans <- lon < 0 & is.finite(lon)
#     lon[trans] <- lon[trans] + 360
#     lon
# }
#
