streamline_ode <- function(
  field,
  starting_points,
  typical_length = 1,
  resolution = 1
) {
  M <- mean(sqrt(field$dx^2 + field$dy^2), na.rm = TRUE)
  n_time <- M * typical_length
  # browser()
  times <- seq(0, n_time, length.out = 10 * resolution)

  field <- field[, c("dx", "dy") := Impute2D(dx + dy ~ x + y)]

  dx <- with(
    .tidy2matrix(field, x ~ y, value.var = "dx", fill = 0),
    list(
      x = rowdims$x,
      y = coldims$y,
      z = matrix
    )
  )

  dy <- with(
    .tidy2matrix(field, x ~ y, value.var = "dy", fill = 0),
    list(
      x = rowdims$x,
      y = coldims$y,
      z = matrix
    )
  )

  force.fun <- function(x, y) {
    X <- cbind(x, y)
    dx <- interpolate_locations(dx, X)
    dy <- interpolate_locations(dy, X)
    return(list(dx = dx, dy = dy))
  }

  # Create initial state: flatten all (x, y, z) into one vector
  starting_points_xyz <- with(
    latlon_to_xyz(starting_points$lat, starting_points$lon),
    c(x, y, z)
  )
  n_streamlines <- length(starting_points$lat)

  solution <- deSolve::ode(
    y = starting_points_xyz,
    times = times,
    parms = NULL,
    func = vfield_sphere,
    force.fun = force.fun,
    method = "rk4"
  )
  streamline_id <- seq(1, n_streamlines)

  colnames(solution) <- c(
    "step",
    paste0("x_", streamline_id),
    paste0("y_", streamline_id),
    paste0("z_", streamline_id)
  )

  solution <- data.table::melt(
    data.table::as.data.table(solution),
    id.vars = "step"
  )
  solution[,
    c("coord", "streamline_id") := data.table::tstrsplit(variable, split = "_")
  ]
  solution[, let(variable = NULL)]

  solution <- data.table::dcast(
    solution,
    step + streamline_id ~ coord,
    value.var = "value"
  )
  solution[, c("y", "x") := xyz_to_latlon(x, y, z)]
  solution[, z := NULL][]
  solution <- add_segment_ids(solution)
  return(solution)
}

#' Convert lon lat coordinated to the standardised range
#'
#' Takes points in lon lat that might have values outside the
#' normal range (e.g. lat < -90 or lon > 360) and moves those
#' points to the corresponding standard range.
#'
#' @param lonlat list with elements x and y that are lon and lat in degrees
#' @param lon_range range of the longitude. Either 360 or 180.
#' Any other value that is not 360 is interpreted as 180
#'
#' @returns list with elements lon and lat
fold_sphere <- function(lonlat, lon_range = 360) {
  lonlat <- latlon_to_xyz(lonlat) |>
    xyz_to_latlon()

  if (lon_range == 360) {
    lonlat$x <- ConvertLongitude(lonlat$x, from = 180)
  }
  return(lonlat)
}

fold_cylinder <- function(lon, lon_range = 360) {
  if (lon_range == 360) {
    lon <- lon %% 360
  } else {
    lon <- ((lon + 180) %% 360) - 180
  }
  return(lon)
}

#' Correct delta values for folding on a sphere
#'
#' When streamlines cross the poles, the direction of latitude changes.
#' This function adjusts the delta values accordingly.
#'
#' @param dy list with element dy (delta latitude)
#' @param y current latitude values
#'
#' @returns adjusted delta list
fold_sphere_delta <- function(dy, y) {
  parity <- (-1)^(floor(abs(y) / 90))

  dy <- dy * parity
  return(dy)
}

deltalonlat_to_deltaxyz <- function(delta, lonlat) {
  dlon_dt <- delta$dx
  dlat_dt <- delta$dy

  dlat_dt[is.na(dlat_dt)] <- 0
  dlon_dt[is.na(dlon_dt)] <- 0

  lat_rad <- lonlat$lat * pi / 180
  lon_rad <- lonlat$lon * pi / 180

  dx <- -sin(lat_rad) * cos(lon_rad) * dlat_dt - sin(lon_rad) * dlon_dt
  dy <- -sin(lat_rad) * sin(lon_rad) * dlat_dt + cos(lon_rad) * dlon_dt
  dz <- cos(lat_rad) * dlat_dt
  return(list(dx = dx, dy = dy, dz = dz))
}


#' Convert latlon to xyz
#'
#' @param lonlat list with elements x and y that are lon and lat in degrees
#'
#' @returns a list with elements  x, y z
latlon_to_xyz <- function(lonlat) {
  lat_rad <- lonlat$y * pi / 180
  lon_rad <- lonlat$x * pi / 180
  list(
    x = cos(lat_rad) * cos(lon_rad),
    y = cos(lat_rad) * sin(lon_rad),
    z = sin(lat_rad)
  )
}

#' Convert xyz to latlon
#'
#' @param x,y,z coordinates on the sphere
#'
#' @returns a list with elements x and y that are lon and lat in degrees
xyz_to_latlon <- function(xyz) {
  lat <- asin(xyz$z) * 180 / pi
  lon <- atan2(xyz$y, xyz$x) * 180 / pi
  list(x = lon, y = lat)
}


#' Function to integrate streamlines on a sphere.
#'
#' @param t time; not used, needed for compatibility with [deSolve::ode]
#' @param params parameters; not used, needed for compatibility with [deSolve::ode]
#' @param state positions of the streamlines.
#' Multiple streamlines are "packed" in this vector by concatenating each
#' streamline's x, y, z coordinates (eg. x1, y1, z1, x2, y2, z2, ..., xn, yn, zn).
#' @param force.fun a function that computes the forcing. It takes two arguments
#' that are the x and y (in that order) and returns a list with elements
#' dx and dy
#'
#' @returns A list with one element, which is the derivative of each streamline.
#'
vfield_sphere <- function(t, state, params = NULL, force.fun) {
  n_streamlines <- length(state) / 3

  x <- state[seq(1, n_streamlines)]
  y <- state[seq(n_streamlines + 1, 2 * n_streamlines)]
  z <- state[seq(2 * n_streamlines + 1, 3 * n_streamlines)]

  latlon <- xyz_to_latlon(x, y, z)
  # browser(expr = anyNA(latlon$lon))
  delta <- force.fun(latlon$lon, latlon$lat)
  dlon_dt <- delta$dx
  dlat_dt <- delta$dy

  dlat_dt[is.na(dlat_dt)] <- 0
  dlon_dt[is.na(dlon_dt)] <- 0

  lat_rad <- latlon$lat * pi / 180
  lon_rad <- latlon$lon * pi / 180

  dx <- -sin(lat_rad) * cos(lon_rad) * dlat_dt - sin(lon_rad) * dlon_dt
  dy <- -sin(lat_rad) * sin(lon_rad) * dlat_dt + cos(lon_rad) * dlon_dt
  dz <- cos(lat_rad) * dlat_dt

  list(c(dx, dy, dz))
}


# Function to add segment IDs based on boundary crossings
add_segment_ids <- function(trajectory_dt) {
  trajectory_dt <- trajectory_dt[order(streamline_id, step)]

  # Calculate lon differences
  trajectory_dt[, lon_diff := c(NA, diff(x)), by = streamline_id]

  # Detect jumps (crossing ±180° boundary)
  # A jump is when |lon_diff| > some threshold (e.g., 180°)
  trajectory_dt[, crossed := abs(lon_diff) > 180]
  trajectory_dt[is.na(crossed), crossed := FALSE]

  # Create segment ID: increment whenever we cross a boundary
  trajectory_dt[, segment_id := cumsum(crossed), by = streamline_id]

  # Create unique ID for plotting
  trajectory_dt[,
    streamline_id_plot := paste(streamline_id, segment_id, sep = "_seg_")
  ]

  # Remove the temporary columns if you want
  trajectory_dt[, c("lon_diff", "crossed", "segment_id") := NULL]

  return(trajectory_dt)
}
