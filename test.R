library(data.table)
library(ggplot2)
library(deSolve)
data(geopotential)

geopotential <- copy(geopotential)[date == date[1]]
geopotential[, gh.z := Anomaly(gh), by = .(lat)]
geopotential[, c("u", "v") := GeostrophicWind(gh.z, lon, lat)]

field <- geopotential[, .(x = lon, y = lat, dx = dlon(u, lat), dy = dlat(v))] |>
  _[, x := ConvertLongitude(x)]


# Create a grid of starting points
lon_grid <- seq(-180, 180, length.out = 5)
lat_grid <- seq(-90, -22, length.out = 5)
start_points <- CJ(lon = lon_grid, lat = lat_grid)
start_points[, let(
  lon = lon + rnorm(.N, sd = 5),
  lat = lat + rnorm(.N, sd = 5)
)]


data.table::setorder(field, x, y)

field <- field[!is.na(dx) & !is.na(dy)]


matrix <- .tidy2matrix(field, x ~ y, value.var = "dx", fill = 0)
dx.field <- list(
  x = matrix$rowdims$x,
  y = matrix$coldims$y,
  z = matrix$matrix
)

matrix <- .tidy2matrix(field, x ~ y, value.var = "dy", fill = 0)
dy.field <- list(
  x = matrix$rowdims$x,
  y = matrix$coldims$y,
  z = matrix$matrix
)

force.fun <- function(X) {
  dx <- interpolate_locations(dx.field, X)
  dy <- interpolate_locations(dy.field, X)
  return(cbind(dx = dx, dy = dy))
}

latlon_to_xyz <- function(lat, lon) {
  lat_rad <- lat * pi / 180
  lon_rad <- lon * pi / 180
  c(
    x = cos(lat_rad) * cos(lon_rad),
    y = cos(lat_rad) * sin(lon_rad),
    z = sin(lat_rad)
  )
}

xyz_to_latlon <- function(xyz) {
  x <- xyz[1]
  y <- xyz[2]
  z <- xyz[3]
  lat <- asin(z) * 180 / pi
  lon <- atan2(y, x) * 180 / pi
  c(lat = lat, lon = lon)
}

# Store metadata about streamlines
n_streamlines <- nrow(start_points)
streamline_ids <- paste(start_points$lat, start_points$lon, sep = "_")

# Create initial state: flatten all (x, y, z) into one vector
initial_state <- numeric(n_streamlines * 3)
for (i in 1:n_streamlines) {
  xyz <- latlon_to_xyz(start_points$lat[i], start_points$lon[i])
  idx <- (i - 1) * 3 + 1:3
  initial_state[idx] <- xyz
}


# Vectorized ODE function
vfield_3d_vectorized <- function(t, state, params) {
  n <- length(state) / 3 # number of streamlines

  # Extract xyz for all streamlines
  x <- JumpBy(state, by = n, start = 1)
  y <- JumpBy(state, by = n, start = 2)
  z <- JumpBy(state, by = n, start = 3)

  # Convert to lat/lon
  lat <- asin(z) * 180 / pi
  lon <- atan2(y, x) * 180 / pi

  # Get dlat/dt and dlon/dt from your field
  fun <- force.fun(cbind(lon, lat))
  dlat_dt <- fun[, 2]
  dlon_dt <- fun[, 1]

  # Handle NAs
  dlat_dt[is.na(dlat_dt)] <- 0
  dlon_dt[is.na(dlon_dt)] <- 0

  # Convert to 3D tangent vectors (vectorized)
  lat_rad <- lat * pi / 180
  lon_rad <- lon * pi / 180

  dx <- -sin(lat_rad) * cos(lon_rad) * dlat_dt - sin(lon_rad) * dlon_dt
  dy <- -sin(lat_rad) * sin(lon_rad) * dlat_dt + cos(lon_rad) * dlon_dt
  dz <- cos(lat_rad) * dlat_dt

  # Interleave back into state vector format
  dstate <- numeric(3 * n)
  dstate[seq(1, 3 * n, 3)] <- dx
  dstate[seq(2, 3 * n, 3)] <- dy
  dstate[seq(3, 3 * n, 3)] <- dz

  list(dstate)
}

# Integrate all streamlines at once
times <- seq(0, 1000000, length.out = 1000)
trajectory_3d <- ode(
  y = initial_state,
  times = times,
  func = vfield_3d_vectorized,
  parms = NULL,
  method = "rk4"
)

# Convert back to lat/lon and reshape into data.table
result_list <- list()

for (i in 1:n_streamlines) {
  idx <- (i - 1) * 3 + 1:3
  xyz_traj <- trajectory_3d[, idx + 1] # +1 because first column is time

  # Normalize and convert
  xyz_norm <- t(apply(xyz_traj, 1, function(xyz) xyz / sqrt(sum(xyz^2))))

  latlon <- t(apply(xyz_norm, 1, xyz_to_latlon))

  result_list[[i]] <- data.table(
    time = trajectory_3d[, 1],
    lat = latlon[, 1],
    lon = latlon[, 2],
    streamline_id = streamline_ids[i]
  )
}

all_trajectories <- rbindlist(result_list)

# Then add segment IDs
all_trajectories <- add_segment_ids(all_trajectories)
#
# # Plot
# all_trajectories |>
#     na.omit() |>
#     ggplot(aes(x = lon, y = lat, group = plot_id)) +
#     geom_path(alpha = 0.7) +
#     coord_fixed() +
#     theme_minimal()
