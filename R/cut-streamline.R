fold_streamline <- function(x, y, x_range = c(0, 360), y_range = c(-90, 90)) {
  # I couldn't crack this nut and gave up. ChatGPT came up with this nonsense.
  # It works but it's a horrible mess. I'm positive that it can be made more tidy.
  # I also need to understand how it works before shipping it to anyone and probably
  # rewrite it myself in my own style.
  if (length(x) != length(y)) {
    stop("x and y must have same length")
  }
  xmin <- x_range[1]
  xmax <- x_range[2]
  xspan <- xmax - xmin
  ymin <- y_range[1]
  ymax <- y_range[2]
  yspan <- ymax - ymin
  half_xspan <- xspan / 2

  # Improved wrapper: keeps xmax (e.g. 360) when an input is an exact positive multiple
  wrap_x <- function(xx) {
    res <- ((xx - xmin) %% xspan) + xmin
    tol <- .Machine$double.eps * pmax(1, abs(xx)) * 100
    # where result equals xmin but original was strictly > xmin (within tol) -> use xmax
    fix_idx <- abs(res - xmin) < tol & (xx - xmin) > tol
    if (any(fix_idx)) {
      res[fix_idx] <- xmax
    }
    res
  }

  reflect_into_range <- function(yy) {
    nref <- 0L
    yv <- yy
    # reflect iteratively
    while (any(yv > ymax)) {
      idx <- yv > ymax
      yv[idx] <- ymax - (yv[idx] - ymax)
      nref <- nref + 1L
    }
    while (any(yv < ymin)) {
      idx <- yv < ymin
      yv[idx] <- ymin - (yv[idx] - ymin)
      nref <- nref + 1L
    }
    list(y = yv, nref = nref)
  }

  out_x <- numeric(0)
  out_y <- numeric(0)
  out_piece <- integer(0)
  piece <- 1L
  x_shift <- 0

  append_point <- function(px, py, pr) {
    out_x <<- c(out_x, px)
    out_y <<- c(out_y, py)
    out_piece <<- c(out_piece, pr)
  }

  seg_events <- function(x1, y1, x2, y2) {
    evs <- list()
    n1 <- floor((x1 - xmin) / xspan)
    n2 <- floor((x2 - xmin) / xspan)
    if (n1 != n2) {
      ks <- seq(min(n1, n2) + 1, max(n1, n2))
      for (k in ks) {
        b <- xmin + k * xspan
        if ((b >= min(x1, x2)) && (b <= max(x1, x2))) {
          t <- (b - x1) / (x2 - x1)
          if (t > 0 && t < 1) {
            evs[[length(evs) + 1]] <- list(t = t, type = "x", boundary = b)
          }
        }
      }
    }
    m1 <- floor((y1 - ymin) / yspan)
    m2 <- floor((y2 - ymin) / yspan)
    if (m1 != m2) {
      ks <- seq(min(m1, m2) + 1, max(m1, m2))
      for (k in ks) {
        b <- ymin + k * yspan
        if ((b >= min(y1, y2)) && (b <= max(y1, y2))) {
          t <- (b - y1) / (y2 - y1)
          if (t > 0 && t < 1) {
            evs[[length(evs) + 1]] <- list(t = t, type = "y", boundary = b)
          }
        }
      }
    }
    if (length(evs) == 0) {
      return(NULL)
    }
    evs[order(vapply(evs, `[[`, numeric(1), "t"))]
  }

  n <- length(x)
  if (n == 0) {
    return(list(x = numeric(0), y = numeric(0), piece = integer(0)))
  }
  # start
  append_point(wrap_x(x[1] + x_shift), reflect_into_range(y[1])$y, piece)

  i <- 1L
  while (i < n) {
    x1 <- x[i]
    y1 <- y[i]
    x2 <- x[i + 1]
    y2 <- y[i + 1]
    events <- seg_events(x1, y1, x2, y2)
    if (is.null(events)) {
      r <- reflect_into_range(y2)
      append_point(wrap_x(x2 + x_shift), r$y, piece)
    } else {
      # process events in order, track local x_shift
      cur_x_shift <- x_shift
      for (ev in events) {
        t <- ev$t
        xe <- x1 + t * (x2 - x1)
        ye <- y1 + t * (y2 - y1)
        if (ev$type == "x") {
          d <- x2 - x1
          boundary_prev <- if (d > 0) xmax else xmin
          boundary_next <- if (d > 0) xmin else xmax
          r_prev <- reflect_into_range(ye)
          # append previous-piece end at explicit boundary_prev (do NOT wrap it into xmin)
          append_point(boundary_prev, r_prev$y, piece)
          piece <- piece + 1L
          append_point(boundary_next, r_prev$y, piece)
          # no change to x_shift
          x_shift <- cur_x_shift
        } else if (ev$type == "y") {
          boundary_val <- ev$boundary
          xe_prev <- wrap_x(xe + cur_x_shift)
          append_point(xe_prev, boundary_val, piece)
          cur_x_shift <- cur_x_shift + half_xspan
          piece <- piece + 1L
          xe_next <- wrap_x(xe + cur_x_shift)
          append_point(xe_next, boundary_val, piece)
          x_shift <- cur_x_shift
        }
      }
      r_end <- reflect_into_range(y2)
      append_point(wrap_x(x2 + x_shift), r_end$y, piece)
    }
    i <- i + 1L
  }

  # final normalisation: apply wrap_x but preserve explicit xmax already set
  # (wrap_x is safe for vectors)
  out_x <- wrap_x(out_x)
  list(x = out_x, y = out_y, piece = out_piece)
}
