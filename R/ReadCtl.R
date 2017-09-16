# http://cola.gmu.edu/grads/gadoc/descriptorfile.html
library(lubridate)
ReadCtl <- function(descriptor, hd.rm = T) {
    components <- .ParseCTL(readLines(descriptor))

    # DSET can start with "^" (grib files are on the same dir as descriptor) or
    # can contain a full path.
    if (substr(components$DSET, 1, 1) == "^") {
        pattern <- substr(components$DSET, 2, nchar(components$DSET))
        dir <- dirname(descriptor)
    } else {
        pattern <- dirname(components$DSET)
        file <- basename(components$DSET)
    }
    components$DSET <- list(pattern = pattern, dir = dir)

    # Spatiotemporal rids.
    x.grid <- .GetSpatialGrid(components$XDEF)
    y.grid <- .GetSpatialGrid(components$YDEF)
    z.grid <- .GetSpatialGrid(components$ZDEF)
    t.grid <- .GetTimeGrid(components$TDEF)

    # --------
    # Build files from template and t.grid
    # test with ^attm106_%y4.grd

    files <- .FilesFromTemplate(t.grid, components$DSET$pattern)
    unique.files <- unique(files)
    # Variables
    vars <- sapply(components$VARS, function(x) x[[1]])
    varlevs <- vars[1:9]

    field.levs <- vector()
    for (f in seq_along(unique.files)) {
        print(f)
        path <- file.path(components$DSET$dir, files[f])
        number_lines <- file.info(path)$size/4

        file <- file(path, "rb")
        field <- readBin(file, "double", n  = number_lines*4, size = 4, endian = "big")
        close(file)
        field <- field[field != field[1]]

        t.grid.file <- t.grid[files %in% unique.files[f]]
        size <- length(x.grid)*length(y.grid)*length(z.grid)*length(varlevs)*length(t.grid.file)
        field.levs <- c(field.levs, field[1:size])
    }
    field.levs2 <- array(field.levs,
                         dim = c(length(x.grid), length(y.grid), length(z.grid),
                                 length(varlevs), length(t.grid)),
                         dimnames = list(x = x.grid, y = y.grid, z = z.grid,
                                         var = varlevs, t = t.grid)
                         )

    dimnames(field.levs) <- list(lon = x.grid, lat = y.grid,
                                 lev = z.grid, var = vars[1:9],
                                 date = as.character(t.grid[1:24]))

    # Remove bullsh*t headers
    if (hd.rm == T) {
        fields <- fields[fields != fields[1]]

        return(fields)
    } else {
        return(fields)
    }

}

.ctlcomp <- c("DSET", "CHSUB", "DTYPE", "INDEX", "STNMAP", "TITLE", "UNDEF",
              "UNPACK ", "FILEHEADER", "XYHEADER", "XYTRAILER", "THEADER",
              "HEADERBYTES", "TRAILERBYTES", "XVAR", "YVAR", "ZVAR", "STID",
              "TVAR ", "TOFFVAR", "CACHESIZE", "OPTIONS", "PDEF ", "XDEF",
              "YDEF", "ZDEF", "TDEF", "EDEF", "VECTORPAIRS", "VARS", "ENDVARS",
              "ATTRIBUTE METADATA ", "COMMENTS")
.listcomp <- c("VARS", "EDEF")
.endcomp <- paste0("END", .listcomp)

.ParseCTL <- function(lines) {
    lines.parsed <- list()
    for (i in seq_along(lines)) {
        l <- strsplit(lines[i], " ")[[1]]
        l <- l[nchar(l) > 0]
        comp <- l[1]

        if ((comp %in% .ctlcomp)) {
            if (comp %in% .listcomp) {
                lines.parsed[[comp]] <- list()
            } else if (comp %in% .endcomp) {
            } else {
                lines.parsed[[comp]] <- l[-1]
            }
        } else {
            N <- length(lines.parsed)
            if (is.list(lines.parsed[[N]])) {
                lines.parsed[[N]] <- c(lines.parsed[[N]], list(l))
            } else {
                lines.parsed[[N]] <- c(lines.parsed[[N]], l)
            }
        }
    }
    return(lines.parsed)
}

.GetSpatialGrid <- function(def) {
    # Number of grid points.
    num <- as.numeric(def[1])

    # Linear or levels.
    linear <- grep("LINEAR", def, ignore.case = TRUE)
    levels <- grep("LEVELS", def, ignore.case = TRUE)
    if (length(linear) != 0) {
        start <- as.numeric(def[linear + 1])
        increment <- as.numeric(def[linear + 2])
        grid <- seq(start, by = increment, length.out = num)
    } else if (length(levels) != 0) {
        grid <- as.numeric(def[-c(1, 2)])
    }

    return(grid)
}

.GetTimeGrid <- function(tdef) {
    nt <- tdef[1]
    start <- tdef[3]
    increment <- tdef[4]

    start <- tolower(start)
    start <- strsplit(start, "z")[[1]]
    month.abb.l <- tolower(month.abb)

    if (length(start) > 1) {
        time <- start[1]
        date <- start[2]
    } else {
        time <- "00:00"
        date <- start[1]
    }

    # Parsing date
    L <- nchar(date)

    if (substr(date, 1, 3) %in% month.abb.l) {
        day <- 1
        month <- substr(date, 1, 3)
        month <- which((month == month.abb.l) == TRUE)
        year <- as.numeric(substr(date, 4, L))
        nyear <- L - 3
    } else {
        if (L == 6) {
            nday <- 1
            nyear <- 2
        } else if (L == 7) {
            nday <- 2
            nyear <- 2
        } else if (L == 8) {
            nday <- 1
            nyear <- 4
        } else if (L == 9) {
            nday <- 2
            nyear <- 4
        }

        day <- as.numeric(substr(date, 1, nday))
        year <- as.numeric(substr(date, L - nyear + 1, L))

        month <- substr(date, nday + 1, nday + 3)
        month <- which((month == month.abb.l) == TRUE)
    }
    if (nyear == 2) {
        year <- ifelse(year > 49, year + 1900, year + 2000)
    }

    date <- paste(year, month, day, sep = "-")
    # Parsing time
    time <- strsplit(time, ":")[[1]]

    if (length(time) > 1) {
        hour <- as.numeric(time[1])
        minute <- as.numeric(time[2])
    } else {
        hour <- as.numeric(time)
        minute <- 0
    }

    time <- paste(hour, minute, sep = ":")
    date.time <- lubridate::ymd_hm(paste(date, time, sep = " "))

    # Parse increment
    L <- nchar(increment)
    unit <- substr(increment, L-1, L)
    unit <- switch(unit,
                   mn = "minutes",
                   hr = "hours",
                   dy = "days",
                   mo = "months",
                   yr = "years")
    unit.fun <- match.fun(unit)
    increment <- as.numeric(substr(increment, 1, L - 2))

    fun.date <- function(x) {
        date.time + increment*unit.fun(x)
    }
    return(fun.date(0:nt))
}


template <- "attm_%y4.ctl"
.FilesFromTemplate <- function(date, template) {
    file.date <- rep(template, times = length(date))
    for (d in seq_along(date)) {
        for (f in seq_along(names(.template.funs))) {
            file.date[d] <- gsub(names(.template.funs)[f],
                                 .template.funs[[f]](date[d]),
                                 file.date[d])
        }
    }
    return(file.date)
}


# Template functions
.template.funs <- list(
    `%y4` = function(date) {
        y <- lubridate::year(date)
        sprintf("%04d", y)
    },
    `%y2` = function(date) {
        y <- lubridate::year(date)
        ifelse(y > 2000, y - 2000, y - 1900)
    },
    `%m1` = function(date) {
        lubridate::month(date)
    },
    `%m2` = function(date) {
        m <- lubridate::month(date)
        sprintf("%02d", m)
    },
    `%mc` = function(date) {
        tolower(month.abb[lubridate::month(date)])
    },
    `%d1` = function(date) {
        lubridate::day(date)
    },
    `%d2` = function(date) {
        d <- lubridate::day(date)
        sprintf("%02d", d)
    }
)
