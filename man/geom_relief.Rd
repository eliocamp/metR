% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_relief.R, R/geom_shadow.R
\docType{data}
\name{geom_relief}
\alias{geom_relief}
\alias{GeomRelief}
\alias{geom_shadow}
\alias{GeomShadow}
\title{Relief Shading}
\usage{
geom_relief(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  sun.angle = 60,
  raster = TRUE,
  interpolate = TRUE,
  shadow = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_shadow(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  sun.angle = 60,
  range = c(0, 1),
  skip = 0,
  raster = TRUE,
  interpolate = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this
layer, either as a \code{ggproto} \code{Geom} subclass or as a string naming the
stat stripped of the \code{stat_} prefix (e.g. \code{"count"} rather than
\code{"stat_count"})}

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{sun.angle}{angle from which the sun is shining, in degrees
counterclockwise from 12 o' clock}

\item{raster}{if \code{TRUE} (the default), uses \link[ggplot2:geom_tile]{ggplot2::geom_raster},
if \code{FALSE}, uses \link[ggplot2:geom_tile]{ggplot2::geom_tile}.}

\item{interpolate}{If \code{TRUE} interpolate linearly, if \code{FALSE}
(the default) don't interpolate.}

\item{shadow}{if TRUE, adds also a layer of \code{geom_shadow()}}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{range}{transparency range for shadows}

\item{skip}{data points to skip when casting shadows}
}
\description{
\code{geom_relief()} simulates shading caused by relief. Can be useful when
plotting topographic data because relief shading might give a more intuitive
impression of the shape of the terrain than contour lines or mapping height
to colour. \code{geom_shadow()} projects shadows.
}
\details{
\code{light} and \code{dark} must be valid colours determining the light and dark shading
(defaults to "white" and "gray20", respectively).
}
\section{Aesthetics}{

\code{geom_relief()} and \code{geom_shadow()} understands the following aesthetics (required aesthetics are in bold)

\itemize{
\item \strong{x}
\item \strong{y}
\item \strong{z}
\item \code{light}
\item \code{dark}
\item \code{sun.angle}
}
}

\examples{
\dontrun{
library(ggplot2)
ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
      geom_relief(aes(z = value))
}
}
\seealso{
Other ggplot2 helpers: 
\code{\link{MakeBreaks}()},
\code{\link{WrapCircular}()},
\code{\link{geom_arrow}()},
\code{\link{geom_contour2}()},
\code{\link{geom_contour_fill}()},
\code{\link{geom_label_contour}()},
\code{\link{geom_streamline}()},
\code{\link{guide_colourstrip}()},
\code{\link{map_labels}},
\code{\link{reverselog_trans}()},
\code{\link{scale_divergent}},
\code{\link{scale_longitude}},
\code{\link{stat_na}()},
\code{\link{stat_subset}()}
}
\concept{ggplot2 helpers}
\keyword{datasets}
