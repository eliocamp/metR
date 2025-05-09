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

\item{stat}{The statistical transformation to use on the data for this layer.
When using a \verb{geom_*()} function to construct a layer, the \code{stat}
argument can be used the override the default coupling between geoms and
stats. The \code{stat} argument accepts the following:
\itemize{
\item A \code{Stat} ggproto subclass, for example \code{StatCount}.
\item A string naming the stat. To give the stat as a string, strip the
function name of the \code{stat_} prefix. For example, to use \code{stat_count()},
give the stat as \code{"count"}.
\item For more information and other ways to specify the stat, see the
\link[ggplot2:layer_stats]{layer stat} documentation.
}}

\item{position}{A position adjustment to use on the data for this layer. This
can be used in various ways, including to prevent overplotting and
improving the display. The \code{position} argument accepts the following:
\itemize{
\item The result of calling a position function, such as \code{position_jitter()}.
This method allows for passing extra arguments to the position.
\item A string naming the position adjustment. To give the position as a
string, strip the function name of the \code{position_} prefix. For example,
to use \code{position_jitter()}, give the position as \code{"jitter"}.
\item For more information and other ways to specify the position, see the
\link[ggplot2:layer_positions]{layer position} documentation.
}}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}'s \code{params} argument. These
arguments broadly fall into one of 4 categories below. Notably, further
arguments to the \code{position} argument, or aesthetics that are required
can \emph{not} be passed through \code{...}. Unknown arguments that are not part
of the 4 categories below are ignored.
\itemize{
\item Static aesthetics that are not mapped to a scale, but are at a fixed
value and apply to the layer as a whole. For example, \code{colour = "red"}
or \code{linewidth = 3}. The geom's documentation has an \strong{Aesthetics}
section that lists the available options. The 'required' aesthetics
cannot be passed on to the \code{params}. Please note that while passing
unmapped aesthetics as vectors is technically possible, the order and
required length is not guaranteed to be parallel to the input data.
\item When constructing a layer using
a \verb{stat_*()} function, the \code{...} argument can be used to pass on
parameters to the \code{geom} part of the layer. An example of this is
\code{stat_density(geom = "area", outline.type = "both")}. The geom's
documentation lists which parameters it can accept.
\item Inversely, when constructing a layer using a
\verb{geom_*()} function, the \code{...} argument can be used to pass on parameters
to the \code{stat} part of the layer. An example of this is
\code{geom_area(stat = "density", adjust = 0.5)}. The stat's documentation
lists which parameters it can accept.
\item The \code{key_glyph} argument of \code{\link[ggplot2:layer]{layer()}} may also be passed on through
\code{...}. This can be one of the functions described as
\link[ggplot2:draw_key]{key glyphs}, to change the display of the layer in the legend.
}}

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
