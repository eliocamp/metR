# metR: Tools for Easier Analysis of Meteorological Fields

Many useful functions and extensions for dealing with meteorological
data in the tidy data framework. Extends 'ggplot2' for better plotting
of scalar and vector fields and provides commonly used analysis methods
in the atmospheric sciences.

## Overview

Conceptually it's divided into *visualization tools* and *data tools*.
The former are geoms, stats and scales that help with plotting using
'ggplot2', such as
[stat_contour_fill](https://eliocamp.github.io/metR/reference/geom_contour_fill.md)
or
[scale_y_level](https://eliocamp.github.io/metR/reference/scale_longitude.md),
while the later are functions for common data processing tools in the
atmospheric sciences, such as
[Derivate](https://eliocamp.github.io/metR/reference/Derivate.md) or
[EOF](https://eliocamp.github.io/metR/reference/EOF.md); these are
implemented to work in the 'data.table' paradigm, but also work with
regular data frames.

To get started, check the vignettes:

- Visualization Tools:
  [`vignette("Visualization-tools", package = "metR")`](https://eliocamp.github.io/metR/articles/Visualization-tools.md)

- Working with Data:
  [`vignette("Working-with-data", package = "metR")`](https://eliocamp.github.io/metR/articles/Working-with-data.md)` `

## See also

Useful links:

- <https://eliocamp.github.io/metR/>

- <https://github.com/eliocamp/metR>

- Report bugs at <https://github.com/eliocamp/metR/issues>

## Author

**Maintainer**: Elio Campitelli <eliocampitelli@gmail.com>
([ORCID](https://orcid.org/0000-0002-7742-9230))
