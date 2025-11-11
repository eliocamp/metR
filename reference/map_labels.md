# Label longitude and latitude

Provide easy functions for adding suffixes to longitude and latitude for
labelling maps.

## Usage

``` r
LonLabel(lon, east = "°E", west = "°W", zero = "°")

LatLabel(lat, north = "°N", south = "°S", zero = "°")
```

## Arguments

- lon:

  longitude in degrees

- east, west, north, south, zero:

  text to append for each quadrant

- lat:

  latitude in degrees

## Details

The default values are for Spanish.

## See also

Other ggplot2 helpers:
[`MakeBreaks()`](https://eliocamp.github.io/metR/reference/MakeBreaks.md),
[`WrapCircular()`](https://eliocamp.github.io/metR/reference/WrapCircular.md),
[`geom_arrow()`](https://eliocamp.github.io/metR/reference/geom_arrow.md),
[`geom_contour2()`](https://eliocamp.github.io/metR/reference/geom_contour2.md),
[`geom_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md),
[`geom_label_contour()`](https://eliocamp.github.io/metR/reference/geom_text_contour.md),
[`geom_relief()`](https://eliocamp.github.io/metR/reference/geom_relief.md),
[`geom_streamline()`](https://eliocamp.github.io/metR/reference/geom_streamline.md),
[`guide_colourstrip()`](https://eliocamp.github.io/metR/reference/guide_colourstrip.md),
[`reverselog_trans()`](https://eliocamp.github.io/metR/reference/reverselog_trans.md),
[`scale_divergent`](https://eliocamp.github.io/metR/reference/scale_divergent.md),
[`scale_longitude`](https://eliocamp.github.io/metR/reference/scale_longitude.md),
[`stat_na()`](https://eliocamp.github.io/metR/reference/stat_na.md),
[`stat_subset()`](https://eliocamp.github.io/metR/reference/stat_subset.md)

## Examples

``` r
LonLabel(0:360)
#>   [1] "0°"    "1°E"   "2°E"   "3°E"   "4°E"   "5°E"   "6°E"   "7°E"   "8°E"  
#>  [10] "9°E"   "10°E"  "11°E"  "12°E"  "13°E"  "14°E"  "15°E"  "16°E"  "17°E" 
#>  [19] "18°E"  "19°E"  "20°E"  "21°E"  "22°E"  "23°E"  "24°E"  "25°E"  "26°E" 
#>  [28] "27°E"  "28°E"  "29°E"  "30°E"  "31°E"  "32°E"  "33°E"  "34°E"  "35°E" 
#>  [37] "36°E"  "37°E"  "38°E"  "39°E"  "40°E"  "41°E"  "42°E"  "43°E"  "44°E" 
#>  [46] "45°E"  "46°E"  "47°E"  "48°E"  "49°E"  "50°E"  "51°E"  "52°E"  "53°E" 
#>  [55] "54°E"  "55°E"  "56°E"  "57°E"  "58°E"  "59°E"  "60°E"  "61°E"  "62°E" 
#>  [64] "63°E"  "64°E"  "65°E"  "66°E"  "67°E"  "68°E"  "69°E"  "70°E"  "71°E" 
#>  [73] "72°E"  "73°E"  "74°E"  "75°E"  "76°E"  "77°E"  "78°E"  "79°E"  "80°E" 
#>  [82] "81°E"  "82°E"  "83°E"  "84°E"  "85°E"  "86°E"  "87°E"  "88°E"  "89°E" 
#>  [91] "90°E"  "91°E"  "92°E"  "93°E"  "94°E"  "95°E"  "96°E"  "97°E"  "98°E" 
#> [100] "99°E"  "100°E" "101°E" "102°E" "103°E" "104°E" "105°E" "106°E" "107°E"
#> [109] "108°E" "109°E" "110°E" "111°E" "112°E" "113°E" "114°E" "115°E" "116°E"
#> [118] "117°E" "118°E" "119°E" "120°E" "121°E" "122°E" "123°E" "124°E" "125°E"
#> [127] "126°E" "127°E" "128°E" "129°E" "130°E" "131°E" "132°E" "133°E" "134°E"
#> [136] "135°E" "136°E" "137°E" "138°E" "139°E" "140°E" "141°E" "142°E" "143°E"
#> [145] "144°E" "145°E" "146°E" "147°E" "148°E" "149°E" "150°E" "151°E" "152°E"
#> [154] "153°E" "154°E" "155°E" "156°E" "157°E" "158°E" "159°E" "160°E" "161°E"
#> [163] "162°E" "163°E" "164°E" "165°E" "166°E" "167°E" "168°E" "169°E" "170°E"
#> [172] "171°E" "172°E" "173°E" "174°E" "175°E" "176°E" "177°E" "178°E" "179°E"
#> [181] "180°"  "179°W" "178°W" "177°W" "176°W" "175°W" "174°W" "173°W" "172°W"
#> [190] "171°W" "170°W" "169°W" "168°W" "167°W" "166°W" "165°W" "164°W" "163°W"
#> [199] "162°W" "161°W" "160°W" "159°W" "158°W" "157°W" "156°W" "155°W" "154°W"
#> [208] "153°W" "152°W" "151°W" "150°W" "149°W" "148°W" "147°W" "146°W" "145°W"
#> [217] "144°W" "143°W" "142°W" "141°W" "140°W" "139°W" "138°W" "137°W" "136°W"
#> [226] "135°W" "134°W" "133°W" "132°W" "131°W" "130°W" "129°W" "128°W" "127°W"
#> [235] "126°W" "125°W" "124°W" "123°W" "122°W" "121°W" "120°W" "119°W" "118°W"
#> [244] "117°W" "116°W" "115°W" "114°W" "113°W" "112°W" "111°W" "110°W" "109°W"
#> [253] "108°W" "107°W" "106°W" "105°W" "104°W" "103°W" "102°W" "101°W" "100°W"
#> [262] "99°W"  "98°W"  "97°W"  "96°W"  "95°W"  "94°W"  "93°W"  "92°W"  "91°W" 
#> [271] "90°W"  "89°W"  "88°W"  "87°W"  "86°W"  "85°W"  "84°W"  "83°W"  "82°W" 
#> [280] "81°W"  "80°W"  "79°W"  "78°W"  "77°W"  "76°W"  "75°W"  "74°W"  "73°W" 
#> [289] "72°W"  "71°W"  "70°W"  "69°W"  "68°W"  "67°W"  "66°W"  "65°W"  "64°W" 
#> [298] "63°W"  "62°W"  "61°W"  "60°W"  "59°W"  "58°W"  "57°W"  "56°W"  "55°W" 
#> [307] "54°W"  "53°W"  "52°W"  "51°W"  "50°W"  "49°W"  "48°W"  "47°W"  "46°W" 
#> [316] "45°W"  "44°W"  "43°W"  "42°W"  "41°W"  "40°W"  "39°W"  "38°W"  "37°W" 
#> [325] "36°W"  "35°W"  "34°W"  "33°W"  "32°W"  "31°W"  "30°W"  "29°W"  "28°W" 
#> [334] "27°W"  "26°W"  "25°W"  "24°W"  "23°W"  "22°W"  "21°W"  "20°W"  "19°W" 
#> [343] "18°W"  "17°W"  "16°W"  "15°W"  "14°W"  "13°W"  "12°W"  "11°W"  "10°W" 
#> [352] "9°W"   "8°W"   "7°W"   "6°W"   "5°W"   "4°W"   "3°W"   "2°W"   "1°W"  
#> [361] "0°"   
```
