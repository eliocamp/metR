# trajectory works

    Code
      Trajectory(dlon + dlat ~ lon + lat + date, x0 = 180, y0 = -60, data = na.omit(
        geopotential), res = 3, cyclical = c(TRUE, FALSE))
    Output
               lon       lat    id piece      date          dlon          dlat
             <num>     <num> <int> <num>     <num>         <num>         <num>
      1: 180.00000 -60.00000     1     1 631152000  1.527505e-04 -1.373291e-05
      2: 316.37564 -72.26074     1     1 632044800  1.258644e-04 -4.088801e-06
      3:  71.24737 -75.91122     1     2 632937600 -1.124835e-04  1.634286e-06
      4: 328.32208 -74.45213     1     3 633830400  9.383545e-05 -1.294094e-05
      5:  54.59837 -86.00580     1     4 634723200  6.627506e-04 -6.734548e-06

