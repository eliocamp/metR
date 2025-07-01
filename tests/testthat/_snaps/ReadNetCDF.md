# GlanceNetCDF prints nicely

    Code
      print(GlanceNetCDF(file))
    Output
      ----- Variables ----- 
      air:
          mean Daily Air temperature in degK
          Dimensions: lon by lat by level by time
      
      
      ----- Dimensions ----- 
        time: 1 values from 2010-07-09 to 2010-07-09 
        level: 17 values from 10 to 1000 millibar
        lat: 73 values from -90 to 90 degrees_north
        lon: 144 values from 0 to 357.5 degrees_east

# subsetting works

    Code
      ReadNetCDF(file, subset = s)
    Output
                  time level   lat   lon    air
                <POSc> <num> <num> <num>  <num>
         1: 2010-07-09  1000   -70   0.0 249.07
         2: 2010-07-09  1000   -70   2.5 249.82
         3: 2010-07-09  1000   -70   5.0 250.32
         4: 2010-07-09  1000   -70   7.5 250.62
         5: 2010-07-09  1000   -70  10.0 250.77
        ---                                    
      7493: 2010-07-09    10    70 347.5 242.17
      7494: 2010-07-09    10    70 350.0 242.05
      7495: 2010-07-09    10    70 352.5 241.95
      7496: 2010-07-09    10    70 355.0 241.85
      7497: 2010-07-09    10    70 357.5 241.70

