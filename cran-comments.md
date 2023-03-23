
This is a resubmission. In this version I've include code in examples to limit the number of threads used by data.table


## Test environments

* local installation of elementary OS 6.1 Jólnir (Built on Ubuntu 20.04.5 LTS) R version 4.2.3 (2023-03-15) 

* Winbuilder: R Under development (unstable) (2023-03-20 r84011 ucrt)

## R CMD check results

0 errors | 0 warnings | 1 notes 


I get this note. It seems like a false positive since I can browse the URLs with no problems.

    ✖ Error: man/standard_atmosphere.Rd:129:39 Error: SSL certificate problem: unable to get local issuer certificate
    Retrieved 22 February 2021, from \url{https://glossary.ametsoc.org/wiki/Standard_atmosphere}
                                          ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ✖ Error: vignettes/Working-with-data.Rmd:69:84 Error: Empty reply from server
    `GetTopography()` retrieves topographic data from the [ETOPO1 Global Relief Model](https://www.ncei.noaa.gov/products/etopo-global-relief-model) into a convenient tidy `data.table`. By default, it also stores a cached version.
        
Thanks for all the work :)
