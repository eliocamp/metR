
## Test environments

* local installation of elementary OS 5.1.7 Hera (Built on Ubuntu 18.04.4 LTS) R version 4.1.0

* R-hub
    - Ubuntu Linux 20.04.1 LTS, R-release, GCC
    - Windows Server 2008 R2 SP1, R-devel, 32/64 bit

* winbuilder
    - R Under development (unstable) (2021-08-05 r80717)


## R CMD check results

0 errors | 0 warnings | 1 notes 

I get some notes only in winbuilder. I believe these are false positives.

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1175/1520-0469(1985)042<0217:OTTDPO>2.0.CO;2
    From: man/EPflux.Rd
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.1175/1520-0469(2001)058<0608:AFOAPI>2.0.CO;2
    From: man/WaveFlux.Rd
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.1175/1520-0493(2003)131<1011:EEORWP>2.0.CO;2
    From: man/waves.Rd
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.1175/2007JCLI1725.1
    From: man/EPflux.Rd
    Status: 403
    Message: Forbidden
    

I also get this one, which I also seems like a false positive, since I can browse to that
URL with no problems.

  URL: https://glossary.ametsoc.org/wiki/Standard_atmosphere
    From: man/standard_atmosphere.Rd
    Status: Error
    Message: SSL certificate problem: unable to get local issuer certificate


Thanks for all the work :)
