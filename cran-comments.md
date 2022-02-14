
## Test environments

* local installation of elementary OS 6.1 Jólnir (Built on Ubuntu 20.04.3 LTS) R version 4.1.2

* winbuilder 
    - R Under development (unstable) (2022-02-13 r81727 ucrt)

+ rhub 
    - Windows Server 2022, R-devel, 64 bit

## R CMD check results

0 errors | 0 warnings | 2 notes 

I get this note. It seems like a false positive since I can browse the URLs with no problems.

  Found the following (possibly) invalid URLs:
    URL: https://glossary.ametsoc.org/wiki/Standard_atmosphere
      From: man/standard_atmosphere.Rd
      Status: Error
      Message: libcurl error code 60:
        	SSL certificate problem: unable to get local issuer certificate
        	(Status without verification: OK)

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


I also get 

Suggests orphaned package: ‘udunits2’

I was contacted a few weeks ago. About this. Refactoring is in process, but it's difficult because 
udunits2 is central to parsing dates. 

Thanks for all the work :)
