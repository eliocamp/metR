
## Test environments

* local installation of elementary OS 6.1 Jólnir (Built on Ubuntu 20.04.3 LTS) R version 4.1.2

* winbuilder 
    - R Under development (unstable) (2022-10-04 r83023 ucrt)

* rhub 
     - Windows Server 2022, R-devel, 64 bit
     - Ubuntu Linux 20.04.1 LTS, R-release, GCC

     
## R CMD check results

0 errors | 0 warnings | 1 notes 

I get this note. It seems like a false positive since I can browse the URLs with no problems.

  Found the following (possibly) invalid URLs:
    URL: https://glossary.ametsoc.org/wiki/Standard_atmosphere
      From: man/standard_atmosphere.Rd
      Status: Error
      Message: libcurl error code 60:
        	SSL certificate problem: unable to get local issuer certificate
        	(Status without verification: OK)


Thanks for all the work :)
