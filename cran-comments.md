
## Test environments

* local installation of elementary OS 5.1.7 Hera (Built on Ubuntu 18.04.4 LTS) R version 4.0.3
* r-hub
    - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* win-builder
    - R Under development (unstable) (2020-11-21 r79454)
* github actions
    - Ubuntu 20.04.1 (release)
    - Ubuntu 20.04.1 (devel)
    - Microsoft Windows Server 2019 (release)

## R CMD check results

0 errors | 0 warnings | 1 notes 

I get these notes. I'm using the correct \doi{} macro and the links all work fine. 

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1175/1520-0469(1985)042<0217:OTTDPO>2.0.CO;2
    From: man/EPflux.Rd
    Status: Error
    Message: libcurl error code 35:
      	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).
  URL: https://doi.org/10.1175/1520-0469(2001)058<0608:AFOAPI>2.0.CO;2
    From: man/WaveFlux.Rd
    Status: Error
    Message: libcurl error code 35:
      	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).
  URL: https://doi.org/10.1175/1520-0493(2003)131<1011:EEORWP>2.0.CO;2
    From: man/waves.Rd
    Status: Error
    Message: libcurl error code 35:
      	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).
  URL: https://doi.org/10.1175/2007JCLI1725.1
    From: man/EPflux.Rd
    Status: Error
    Message: libcurl error code 35:
      	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).
