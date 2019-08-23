
<!-- README.md is generated from README.Rmd. Please edit that file -->
standview
=========

The standview package in R will generate stand density management diagrams. These graphic presentations of forest stand-level density come in two forms. The first format is derived from Reineke-space and these are generated with a call to dmd.view. The second is derived from Gingrich-space and these are generated by a call to gdmd.view.

Installation
------------

If you already have devtools, you can install standview from github with:

``` r
# install.packages("devtools")
# library(devtools)
devtools::install_github("mwritchie/standview")
```

If the devtools package has not already been installed, then the install.packages("devtools") and the library(devtools) call, which are both shown as comments above, are necessary for installation of standview.

Example
-------

This is a basic example which shows you how to generate a Long and Shaw (2005) density managment diagram in Reineke space for ponderosa pine with annotation on the right only. Note that the file produced will be placed in your working directory for R as defined by setwd():

``` r
library(standview)
pdf("test20.pdf", width=6, height=8)
opar <- par(mar = c(2.0,2.0,2.0,2.0))

dmd.view(ineq=2, insdr=TRUE, insdl=FALSE)

par(opar)
dev.off()
```

Other Information
-----------------

The current version of standview has a brief vignette. Also, at this time, the package has not been submitted to CRAN.

The author contact is: Martin W. Ritchie, Pacific Southwest Research Station, Redding CA. martin.ritchie at usda.gov
