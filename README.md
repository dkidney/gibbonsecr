
<!-- README.md is generated from README.Rmd. Please edit that file -->
gibbonsecr
==========

![](https://travis-ci.org/dkidney/gibbonsecr.svg?branch=master)

gibbonsecr provides tools for analysing data from acoustic gibbon surveys using Spatially Explicit Capture-Recapture (SECR).

Click [**here**](http://dkidney.github.io/gibbonsecr/index.html) view the online manual.

Click [**here**](http://dkidney.github.io/gibbonsecr/docs/index.pdf) to download the PDF manual.

Installation
------------

gibbonsecr is not available on CRAN and needs to be installed directly from Github.

### Prerequisites

**Mac:**

Install XQuartz: <https://www.xquartz.org/>

Install Xcode: <https://developer.apple.com/xcode/>

**Windows:**

Install Rtools: <https://cran.r-project.org/bin/windows/Rtools/>

### Install R

**Mac:**

<https://cran.r-project.org/bin/macosx/>

**Windows:**

<https://cran.r-project.org/bin/windows/base/>

### Install devtools

From the R console:

``` r
install.packages("devtools")
```

### Install gibbonsecr

From the R console:

``` r
# release version
devtools::install_github("dkidney/gibbonsecr")

# development version
devtools::install_github("dkidney/gibbonsecr", ref = "develop")
```

Launch
------

``` r
library(gibbonsecr)
gui()
```

Bug reports
-----------

Please report any bugs on the issues page:

<https://github.com/dkidney/gibbonsecr/issues>

Future developments
-------------------

gibbonsecr is only being minimally maintained, but efforts will be made to fix bugs and maintain the existing funtionalilty.

For access to more general SECR methods for acoustic data, please see the following:

-   The `ascr` graphical user interface: <https://github.com/cmjt/ascr_shiny>
-   The `ascr` R package: <https://github.com/b-steve/ascr>

The `asec` package is being actively maintained and offers a broader range of tools for analysing acoustic spatial capture-recapture data.
