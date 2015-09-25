
gibbonsecr
===========

<br>

## Contents

<a href="#installation">**1. Installation**</a>

<a href="#launch">**2. Launch the user interface**</a>

<a href="#problems">**3. Report problems**</a>

<br>

********************************************************************************

<a name="installation"></a>

## 1. Installation

To download and install the necessary software to run the `gibbonsecr` package follow the instructions below.

### Install R

Make sure you have the latest version of **R** installed. 

[Download R for Windows](https://cran.r-project.org/bin/windows/base/)

[Download R for Mac](https://cran.r-project.org/bin/macosx/)

Optionally, you can also install **RStudio** which is a non-essential but more user-friendly front end for R.

[Download RStudio](https://www.rstudio.com/products/rstudio/download/)

<a name="windows setup"></a>

### Mac setup

Mac users using more recent versions of the OS may need to install **XQuartz**. Users of OS X 10.5 (Leopard), 10.6 (Snow Leopard) and 10.7 (Lion) should already have this installed by default (to check, look for the `X11.app` application in your applications folder). Users of OS X 10.8 (Mountain Lion), 10.9 (Mavericks) and 10.10 (Yosemite) will need to install it manually:

[Download XQuartz](http://xquartz.macosforge.org/landing/)

### Install prerequisite packages

The `gibbonsecr` package uses some other R packages that don't come with the default version of R, so you'll need to install them manually by typing (or cutting and pasting) the code below into the R console.

```{r}
install.packages(c("CircStats", "fields", "MASS", "nlme", "secr", "tcltk2"))
```

### Install the gibbonsecr package

Once the prequisite packages are installed you can install the `gibbonsecr` package by running the R code below.

**Windows users:**
```{r}
install.packages("https://github.com/dkidney/gibbonsecr/raw/master/binaries/gibbonsecr_1.0.zip", 
                 repos = NULL, type = "win.binary")
```

**Mac users:**
```{r}
install.packages("https://github.com/dkidney/gibbonsecr/raw/master/binaries/gibbonsecr_1.0.tgz", 
                 repos = NULL, type = "mac.binary")
```

<br>

********************************************************************************

<a name="launch"></a>

## 2. Launch the user interface

Once everything has been successfully installed, open R (or RStudio) and type the following commands into the console:

```{r}
library(gibbonsecr)
gibbonsecr_gui()
```

You can open the manual for the user interface from the *Help* menu in the interface itself, or by running the following code in R:

```{r}
vignette("gui", package = "gibbonsecr")
```

<br>

********************************************************************************

<a name="problems"></a>

## 3. Report problems

If you experience any problems with the installation process please email me at darrenkidney@yahoo.co.uk (please mention the version numbers for your installations of R, RStudio and XQuartz).  



