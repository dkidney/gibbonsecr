
gibbonsecr
===========

<a href="#installation">1. Installation</a>

<a href="#launch">2. Launch the GUI</a>

<a href="#problems">3. Report problems</a>

<a name="installation"></a>

## 1. Installation

To download and install the `gibbonsecr` package directly from github follow the instructions below.

### Install R

Make sure you have the latest version of **R** installed. 

[Download R for Windows](https://cran.r-project.org/bin/windows/base/)

[Download R for Mac](https://cran.r-project.org/bin/macosx/)

**Note to windows users:** Make sure that the file path of your R installation has **no spaces**. This is necessary for RTools to work (see <a href="#windows setup">Windows setup</a>) -- e.g. “C:/R/R-3.2.2” is fine, but “C:/Program files/R/R-3.2.2” won’t work.

Optionally, you can also install **RStudio** which is a non-essential but more user-friendly front end for R.

[Download RStudio](https://www.rstudio.com/products/rstudio/download/)

<a name="windows setup"></a>

### Windows setup

Windows users will need to have **RTools** installed.

[Download RTools](https://cran.r-project.org/bin/windows/Rtools/)

* Make sure that the version of RTools is compatible with your version of R -- the best way to ensure this is to **install the most up-to-date versions** of both.

* Make sure that the file path of your installation of R has **no spaces** in it.

* During the installation of RTools make sure you allow the installer to update your system path.

### Mac setup

Mac users using more recent versions of the OS may need to install **XQuartz** and **Xcode**.

[Download XQuartz](http://xquartz.macosforge.org/landing/)

[Download Xcode](https://developer.apple.com/xcode/downloads/)

### Install the package

To download and install the `gibbonsecr` R package, open R (or RStudio) and run the code below.

```{r}
install.packages("devtools")
devtools::install_github(repo = "dkidney/gibbonsecr", 
                         args = "--no-multiarch --with-keep.source")
```

<a name="launch"></a>

## 2. Launch the GUI

Once everything has been successfully installed, load the package and run the `gibbonsecr` function.

```{r}
library(gibbonsecr)
gibbonsecr_gui()
```

You can open the GUI user manual using the help menu in the GUI itself, or by running the following code in the R console.

```{r}
vignette("gui", package = "gibbonsecr")
```

<a name="problems"></a>

## 3. Report problems

If you experience any problems with the installation process please email Darren at darrenkidney@yahoo.co.uk (please inlcude the version numbers of R, RTools, XQuartz and Xcode you are using).  



[Manual](https://github.com/dkidney/gibbonsecr/tree/master/inst/doc/gibbonsecr_1.0-vignette.html)


