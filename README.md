
gibbonsecr: 
===========

SECR Analysis For Acoustic Gibbon Surveys


## Installation and launch instructions

To download and install the `gibbonsecr` package directly from github follow the instructions below.

### Install R

Make sure you have the latest version of **R** installed. 

[Download R for Windows](https://cran.r-project.org/bin/windows/base/)

[Download R for Mac](https://cran.r-project.org/bin/macosx/)

**Note to windows users:** Make sure that the file path of your R installation has **no spaces**. This is necessary for RTools to work (see *Windows setup*) -- e.g. “C:/R/R-3.2.2” is fine, but “C:/Program files/R/R-3.2.2” won’t work.

Optionally, you can also install **RStudio** which is a non-essential but more user-friendly front end for R.

[Download RStudio](https://www.rstudio.com/products/rstudio/download/)

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

### Launch the GUI

Once everything has been successfully installed, load the package and run the `gibbonsecr` function.

```{r}
library(gibbonsecr)
gibbonsecr_gui()
```

### Problems

If you experience any problems with the installation process please email me at darrenkidney@yahoo.co.uk (please inlcude the version numbers of R, RTools, XQuartz and Xcode you are using).  


