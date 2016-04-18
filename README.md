
gibbonsecr
===========

Click [**here**](http://dkidney.github.io/gibbonsecr/docs/index.pdf) to download the PDF manual.

Click [**here**](http://dkidney.github.io/gibbonsecr/index.html) view the online manual

### Contents

<a href="#installation">**1. Installation**</a>

<a href="#launch">**2. Launch the user interface**</a>

<a href="#problems">**3. Report problems**</a>

<br>

********************************************************************************

<a name="installation"></a>

## 1. Installation

This section provides some instructions For downloading and installing the necessary software to run the `gibbonsecr` package.

### Install R

Firstly, make sure you have the latest version of **R** installed. 

[Download R for Windows](https://cran.r-project.org/bin/windows/base/)

[Download R for Mac](https://cran.r-project.org/bin/macosx/)

Optionally, you can also install **RStudio** which is a non-essential but more user-friendly interface for running R.

[Download RStudio](https://www.rstudio.com/products/rstudio/download/)

<a name="windows setup"></a>

### Mac setup

Mac users using more recent versions of the OS may need to install **XQuartz**. Users of OS X 10.5 (Leopard), 10.6 (Snow Leopard) and 10.7 (Lion) should already have this installed by default (to check, look for the `X11.app` application in your applications folder). Users of OS X 10.8 (Mountain Lion), 10.9 (Mavericks) and 10.10 (Yosemite) will need to install it manually:

[Download XQuartz](http://xquartz.macosforge.org/landing/)

### Install prerequisite R packages

The `gibbonsecr` package uses some other R packages that don't come with the default version of R, so you'll need to install them manually by typing (or cutting and pasting) the code below into the R (or RStudio) console.

```{r}
install.packages(c("CircStats", "dplyr", "MASS", "secr", "tcltk2"), 
                 dependencies = TRUE)
```

### Install the gibbonsecr package

Once the prequisite R packages are installed you can install the `gibbonsecr` package by running the R code below.

**Windows:**
```{r}
install.packages("https://github.com/dkidney/gibbonsecr/raw/master/binaries/gibbonsecr_1.0.zip", 
                 repos = NULL, type = "win.binary")
```

**Mac:**
```{r}
install.packages("devtools", dependencies = TRUE)
devtools::install_url("https://github.com/dkidney/gibbonsecr/raw/master/binaries/gibbonsecr_1.0.tgz")

```

You only have to install the packages once (i.e. you wont need to run the code above every time you use the `giboonsecr` package). 

<br>

********************************************************************************

<a name="launch"></a>

## 2. Launch the user interface

Once everything has been successfully installed, open R (or RStudio) and type the following commands into the console:

```{r}
library(gibbonsecr)
gibbonsecr_gui()
```

The user interface should magically appear.

You can open the manual for the user interface from the *Help* menu in the interface itself, or by running the following code in R:

```{r}
vignette("gui", package = "gibbonsecr")
```

<br>

********************************************************************************

<a name="problems"></a>

## 3. Report problems

If you experience any problems with the installation process please email me at darrenkidney@googlemail.com (please let me know what operating system you are using and the version numbers for your installations of R, RStudio and XQuartz).  



