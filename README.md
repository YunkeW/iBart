# iBart
## Installation

Before installing the iBART package in R, you first need to install Java JDK and rJava R package. 

### Install Java JDK (not JRE)

Download [Java 17 JDK or above](https://www.oracle.com/java/technologies/javase/jdk17-archive-downloads.html) and install it properly. Then run `R CMD javareconf` from the command line to configure Java in R. iBART requires bartMachine and rJava which require Java JDK; Java JRE won't work!

### Install rJava

Run `install.packages("rJava", INSTALL_opts = "--no-multriarch")` within R. To reproduce results in the paper, please install `rJava 1.0-4`.

### Install bartMachine

Run `install.packages("bartMachine", INSTALL_opts = "--no-multiarch")` within R. To reproduce results in the paper, please install `bartMachineJARs 1.1` and `bartMachine 1.2.6`. If you experience error, please see the [bartMachine repo](https://github.com/kapelner/bartMachine) for detailed instructions.


### Install glmnet

Run `install.packages("glmnet")` within R. To reproduce results in the paper, please install `glmnet 4.1-1`.

### Install iBART via CRAN

Run `install.packages("iBART")` within R.

### Install iBART via devtools

Run `devtools::install_github("YunkeW/iBart", INSTALL_opts = "--no-multriarch", build_vignettes = TRUE)` within R.

