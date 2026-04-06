
# Transforming Excel tables into SDMX tables

The goal of EXCELSDMX package is to allow users to be able to generate SDMX files out from published excel statistical tables files. 

## Installation of sdd_excel2sdmx package

You can install and execute the sdd_excel2sdmx package as per the following steps:

``` r
# **************** How to install the package from R console ******************** #

install.packages("remotes") # Ensure remotes package is installed before proceeding
library(remotes) # load the remotes package
remotes::install_github("https://github.com/fiji-bureau-of-statistics/excel2sdmx") # Install the sdd_excel2sdmx package
# if "remotes::install_github("https://github.com/fiji-bureau-of-statistics/excel2sdmx")" does not work. try the following
remotes::install_github("https://github.com/fiji-bureau-of-statistics/excel2sdmx", force = TRUE)

# **************** How to run the imtshiny app from R console ******************** #
library(sdd_excel2sdmx) # Load the sdd_excel2sdmx package
sdd_excel2sdmx::run_app() # Execute the sdd_excel2sdmx package 

```
