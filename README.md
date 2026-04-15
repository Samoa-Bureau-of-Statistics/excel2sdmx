
# Transforming Excel tables into SDMX tables

The goal of EXCELSDMX package is to allow users to be able to generate SDMX files out from published excel statistical tables files. 

## Installation of excel2sdmx package

You can install and execute the sdd_excel2sdmx package as per the following steps:

``` r
# **************** How to install the package from R console ******************** #
# Before installing the package, we need to restart the R session
.rs.restartR()

install.packages("remotes") # Ensure remotes package is installed before proceeding
library(remotes) # load the remotes package
remotes::install_github("https://github.com/Samoa-Bureau-of-Statistics/excel2sdmx") # Install the sdd_excel2sdmx package
# if "remotes::install_github("https://github.com/Samoa-Bureau-of-Statistics/excel2sdmx")" does not work. try the following
remotes::install_github("https://github.com/Samoa-Bureau-of-Statistics/excel2sdmx", force = TRUE)

# **************** How to run the imtshiny app from R console ******************** #
library(sbsSDMX) # Load the sdd_excel2sdmx package
sbsSDMX::run_sbsSDMX() # Execute the sdd_excel2sdmx package 

```
