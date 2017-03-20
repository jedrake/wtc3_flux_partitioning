#- check if the data and output directories exist. If they don't, create them.
if(!dir.exists("data"))dir.create("data")
if(!dir.exists("output"))dir.create("output")
            
#- set up HIEv.
if(!require(HIEv)){
  stop("Install the HIEv package first from bitbucket.org/remkoduursma/HIEv")
}

#- set up plotBy
if(!require(plotBy)){
  stop("Install the plotBy package first from bitbucket.org/remkoduursma/plotBy")
}

setToken(tokenfile="tokenfile.txt", quiet=TRUE)



#---------------------------------------------------------------------
#- function to load a package, and install it if necessary
Library <- function(pkg, ...){
  
  PACK <- .packages(all.available=TRUE)
  pkgc <- deparse(substitute(pkg))
  
  if(pkgc %in% PACK){
    library(pkgc, character.only=TRUE)
  } else {
    install.packages(pkgc, ...)
    library(pkgc, character.only=TRUE)
  }
  
}
#---------------------------------------------------------------------

#- load required libraries.
Library(pacman)
Library(shape)
Library(forecast)
Library(mgcv)
Library(scales)
Library(gplots)
Library(magicaxis)
Library(lubridate)
Library(doBy)
Library(Hmisc)
Library(zoo)
Library(hexbin)
Library(nlme)
Library(car)
Library(data.table)
Library(shape)
Library(reshape2)
Library(calibrate)
Library(dplyr)
Library(RColorBrewer)

#- the following libraries aren't on CRAN, but can be installed from github or bitbucket with devtools
if (require("devtools")==F) {install.packages("devtools")
  library(devtools,quietly=T)}
if (require("plantecophys")==F) {
  install_bitbucket("remkoduursma/plantecophys")
  library(plantecophys,quietly=T)}
if (require("HIEv")==F) {
  install_bitbucket("remkoduursma/HIEv")
  library(HIEv,quietly=T)}
if (require("plotBy")==F){ 
  install_bitbucket("remkoduursma/plotBy")
  library(plotBy,quietly=T)}


# if(!require(pacman))install.packages("pacman")
# pacman::p_load(dplyr, doBy, readxl) # add other packages needed to this list


# Sourcing all R files in the modules subdirectory
sourcefiles <- dir("R/functions/", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)



#--- download some data from HIEv. Eventually needs updating to download new published datasets
downloadHIEv(hiev=searchHIEv("WTC_TEMP_CM_BARKDENSITY_20140528_L1.csv"),topath="data/")
downloadHIEv(searchHIEv("WTC_TEMP_CM_WOODDENSITY_20140528_L1.csv"),topath="data/")
downloadHIEv(searchHIEv("WTC_TEMP_CM_BRANCHCENSUS_20130910-20140516_L0_v1.csv"),topath="data/")
downloadHIEv(searchHIEv("WTC_TEMP_CM_LAYERHEIGHT_20140519_L1.csv"),topath="data/")
downloadHIEv(searchHIEv("WTC_TEMP_CM_HARVEST-CANOPY_20140526-20140528_L1_v1.csv"),topath="data/")
downloadHIEv(searchHIEv("WTC_TEMP_CM_HARVEST-ROOTS_20140529-20140606_L1_v1.csv"),topath="data/")
downloadHIEv(searchHIEv("WTC_TEMP_CM_BIOMASS-ROOT-CORES_20140529_L1_v1.csv"),topath="data/")
downloadHIEv(searchHIEv("WTC_TEMP_CM_WATERPOTENTIAL-DIURNALS_20140214-20140514_R.csv"),topath="data")

