#- check if the data and output directories exist. If they don't, create them.
if(!dir.exists("data"))dir.create("data")
if(!dir.exists("output"))dir.create("output")
            
#- set up HIEv.
if(!require(HIEv)){
  stop("Install the HIEv package first from bitbucket.org/remkoduursma/HIEv")
}

setToken(tokenfile="tokenfile.txt", quiet=TRUE)
setToPath("download")



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
if(!require(pacman))install.packages("pacman")
pacman::p_load(dplyr, doBy, readxl) # add other packages needed to this list


# Loading constants
source("definitions/constants.R")

# Sourcing all R files in the modules subdirectory
sourcefiles <- dir("modules", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)
