
#-------------------------------------------------------------------------------------------------------------------
#- Load the libraries that do all of the actual work
source("R/loadLibraries.R")
#-------------------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------------------
#- Download the data associated with Drake et al. 2016 New Phyt.
#  This downloads the zipfile from figshare and extracts it to a folder named "data".
download.file("https://ndownloader.figshare.com/files/4857112?private_link=cdc9a3caf5bffc0add94", "data.zip", mode="wb")
unzip("data.zip",overwrite=F)
#-------------------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------------------
#- If fluxes have not been paritioned, partition net C fluxes into GPP and Ra.
#  Otherwise, just read in the paritioned file. Partitioned fluxes are in dataframe "dat.hr.gf"
if(!file.exists("data/WTC_TEMP_CM_WTCFLUX-PARTITIONED_20130914-20140526_L2_V2.csv")){
  
  #- read in the hourly flux data
  dat.hr <- read.csv("data/WTC_TEMP_CM_WTCFLUX_20130914-20140526_L2_V2.csv")
  dat.hr$DateTime <- as.POSIXct(dat.hr$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
  dat.hr$Date <- as.Date(dat.hr$DateTime)
  
  #- partition the net fluxes into GPP and Ra components. Specify the actitation energy and the number
  #-  of prior nights to include for the estimate of the basal respiration rate (lagdates)
  dat.hr.gf <- partitionHourlyFluxCUE_arr(dat.hr.gf=dat.hr,Ea=57.69,lagdates=3)
  
  #- write out a csv of the partitioned fluxes
  write.csv(dat.hr.gf,"data/WTC_TEMP_CM_WTCFLUX-PARTITIONED_20130914-20140526_L2_V2.csv",row.names=F)
}
if(file.exists("data/WTC_TEMP_CM_WTCFLUX-PARTITIONED_20130914-20140526_L2_V2.csv")){
  dat.hr.gf <- data.frame(data.table::fread("data/WTC_TEMP_CM_WTCFLUX-PARTITIONED_20130914-20140526_L2_V2.csv"))
  dat.hr.gf$Date <- as.Date(dat.hr.gf$DateTime)
}
#-------------------------------------------------------------------------------------------------------------------









#-------------------------------------------------------------------------------------------------------------------
#- Get an estimate of branch, stem, and leaf mass as well as leaf area for each day of the experiment
treeMass <- returnTreeMass()
treeMassFlux <- merge(dat.hr.gf,treeMass,by=c("chamber","Date"))

#- create daily sums of GPP, Ra, and CUE
cue.list <- returnCUEdaily(treeMassFlux)
cue.day <- cue.list[[1]]
cue.day.trt <- cue.list[[2]]
#--------------------------------------------------------------------------------------------------
