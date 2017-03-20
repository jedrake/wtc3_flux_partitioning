#- If fluxes have not been paritioned, partition net C fluxes into GPP and Ra.
#  Otherwise, just read in the paritioned file. Partitioned fluxes are in dataframe "dat.hr.gf"
partitionHourlyWrapper <- function(){
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
  return(dat.hr.gf)
}