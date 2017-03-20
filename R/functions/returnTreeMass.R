#----------------------------------------------------------------------------------------------------------------
#-- Code to return tree mass for every day of the experiment.
#- this code from WTC_Rallometry_script.R in the WTC3 folder. I'm trying to estimate tree mass at the beginning of the experiment
returnTreeMass <- function(plotson=F){
 
  #- get an estimate of d2h for every day of the experiment by interpolation
  size <- returnd2h()
  
  #- get an estimate of volume and mass for wood and bark for each measurement day
  print("Calculating stem volume")
  vol <- getvol()
  
  #- interpolate volume for every day of the experiment
  print("Interpolating stem volume")
  vol.all <- gapfillvol(vol)
  
  
  #- get branch mass
  branchMass <- interpolateBranchDW(vol.all=vol.all,plotson=0) 
  stem_branch_mass <- merge(vol.all,branchMass,by=c("chamber","Date"))
  
  
  
  #- now get an estimate of leaf mass for every day of the experiment
  # get the interpolated leaf areas (laDaily)
  
  dat.hr <- data.frame(data.table::fread("data/WTC_TEMP_CM_WTCFLUX_20130914-20140526_L2_V2.csv"))
  dat.hr$DateTime <- as.POSIXct(dat.hr$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
  dat.hr$Date <- as.Date(dat.hr$DateTime)
  dat.hr$chamber <- as.factor(dat.hr$chamber)
  laDaily <- summaryBy(leafArea~Date+chamber,data=dat.hr,keep.names=T,na.rm=T)
  
  # get a canopy-weighted SLA estimate
  #setwd("//ad.uws.edu.au/dfshare/HomesHWK$/30035219/My Documents/Work/HFE/WTC3/gx_wtc3/")
  harvest <- read.csv("data/WTC_TEMP_CM_HARVEST-CANOPY_20140526-20140528_L1_v1.csv")
  leafmass <- summaryBy(TotalLeafDM~chamber,data=harvest,FUN=sum,keep.names=F)
  harvest2 <- merge(harvest,leafmass,by="chamber")
  harvest2$weights <- with(harvest2,TotalLeafDM/TotalLeafDM.sum)
  SLA <- plyr::ddply(harvest2,~chamber,summarise, SLA=weighted.mean(SLA,weights))
  
  # merge SLA and leaf area to estimate leaf mass for each day
  leafMass <- merge(laDaily,SLA,by=c("chamber"))
  leafMass$leafMass <- with(leafMass,leafArea/SLA*10000)
  
  treeMass1 <- merge(stem_branch_mass,leafMass,by=c("chamber","Date"))
  treeMass1$boleMass <- with(treeMass1,mass_wood+mass_bark)
  treeMass1$totMass <- rowSums(treeMass1[,c("boleMass","branchMass","leafMass")])
  treeMass <-subset(treeMass1,select=c("chamber","T_treatment","Water_treatment","Date","Measurement","Days_since_transplanting","branchMass","boleMass","leafMass","totMass","SLA"))
  
  print("Done. Returned treeMass")
  return(treeMass)
}
#----------------------------------------------------------------------------------------------------------------