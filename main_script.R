
#-------------------------------------------------------------------------------------------------------------------
#- Load the libraries that do all of the actual work
source("R/loadLibraries.R")
#-------------------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------------------
#- Download the data associated with Drake et al. 2016 New Phyt.
#  This downloads the zipfile from figshare and extracts it to a folder named "data".
if(!file.exists("data.zip")){
  download.file("https://ndownloader.figshare.com/files/4857112?private_link=cdc9a3caf5bffc0add94", "data.zip", mode="wb")
  unzip("data.zip",overwrite=F)
}
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
treeMassFlux <- merge(dat.hr.gf,treeMass,by=c("chamber","Date","T_treatment","Water_treatment"))

#- create daily sums of GPP, Ra, and CUE
cue.list <- returnCUEdaily(treeMassFlux)
cue.day <- cue.list[[1]]
cue.day.trt <- cue.list[[2]]
#--------------------------------------------------------------------------------------------------











#--------------------------------------------------------------------------------------------------
#-- integrate chamber flux and growth data
growth.list <- integrate_fluxes_growth(dat.hr.gf=dat.hr.gf,cue.day=cue.day,treeMass=treeMass,plotson=F)
growth <- growth.list[[1]]
growth.trt <- subset(growth.list[[2]],Date<=as.Date("2014-5-13"))
#--------------------------------------------------------------------------------------------------







#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#- Plot partitioning over time


part <- summaryBy(CUEa+Hloss+RtoA+residtoGPP~Date+T_treatment+Water_treatment,data=subset(growth, Date < as.Date("2014-5-1")),na.rm=T,FUN=c(mean,se))
part.ac <- subset(part,T_treatment=="ambient" & Water_treatment=="control")
part.ec <- subset(part,T_treatment=="elevated" & Water_treatment=="control")
part.ad <- subset(part,T_treatment=="ambient" & Water_treatment=="drydown")
part.ed <- subset(part,T_treatment=="elevated" & Water_treatment=="drydown")

windows(30,40);par(mfrow=c(3,1),mar=c(0,0,0,0),oma=c(6,7,3,3))
ptsize <- 1.5
#NPPa/GPP
plotBy(CUEa.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),ylim=c(0.1,0.7),pch=16,cex=ptsize,
       type="b",col=c("blue","red"),legend=F,axes=F,lwd=2)

magaxis(side=c(2,4),labels=c(1,1),las=1,frame.plot=T)
axis.Date(1, at = seq(as.Date("2012/1/1"), as.Date("2014/12/1"), by="month"),
          labels = F, tcl = 0.4,format="%m/%Y",las=3,cex.axis=1.5)
title(ylab="NPPa/GPP",outer=T,adj=0.9,cex.lab=1.5)
adderrorbars(x=part.ac$Date,y=part.ac$CUEa.mean,SE=part.ac$CUEa.se,
             col="blue",direction="updown",barlen=0)
adderrorbars(x=part.ec$Date,y=part.ec$CUEa.mean,SE=part.ec$CUEa.se,
             col="red",direction="updown",barlen=0)
adderrorbars(x=part.ad$Date,y=part.ad$CUEa.mean,SE=part.ad$CUEa.se,
             col="blue",direction="updown",barlen=0)
adderrorbars(x=part.ed$Date,y=part.ed$CUEa.mean,SE=part.ed$CUEa.se,
             col="red",direction="updown",barlen=0)
plotBy(CUEa.mean~Date|T_treatment,data=subset(part,Water_treatment=="drydown"),ylim=c(0.2,0.7),pch=1,cex=ptsize,
       type="b",col=c("blue","red"),legend=F,axes=F,lwd=2,add=T,lty=2)


#Ra/GPP
plotBy(RtoA.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),ylim=c(0,0.7),pch=16,cex=ptsize,
       type="b",col=c("blue","red"),legend=F,axes=F,lwd=2)

magaxis(side=c(2,4),labels=c(1,1),las=1,frame.plot=T)
axis.Date(1, at = seq(as.Date("2012/1/1"), as.Date("2014/12/1"), by="month"),
          labels = F, tcl = 0.4,format="%m/%Y",las=3,cex.axis=1.5)
title(ylab="Ra/GPP",outer=T,adj=0.5,cex.lab=1.5)
adderrorbars(x=part.ac$Date,y=part.ac$RtoA.mean,SE=part.ac$RtoA.se,
             col="blue",direction="updown",barlen=0)
adderrorbars(x=part.ec$Date,y=part.ec$RtoA.mean,SE=part.ec$RtoA.se,
             col="red",direction="updown",barlen=0)
adderrorbars(x=part.ad$Date,y=part.ad$RtoA.mean,SE=part.ad$RtoA.se,
             col="blue",direction="updown",barlen=0)
adderrorbars(x=part.ed$Date,y=part.ed$RtoA.mean,SE=part.ed$RtoA.se,
             col="red",direction="updown",barlen=0)
plotBy(RtoA.mean~Date|T_treatment,data=subset(part,Water_treatment=="drydown"),ylim=c(0,0.7),pch=1,cex=ptsize,
       type="b",col=c("blue","red"),legend=F,axes=F,lwd=2,add=T,lty=2)

#Residual
plotBy(residtoGPP.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),ylim=c(0,0.7),pch=16,cex=ptsize,
       type="b",col=c("blue","red"),legend=F,axes=F,lwd=2)

magaxis(side=c(2,4),labels=c(1,1),las=1,frame.plot=T)
axis.Date(1, at = seq(as.Date("2012/1/1"), as.Date("2014/12/1"), by="month"),
          labels = T, tcl = 0.4,format="%m/%Y",las=3,cex.axis=1.5)
title(ylab="Residual/GPP",outer=T,adj=0.1,cex.lab=1.5)
adderrorbars(x=part.ac$Date,y=part.ac$residtoGPP.mean,SE=part.ac$residtoGPP.se,
             col="blue",direction="updown",barlen=0)
adderrorbars(x=part.ec$Date,y=part.ec$residtoGPP.mean,SE=part.ec$residtoGPP.se,
             col="red",direction="updown",barlen=0)
adderrorbars(x=part.ad$Date,y=part.ad$residtoGPP.mean,SE=part.ad$residtoGPP.se,
             col="blue",direction="updown",barlen=0)
adderrorbars(x=part.ed$Date,y=part.ed$residtoGPP.mean,SE=part.ed$residtoGPP.se,
             col="red",direction="updown",barlen=0)
plotBy(residtoGPP.mean~Date|T_treatment,data=subset(part,Water_treatment=="drydown"),ylim=c(0,0.7),pch=1,cex=ptsize,
       type="b",col=c("blue","red"),legend=F,axes=F,lwd=2,add=T,lty=2)
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------





