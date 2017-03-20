
#-------------------------------------------------------------------------------------------------------------------
#- Load the libraries that do all of the actual work
source("R/loadLibraries.R")

#- Download the data from Drake 2016 New Phyt
downloadNewPhytData()
#-------------------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------------------
#- If fluxes have not been paritioned, partition net C fluxes into GPP and Ra.
#  Otherwise, just read in the paritioned file. Partitioned fluxes are in dataframe "dat.hr.gf"
dat.hr.gf <- partitionHourlyWrapper()
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
#- plot tree size over time


#-------------------
#- Get the three direct observations of leaf area. They happened on 9 Sept 2013, 10 Feb 2014, and
#    during the harvest at ~25 May 2014.
treeMass <- read.csv("data/WTC_TEMP_CM_WTCFLUX_20130914-20140526_L2_V2.csv")
treeMass.sub <- subset(treeMass,as.Date(DateTime) %in% as.Date(c("2013-09-14","2014-02-10","2014-05-27")))
treeMass.sub$Date <- as.Date(treeMass.sub$DateTime)
leafArea <- summaryBy(leafArea~Date+chamber+T_treatment+Water_treatment,data=treeMass.sub,FUN=c(mean),keep.names=T)
leafArea1 <- summaryBy(leafArea~Date+T_treatment+Water_treatment,data=leafArea,FUN=c(mean,se))

#-------------------
#- get an estimate of volume and mass for wood and bark for each measurement day
vol <- getvol()
vol$diam <- vol$diam/10      # convert to cm
vol$height <- vol$height/100 # convert to m
vol$vol <- vol$vol/10000     # convert to m3

size.m <- summaryBy(diam+height+vol~DateTime+T_treatment+Water_treatment,
                    data=subset(vol,Days_since_transplanting>0),FUN=c(mean,se))






#--------------------------------------------------------------------------------------------------
#- plot GPP partitioning over time
plot_partitioning_time(growth)
#--------------------------------------------------------------------------------------------------





