
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
#- F I G U R E S
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

#- plot tree size over time (Figure 1)
plotsize()

#- plot CO2 and H2O fluxes over time (Figure 2)
plot_fluxes_time_sum(cue.day)

#- plot GPP partitioning over time
plot_partitioning_time(growth)
#--------------------------------------------------------------------------------------------------





