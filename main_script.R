
#-------------------------------------------------------------------------------------------------------------------
#- Load the libraries that do all of the actual work
source("R/loadLibraries.R")

#- Download the data from Drake 2016 New Phyt, if it doesn't exist in the working directory.
downloadNewPhytData()
#-------------------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------------------
#- If fluxes have not been paritioned, partition net C fluxes into GPP and Ra.
#  Otherwise, just read in the paritioned file. Partitioned fluxes are in dataframe "dat.hr.gf".
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

#- plot soil moisture and pre-dawn leaf water potential over time (Figure 3)
plot_soil_moisture_LWPpd()

#- plot soil moisture as measured by the nuetron probe method (Figure 4)
plot_neutron_probe_data()

#- plot the final harvest data and the root mass ratio
plot_harvest_root_mass_ratio() #  interaction for root-mass-ratio (p = 0.07)

#- plot fluxes of GPP, NPPa, Ra, and residual  over time. 
plot_flux_terms_time(growth)

#- plot GPP partitioning over time. 
plot_partitioning_time(growth)

#- plot respiration, in growth vs. maintenence framework of Penning and DeVries. 
plot_respiration_growth_maintenence(growth)
#--------------------------------------------------------------------------------------------------







#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#- Small calculations, needed throughout the text
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

#- tree size upon floor installation
summaryBy(leafArea~T_treatment,data=subset(treeMassFlux,Date==as.Date("2013-09-14")))

#- get an estimate of d2h for every day of the experiment by interpolation
size <- returnd2h()
summaryBy(diam+Plant_height~T_treatment,data=subset(size,Date==as.Date("2013-09-14")))


#- get an estimate of volume and mass for wood and bark for each measurement day
print("Calculating stem volume")
vol <- getvol()
#- interpolate volume for every day of the experiment
print("Interpolating stem volume")
vol.all <- gapfillvol(vol)
summaryBy(vol~T_treatment,data=subset(vol.all,Date==as.Date("2013-09-14")))

