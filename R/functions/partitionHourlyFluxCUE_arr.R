#----------------------------------------------------------------------------------------------------------------
#- function to parition the hourly net flux observations into GPP and Ra using an Arrhenious function
partitionHourlyFluxCUE_arr <- function(dat.hr.gf=dat.hr.gf,Ea=57.69,lagdates,leafRtoTotal = 1,leafRreduction=0){
  rvalue = 8.134
  require(data.table)
  
  #-- convert mmolCO2 s-1 to gC hr-1
  dat.hr.gf$FluxCO2_g <- with(dat.hr.gf,FluxCO2*60*60/1000*12.0107)
  dat.hr.gf$period <- ifelse(dat.hr.gf$PAR>2,"Day","Night")
  
  #-- convert mol H2) s-1 to kg H2O hr-1
  dat.hr.gf$FluxH2O_kg <- with(dat.hr.gf,FluxH2O*60*60/1000*18.015)
  
  #-- partition day-time net C exchange into GPP and Ra, similar to how it is done in eddy-covariance.
  #-- create a series of dates
  date.vec <- seq.Date(from=min(dat.hr.gf$Date),to=max(dat.hr.gf$Date),by="day")
  
  #-- estimate R-Tref and Tref for each date for each chamber
  #lagDates <- 3 # establish how many prior days to include
  RTdat <- expand.grid(Date=date.vec,chamber=levels(dat.hr.gf$chamber))
  RTdat$Tref <- RTdat$R_Tref <- NA
  
  
  print("Partitioning Net CO2 fluxes into GPP and Ra")
  #- set up progress bar to track that this is working
  pb <- txtProgressBar(min = 0, max = nrow(RTdat), style = 3)
  
  for (i in 1:nrow(RTdat)){
    #- trial a data.table alternative to speed this up. The filter thing was actually slower.
    
    #dat <- dplyr::filter(dat.hr.gf,chamber==RTdat$chamber[i],Date <= RTdat$Date[i], Date >= (RTdat$Date[i]-lagDates),period =="Night")
    #RTdat$Tref[i] <- mean(dat$Tair_al,na.rm=T)
    #RTdat$R_Tref[i] <- mean(dat$FluxCO2_g,na.rm=T)
    
    inds <- which(dat.hr.gf$chamber==RTdat$chamber[i] & dat.hr.gf$Date <= RTdat$Date[i] & dat.hr.gf$Date >= (RTdat$Date[i]-lagdates) & dat.hr.gf$period =="Night" )
    RTdat$Tref[i] <- mean(dat.hr.gf$Tair_al[inds],na.rm=T)
    RTdat$R_Tref[i] <- mean(dat.hr.gf$FluxCO2_g[inds],na.rm=T)
    setTxtProgressBar(pb, i)
    
  }
  close(pb)
  
  RTdat$Tref_K <- with(RTdat,Tref+273.15)
  
  #-- merge these reference data into the gap-filled flux dataframe to estimate Ra during the daytime, and hence GPP
  dat.hr.gf3 <- merge(dat.hr.gf,RTdat,by=c("Date","chamber"))
  #dat.hr.gf3$Ra_est <- with(dat.hr.gf3,R_Tref*Q10^((Tair_al-Tref)/10)) # estimate respiration rate. This is a negative number.
  dat.hr.gf3$Ra_est <- with(dat.hr.gf3,R_Tref*exp((Ea*1000/(rvalue*Tref_K))*(1-Tref_K/(Tair_al+273.15)))) # estimate respiration rate. This is a negative number.
  dat.hr.gf3$Ra_est <- ifelse(dat.hr.gf3$period=="Day",
                              dat.hr.gf3$Ra_est-leafRreduction*leafRtoTotal*dat.hr.gf3$Ra_est,
                              dat.hr.gf3$Ra_est) # estimate respiration rate. This is a negative number. If it's day, subtract 30% from the leaf R fraction
  
  
  dat.hr.gf3$GPP <- ifelse(dat.hr.gf3$period=="Night",0,dat.hr.gf3$FluxCO2_g-dat.hr.gf3$Ra_est)
  dat.hr.gf3$Ra <- ifelse(dat.hr.gf3$period=="Night",dat.hr.gf3$FluxCO2_g,dat.hr.gf3$Ra_est)
  
  return(dat.hr.gf3)
}
#----------------------------------------------------------------------------------------------------------------
