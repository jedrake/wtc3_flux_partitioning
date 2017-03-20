
#- function to summarize the GPP, Ra, and CUE data by day. Returns a list of (1) daily averages by chamber and (2) daily averages by treatment
returnCUEdaily <- function(treeMassFlux=treeMassFlux){
  
  treeMassFlux$PAR_mol <- treeMassFlux$PAR*60*60*1*10^-6
  treeMassFlux$FluxH2O_kg <- treeMassFlux$FluxH2O*60*60/1000*18
  
  #- create daily sums 
  dat.day <- dplyr::summarize(group_by(treeMassFlux,Date,chamber,T_treatment,Water_treatment,period),
                              GPP = sum(GPP,na.rm=T),
                              Ra = sum(Ra,na.rm=T),
                              FluxCO2_g=sum(FluxCO2_g,na.rm=T),
                              FluxH2O_kg=sum(FluxH2O_kg,na.rm=T),
                              Tair_al=mean(Tair_al,na.rm=T),
                              VPDair=max(VPDair,na.rm=T),
                              PAR=sum(PAR_mol,na.rm=T),
                              branchMass=mean(branchMass,na.rm=T),
                              boleMass=mean(boleMass,na.rm=T),
                              leafMass=mean(leafMass,na.rm=T),
                              leafArea=mean(leafArea,na.rm=T))
  dat.day <- as.data.frame(dat.day)
  dat.day <- dat.day[with(dat.day,order(Date,chamber)),]
  names(dat.day)[1] <- "Date"
  
  Tair_day <- dplyr::summarize(group_by(treeMassFlux,Date,chamber,T_treatment,Water_treatment),
                               Tair_al=mean(Tair_al,na.rm=T))
  names(Tair_day)[5] <- "Tair_24hrs"                   
  
  dat.day_tair <- as.data.frame(Tair_day)
  dat.day <- as.data.frame(dat.day)
  
  #- merge night and day data
  dat.day2d <- subset(dat.day,period=="Day")[,c("Date","chamber","T_treatment","Water_treatment","GPP","Ra","FluxCO2_g","FluxH2O_kg","PAR","Tair_al","VPDair","branchMass","boleMass","leafMass","leafArea")]
  names(dat.day2d)[6:10] <- c("Raday","Cgain","Hloss_day","PAR","T_day")
  dat.day2n <- subset(dat.day,period=='Night')[,c("Date","chamber","T_treatment","Water_treatment","Ra","FluxCO2_g","FluxH2O_kg","Tair_al","VPDair")]
  names(dat.day2n)[5:9] <- c("Ranight","Closs","Hloss_night","T_night","VPDair_night")
  names(dat.day2n)[1] <- c("Date")
  
  #- merge data such that the nightly data are one day ahead of the daily data (i.e., does tonight's respiration depend on today's photosynthesis?)
  #dat.day2n$Date <- dat.day2n$Date_preceding_day+1
  
  cue.day1 <- subset(merge(dat.day2d,dat.day2n,by=c("Date","chamber","T_treatment","Water_treatment")))#,Closs<0 & Cgain>0)
  cue.day <- merge(cue.day1,dat.day_tair,by.x=c("Date","chamber","T_treatment","Water_treatment"),by.y=c("Date","chamber","T_treatment","Water_treatment"))
  cue.day$Ra <- with(cue.day,-1*(Raday+Ranight))
  cue.day$RtoA <- with(cue.day,Ra/GPP)
  cue.day$Hloss <- with(cue.day,(Hloss_night+Hloss_day))
  cue.day$WUE <- with(cue.day,(GPP)/(Hloss))
  cue.day$CUE <- with(cue.day,(1-(Ra/GPP)))
  cue.day$GPP_la <- with(cue.day,GPP/leafArea)
  cue.day$Ra_mass <- with(cue.day,Ra/(branchMass+boleMass+leafMass)*1000) #mg C g-1 day-1
  cue.day$Ra_la <- with(cue.day,Ra/(leafArea)) #g C m-2 day-1
  
  cue.day <- cue.day[-which(cue.day$Ra<0),]
  
  
  cue.day.trt <- summaryBy(GPP+GPP_la+Ra+Ra_mass+Ra_la+RtoA+Hloss+CUE+PAR+T_day+T_night+Tair_24hrs+VPDair+VPDair_night~Date+T_treatment+Water_treatment,data=cue.day,FUN=c(mean,se))
  return(list(cue.day,cue.day.trt))
}