#--------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------
#- This long function compares the chamber flux sums across the experiment 
#    with an estimate of how much C the trees put on  during the same time 
#    (final harvest - intial mass estimate).
#--------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------
integrate_fluxes_growth <- function(dat.hr.gf=dat.hr.gf,cue.day=cue.day,treeMass=treeMass,plotson=F){
  
  
  
  #- create total sums across chambers
  dat.ch <- dplyr::summarize(group_by(dat.hr.gf,chamber,T_treatment),
                             FluxCO2_g=sum(FluxCO2_g,na.rm=T),
                             FluxH2O_kg=sum(FluxH2O_kg,na.rm=T),
                             GPP = sum(GPP,na.rm=T),
                             Ra = sum(Ra, na.rm=T))
  dat.ch <- as.data.frame(dat.ch)
  dat.ch$WUE <- dat.ch$FluxCO2_g/dat.ch$FluxH2O_kg
  
  
  
  #------------------------------------------------
  # read in the harvest data, compare to integrated chamber fluxes
  harvest <- read.csv("data/WTC_TEMP_CM_HARVEST-CANOPY_20140526-20140528_L1_v1.csv")
  #harvest <- read.csv(file="//ad.uws.edu.au/dfshare/HomesHWK$/30035219/My Documents/Work/HFE/WTC3/harvest/totalTreeLeafBranchStemMass_290714.csv")
  #harvest$tot <- rowSums(harvest[,2:4])
  harvest$tot <- rowSums(harvest[,c("BranchDM","StemDM","TotalLeafDM")])
  harvest.sum <- summaryBy(BranchDM+StemDM+TotalLeafDM+tot~chamber,data=harvest,FUN=sum,keep.names=T)
  
  
  harvest.roots <-  read.csv(file="data/WTC_TEMP_CM_HARVEST-ROOTS_20140529-20140606_L1_v1.csv")
  harvest.roots$CoarseRootDM <- rowSums(harvest.roots[,c(11:13)])
  
  #------
  #-- process fine root data
  core <-  read.csv(file="data/WTC_TEMP_CM_BIOMASS-ROOT-CORES_20140529_L1_v1.csv")
  core$FRD <-with(core,FineRootDM/(coreVol)) #grams root per cm3
  core <- core[complete.cases(core),]
  
  # figure out the vertical distance of each core in cm
  depthlist <- as.data.frame(do.call(rbind,strsplit(as.character(core$coreDepth),split="-")))
  length <- with(depthlist,as.numeric(as.character(V2))-as.numeric(as.character(V1)))
  core$length <- length
  
  #get the chamber diameters from the harvest dataset
  diams <- harvest.roots[,1:2]
  core2 <- merge(core,diams,by="chamber")
  core2$soilVol <- with(core2,pi*(chamberDiam/2)^2*length)
  
  core2$FRM <- with(core2,FRD*soilVol)
  
  #sum up the depths for each quadrat
  core2.sum <- summaryBy(FRM~chamber+quadrat,data=core2,FUN=sum,keep.names=T)
  
  #average each chamber
  core3 <- summaryBy(FRM~chamber,data=core2,FUN=c(mean,sd),keep.names=F) 
  #- finished with fine root data
  #------
  
  #---- merge aboveground biomass, coarse root, and fine root biomass estimates
  coarse.roots <- harvest.roots[complete.cases(harvest.roots),c("chamber","CoarseRootDM")]
  aboveground <- harvest.sum[,c("chamber","tot")]
  treemass <- cbind(core3[,c(1,2)],coarse.roots[,"CoarseRootDM"],aboveground[,"tot"])
  treemass$TotalMass <- rowSums(treemass[,c(2,3,4)])
  
  names(treemass) <- c("chamber","fineRoots","coarseRoots","aboveground","TotalMass")
  treemass$TotalMass_g <- treemass$TotalMass*0.47 # assume 47% C content
  #-----------------------------------------------------------------------------------------------------
  
  
  
  
  
  #- start here
  # #-----------------------------------------------------------------------------------------------------
  # #- this code from WTC_Rallometry_script.R in the WTC3 folder. I'm trying to estimate tree mass at the beginning of the experiment
  # #source("//ad.uws.edu.au/dfshare/HomesHWK$/30035219/My Documents/Work/HFE/WTC3/gx_wtc3/R/WTC fluxes/WTC_Rallometry_functions.R")
  # 
  # #wd.orig <- getwd()
  # #setwd("//ad.uws.edu.au/dfshare/HomesHWK$/30035219/My Documents/Work/HFE/WTC3/gx_wtc3")
  # 
  # #- get an estimate of d2h for every day of the experiment by interpolation
  # size <- returnd2h()
  # # get an estimate of volume and mass for wood and bark for each measurement day
  # vol <- getvol()
  # # interpolate volume for every day of the experiment
  # #vol.all <- gapfillvol(subset(vol,Measurement!=38))
  # vol.all <- gapfillvol(vol)
  # 
  # 
  # #- get branch mass
  # branchMass <- interpolateBranchDW(plotson=0) #  This needs to get updated with remko and court's method
  # stem_branch_mass <- merge(branchMass,vol.all,by.x=c("chamber","Date"),
  #                           by.y=c("chamber","DateTime"))
  # setwd(wd.orig)
  # #-
  # # now get an estimate of leaf mass for every day of the experiment
  # # get the interpolated leaf areas (laDaily)
  # wd <- getwd()
  # source("C:/Repos/wtc3_flux/R/LeafArea_Height_script.R")
  # setwd(wd)
  # 
  # # get a canopy-weighted SLA estimate
  # 
  # harvest <- read.csv("data/from HIEv/WTC_TEMP_CM_HARVEST-CANOPY_20140526-20140528_L1_v1.csv")
  # leafmass <- summaryBy(TotalLeafDM~chamber,data=harvest,FUN=sum,keep.names=F)
  # harvest2 <- merge(harvest,leafmass,by="chamber")
  # harvest2$weights <- with(harvest2,TotalLeafDM/TotalLeafDM.sum)
  # SLA <- plyr::ddply(harvest2,~chamber,summarise, SLA=weighted.mean(SLA,weights))
  # 
  # # merge SLA and leaf area to estimate leaf mass for each day
  # leafMass <- merge(laDaily,SLA,by=c("chamber"))
  # leafMass$leafMass <- with(leafMass,cumLA/SLA*10000)
  # 
  treeMass2 <- subset(treeMass,Date==as.Date("2013-09-14"))
  
  # merge branch, stem, and leaf biomass estimates together
  #treeMass2 <- merge(stem_branch_mass[,c(1,2,3,5,7,13,14)],leafMass[,c(1,2,4,5,6,9,10,11)],by.x=c("Date","chamber"),by.y=c("Date","chamber"))
  # assumes trees are 47%C by dry mass, and the (unmeasured) root component is 25% of the mass
  treeMass2$InitialTreeMass <- with(treeMass2,totMass*0.47*1.25)
  
  
  treeMassInitial <- treeMass2[,c("chamber","InitialTreeMass")]
  #-----------------------------------------------------------------------------------------------------
  
  
  
  
  
  #------------------- 
  #- compare flux sums to tree growth over the experiment
  compare <- cbind(dat.ch,treemass[,6],treeMassInitial[,2])
  names(compare)[8:9] <- c("treeMassFinal_g","treeMassInitial_g")
  compare$treeGrowth_g <- with(compare,treeMassFinal_g-treeMassInitial_g)
  
  
  if(plotson==T){
    #-- plot relationship between chamber flux sums and harvest sums. We may have underestimated fluxes?
    windows(20,20);par(mfrow=c(2,1))
    plot(treeGrowth_g~FluxCO2_g,data=compare,xlab="Chamber flux sum (g C)",ylab="Tree growth (Final mass - initial mass; g C)",
         pch=16,ylim=c(1000,9000),xlim=c(4000,11000),col=c("black","red"),cex.lab=1.3)
    textxy(X=compare$FluxCO2_g,Y=compare$treeGrowth_g,labs=dat.ch$chamber,cex=0.75)
    abline(0,1)
    title(main="Slope implies we missed ~50% of the respiration (belowground)")
    lm1 <- lm(treeGrowth_g~FluxCO2_g,data=compare)
    abline(lm1,lty=2)
    text(x=10000,y=3000,paste("Slope = ",unname(round(coef(lm1)[2],2))))
    text(x=9300,y=9000,"1:1")
    
    #- plot C gain relative to water loss
    plotBy(FluxCO2_g~FluxH2O_kg|T_treatment,data=dat.ch,pch=16,col=c("black","red"),legend=F,
           xlab="Total water loss (kg)",ylab="Total C gain (g C)",
           xlim=c(1700,4000),ylim=c(3300,12000))
    textxy(X=dat.ch$FluxH2O_kg,Y=dat.ch$FluxCO2_g,labs=dat.ch$chamber,cex=0.75)
    lm2 <- lm(FluxCO2_g~FluxH2O_kg,data=dat.ch)
    abline(lm2,lty=2)
    text(x=3500,y=4000,paste("Slope = ",unname(round(coef(lm2)[2],3))," g C / kg H2O"))
    
    #------------------------------------------------
  }
  
  
  
  
  
  
  
  #------------------------------------------------
  #-- analyze tree growth dataset to evaluate RGR and AGR between growth intervals
  #-- do these growth metrics relate to integrated fluxes during these times?
  
  
  #-----------
  #- process the growth data
  treeMass3 <- treeMass
  
  
  growth <- subset(treeMass3,is.na(Measurement)==F)
  growth <- growth[with(growth,order(chamber,Measurement)),]
  
  #- calculate change in time, change in mass, RGR, and AGR
  growth$dMass <- c(NA,diff(growth$totMass))
  growth[which(growth$dMass<0),"dMass"] <- NA
  
  growth$dTime <- c(NA,diff(growth$Date))
  growth[which(growth$dTime<0),"dTime"] <- NA
  
  growth$RGR <- c(NA,diff(log(growth$totMass)))*1000/growth$dTime # RGR in mg g-1 day-1
  growth[which(growth$RGR<0),"RGR"] <- NA
  
  growth$AGR <- c(NA,diff(growth$totMass))/growth$dTime           # AGR in g day-1
  growth[which(growth$RGR<0),"AGR"] <- NA
  
  #RGR declines over time from ~15 to 5 mg g-1 day-1
  #plotBy(RGR~Date|chamber,data=growth,legend=F,type="b")
  #plotBy(AGR~Date|chamber,data=growth,legend=F,type="b")
  
  #- get rid of the leading NA's in the growth dataset
  growth1 <- subset(growth,is.na(dMass)==F)
  growth <- growth1
  
  #-----------
  #- merge the growth and flux data
  
  #- loop over each line in the growth dataset
  growth$GPP <- NA
  growth$Ra <- NA
  growth$T_day <- NA
  growth$PAR <- NA
  growth$VPD <- NA
  growth$T_night <- NA
  growth$Tair_24hrs <- NA
  growth$Hloss <- NA
  growth$CUEi_mean <- NA
  for (i in 2:nrow(growth)){
    startdate <- growth$Date[i-1]
    enddate <- growth$Date[i]
    growth$GPP[i] <- sum(cue.day$GPP[which(cue.day$Date>=startdate & cue.day$Date < enddate & cue.day$chamber==growth$chamber[i])],na.rm=T)
    growth$Ra[i] <-  sum(cue.day$Ra[which(cue.day$Date>=startdate & cue.day$Date < enddate & cue.day$chamber==growth$chamber[i])],na.rm=T)
    growth$Hloss[i] <- sum(cue.day$Hloss[which(cue.day$Date>=startdate & cue.day$Date < enddate & cue.day$chamber==growth$chamber[i])],na.rm=T)
    
    growth$CUEi_mean[i] <- mean(cue.day$CUE[which(cue.day$Date>=startdate & cue.day$Date < enddate & cue.day$chamber==growth$chamber[i])],na.rm=T)
    
    growth$T_day[i] <- mean(cue.day$T_day[which(cue.day$Date>=startdate & cue.day$Date < enddate & cue.day$chamber==growth$chamber[i])],na.rm=T)
    growth$T_night[i] <- mean(cue.day$T_night[which(cue.day$Date>=startdate & cue.day$Date < enddate & cue.day$chamber==growth$chamber[i])],na.rm=T)
    growth$Tair_24hrs[i] <- mean(cue.day$Tair_24hrs[which(cue.day$Date>=startdate & cue.day$Date < enddate & cue.day$chamber==growth$chamber[i])],na.rm=T)
    growth$PAR[i] <- sum(cue.day$PAR[which(cue.day$Date>=startdate & cue.day$Date < enddate & cue.day$chamber==growth$chamber[i])],na.rm=T)
    growth$VPD[i] <- mean(cue.day$VPDair[which(cue.day$Date>=startdate & cue.day$Date < enddate & cue.day$chamber==growth$chamber[i])],na.rm=T)
    
  }
  
  growth2 <- growth[complete.cases(growth),]
  #growth2$CUE <- with(growth2,RGR/(Cgain/(dTime*totMass)))
  growth2$CUEa <- with(growth2,dMass*0.47/GPP)
  growth2$CUEi <- with(growth2,1-Ra/GPP)
  growth2$RtoA <- with(growth2,Ra/GPP)
  growth2$leafToTotal <- with(growth2,leafMass/totMass)
  growth2$totMass_c <- with(growth2,totMass*0.47)
  growth2$dMass_c <- with(growth2,dMass*0.47)
  growth2$WUEc <- with(growth2,dMass_c/Hloss)
  growth2$WUEi <- with(growth2,GPP/Hloss)
  growth2$resid <- with(growth2,GPP-dMass_c-Ra)
  growth2$residtoGPP <- with(growth2,resid/GPP)
  
  #- get the treatment key to see when chambers were droughted
  # key <- searchHIEv("WTC_TEMP_CM_TREATKEY")
  # downloadHIEv(key,topath="./data/from HIEv/")
  # key <- read.csv("./data/from HIEv/WTC_TEMP_CM_TREATKEY_20121212-20140528_L1_v1.csv")
  # 
  # growth2 <- merge(growth2,key,by=c("chamber","Date","T_treatment"))
  
  
  
  
  #- average across treatments
  growth2.trt <- summaryBy(RGR+AGR+totMass_c+dMass_c+GPP+Ra+resid+CUEa+CUEi+RtoA+residtoGPP+Hloss+WUEc+WUEi+T_day+T_night+Tair_24hrs+PAR+VPD~Date+T_treatment+Water_treatment,data=growth2,
                           FUN=c(mean,se),keep.names=T)
  
  return(list(growth2,growth2.trt))
}
