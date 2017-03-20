#--------------------------------------------------------------------------------------------------
# Estimate branch biomass through time in WTC3.
interpolateBranchDW <- function(vol.all=vol.all,plotson=0){
  
  bc <- read.csv("data/WTC_TEMP_CM_BRANCHCENSUS_20130910-20140516_L0_v1.csv")
  bc$Date <- as.Date(bc$Date)
  bc$branchid <- as.factor(paste(bc$chamber,bc$branchnumber,sep="-"))
  
  #--------------------------------------------------------------------------------------------------
  # okay, so now can we estimate branch mass allometrically at the harvest? 
  branches <- read.csv("data/WTC_TEMP_CM_GX-RBRANCH_20140513-20140522_L1_v1.csv")
  
  br.allom <- lm(log10(branch_dw)~log10(diam.m),data=branches) # fit log-log branch allometry
  if(plotson==1){
    windows(12,12)
    plotBy(branch_dw~diam.m|T_treatment,data=branches,log="xy",pch=15,col=c("black","red"),cex.lab=1.5,cex.axis=1.3,
           xlab= "Branch diameter (mm)",ylab="Branch mass (g)");abline(br.allom)
  }
  br.coef <- coef(br.allom) # get the coefficients
  
  #apply allometry to the branch census dataset
  bc$branch_dw <- 10^(br.coef[1]+br.coef[2]*log10(bc$branchdiameter))
  bc <- subset(bc,branch_dw>0)
  
  #get the layer heights
  layers <- read.csv("data/WTC_TEMP_CM_LAYERHEIGHT_20140519_L1.csv")
  cuts <- c(0,mean(layers$toplayer1fromfloor),mean(layers$toplayer2fromfloor))
  layer.df <- data.frame(layer=c("low","mid","top"),layerlimit=cuts)
  
  bc$layer <- ifelse(bc$heightinsertion<=cuts[2],"low",ifelse(bc$heightinsertion<=cuts[3],"mid","top"))
  #sum up estimate of branch mass for each date for each chamber
  bc.sum <- summaryBy(branch_dw~chamber+Date+layer,data=bc,FUN=sum,keep.names=T)
  palette(c("black","blue","green","forestgreen","darkmagenta","chocolate1","cyan","darkgrey","darkgoldenrod1",
            "brown1","darkred","deeppink"))
  #compare branch allometry number to the harvest number
  harvest <- read.csv("data/WTC_TEMP_CM_HARVEST-CANOPY_20140526-20140528_L1_v1.csv")
  harvest.sum <- summaryBy(BranchDM~chamber+layer,data=harvest,FUN=sum,keep.names=T)
  harvest.sum$branch_allom <- subset(bc.sum,Date==as.Date("2014-05-16"))$branch_dw
  
  if(plotson==1){
    windows(12,12)
    plotBy(branch_allom~BranchDM|layer,data=harvest.sum,xlab="Measured branch mass (g)",ylab="Allometric estimate (g)",cex.lab=1.3,xlim=c(0,1000),ylim=c(0,1000),pch=15);abline(0,1)
    textxy(X=harvest.sum$BranchDM,Y=harvest.sum$branch_allom,labs=harvest.sum$chamber,cex=0.9)
  }
  #--------------------------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------------------------
 
  
  bc.sum <- summaryBy(branch_dw~chamber+Date,data=bc,FUN=sum,keep.names=T)

  size2 <- merge(bc.sum,vol.all,by=c("Date","chamber"),all=T)
  
  #there is a nice relationship between tree volume and total branch mass
  if (plotson==1){
    windows(12,12)
    plotBy(branch_dw~vol|chamber,pch=15,type="b",cex=1.2,data=size2,log="xy",xlim=c(300,21000),
           xlab="Stem volume (cm3)",ylab="Branch mass (g)",cex.lab=1.3)
  }
  
  #- fit log-linear regression for each chamber, but all at onece
  lm.all <- lm(log10(branch_dw)~log10(vol)*chamber,data=size2)
  
  # fit log- linear regression for each chamber
  dat.l <- split(size2,size2$chamber)
  lms1 <- list()
  for (i in 1:length(dat.l)){
    tofit <- dat.l[[i]]
    lms1[[i]] <- lm(log10(branch_dw)~log10(vol),data=subset(tofit,branch_dw>0))
    if (plotson==1) abline(lms1[[i]],col=i)
  }
  lms1 <- as.data.frame(do.call(rbind,lapply(lms1,coef)))
  lms1$chamber <- levels(size2$chamber)
  names(lms1) <- c("int","slope","chamber")
  
  
  # merge allometric estimates into the large size dataframe
  size3 <- merge(size2,lms1,by="chamber",all=T)
  size3$branch_dw_est <- with(size3,10^(int+slope*log10(vol)))
  #--------------------------------------------------------------------------------------------------
  
  if (plotson==1){
    windows(12,12)
    plotBy(branch_dw_est~Date|chamber,data=size3,ylab="Branch mass (g)",cex.lab=1.3)
  }
  
  
  out <- size3[,c("chamber","Date","branch_dw_est")]
  out <- out[with(out,order(chamber,Date)),]
  names(out)[3] <- "branchMass"
  return(out)
}