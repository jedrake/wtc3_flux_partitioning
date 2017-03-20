
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#- Function to get and plot the root-to-shoot ratios from the final harvest
plot_harvest_root_mass_ratio <- function(export=T){
  #------------------------------------------------
  # read in the harvest data, compare to integrated chamber fluxes
  harvest <- read.csv("data/WTC_TEMP_CM_HARVEST-CANOPY_20140526-20140528_L1_v1.csv")
  harvest$tot <- rowSums(harvest[,c("BranchDM","StemDM","TotalLeafDM")])
  
  harvest.sum <- summaryBy(BranchDM+StemDM+TotalLeafDM+tot~chamber+T_treatment,data=harvest,FUN=sum,keep.names=T)
  
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
  aboveground <- harvest.sum
  treemass <- cbind(core3[,c(1,2)],coarse.roots[,"CoarseRootDM"],aboveground)
  treemass$tot <- NULL
  treemass$TotalMass <- rowSums(treemass[,c(2,3,6,7,8)])
  
  names(treemass) <- c("chamber","fineRoots","coarseRoots","chamber_2",
                       "T_treatment","branches","stem","leaves","TotalMass")
  #treemass$TotalMass_g <- treemass$TotalMass*0.47 # assume 47% C content
  #-----------------------------------------------------------------------------------------------------
  
  
  #- merge in the treatment key
  key <- read.csv("data/WTC_TEMP_CM_TREATKEY_20121212-20140528_L1_v1.csv")
  key2 <- subset(key,T_treatment !="reference" & Date=="2014-05-28",select=c("chamber","T_treatment",
                                                                             "Water_treatment"))
  
  treemass <- merge(treemass,key2,by=c("chamber","T_treatment"))
  treemass$T_treatment <- factor(treemass$T_treatment)
  treemass$root_shoot <- with(treemass,(fineRoots+coarseRoots)/(leaves+branches+stem))
  treemass$root_mass_ratio <- with(treemass,(fineRoots+coarseRoots)/(TotalMass))
  
  
  lm.rs.all <- lm(root_shoot~T_treatment*Water_treatment,data=treemass)
  lm.rs <- lm(root_shoot~T_treatment,data=subset(treemass,Water_treatment=="control"))
  anova(lm.rs.all)
  anova(lm.rs)
  
  
  lm.rmr.all <- lm(root_mass_ratio~T_treatment*Water_treatment,data=treemass)
  lm.rmr <- lm(root_mass_ratio~T_treatment,data=subset(treemass,Water_treatment=="control"))
  anova(lm.rmr.all)
  anova(lm.rmr)
  
  
  
  #-- make a stacked bar chart of aboveground and belowground mass for each treatment
  #- first average the data, then convert to wide
  treemass.m <- summaryBy(fineRoots+coarseRoots+leaves+branches+stem~T_treatment+Water_treatment,
                          data=treemass,FUN=mean,keep.names=T)
  
  treemass.l <- melt(treemass.m,id.vars=c("T_treatment","Water_treatment"))
  treemass.l$combo <- factor(paste(treemass.l$T_treatment,treemass.l$Water_treatment))
  
  data <- matrix(treemass.l[,c(4)],nrow=5,ncol=4,byrow=T)
  #Label the columns and rows
  colnames(data)=levels(treemass.l$combo)
  rownames(data)=levels(treemass.l$variable)
  data <- data[c(1,2,5,3,4),]
  
  #- get the standard error of total mass for each treatment
  treemass.se <- summaryBy(TotalMass~T_treatment+Water_treatment,data=treemass,FUN=c(mean,se))
  
  
  #------------- 
  if (export==T) {
    
    
    #- plot final mass and root:shoot ratio
    windows(60,40);par(mfrow=c(1,2),mar=c(6,8,2,1),cex.lab=2)
    
    #- make the stacked barchart of final mass
    xvals <- barplot(data,las=1,names.arg=c("A-Wet","A-Dry","W-Wet","W-Dry"),ylim=c(0,20000),xlab="Treatment",
                     col=c("black","grey","brown","orange","forestgreen"),cex.names=1.3,cex.axis=1.3)
    box()
    title(ylab=expression(Final~mass~(g)),line=4)
    adderrorbars(x=xvals,y=treemass.se$TotalMass.mean,SE=treemass.se$TotalMass.se,direction="updown")
    legend("topleft",legend=c("Leaves","Branches","Stem","Coarse roots","Fine roots"),
           fill=rev(c("black","grey","brown","orange","forestgreen")),cex=1.2)
    legend("topright",bty="n",letters[1],cex=1.2)
    palette(c(rev(brewer.pal(4,"Set1")[1:2]),brewer.pal(4,"Set1")[3:4]))
    
    #- plot root:shoot ratio
    boxplot(root_mass_ratio~Water_treatment+T_treatment,data=treemass,las=1,names=c("A-Wet","A-Dry","W-Wet","W-Dry"),
            xlab="Treatment",ylim=c(0.15,0.35),
            ylab="",cex.axis=1.3,col=palette()[c(1,3,2,4)])
    title(ylab="Root mass ratio",line=4)
    legend("topright",bty="n",letters[2],cex=1.2)
  }
  #return(treemass)
}
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------




