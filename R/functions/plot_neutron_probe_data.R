#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#- Process and plot the neutron probe data
plot_neutron_probe_data <- function(){
  
  
  
  #-----------------------------------------------------------------------------------------------------------
  #- read in the data
  
  #- get all the filenames. Remove the one that was incomplete and "not used"
  files <-list.files(path="data/neutron_probe/",pattern="WT")
  files <- files[-grep("notused",files)]
  
  
  # loop over each file, read in data
  dat.raw1 <- list()
  for(i in 1:length(files)){
    #print(i)
    nrow=19
    if(i==16)nrow=18
    dat.temp <- read.table(file=paste("data/neutron_probe/",files[i],sep=""),skip=22,nrow=nrow,header=T)
    dat.temp <- dat.temp[-which(dat.temp$ID==999),] #get rid of first row
    
    #add "Up.depths" as rownames
    names(dat.temp)[3:15] <- c(475,425,375,325,275,225,175,150,125,100,75,50,25)
    
    #- wide to long format
    names <- names(dat.temp)[4:15]
    dat.long1 <- reshape(dat.temp, 
                         varying = names, 
                         v.names = "count",
                         timevar = "depth", 
                         times = names,
                         direction = "long")
    
    #- add a chamber variable
    dat.long <- subset(dat.long1,ID<=12)
    dat.long$chamber <- as.factor(paste0("C", sprintf("%02.0f", dat.long$ID)))
    dat.long$depth <- as.numeric(dat.long$depth)
    
    #- add a date variable from the filename
    dat.long$Date <- as.Date(substr(files[i],start=3,stop=8),format="%d%m%y")
    
    dat.raw1[[i]] <- dat.long[,c("chamber","Date","depth","count")]
  }
  probedat1 <- do.call(rbind,dat.raw1)
  
  
  #-- Apply Teresa's corrections
  # Convert from NP raw count to VWC using the in-house calibration for each texture class (clay & non-clay)
  probedat.up <- subset(probedat1,depth<=325)
  probedat.up$VWC <- (-4.567589 + 0.002054116 * probedat.up$count)/100
  probedat.deep <- subset(probedat1, depth >= 375)
  probedat.deep$VWC <- (-18.21883 + 0.00363426 * probedat.deep$count)/100
  probedat <- rbind(probedat.up, probedat.deep)
  
  
  #-- Merge with the treatment key
  #- get the d2h dataset, for the treatment key
  d2h <- returnd2h()
  dat <- merge(probedat,d2h,by=c("Date","chamber"))[,c("Date","chamber","T_treatment","Water_treatment","depth","count","VWC")]
  
  
  
  
  
  #- average across dates, treatments and depths
  # Note that we will look only at data >= 150 cm depth, as this depth was not affected by 
  #    the irrigation. Shallow depths were affected.
  dat.m <- summaryBy(VWC~Date+Water_treatment+depth,data=dat,FUN=mean,keep.names=T)
  
  plotBy(VWC~Date|Water_treatment,data=subset(dat.m,depth==25),legendwhere="bottomleft",type="o")
  plotBy(VWC~Date|Water_treatment,data=subset(dat.m,depth==50),legendwhere="bottomleft",type="o")
  plotBy(VWC~Date|Water_treatment,data=subset(dat.m,depth==75),legendwhere="bottomleft",type="o")
  plotBy(VWC~Date|Water_treatment,data=subset(dat.m,depth==100),legendwhere="bottomleft",type="o")
  plotBy(VWC~Date|Water_treatment,data=subset(dat.m,depth==125),legendwhere="bottomleft",type="o")
  plotBy(VWC~Date|Water_treatment,data=subset(dat.m,depth==150),legendwhere="bottomleft",type="o")
  plotBy(VWC~Date|Water_treatment,data=subset(dat.m,depth==175),legendwhere="bottomleft",type="o")
  plotBy(VWC~Date|Water_treatment,data=subset(dat.m,depth==225),legendwhere="bottomleft",type="o")
  plotBy(VWC~Date|Water_treatment,data=subset(dat.m,depth==275),legendwhere="bottomleft",type="o")
  plotBy(VWC~Date|Water_treatment,data=subset(dat.m,depth==325),legendwhere="bottomleft",type="o")
  plotBy(VWC~Date|Water_treatment,data=subset(dat.m,depth==375),legendwhere="bottomleft",type="o")
  plotBy(VWC~Date|Water_treatment,data=subset(dat.m,depth==425),legendwhere="bottomleft",type="o")
  
  
  
  
  
  
  
  #-----------------------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------------------
  #- calculate the proportional water uptake for each depth, comparing the measurements prior to the 
  #  drought (on 2014-2-11) and at teh end of the drought (on 2014-5-09)
  
  # subset to the two relevant dates
  probedat.pre <- subset(dat,Date==as.Date("2014-2-17"))
  names(probedat.pre)[6:7] <- c("count.pre","VWC.pre")
  probedat.post <- subset(dat,Date==as.Date("2014-4-24"))
  names(probedat.post)[6:7] <- c("count.post","VWC.post")
  
  # merge, calculate the difference, and average
  probedat.diff <- merge(probedat.pre,probedat.post,by=c("chamber","depth","Water_treatment","T_treatment"))
  probedat.diff$count.diff <- with(probedat.diff,count.post-count.pre)
  probedat.diff$VWC.diff <- with(probedat.diff,VWC.post-VWC.pre)
  
  probedat.diff.m <- summaryBy(VWC.diff~depth+Water_treatment+T_treatment,data=probedat.diff,FUN=c(mean,se),keep.names=F)
  
  #-----------------------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------------------
  
  windows(20,20)
  par(cex.lab=2,mar=c(6,7,1,1))
  plotBy(VWC.diff.mean~depth-5|T_treatment,legend=F,ylim=c(-0.05,0.05),cex=1.5,xlim=c(0,450),
         data=subset(probedat.diff.m,Water_treatment=="control" & depth>=0),col=c("blue","red"),pch=16,type="o",
         ylab=expression(Change~"in"~VWC~(m^3~m^-3)),xlab=expression(Depth~(cm)))
  adderrorbars(x=subset(probedat.diff.m,Water_treatment=="control" & depth>=0)$depth-5,
               y=subset(probedat.diff.m,Water_treatment=="control" & depth>=0)$VWC.diff.mean,
               SE=subset(probedat.diff.m,Water_treatment=="control" & depth>=0)$VWC.diff.se,
               direction="updown",col=c("blue","red"),barlen=0)
  plotBy(VWC.diff.mean~depth|T_treatment,legend=F,add=T,cex=1.5,
         data=subset(probedat.diff.m,Water_treatment=="drydown" & depth>=0),col=c("blue","red"),pch=1,type="o",lty=2)
  adderrorbars(x=subset(probedat.diff.m,Water_treatment=="drydown" & depth>=0)$depth,
               y=subset(probedat.diff.m,Water_treatment=="drydown" & depth>=0)$VWC.diff.mean,
               SE=subset(probedat.diff.m,Water_treatment=="drydown" & depth>=0)$VWC.diff.se,
               direction="updown",col=c("blue","red"),barlen=0)
  
  abline(h=0,lty=3)
  legend("topright",legend=c("A-Con","A-Dry","W-Con","W-Dry"),ncol=2,pch=c(21),
         seg.len=3,bty="n",lwd=2,pt.lwd=1,col=c("blue","blue","red","red"),
         pt.bg=c("blue","white","red","white"),pt.cex=1.5,cex=1.2,lty=c(1,2,1,2))
}
#-----------------------------------------------------------------------------------------------------------