#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#- plot metrics of size over time.

plotsize <- function(output=T){
  
  
  
  #------------------------------------------------------------------------------------------------------------
  # #- get initial tree size 
  size <- read.csv("data/WTC_TEMP_CM_TREE-HEIGHT-DIAMETER_20121120-20140527_L1_V2.CSV")
  size$chamber_n <- as.numeric(substr(size$chamber,start=2,stop=3))
  size$DateTime <- as.Date(size$DateTime)
  
  summaryBy(Plant_height~T_treatment,data=subset(size,DateTime==as.Date("2012-12-12")),FUN=c(mean,se))
  #------------------------------------------------------------------------------------------------------------
  
  
  
  
  #------------------------------------------------------------------------------------------------------------
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
  vol$vol <- vol$vol/1000000     # convert to m3
  
  size.m <- summaryBy(diam+height+vol~DateTime+T_treatment+Water_treatment,
                      data=subset(vol,Days_since_transplanting>0),FUN=c(mean,se))
  
  
  
  #------------------------------------------------------------------------------------------------------------
  # why do diameter and height converge later in the experiment, but volume does not?
  windows(80,50)
  par(mfrow=c(1,2),cex.lab=2,mar=c(6,6,1,1))
  plot(vol~diam,data=vol,col=T_treatment,pch=16)
  legend("topleft",title="Treatment",pch=16,col=palette()[1:2],legend=c("Ambient","Warmed"),cex=1.5)
  plot(vol~height,data=vol,col=T_treatment,pch=16)
  #------------------------------------------------------------------------------------------------------------
  
  
  
  #------------------------------------------------------------------------------------------------------------
  #- statistical analysis of volume increment during the drought period
  vol2 <- subset(vol,Days_since_transplanting>340)
  
  #- calculate volume increment (note, data are already sorted by chamber). NA fill every eigth value
  vol2$dvol <- c(diff(vol2$vol),NA)
  tonafill <- which(vol2$Measurement==40)
  vol2$dvol[tonafill] <- NA
  
  vol2$Datefac <- factor(vol2$DateTime)
  lm.dvol <- lme(dvol~Datefac*T_treatment*Water_treatment,random=(~1|chamber),data=subset(vol2,!is.na(dvol)))
  anova(lm.dvol,test="marginal")
  
  #- final volume has no statistical differences
  lm1 <- lm(vol~T_treatment*Water_treatment,data=subset(vol,Days_since_transplanting==441))
  Anova(lm1,test="F")
  anova(lm1,test="marginal")
  #------------------------------------------------------------------------------------------------------------
  
  
  
  windows(40,40);par(mfrow=c(2,2),mar=c(0,5.5,0,3),oma=c(5,2,2,0),las=1,cex.axis=1.5)
  palette(c("#377EB8","#E41A1C"))
  
  #- plot diameter
  xlims <-as.Date(c("2013-3-1","2014-6-1"))
  yearlims <-as.Date(c("2013-1-1","2014-1-1"))
  plotBy(diam.mean~DateTime|T_treatment,data=subset(size.m,Water_treatment=="control"),pch=16,type="o",ylim=c(0,8),
         xlim=xlims,
         xaxt="n",yaxt="n",xlab="",ylab="",legend=F)
  adderrorbars(x=subset(size.m,Water_treatment=="control")$DateTime,
               y=subset(size.m,Water_treatment=="control")$diam.mean,barlen=0.0,
               SE=subset(size.m,Water_treatment=="control")$diam.se,direction="updown",
               col=c("#377EB8","#E41A1C"))       
  plotBy(diam.mean~DateTime|T_treatment,data=subset(size.m,Water_treatment=="drydown"),pch=1,type="o",add=T,legend=F)
  adderrorbars(x=subset(size.m,Water_treatment=="drydown")$DateTime,
               y=subset(size.m,Water_treatment=="drydown")$diam.mean,barlen=0.0,
               SE=subset(size.m,Water_treatment=="drydown")$diam.se,direction="updown",
               col=c("#377EB8","#E41A1C"))    
  magaxis(side=c(2,4),labels=c(1,1),frame.plot=T,majorn=3,las=1)
  axis.Date(side=1,at=seq.Date(from=xlims[1],to=xlims[2],by="month"),tcl=0.25,labels=F)
  axis.Date(side=1,at=seq.Date(from=xlims[1],to=xlims[2],by="quarter"),tcl=0.75,labels=F)
  title(ylab=expression(Diameter~(cm)),cex.lab=2)
  abline(v=as.Date("2013-9-13"),lty=2)
  #text(x=as.Date("2013-9-13"),y=8.8,labels="Flux measurements begin",xpd=NA,cex=1.3)
  legend("topleft",pch=c(16,16,1,1),lty=c(1),col=c("#377EB8","#E41A1C"),seg.len=1.5,
         legend=c("A-Con","W-Con","A-Dry","W-Dry"),bty="n",cex=1.2)
  legend("bottomright","a",bty="n",inset=0.002,cex=1.2)
  
  
  #-- plot leaf area over time
  plotBy(leafArea.mean~Date|T_treatment,data=subset(leafArea1,Water_treatment=="control"),pch=16,type="p",ylim=c(0,25),cex=1.5,
         xlim=xlims,
         xaxt="n",yaxt="n",xlab="",ylab="",legend=F)
  adderrorbars(x=subset(leafArea1,Water_treatment=="control")$Date,
               y=subset(leafArea1,Water_treatment=="control")$leafArea.mean,barlen=0.0,
               SE=subset(leafArea1,Water_treatment=="control")$leafArea.se,direction="updown",
               col=c("#377EB8","#E41A1C"))
  plotBy(leafArea.mean~Date|T_treatment,data=subset(leafArea1,Water_treatment=="drydown"),pch=1,type="p",add=T,legend=F,cex=1.5)
  adderrorbars(x=subset(leafArea1,Water_treatment=="drydown")$Date,
               y=subset(leafArea1,Water_treatment=="drydown")$leafArea.mean,barlen=0.0,
               SE=subset(leafArea1,Water_treatment=="drydown")$leafArea.se,direction="updown",
               col=c("#377EB8","#E41A1C"))
  magaxis(side=c(2,4),labels=c(1,1),frame.plot=T,majorn=3,las=1)
  axis.Date(side=1,at=seq.Date(from=xlims[1],to=xlims[2],by="month"),tcl=0.25,labels=F)
  axis.Date(side=1,at=seq.Date(from=xlims[1],to=xlims[2],by="quarter"),tcl=0.75,labels=F)
  title(ylab=expression(Total~leaf~area~(m^2)),cex.lab=2)
  abline(v=as.Date("2013-9-13"),lty=2)
  legend("bottomright","c",bty="n",inset=0.002,cex=1.2)
  
  
  #- plot height
  plotBy(height.mean~DateTime|T_treatment,data=subset(size.m,Water_treatment=="control"),pch=16,type="o",ylim=c(0,11),
         xlim=xlims,
         xaxt="n",yaxt="n",xlab="",ylab="",legend=F)
  adderrorbars(x=subset(size.m,Water_treatment=="control")$DateTime,
               y=subset(size.m,Water_treatment=="control")$height.mean,barlen=0.0,
               SE=subset(size.m,Water_treatment=="control")$height.se,direction="updown",
               col=c("#377EB8","#E41A1C"))
  plotBy(height.mean~DateTime|T_treatment,data=subset(size.m,Water_treatment=="drydown"),pch=1,type="o",add=T,legend=F)
  adderrorbars(x=subset(size.m,Water_treatment=="drydown")$DateTime,
               y=subset(size.m,Water_treatment=="drydown")$height.mean,barlen=0.0,
               SE=subset(size.m,Water_treatment=="drydown")$height.se,direction="updown",
               col=c("#377EB8","#E41A1C"))
  
  magaxis(side=c(2,4),labels=c(1,1),frame.plot=T,majorn=3,las=1)
  axis.Date(side=1,at=seq.Date(from=xlims[1],to=xlims[2],by="month"),tcl=0.25,labels=F)
  axis.Date(side=1,at=seq.Date(from=xlims[1],to=xlims[2],by="quarter"),tcl=0.75,labels=T,las=2,
            format="%b")
  title(ylab=expression(Height~(m)),cex.lab=2)
  abline(v=as.Date("2013-9-13"),lty=2)
  legend("bottomright","b",bty="n",inset=0.002,cex=1.2)
  
  
  #- plot stem volume
  plotBy(vol.mean~DateTime|T_treatment,data=subset(size.m,Water_treatment=="control"),pch=16,type="o",
         ylim=c(0,0.02),
         xlim=xlims,
         xaxt="n",yaxt="n",xlab="",ylab="",legend=F)
  adderrorbars(x=subset(size.m,Water_treatment=="control")$DateTime,
               y=subset(size.m,Water_treatment=="control")$vol.mean,barlen=0.0,
               SE=subset(size.m,Water_treatment=="control")$vol.se,direction="updown",
               col=c("#377EB8","#E41A1C"))
  plotBy(vol.mean~DateTime|T_treatment,data=subset(size.m,Water_treatment=="drydown"),pch=1,type="o",add=T,legend=F)
  adderrorbars(x=subset(size.m,Water_treatment=="drydown")$DateTime,
               y=subset(size.m,Water_treatment=="drydown")$vol.mean,barlen=0.0,
               SE=subset(size.m,Water_treatment=="drydown")$vol.se,direction="updown",
               col=c("#377EB8","#E41A1C"))
  
  magaxis(side=c(2,4),labels=c(1,1),frame.plot=T,majorn=3,las=1)
  axis.Date(side=1,at=seq.Date(from=xlims[1],to=xlims[2],by="month"),tcl=0.25,labels=F)
  axis.Date(side=1,at=seq.Date(from=xlims[1],to=xlims[2],by="quarter"),tcl=0.75,labels=T,las=2,
            format="%b")
  title(ylab=expression(Stem~volume~(m^3)),cex.lab=2)
  abline(v=as.Date("2013-9-13"),lty=2)
  legend("bottomright","d",bty="n",inset=0.002,cex=1.2)
  
  
  
  if(output==T) dev.copy2pdf(file="output/treeSize.pdf")
}
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
