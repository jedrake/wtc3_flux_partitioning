#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#- plot metrics of size over time.

plotsize <- function(output=T){
  
  windows(40,40);par(mfrow=c(2,2),mar=c(0,5.5,0,3),oma=c(5,2,2,0),las=1,cex.axis=1.5)
  palette(c("black","red"))
  
  #- plot diameter
  xlims <-as.Date(c("2013-3-1","2014-6-1"))
  yearlims <-as.Date(c("2013-1-1","2014-1-1"))
  plotBy(diam.mean~DateTime|T_treatment,data=subset(size.m,Water_treatment=="control"),pch=16,type="o",ylim=c(0,8),
         xlim=xlims,
         xaxt="n",yaxt="n",xlab="",ylab="",legend=F)
  adderrorbars(x=subset(size.m,Water_treatment=="control")$DateTime,
               y=subset(size.m,Water_treatment=="control")$diam.mean,barlen=0.02,
               SE=subset(size.m,Water_treatment=="control")$diam.se,direction="updown",
               col=c("black","red"))       
  plotBy(diam.mean~DateTime|T_treatment,data=subset(size.m,Water_treatment=="drydown"),pch=1,type="o",add=T,legend=F)
  adderrorbars(x=subset(size.m,Water_treatment=="drydown")$DateTime,
               y=subset(size.m,Water_treatment=="drydown")$diam.mean,barlen=0.02,
               SE=subset(size.m,Water_treatment=="drydown")$diam.se,direction="updown",
               col=c("black","red"))    
  magaxis(side=c(2,4),labels=c(1,1),frame.plot=T,majorn=3,las=1)
  axis.Date(side=1,at=seq.Date(from=xlims[1],to=xlims[2],by="month"),tcl=0.25,labels=F)
  axis.Date(side=1,at=seq.Date(from=xlims[1],to=xlims[2],by="quarter"),tcl=0.75,labels=F)
  title(ylab=expression(Diameter~(cm)),cex.lab=2)
  abline(v=as.Date("2013-9-13"),lty=2)
  #text(x=as.Date("2013-9-13"),y=8.8,labels="Flux measurements begin",xpd=NA,cex=1.3)
  legend("topleft",pch=c(16,16,1,1),lty=c(1),col=c("black","red"),seg.len=1.5,
         legend=c("A-Wet","W-Wet","A-Dry","W-Dry"),bty="n",cex=1.2)
  legend("bottomright","a",bty="n",inset=0.002,cex=1.2)
  
  
  #-- plot leaf area over time
  plotBy(leafArea.mean~Date|T_treatment,data=subset(leafArea1,Water_treatment=="control"),pch=16,type="p",ylim=c(0,25),cex=1.5,
         xlim=xlims,
         xaxt="n",yaxt="n",xlab="",ylab="",legend=F)
  adderrorbars(x=subset(leafArea1,Water_treatment=="control")$Date,
               y=subset(leafArea1,Water_treatment=="control")$leafArea.mean,barlen=0.02,
               SE=subset(leafArea1,Water_treatment=="control")$leafArea.se,direction="updown",
               col=c("black","red"))
  plotBy(leafArea.mean~Date|T_treatment,data=subset(leafArea1,Water_treatment=="drydown"),pch=1,type="p",add=T,legend=F,cex=1.5)
  adderrorbars(x=subset(leafArea1,Water_treatment=="drydown")$Date,
               y=subset(leafArea1,Water_treatment=="drydown")$leafArea.mean,barlen=0.02,
               SE=subset(leafArea1,Water_treatment=="drydown")$leafArea.se,direction="updown",
               col=c("black","red"))
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
               y=subset(size.m,Water_treatment=="control")$height.mean,barlen=0.02,
               SE=subset(size.m,Water_treatment=="control")$height.se,direction="updown",
               col=c("black","red"))
  plotBy(height.mean~DateTime|T_treatment,data=subset(size.m,Water_treatment=="drydown"),pch=1,type="o",add=T,legend=F)
  adderrorbars(x=subset(size.m,Water_treatment=="drydown")$DateTime,
               y=subset(size.m,Water_treatment=="drydown")$height.mean,barlen=0.02,
               SE=subset(size.m,Water_treatment=="drydown")$height.se,direction="updown",
               col=c("black","red"))
  
  magaxis(side=c(2,4),labels=c(1,1),frame.plot=T,majorn=3,las=1)
  axis.Date(side=1,at=seq.Date(from=xlims[1],to=xlims[2],by="month"),tcl=0.25,labels=F)
  axis.Date(side=1,at=seq.Date(from=xlims[1],to=xlims[2],by="quarter"),tcl=0.75,labels=T,las=2,
            format="%b")
  title(ylab=expression(Height~(m)),cex.lab=2)
  abline(v=as.Date("2013-9-13"),lty=2)
  legend("bottomright","b",bty="n",inset=0.002,cex=1.2)
  
  
  #- plot stem volume
  plotBy(vol.mean~DateTime|T_treatment,data=subset(size.m,Water_treatment=="control"),pch=16,type="o",ylim=c(0,2),
         xlim=xlims,
         xaxt="n",yaxt="n",xlab="",ylab="",legend=F)
  adderrorbars(x=subset(size.m,Water_treatment=="control")$DateTime,
               y=subset(size.m,Water_treatment=="control")$vol.mean,barlen=0.02,
               SE=subset(size.m,Water_treatment=="control")$vol.se,direction="updown",
               col=c("black","red"))
  plotBy(vol.mean~DateTime|T_treatment,data=subset(size.m,Water_treatment=="drydown"),pch=1,type="o",add=T,legend=F)
  adderrorbars(x=subset(size.m,Water_treatment=="drydown")$DateTime,
               y=subset(size.m,Water_treatment=="drydown")$vol.mean,barlen=0.02,
               SE=subset(size.m,Water_treatment=="drydown")$vol.se,direction="updown",
               col=c("black","red"))
  
  magaxis(side=c(2,4),labels=c(1,1),frame.plot=T,majorn=3,las=1)
  axis.Date(side=1,at=seq.Date(from=xlims[1],to=xlims[2],by="month"),tcl=0.25,labels=F)
  axis.Date(side=1,at=seq.Date(from=xlims[1],to=xlims[2],by="quarter"),tcl=0.75,labels=T,las=2,
            format="%b")
  title(ylab=expression(Stem~volume~(m^3)),cex.lab=2)
  abline(v=as.Date("2013-9-13"),lty=2)
  legend("bottomright","d",bty="n",inset=0.002,cex=1.2)
  
  
  
  if(export==T) dev.copy2pdf(file="output/treeSize.pdf")
}
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
