plot_partitioning_time <- function(growth){
  
  
  
  
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  #- Derive averages to present in results
  part.chamber.pre <-  summaryBy(CUEa+Hloss+RtoA+residtoGPP~chamber+T_treatment,
                                 data=subset(growth, Date < as.Date("2014-2-1")),na.rm=T,FUN=c(mean),keep.names=T)
  part.trt.pre <-  summaryBy(CUEa+Hloss+RtoA+residtoGPP~T_treatment,
                             data=part.chamber.pre,na.rm=T,FUN=c(mean,se),keep.names=F)
  
  part.chamber.dry <-  summaryBy(CUEa+Hloss+RtoA+residtoGPP~chamber+Water_treatment,
                                 data=subset(growth, Date > as.Date("2014-2-1")),na.rm=T,FUN=c(mean),keep.names=T)
  part.trt.dry <-  summaryBy(CUEa+Hloss+RtoA+residtoGPP~Water_treatment,
                             data=part.chamber.dry,na.rm=T,FUN=c(mean,se),keep.names=F)
  
  part.chamber.all <-  summaryBy(CUEa+Hloss+RtoA+residtoGPP~chamber+T_treatment,
                                 data=growth,na.rm=T,FUN=c(mean),keep.names=T)
  part.trt.all <-  summaryBy(CUEa+Hloss+RtoA+residtoGPP~T_treatment,
                             data=part.chamber.all,na.rm=T,FUN=c(mean,se),keep.names=F)
  
  grand.means <-  summaryBy(CUEa+Hloss+RtoA+residtoGPP~1,
                            data=growth,na.rm=T,FUN=c(mean,se),keep.names=F)
  
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  
  
  
  
  
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  #- Plot partitioning over time
  
  
  part <- summaryBy(CUEa+Hloss+RtoA+residtoGPP~Date+T_treatment+Water_treatment,data=subset(growth, Date < as.Date("2014-5-1")),na.rm=T,FUN=c(mean,se))
  
  
  part$CUEa.high <- with(part,CUEa.mean+CUEa.se)
  part$CUEa.low <- with(part,CUEa.mean-CUEa.se)
  part$RtoA.high <- with(part,RtoA.mean+RtoA.se)
  part$RtoA.low <- with(part,RtoA.mean-RtoA.se)
  part$residtoGPP.high <- with(part,residtoGPP.mean+residtoGPP.se)
  part$residtoGPP.low <- with(part,residtoGPP.mean-residtoGPP.se)
  
  part.ac <- subset(part,T_treatment=="ambient" & Water_treatment=="control")
  part.ec <- subset(part,T_treatment=="elevated" & Water_treatment=="control")
  part.ad <- subset(part,T_treatment=="ambient" & Water_treatment=="drydown")
  part.ed <- subset(part,T_treatment=="elevated" & Water_treatment=="drydown")
  
  
  
  #- start plot
  windows(35,25)
  layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = TRUE), 
         widths=c(2,1,2,1,2,1), heights=c(2,2,2,2,2,2))
  par( mar=c(0,5,0,2), oma=c(5,3,1.5,1.5),las=1,cex.lab=1.5,cex.axis=1.2)
  ptsize <- 1.5
  palette(c(rev(brewer.pal(4,"Set1")[1:2]),brewer.pal(4,"Set1")[3:4]))
  
  
  #---------- NPPa/GPP
  plotBy(CUEa.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),type="l",lwd=2,
         ylim=c(0,0.6),legend=F,axes=F,ylab="",xlab="")
  
  #- gussy up the graph
  magaxis(side=c(2),labels=c(1),las=1,frame.plot=T,cex.axis=1.2)
  axis.Date(side=1,at=seq.Date(from=as.Date("2013-9-1"),to=as.Date("2014-6-1"),by="month"),tcl=0.5,cex.axis=1.2,labels=F)
  title(ylab="NPPa/GPP",outer=T,adj=0.90,cex.lab=1.5,line=-2)
  
  #- add polygons showing SE
  polygon(x = c(part.ac$Date, rev(part.ac$Date)), 
          y = c(part.ac$CUEa.low,rev(part.ac$CUEa.high)) , col = alpha(palette()[1],0.15), border = F)
  polygon(x = c(part.ec$Date, rev(part.ec$Date)), 
          y = c(part.ec$CUEa.low,rev(part.ec$CUEa.high)) , col = alpha(palette()[2],0.15), border = F)
  polygon(x = c(part.ad$Date, rev(part.ad$Date)), 
          y = c(part.ad$CUEa.low,rev(part.ad$CUEa.high)) , col = alpha(palette()[3],0.15), border = F)
  polygon(x = c(part.ed$Date, rev(part.ed$Date)), 
          y = c(part.ed$CUEa.low,rev(part.ed$CUEa.high)) , col = alpha(palette()[4],0.15), border = F)
  #- overlay bolded means
  plotBy(CUEa.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),type="l",lwd=3,
         ylim=c(0,100),add=T,legend=F)
  plotBy(CUEa.mean~Date|T_treatment,data=subset(part,Water_treatment=="drydown"),col=palette()[3:4],
         type="l",lwd=3,add=T,legend=F)
  legend("bottomleft",legend=c("A-Wet","W-Wet","A-Dry","W-Dry"),fill=palette()[1:4],seg.len=3,cex=1.5)
  legend("topright",letters[1],bty="n",cex=1.5)
  
  #- add experiment-wise boxplots
  xvals <- boxplot(CUEa.mean~T_treatment+Water_treatment,data=part,ylim=c(0,0.6),col=palette()[1:4],
                   names=NA)
  #graphics::text(x=1:4,y=-0.15,labels=c("A-Wet","W-Wet","A-Dry","W-Dry"),cex=1.5,xpd=NA)
  title(ylab="NPPa/GPP")
  legend("topright",letters[2],bty="n",cex=1.5)
  
  
  
  #---------- Ra/GPP
  plotBy(RtoA.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),type="l",lwd=2,
         ylim=c(0,0.6),legend=F,axes=F,ylab="",xlab="")
  
  #- gussy up the graph
  magaxis(side=c(2),labels=c(1),las=1,frame.plot=T,cex.axis=1.2)
  axis.Date(side=1,at=seq.Date(from=as.Date("2013-9-1"),to=as.Date("2014-6-1"),by="month"),tcl=0.5,cex.axis=1.2,labels=F)
  title(ylab="Ra/GPP",outer=T,adj=0.5,cex.lab=1.5,line=-2)
  
  #- add polygons showing SE
  polygon(x = c(part.ac$Date, rev(part.ac$Date)), 
          y = c(part.ac$RtoA.low,rev(part.ac$RtoA.high)) , col = alpha(palette()[1],0.15), border = F)
  polygon(x = c(part.ec$Date, rev(part.ec$Date)), 
          y = c(part.ec$RtoA.low,rev(part.ec$RtoA.high)) , col = alpha(palette()[2],0.15), border = F)
  polygon(x = c(part.ad$Date, rev(part.ad$Date)), 
          y = c(part.ad$RtoA.low,rev(part.ad$RtoA.high)) , col = alpha(palette()[3],0.15), border = F)
  polygon(x = c(part.ed$Date, rev(part.ed$Date)), 
          y = c(part.ed$RtoA.low,rev(part.ed$RtoA.high)) , col = alpha(palette()[4],0.15), border = F)
  #- overlay bolded means
  plotBy(RtoA.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),type="l",lwd=3,
         ylim=c(0,100),add=T,legend=F)
  plotBy(RtoA.mean~Date|T_treatment,data=subset(part,Water_treatment=="drydown"),col=palette()[3:4],
         type="l",lwd=3,add=T,legend=F)
  legend("topright",letters[3],bty="n",cex=1.5)
  
  #- add experiment-wise boxplots
  xvals <- boxplot(RtoA.mean~T_treatment+Water_treatment,data=part,ylim=c(0,0.6),col=palette()[1:4],
                   names=NA)
  #graphics::text(x=1:4,y=-0.15,labels=c("A-Wet","W-Wet","A-Dry","W-Dry"),cex=1.5,xpd=NA)
  title(ylab="Ra/GPP")
  legend("topright",letters[4],bty="n",cex=1.5)
  
  
  
  
  
  #---------- Resid to /GPP
  plotBy(residtoGPP.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),type="l",lwd=2,
         ylim=c(0,0.6),legend=F,axes=F,ylab="",xlab="")
  
  #- gussy up the graph
  magaxis(side=c(2),labels=c(1),las=1,frame.plot=T,cex.axis=1.2)
  axis.Date(side=1,at=seq.Date(from=as.Date("2013-9-1"),to=as.Date("2014-6-1"),by="month"),tcl=0.5,cex.axis=1.2)
  title(ylab="Residual/GPP",outer=T,adj=0.10,cex.lab=1.5,line=-2)
  
  #- add polygons showing SE
  polygon(x = c(part.ac$Date, rev(part.ac$Date)), 
          y = c(part.ac$residtoGPP.low,rev(part.ac$residtoGPP.high)) , col = alpha(palette()[1],0.15), border = F)
  polygon(x = c(part.ec$Date, rev(part.ec$Date)), 
          y = c(part.ec$residtoGPP.low,rev(part.ec$residtoGPP.high)) , col = alpha(palette()[2],0.15), border = F)
  polygon(x = c(part.ad$Date, rev(part.ad$Date)), 
          y = c(part.ad$residtoGPP.low,rev(part.ad$residtoGPP.high)) , col = alpha(palette()[3],0.15), border = F)
  polygon(x = c(part.ed$Date, rev(part.ed$Date)), 
          y = c(part.ed$residtoGPP.low,rev(part.ed$residtoGPP.high)) , col = alpha(palette()[4],0.15), border = F)
  #- overlay bolded means
  plotBy(residtoGPP.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),type="l",lwd=3,
         ylim=c(0,100),add=T,legend=F)
  plotBy(residtoGPP.mean~Date|T_treatment,data=subset(part,Water_treatment=="drydown"),col=palette()[3:4],
         type="l",lwd=3,add=T,legend=F)
  legend("topright",letters[5],bty="n",cex=1.5)
  
  #- add experiment-wise boxplots
  xvals <- boxplot(residtoGPP.mean~T_treatment+Water_treatment,data=part,ylim=c(0,0.6),col=palette()[1:4],
                   names=NA)
  graphics::text(x=1:4,y=-0.15,labels=c("A-Wet","W-Wet","A-Dry","W-Dry"),cex=1.5,xpd=NA)
  title(ylab="Residual/GPP")
  legend("topright",letters[6],bty="n",cex=1.5)
  
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
}