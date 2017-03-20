plot_partitioning_time <- function(growth){



#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#- Plot partitioning over time


part <- summaryBy(CUEa+Hloss+RtoA+residtoGPP~Date+T_treatment+Water_treatment,data=subset(growth, Date < as.Date("2014-5-1")),na.rm=T,FUN=c(mean,se))
part.ac <- subset(part,T_treatment=="ambient" & Water_treatment=="control")
part.ec <- subset(part,T_treatment=="elevated" & Water_treatment=="control")
part.ad <- subset(part,T_treatment=="ambient" & Water_treatment=="drydown")
part.ed <- subset(part,T_treatment=="elevated" & Water_treatment=="drydown")

windows(30,40);par(mfrow=c(3,1),mar=c(0,0,0,0),oma=c(6,7,3,3))
ptsize <- 1.5
#NPPa/GPP
plotBy(CUEa.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),ylim=c(0,0.6),pch=16,cex=ptsize,
       type="b",col=c("blue","red"),legend=F,axes=F,lwd=2)

magaxis(side=c(2,4),labels=c(1,1),las=1,frame.plot=T)
axis.Date(1, at = seq(as.Date("2012/1/1"), as.Date("2014/12/1"), by="month"),
          labels = F, tcl = 0.4,format="%m/%Y",las=3,cex.axis=1.5)
title(ylab="NPPa/GPP",outer=T,adj=0.9,cex.lab=1.5)
adderrorbars(x=part.ac$Date,y=part.ac$CUEa.mean,SE=part.ac$CUEa.se,
             col="blue",direction="updown",barlen=0)
adderrorbars(x=part.ec$Date,y=part.ec$CUEa.mean,SE=part.ec$CUEa.se,
             col="red",direction="updown",barlen=0)
adderrorbars(x=part.ad$Date,y=part.ad$CUEa.mean,SE=part.ad$CUEa.se,
             col="blue",direction="updown",barlen=0)
adderrorbars(x=part.ed$Date,y=part.ed$CUEa.mean,SE=part.ed$CUEa.se,
             col="red",direction="updown",barlen=0)
plotBy(CUEa.mean~Date|T_treatment,data=subset(part,Water_treatment=="drydown"),ylim=c(0,0.6),pch=1,cex=ptsize,
       type="b",col=c("blue","red"),legend=F,axes=F,lwd=2,add=T,lty=2)


#Ra/GPP
plotBy(RtoA.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),ylim=c(0,0.6),pch=16,cex=ptsize,
       type="b",col=c("blue","red"),legend=F,axes=F,lwd=2)

magaxis(side=c(2,4),labels=c(1,1),las=1,frame.plot=T)
axis.Date(1, at = seq(as.Date("2012/1/1"), as.Date("2014/12/1"), by="month"),
          labels = F, tcl = 0.4,format="%m/%Y",las=3,cex.axis=1.5)
title(ylab="Ra/GPP",outer=T,adj=0.5,cex.lab=1.5)
adderrorbars(x=part.ac$Date,y=part.ac$RtoA.mean,SE=part.ac$RtoA.se,
             col="blue",direction="updown",barlen=0)
adderrorbars(x=part.ec$Date,y=part.ec$RtoA.mean,SE=part.ec$RtoA.se,
             col="red",direction="updown",barlen=0)
adderrorbars(x=part.ad$Date,y=part.ad$RtoA.mean,SE=part.ad$RtoA.se,
             col="blue",direction="updown",barlen=0)
adderrorbars(x=part.ed$Date,y=part.ed$RtoA.mean,SE=part.ed$RtoA.se,
             col="red",direction="updown",barlen=0)
plotBy(RtoA.mean~Date|T_treatment,data=subset(part,Water_treatment=="drydown"),ylim=c(0,0.6),pch=1,cex=ptsize,
       type="b",col=c("blue","red"),legend=F,axes=F,lwd=2,add=T,lty=2)

#Residual
plotBy(residtoGPP.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),ylim=c(0,0.6),pch=16,cex=ptsize,
       type="b",col=c("blue","red"),legend=F,axes=F,lwd=2)

magaxis(side=c(2,4),labels=c(1,1),las=1,frame.plot=T)
axis.Date(1, at = seq(as.Date("2012/1/1"), as.Date("2014/12/1"), by="month"),
          labels = T, tcl = 0.4,format="%m/%Y",las=3,cex.axis=1.5)
title(ylab="Residual/GPP",outer=T,adj=0.1,cex.lab=1.5)
adderrorbars(x=part.ac$Date,y=part.ac$residtoGPP.mean,SE=part.ac$residtoGPP.se,
             col="blue",direction="updown",barlen=0)
adderrorbars(x=part.ec$Date,y=part.ec$residtoGPP.mean,SE=part.ec$residtoGPP.se,
             col="red",direction="updown",barlen=0)
adderrorbars(x=part.ad$Date,y=part.ad$residtoGPP.mean,SE=part.ad$residtoGPP.se,
             col="blue",direction="updown",barlen=0)
adderrorbars(x=part.ed$Date,y=part.ed$residtoGPP.mean,SE=part.ed$residtoGPP.se,
             col="red",direction="updown",barlen=0)
plotBy(residtoGPP.mean~Date|T_treatment,data=subset(part,Water_treatment=="drydown"),ylim=c(0,0.7),pch=1,cex=ptsize,
       type="b",col=c("blue","red"),legend=F,axes=F,lwd=2,add=T,lty=2)
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
}