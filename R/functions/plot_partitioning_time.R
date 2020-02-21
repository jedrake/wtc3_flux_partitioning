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
  
  
  
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  #-- calculate means for entire experiment, for wet only
  cue.sums <- summaryBy(CUEa+Hloss+RtoA+residtoGPP~chamber+T_treatment,
                        data=subset(growth,Water_treatment=="control" & Date < as.Date("2014-01-19")),FUN=mean,keep.names=T)
  cue.sums.trt <- summaryBy(CUEa+Hloss+RtoA+residtoGPP~T_treatment,
                            data=cue.sums,FUN=c(mean,se))
  
  #-- calculate sums for dry-down period only experiment. Does this really just do drydown data?
  cue.sums.dd <- summaryBy(CUEa+Hloss+RtoA+residtoGPP~chamber+T_treatment+Water_treatment,
                           data=subset(growth,Date>=as.Date("2014-01-19")),FUN=mean,keep.names=T)
  cue.sums.dd.trt <- summaryBy(CUEa+Hloss+RtoA+residtoGPP~Water_treatment+T_treatment,
                               data=cue.sums.dd,FUN=c(mean,se))
  
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  
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
  legend("bottomleft",legend=c("A-Con","W-Con","A-Dry","W-Dry"),fill=palette()[1:4],seg.len=3,cex=1.5)
  legend("topright",letters[1],bty="n",cex=1.5)
  
  #- add experiment-wise boxplots
  #xvals <- boxplot(CUEa.mean~T_treatment+Water_treatment,data=part,ylim=c(0,0.6),col=palette()[1:4],
  #                 names=NA)
  #graphics::text(x=1:4,y=-0.15,labels=c("A-Wet","W-Wet","A-Dry","W-Dry"),cex=1.5,xpd=NA)
  
  #- add experiment-wise means
  xvals <- graphics::barplot(c(cue.sums.trt$CUEa.mean,cue.sums.dd.trt$CUEa.mean),ylim=c(0,0.6),
                             col=c(palette()[1:2],palette()[1:4]),las=2,
                             names.arg=c("A-Con","W-Con","A-Con","W-Con","A-Dry","W-Dry"))
  abline(v=2.5,lty=2)
  adderrorbars(x=xvals,y=c(cue.sums.trt$CUEa.mean,cue.sums.dd.trt$CUEa.mean),
               SE=c(cue.sums.trt$CUEa.se,cue.sums.dd.trt$CUEa.se),direction="updown")
  #title(ylab=expression(C~uptake~(g)),cex.lab=1.5)
  title(ylab="NPPa/GPP")
  legend(x=6,y=0.5,letters[2],bty="n",cex=1.5)
  
  
  
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
  #xvals <- boxplot(RtoA.mean~T_treatment+Water_treatment,data=part,ylim=c(0,0.6),col=palette()[1:4],
  #                 names=NA)
  #- add experiment-wise means
  xvals <- graphics::barplot(c(cue.sums.trt$RtoA.mean,cue.sums.dd.trt$RtoA.mean),ylim=c(0,0.6),
                             col=c(palette()[1:2],palette()[1:4]),las=2,
                             names.arg=c("A-Con","W-Con","A-Con","W-Con","A-Dry","W-Dry"))
  abline(v=2.5,lty=2)
  adderrorbars(x=xvals,y=c(cue.sums.trt$RtoA.mean,cue.sums.dd.trt$RtoA.mean),
               SE=c(cue.sums.trt$RtoA.se,cue.sums.dd.trt$RtoA.se),direction="updown")
  
  #graphics::text(x=1:4,y=-0.15,labels=c("A-Wet","W-Wet","A-Dry","W-Dry"),cex=1.5,xpd=NA)
  title(ylab="Ra/GPP")
  legend(x=6,y=0.5,letters[4],bty="n",cex=1.5)
  
  
  
  
  
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
  # xvals <- boxplot(residtoGPP.mean~T_treatment+Water_treatment,data=part,ylim=c(0,0.6),col=palette()[1:4],
  #                  names=NA)
  # graphics::text(x=1:4,y=-0.15,labels=c("A-Wet","W-Wet","A-Dry","W-Dry"),cex=1.5,xpd=NA)
  #- add experiment-wise means
  xvals <- graphics::barplot(c(cue.sums.trt$residtoGPP.mean,cue.sums.dd.trt$residtoGPP.mean),ylim=c(0,0.6),
                             col=c(palette()[1:2],palette()[1:4]),las=2,
                             names.arg=c("A-Con","W-Con","A-Con","W-Con","A-Dry","W-Dry"))
  abline(v=2.5,lty=2)
  adderrorbars(x=xvals,y=c(cue.sums.trt$residtoGPP.mean,cue.sums.dd.trt$residtoGPP.mean),
               SE=c(cue.sums.trt$residtoGPP.se,cue.sums.dd.trt$residtoGPP.se),direction="updown")
  
  title(ylab="Residual/GPP")
  legend(x=6,y=0.5,letters[6],bty="n",cex=1.5)
  
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  
  
  
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  #- At Peter's suggestion, plot partitioning "directly", showing Ra, NPP, and resid vs. GPP, averaged
  #    for the 12 chambers. I still need to fit linear models and add polygons describing the fit for each flux
  growth.chamber <- summaryBy(Ra+resid+dMass_c+GPP~chamber+T_treatment,data=subset(growth,Date<as.Date("2014-2-1")),FUN=mean,keep.names=T)
  
  #- start plot
  windows(15,25)
  layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), 
         widths=c(2,2,2), heights=c(1,1,1))
  par( mar=c(0,5,0,2), oma=c(7,3,1.5,1.5),las=1,cex.lab=2,cex.axis=1.4)
  ptsize <- 2
  palette(c(rev(brewer.pal(4,"Set1")[1:2]),brewer.pal(4,"Set1")[3:4]))
  xlims=c(0,800)
  ylims=c(0,400)
  
  #-------------------
  #--- plot NPP vs. GPP
  plotBy(dMass_c~GPP|T_treatment,data=growth.chamber,pch=16,xaxt="n",xlim=xlims,ylim=ylims,legend=F,cex=ptsize,
         ylab=expression(NPP[a]~(gC~fortnight^-1)),
         xlab=expression(GPP~(gC~fortnight^-1)))
  axis(1,labels=F,tcl=0.5)
  legend("topleft",legend=c("Ambient","Warmed"),pch=16,col=palette()[1:2],cex=1.2)
  
  #- add model predictions
  model1 <- lm(dMass_c~GPP,data=subset(growth.chamber,T_treatment=="ambient"))
  model2 <- lm(dMass_c~GPP,data=subset(growth.chamber,T_treatment=="elevated"))
  
  xvar1 <- seq(from=min(subset(growth.chamber,T_treatment=="ambient")$GPP,na.rm=T),
               to=max(subset(growth.chamber,T_treatment=="ambient")$GPP,na.rm=T),length.out=101)
  newdata1 <- expand.grid(GPP=xvar1)
  preds <- as.data.frame(predict(model1,newdata=newdata1,interval="confidence"))
  lines(preds$fit~xvar1,col=palette()[1],lwd=2)
  lines(preds$upr~xvar1,col=palette()[1],lty=2,lwd=1)
  lines(preds$lwr~xvar1,col=palette()[1],lty=2,lwd=1)
  
  xvar2 <- seq(from=min(subset(growth.chamber,T_treatment=="elevated")$GPP,na.rm=T),
               to=max(subset(growth.chamber,T_treatment=="elevated")$GPP,na.rm=T),length.out=101)
  newdata2 <- expand.grid(GPP=xvar2)
  preds <- as.data.frame(predict(model2,newdata=newdata2,interval="confidence"))
  lines(preds$fit~xvar2,col=palette()[2],lwd=2)
  lines(preds$upr~xvar2,col=palette()[2],lty=2,lwd=1)
  lines(preds$lwr~xvar2,col=palette()[2],lty=2,lwd=1)
  
  legend("bottomright",legend=letters[1],cex=1.2,bty="n")
  #-------------------
  
  
  #-------------------
  #--- plot Ra vs. GPP
  plotBy(Ra~GPP|T_treatment,data=growth.chamber,pch=16,xaxt="n",legend=F,xlim=xlims,ylim=ylims,cex=ptsize,
         ylab=expression(R[a]~(gC~fortnight^-1)),
         xlab=expression(GPP~(gC~fortnight^-1)))
  axis(1,labels=F,tcl=0.5)
  
  #- add model predictions
  model1 <- lm(Ra~GPP,data=subset(growth.chamber,T_treatment=="ambient"))
  model2 <- lm(Ra~GPP,data=subset(growth.chamber,T_treatment=="elevated"))
  
  xvar1 <- seq(from=min(subset(growth.chamber,T_treatment=="ambient")$GPP,na.rm=T),
               to=max(subset(growth.chamber,T_treatment=="ambient")$GPP,na.rm=T),length.out=101)
  newdata1 <- expand.grid(GPP=xvar1)
  preds <- as.data.frame(predict(model1,newdata=newdata1,interval="confidence"))
  lines(preds$fit~xvar1,col=palette()[1],lwd=2)
  lines(preds$upr~xvar1,col=palette()[1],lty=2,lwd=1)
  lines(preds$lwr~xvar1,col=palette()[1],lty=2,lwd=1)
  
  xvar2 <- seq(from=min(subset(growth.chamber,T_treatment=="elevated")$GPP,na.rm=T),
               to=max(subset(growth.chamber,T_treatment=="elevated")$GPP,na.rm=T),length.out=101)
  newdata2 <- expand.grid(GPP=xvar2)
  preds <- as.data.frame(predict(model2,newdata=newdata2,interval="confidence"))
  lines(preds$fit~xvar2,col=palette()[2],lwd=2)
  lines(preds$upr~xvar2,col=palette()[2],lty=2,lwd=1)
  lines(preds$lwr~xvar2,col=palette()[2],lty=2,lwd=1)
  
  legend("bottomright",legend=letters[2],cex=1.2,bty="n")
  #-------------------

  
  
  #-------------------
  #--- plot residual vs. GPP
  plotBy(resid~GPP|T_treatment,data=growth.chamber,pch=16,xaxt="n",legend=F,xlim=xlims,ylim=ylims,cex=ptsize,
         ylab=expression(Residual~(gC~fortnight^-1)),
         xlab=expression(GPP~(gC~fortnight^-1)))
  axis(1,labels=T,tcl=0.5)
  
  #- add model predictions
  model1 <- lm(resid~GPP,data=subset(growth.chamber,T_treatment=="ambient"))
  model2 <- lm(resid~GPP,data=subset(growth.chamber,T_treatment=="elevated"))
  
  xvar1 <- seq(from=min(subset(growth.chamber,T_treatment=="ambient")$GPP,na.rm=T),
               to=max(subset(growth.chamber,T_treatment=="ambient")$GPP,na.rm=T),length.out=101)
  newdata1 <- expand.grid(GPP=xvar1)
  preds <- as.data.frame(predict(model1,newdata=newdata1,interval="confidence"))
  lines(preds$fit~xvar1,col=palette()[1],lwd=2)
  lines(preds$upr~xvar1,col=palette()[1],lty=2,lwd=1)
  lines(preds$lwr~xvar1,col=palette()[1],lty=2,lwd=1)
  
  xvar2 <- seq(from=min(subset(growth.chamber,T_treatment=="elevated")$GPP,na.rm=T),
               to=max(subset(growth.chamber,T_treatment=="elevated")$GPP,na.rm=T),length.out=101)
  newdata2 <- expand.grid(GPP=xvar2)
  preds <- as.data.frame(predict(model2,newdata=newdata2,interval="confidence"))
  lines(preds$fit~xvar2,col=palette()[2],lwd=2)
  lines(preds$upr~xvar2,col=palette()[2],lty=2,lwd=1)
  lines(preds$lwr~xvar2,col=palette()[2],lty=2,lwd=1)
  
  title(xlab=expression(GPP~(gC~fortnight^-1)),outer=T,line=4)
  legend("bottomright",legend=letters[3],cex=1.2,bty="n")
  #-------------------
  
}
