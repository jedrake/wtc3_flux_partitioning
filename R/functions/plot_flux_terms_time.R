plot_flux_terms_time <- function(growth){
  
  
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  #- Derive averages to present in results
  part.chamber.pre <-  summaryBy(GPP+dMass_c+Ra+resid~chamber+T_treatment,
                                 data=subset(growth, Date < as.Date("2014-2-1")),na.rm=T,FUN=c(mean),keep.names=T)
  part.trt.pre <-  summaryBy(GPP+dMass_c+Ra+resid~T_treatment,
                                 data=part.chamber.pre,na.rm=T,FUN=c(mean,se),keep.names=F)
  
  part.chamber.dry <-  summaryBy(GPP+dMass_c+Ra+resid~chamber+Water_treatment,
                                 data=subset(growth, Date > as.Date("2014-2-1")),na.rm=T,FUN=c(mean),keep.names=T)
  part.trt.dry <-  summaryBy(GPP+dMass_c+Ra+resid~Water_treatment,
                             data=part.chamber.dry,na.rm=T,FUN=c(mean,se),keep.names=F)
  
  part.chamber.all <-  summaryBy(GPP+dMass_c+Ra+resid~chamber+T_treatment,
                                 data=growth,na.rm=T,FUN=c(mean),keep.names=T)
  part.trt.all <-  summaryBy(GPP+dMass_c+Ra+resid~T_treatment,
                             data=part.chamber.all,na.rm=T,FUN=c(mean,se),keep.names=F)
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  
  
  
  
  
  
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  #- Plot fluxes over time
  
  
  part <- summaryBy(GPP+dMass_c+Ra+resid~Date+T_treatment+Water_treatment,data=subset(growth, Date < as.Date("2014-5-1")),na.rm=T,FUN=c(mean,se))
  
  
  part$GPP.high <- with(part,GPP.mean+GPP.se)
  part$GPP.low <- with(part,GPP.mean-GPP.se)
  part$dMass_c.high <- with(part,dMass_c.mean+dMass_c.se)
  part$dMass_c.low <- with(part,dMass_c.mean-dMass_c.se)
  part$Ra.high <- with(part,Ra.mean+Ra.se)
  part$Ra.low <- with(part,Ra.mean-Ra.se)
  part$resid.high <- with(part,resid.mean+resid.se)
  part$resid.low <- with(part,resid.mean-resid.se)
  
  part.ac <- subset(part,T_treatment=="ambient" & Water_treatment=="control")
  part.ec <- subset(part,T_treatment=="elevated" & Water_treatment=="control")
  part.ad <- subset(part,T_treatment=="ambient" & Water_treatment=="drydown")
  part.ed <- subset(part,T_treatment=="elevated" & Water_treatment=="drydown")
  
  
  
  #- start plot
  windows(25,25)
  layout(matrix(c(1,2,3,4,5,6), 2, 2, byrow = TRUE), 
         widths=c(2,2,2,2), heights=c(2,2,2,2))
  par( mar=c(0,5,0,2), oma=c(5,3,1.5,1.5),las=1,cex.lab=1.5,cex.axis=1.2)
  ptsize <- 1.5
  palette(c(rev(brewer.pal(4,"Set1")[1:2]),brewer.pal(4,"Set1")[3:4]))
  
  
  #---------- GPP
  plotBy(GPP.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),type="l",lwd=2,
         ylim=c(0,1000),legend=F,axes=F,ylab="",xlab="")
  
  #- gussy up the graph
  magaxis(side=c(2),labels=c(1),las=1,frame.plot=T,cex.axis=1.2)
  axis.Date(side=1,at=seq.Date(from=as.Date("2013-9-1"),to=as.Date("2014-6-1"),by="month"),tcl=0.5,cex.axis=1.2,labels=F)
  title(ylab="GPP",outer=T,adj=0.75,cex.lab=1.5,line=-2)
  
  #- add polygons showing SE
  polygon(x = c(part.ac$Date, rev(part.ac$Date)), 
          y = c(part.ac$GPP.low,rev(part.ac$GPP.high)) , col = alpha(palette()[1],0.15), border = F)
  polygon(x = c(part.ec$Date, rev(part.ec$Date)), 
          y = c(part.ec$GPP.low,rev(part.ec$GPP.high)) , col = alpha(palette()[2],0.15), border = F)
  polygon(x = c(part.ad$Date, rev(part.ad$Date)), 
          y = c(part.ad$GPP.low,rev(part.ad$GPP.high)) , col = alpha(palette()[3],0.15), border = F)
  polygon(x = c(part.ed$Date, rev(part.ed$Date)), 
          y = c(part.ed$GPP.low,rev(part.ed$GPP.high)) , col = alpha(palette()[4],0.15), border = F)
  #- overlay bolded means
  plotBy(GPP.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),type="l",lwd=3,
         add=T,legend=F)
  plotBy(GPP.mean~Date|T_treatment,data=subset(part,Water_treatment=="drydown"),col=palette()[3:4],
         type="l",lwd=3,add=T,legend=F)
  legend("bottomright",legend=c("A-Wet","W-Wet","A-Dry","W-Dry"),fill=palette()[1:4],seg.len=3,cex=1.2,bty="n")
  legend("topleft",letters[1],bty="n",cex=1.5)
  
  
  
  
  #---------- NPP
  plotBy(dMass_c.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),type="l",lwd=2,
         ylim=c(0,500),legend=F,axes=F,ylab="",xlab="")
  
  #- gussy up the graph
  magaxis(side=c(2),labels=c(1),las=1,frame.plot=T,cex.axis=1.2)
  axis.Date(side=1,at=seq.Date(from=as.Date("2013-9-1"),to=as.Date("2014-6-1"),by="month"),tcl=0.5,cex.axis=1.2,labels=F)
  title(ylab="NPPa",outer=T,adj=0.75,cex.lab=1.5,line=-28)
  
  #- add polygons showing SE
  polygon(x = c(part.ac$Date, rev(part.ac$Date)), 
          y = c(part.ac$dMass_c.low,rev(part.ac$dMass_c.high)) , col = alpha(palette()[1],0.15), border = F)
  polygon(x = c(part.ec$Date, rev(part.ec$Date)), 
          y = c(part.ec$dMass_c.low,rev(part.ec$dMass_c.high)) , col = alpha(palette()[2],0.15), border = F)
  polygon(x = c(part.ad$Date, rev(part.ad$Date)), 
          y = c(part.ad$dMass_c.low,rev(part.ad$dMass_c.high)) , col = alpha(palette()[3],0.15), border = F)
  polygon(x = c(part.ed$Date, rev(part.ed$Date)), 
          y = c(part.ed$dMass_c.low,rev(part.ed$dMass_c.high)) , col = alpha(palette()[4],0.15), border = F)
  #- overlay bolded means
  plotBy(dMass_c.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),type="l",lwd=3,
         ylim=c(0,100),add=T,legend=F)
  plotBy(dMass_c.mean~Date|T_treatment,data=subset(part,Water_treatment=="drydown"),col=palette()[3:4],
         type="l",lwd=3,add=T,legend=F)
  legend("topleft",letters[2],bty="n",cex=1.5)
  
  
  
  #---------- Ra
  plotBy(Ra.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),type="l",lwd=2,
         ylim=c(0,500),legend=F,axes=F,ylab="",xlab="")
  
  #- gussy up the graph
  magaxis(side=c(2),labels=c(1),las=1,frame.plot=T,cex.axis=1.2)
  axis.Date(side=1,at=seq.Date(from=as.Date("2013-9-1"),to=as.Date("2014-6-1"),by="month"),tcl=0.5,cex.axis=1.2,labels=T)
  title(ylab="Ra",outer=T,adj=0.25,cex.lab=1.5,line=-2)
  
  #- add polygons showing SE
  polygon(x = c(part.ac$Date, rev(part.ac$Date)), 
          y = c(part.ac$Ra.low,rev(part.ac$Ra.high)) , col = alpha(palette()[1],0.15), border = F)
  polygon(x = c(part.ec$Date, rev(part.ec$Date)), 
          y = c(part.ec$Ra.low,rev(part.ec$Ra.high)) , col = alpha(palette()[2],0.15), border = F)
  polygon(x = c(part.ad$Date, rev(part.ad$Date)), 
          y = c(part.ad$Ra.low,rev(part.ad$Ra.high)) , col = alpha(palette()[3],0.15), border = F)
  polygon(x = c(part.ed$Date, rev(part.ed$Date)), 
          y = c(part.ed$Ra.low,rev(part.ed$Ra.high)) , col = alpha(palette()[4],0.15), border = F)
  #- overlay bolded means
  plotBy(Ra.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),type="l",lwd=3,
         ylim=c(0,100),add=T,legend=F)
  plotBy(Ra.mean~Date|T_treatment,data=subset(part,Water_treatment=="drydown"),col=palette()[3:4],
         type="l",lwd=3,add=T,legend=F)
  legend("topleft",letters[3],bty="n",cex=1.5)
  
  
  
  #---------- residual
  plotBy(resid.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),type="l",lwd=2,
         ylim=c(0,500),legend=F,axes=F,ylab="",xlab="")
  
  #- gussy up the graph
  magaxis(side=c(2),labels=c(1),las=1,frame.plot=T,cex.axis=1.2)
  axis.Date(side=1,at=seq.Date(from=as.Date("2013-9-1"),to=as.Date("2014-6-1"),by="month"),tcl=0.5,cex.axis=1.2,labels=T)
  title(ylab="Residual",outer=T,adj=0.25,cex.lab=1.5,line=-28)
  
  #- add polygons showing SE
  polygon(x = c(part.ac$Date, rev(part.ac$Date)), 
          y = c(part.ac$resid.low,rev(part.ac$resid.high)) , col = alpha(palette()[1],0.15), border = F)
  polygon(x = c(part.ec$Date, rev(part.ec$Date)), 
          y = c(part.ec$resid.low,rev(part.ec$resid.high)) , col = alpha(palette()[2],0.15), border = F)
  polygon(x = c(part.ad$Date, rev(part.ad$Date)), 
          y = c(part.ad$resid.low,rev(part.ad$resid.high)) , col = alpha(palette()[3],0.15), border = F)
  polygon(x = c(part.ed$Date, rev(part.ed$Date)), 
          y = c(part.ed$resid.low,rev(part.ed$resid.high)) , col = alpha(palette()[4],0.15), border = F)
  #- overlay bolded means
  plotBy(resid.mean~Date|T_treatment,data=subset(part,Water_treatment=="control"),type="l",lwd=3,
         ylim=c(0,100),add=T,legend=F)
  plotBy(resid.mean~Date|T_treatment,data=subset(part,Water_treatment=="drydown"),col=palette()[3:4],
         type="l",lwd=3,add=T,legend=F)
  legend("topleft",letters[4],bty="n",cex=1.5)
  
  
  
  
  
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
}