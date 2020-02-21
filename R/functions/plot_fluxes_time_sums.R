#--------------------------------------------------------------------------------------------------
plot_fluxes_time_sum <- function(cue.day){
  
  #- create weekly averages, first for chambers, then for treatments
  cue.day$Date_week <- lubridate::floor_date(cue.day$Date,unit="week")
  cue.day.trt1 <- summaryBy(GPP+Ra+RtoA+Cgain+Hloss+CUE+PAR+T_day+T_night+Tair_24hrs~
                              chamber+Date_week+T_treatment+Water_treatment,data=cue.day,FUN=mean,keep.names=T)
  
  cue.day.trt <- summaryBy(GPP+Ra+RtoA+Cgain+Hloss+CUE+PAR+T_day+T_night+Tair_24hrs~
                             Date_week+T_treatment+Water_treatment,data=cue.day.trt1,FUN=c(mean,se))
  cue.day.trt$Cgain.high <- with(cue.day.trt,Cgain.mean+Cgain.se)
  cue.day.trt$Cgain.low <- with(cue.day.trt,Cgain.mean-Cgain.se)
  cue.day.trt$Hloss.high <- with(cue.day.trt,Hloss.mean+Hloss.se)
  cue.day.trt$Hloss.low <- with(cue.day.trt,Hloss.mean-Hloss.se)
  
  
  
  
  
  #--------------------------------------------------------------------------------------------------
  #- plot fluxes over time
  
  
  #-- calculate sums for entire experiment, for wet only
  cue.sums <- summaryBy(Cgain+Hloss~chamber+T_treatment,
                        data=subset(cue.day,Water_treatment=="control" & Date < as.Date("2014-01-19")),FUN=sum,keep.names=T)
  cue.sums.trt <- summaryBy(Cgain+Hloss~T_treatment,
                            data=cue.sums,FUN=c(mean,se))
  
  #-- calculate sums for dry-down period only experiment. Does this really just do drydown data?
  cue.sums.dd <- summaryBy(Cgain+Hloss~chamber+T_treatment+Water_treatment,data=subset(cue.day,Date>as.Date("2014-01-19")),FUN=sum,keep.names=T)
  cue.sums.dd.trt <- summaryBy(Cgain+Hloss~Water_treatment+T_treatment,
                               data=cue.sums.dd,FUN=c(mean,se))
  
  #--------------------------------------------------------------------------------------------------
  
  
  windows(35,25)
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), 
         widths=c(2,1,2,1), heights=c(2,2,2,2))
  par( mar=c(4,5,0.5,2), oma=c(3,3,0.5,1.5))
  
  #palette(brewer.pal(5,"Accent")[c(1,2,3,5)])
  #palette(brewer.pal(4,"Dark2"))
  
  palette(c(rev(brewer.pal(4,"Set1")[1:2]),brewer.pal(4,"Set1")[3:4]))
  
  
  #---------- plot C fluxes
  #- start plot
  plotBy(Cgain.mean~Date_week|T_treatment,data=subset(cue.day.trt,Water_treatment=="control"),type="l",lwd=2,
         ylim=c(0,80),legend=F,axes=F,ylab="",xlab="")
  
  #- gussy up the graph
  magaxis(side=c(2),labels=c(1),las=1,frame.plot=T,cex.axis=1.2)
  axis.Date(side=1,at=seq.Date(from=as.Date("2013-9-1"),to=as.Date("2014-6-1"),by="month"),tcl=0.5,cex.axis=1.2)
  title(ylab=expression(C~uptake~(g~d^-1)),cex.lab=1.7,xpd=NA)
  
  #- add polygons showing SE
  cue.day.trt_a <- subset(cue.day.trt,T_treatment=="ambient" & Water_treatment=="control")
  cue.day.trt_e <- subset(cue.day.trt,T_treatment=="elevated" & Water_treatment=="control")
  cue.day.trt_a_dry <- subset(cue.day.trt,T_treatment=="ambient" & Water_treatment=="drydown")
  cue.day.trt_e_dry <- subset(cue.day.trt,T_treatment=="elevated" & Water_treatment=="drydown")
  polygon(x = c(cue.day.trt_a$Date_week, rev(cue.day.trt_a$Date_week)), 
          y = c(cue.day.trt_a$Cgain.low,rev(cue.day.trt_a$Cgain.high)) , col = alpha(palette()[1],0.15), border = F)
  polygon(x = c(cue.day.trt_e$Date_week, rev(cue.day.trt_e$Date_week)), 
          y = c(cue.day.trt_e$Cgain.low,rev(cue.day.trt_e$Cgain.high)) , col = alpha(palette()[2],0.15), border = F)
  polygon(x = c(cue.day.trt_a_dry$Date_week, rev(cue.day.trt_a_dry$Date_week)), 
          y = c(cue.day.trt_a_dry$Cgain.low,rev(cue.day.trt_a_dry$Cgain.high)) , col = alpha(palette()[3],0.15), border = F)
  polygon(x = c(cue.day.trt_e_dry$Date_week, rev(cue.day.trt_e_dry$Date_week)), 
          y = c(cue.day.trt_e_dry$Cgain.low,rev(cue.day.trt_e_dry$Cgain.high)) , col = alpha(palette()[4],0.15), border = F)
  
  #- overlay bolded means
  plotBy(Cgain.mean~Date_week|T_treatment,data=subset(cue.day.trt,Water_treatment=="control"),type="l",lwd=3,
         ylim=c(0,100),add=T,legend=F)
  plotBy(Cgain.mean~Date_week|T_treatment,data=subset(cue.day.trt,Water_treatment=="drydown"),col=palette()[3:4],
         type="l",lwd=3,add=T,legend=F)
  legend("topleft",legend=c("A-Con","W-Con","A-Dry","W-Dry"),fill=palette()[1:4],seg.len=3,cex=1.5)
  legend("topright",letters[1],bty="n",cex=1.5)
  #---
  
  #- add experiment-wise sums
  xvals <- graphics::barplot(c(cue.sums.trt$Cgain.mean,cue.sums.dd.trt$Cgain.mean),ylim=c(0,7000),
                             col=c(palette()[1:2],palette()[1:4]),las=2,
                             names.arg=c("A-Con","W-Con","A-Con","W-Con","A-Dry","W-Dry"))
  abline(v=2.5,lty=2)
  adderrorbars(x=xvals,y=c(cue.sums.trt$Cgain.mean,cue.sums.dd.trt$Cgain.mean),
               SE=c(cue.sums.trt$Cgain.se,cue.sums.dd.trt$Cgain.se),direction="updown")
  title(ylab=expression(C~uptake~(g)),cex.lab=1.5)
  
  #graphics::text(x=1.2,y=7200,"Pre",cex=2)
  #graphics::text(x=4.5,y=6800,"Drought",cex=2)
  legend("topright",letters[2],bty="n",cex=1.5)
  
  #---------- 
  
  
  #---------- plot water fluxes
  #- start plot
  plotBy(Hloss.mean~Date_week|T_treatment,data=subset(cue.day.trt,Water_treatment=="control"),type="l",lwd=2,
         ylim=c(0,20),legend=F,axes=F,ylab="",xlab="")
  
  #- gussy up the graph
  magaxis(side=c(2),labels=c(1),las=1,frame.plot=T,cex.axis=1.2)
  axis.Date(side=1,at=seq.Date(from=as.Date("2013-9-1"),to=as.Date("2014-6-1"),by="month"),tcl=0.5,cex.axis=1.2)
  title(ylab=expression(H[2]*O~loss~(kg~d^-1)),cex.lab=1.7,xpd=NA)
  
  #- add polygons showing SE
  cue.day.trt_a <- subset(cue.day.trt,T_treatment=="ambient" & Water_treatment=="control")
  cue.day.trt_e <- subset(cue.day.trt,T_treatment=="elevated" & Water_treatment=="control")
  cue.day.trt_a_dry <- subset(cue.day.trt,T_treatment=="ambient" & Water_treatment=="drydown")
  cue.day.trt_e_dry <- subset(cue.day.trt,T_treatment=="elevated" & Water_treatment=="drydown")
  polygon(x = c(cue.day.trt_a$Date_week, rev(cue.day.trt_a$Date_week)), 
          y = c(cue.day.trt_a$Hloss.low,rev(cue.day.trt_a$Hloss.high)) , col = alpha(palette()[1],0.15), border = F)
  polygon(x = c(cue.day.trt_e$Date_week, rev(cue.day.trt_e$Date_week)), 
          y = c(cue.day.trt_e$Hloss.low,rev(cue.day.trt_e$Hloss.high)) , col = alpha(palette()[2],0.15), border = F)
  polygon(x = c(cue.day.trt_a_dry$Date_week, rev(cue.day.trt_a_dry$Date_week)), 
          y = c(cue.day.trt_a_dry$Hloss.low,rev(cue.day.trt_a_dry$Hloss.high)) , col = alpha(palette()[3],0.15), border = F)
  polygon(x = c(cue.day.trt_e_dry$Date_week, rev(cue.day.trt_e_dry$Date_week)), 
          y = c(cue.day.trt_e_dry$Hloss.low,rev(cue.day.trt_e_dry$Hloss.high)) , col = alpha(palette()[4],0.15), border = F)
  
  #- overlay bolded means
  plotBy(Hloss.mean~Date_week|T_treatment,data=subset(cue.day.trt,Water_treatment=="control"),type="l",lwd=3,
         ylim=c(0,100),add=T,legend=F)
  plotBy(Hloss.mean~Date_week|T_treatment,data=subset(cue.day.trt,Water_treatment=="drydown"),col=palette()[3:4],
         type="l",lwd=3,add=T,legend=F)
  legend("topright",letters[3],bty="n",cex=1.5)
  
  #---
  
  #- add experiment-wise sums
  xvals <- graphics::barplot(c(cue.sums.trt$Hloss.mean,cue.sums.dd.trt$Hloss.mean),ylim=c(0,2000),
                             col=c(palette()[1:2],palette()[1:4]),las=2,
                             names.arg=c("A-Con","W-Con","A-Con","W-Con","A-Dry","W-Dry"))
  abline(v=2.5,lty=2)
  adderrorbars(x=xvals,y=c(cue.sums.trt$Hloss.mean,cue.sums.dd.trt$Hloss.mean),
               SE=c(cue.sums.trt$Hloss.se,cue.sums.dd.trt$Hloss.se),direction="updown")
  title(ylab=expression(H[2]*O~loss~(kg)),cex.lab=1.5)
  legend("topright",letters[4],bty="n",cex=1.5)
  
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
}