#plot recent soil moisture and precip, for the drought experiment in the WTCs
plot_soil_moisture_LWPpd <- function(){

dfr <- downloadTOA5("WTC[0-9]{1,2}_Table1", maxnfiles=500,startDate= ("2013-12-01"),endDate="2014-7-01",
                    topath="met data/",
                    cachefile="met data/WTC_soilmet.rdata")
dfr$chamber <- paste0("ch", str_extract(dfr$Source, "[0-9]{2}"))
dfr$date <- as.Date(dfr$DateTime)

#exclude data prior to 15 May 2013
dfr3 <- subset(dfr,date>as.Date("2013-12-01"))


#integrate drought treatment into dataframe
chamber <- c("ch01","ch02","ch03","ch04","ch05","ch06",
             "ch07","ch08","ch09","ch10","ch11","ch12")
wtreat <- c("drydown","control","drydown","drydown","control","drydown",
            "control","drydown","control","control","drydown","control")
wtreats <- data.frame(chamber,wtreat)
dfr4 <- merge(dfr3,wtreats,by="chamber")
dfr4$ttreat <- ifelse(as.numeric(substr(dfr4$chamber,start=3,stop=4)) %% 2 == 1,"ambient","warmed")


#calculate daily means, then treatment means
dfr.day <- summaryBy(.~date+chamber+ttreat+wtreat,data=dfr4,FUN=mean,keep.names=T,na.rm=T)
dfr.day.treat <- summaryBy(.~date+ttreat+wtreat,data=dfr.day,FUN=c(mean,se),keep.names=T,na.rm=T)
dfr.day.treat$combotreat <- paste(dfr.day.treat$ttreat,dfr.day.treat$wtreat,sep="_")

#split
dat1.list <- split(dfr.day.treat,dfr.day.treat$combotreat)
#--------------------------------------------------------------------------------------------------




#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#- download the leaf water potential data, add to plot
lwp <- read.csv("data/WTC_TEMP_CM_WATERPOTENTIAL-PREDAWN-MIDDAY_20130515-20140424_L2.csv")
lwp$Date <- as.Date(lwp$date,format="%d/%m/%Y")
lwp$LWPpd <- -0.1*lwp$predawn


#average by chamber and date
lwp2 <- summaryBy(LWPpd~Date+T_treatment+Water_treatment+chamber,data=lwp,FUN=mean,keep.names=T,na.rm=T)

#average by treatments
lwp3 <- summaryBy(LWPpd~Date+T_treatment+Water_treatment,data=subset(lwp2,Date>as.Date("2012-01-01")),FUN=c(mean,se),na.rm=T)
lwp3$combotreat <- paste(lwp3$T_treatment,lwp3$Water_treatment,sep="_")


#- extract a conversion table for chamber, T_treatment, and Water_treatment
link <- summaryBy(LWPpd~chamber+T_treatment+Water_treatment,data=lwp2,FUN=mean)[,1:3]

#----
#- also get Sebastian's leaf water potential data
lwp.s <- read.csv("data/WTC_TEMP_CM_WATERPOTENTIAL-DIURNALS_20140214-20140514_R.csv")
lwp.s <- subset(lwp.s,Date=="2014-05-14" & time=="6:00")
lwp.s$LWPpd <- -0.1*lwp.s$LWP
lwp.s$Date <-as.Date(lwp.s$Date)
lwp.s <- merge(lwp.s,link,by="chamber")

lwp.s2 <- summaryBy(LWPpd~Date+T_treatment+Water_treatment+chamber,data=lwp.s,FUN=mean,keep.names=T,na.rm=T)

#average by treatments
lwp.s3 <- summaryBy(LWPpd~Date+T_treatment+Water_treatment,data=lwp.s2,FUN=c(mean,se),na.rm=T)
lwp.s3$combotreat <- paste(lwp.s3$T_treatment,lwp.s3$Water_treatment,sep="_")

#- put Sebastian's data on the bottom of Mikes
lwp3 <- rbind(lwp3,lwp.s3)
lwp.list <- split(lwp3,lwp3$combotreat)



#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------










#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#plot soil moisture over time, focusing on the drought treatments

#plotting parameters
startdate <- as.Date("2013-12-01")
enddate <- as.Date("2014-05-25")
cols <- c("blue","blue","red","red")
bgcols <- c("blue","white","red","white","white","forestgreen")


#now plot soil moisture
windows(12,15)
par(mfrow=c(4,1),oma=c(8,7,2,9),mar=c(0.25,0.25,0.25,0.25))
plot.new()
plot.window(xlim=c(startdate,enddate),ylim=c(0,0.35))
box()
title(xlab="",ylab=expression("Volumetric water content"~"("*m^3~m^-3*")"),cex.lab=2,outer=T,line=4,las=1,adj=0.8)
title(xlab="",ylab=expression(Psi[pd]~"("*MPa*")"),cex.lab=2,outer=T,line=4,las=1,adj=0.05)

axis(side=2,labels=T,cex.axis=1.5,tck=0.025,las=2,at=c(0,0.1,0.2,0.3))
axis(side=4,labels=T,cex.axis=1.5,tck=0.025,las=2,at=c(0,0.1,0.2,0.3))
legend("topright",legend=letters[1],bty="n",cex=1.4)
for (i in 1:length(dat1.list)){
  dat1 <- dat1.list[[i]]
  #plot surface
  adderrorbars(x=dat1$date,y=dat1$VW_Avg.1..mean,SE=dat1$VW_Avg.1..se,direction="updown",
               col=cols[i],add=T)
  abline(h=0.05,lty=2,lwd=2)
  points(VW_Avg.1..mean~date,data=dat1,pch=21,col=cols[i],bg=bgcols[i],add=T,cex=1.4,type="b",lty=i)
}
legend("top",legend=c("A-Wet","A-Dry","W-Wet","W-Dry"),ncol=2,pch=c(21),
       seg.len=3,bty="n",lwd=2,pt.lwd=1,col=c("blue","blue","red","red"),
       pt.bg=c("blue","white","red","white"),pt.cex=1.5,cex=1.2,lty=c(1,2,1,2))
axis.Date(side=1,at=seq.Date(from=as.Date("2013-12-1"),to=as.Date("2014-6-1"),by="month"),tcl=0.5,cex.axis=1.3,
          labels=F)
#plot mid depth
plot.new()
plot.window(xlim=c(startdate,enddate),ylim=c(0,0.35))
legend("topright",legend=letters[2],bty="n",cex=1.4)
box()
axis(side=2,labels=T,cex.axis=1.5,tck=0.025,las=2,at=c(0,0.1,0.2,0.3))
axis(side=4,labels=T,cex.axis=1.5,tck=0.025,las=2,at=c(0,0.1,0.2,0.3))
for (i in 1:length(dat1.list)){
  dat1 <- dat1.list[[i]]
  #plot middle depth
  adderrorbars(x=dat1$date,y=dat1$VW_Avg.2..mean,SE=dat1$VW_Avg.2..se,direction="updown",
               col=cols[i],add=T)
  abline(h=0.05,lty=2,lwd=2)
  
  points(VW_Avg.2..mean~date,data=dat1,pch=21,col=cols[i],bg=bgcols[i],add=T,cex=1.4,type="b",lty=i)
}
axis.Date(side=1,at=seq.Date(from=as.Date("2013-12-1"),to=as.Date("2014-6-1"),by="month"),tcl=0.5,cex.axis=1.3,
          labels=F)

#plot deep depth
plot.new()
plot.window(xlim=c(startdate,enddate),ylim=c(0,0.35))
legend("topright",legend=letters[3],bty="n",cex=1.4)
box()
axis(side=2,labels=T,cex.axis=1.5,tck=0.025,las=2,at=c(0,0.1,0.2,0.3))
axis(side=4,labels=T,cex.axis=1.5,tck=0.025,las=2,at=c(0,0.1,0.2,0.3))

for (i in 1:length(dat1.list)){
  dat1 <- dat1.list[[i]]
  #plot deepest depth
  adderrorbars(x=dat1$date,y=dat1$VW_Avg.3..mean,SE=dat1$VW_Avg.3..se,direction="updown",
               col=cols[i],add=T)
  abline(h=0.05,lty=2,lwd=2)
  
  points(VW_Avg.3..mean~date,data=dat1,pch=21,col=cols[i],bg=bgcols[i],add=T,cex=1.4,type="b",lty=i)
}


#plot x-axis labels
#axis(1, xlab="Date",dat1.list[[1]]$date, format(dat1.list[[1]]$date, "%b %d"),cex.axis = 1.3)
axis.Date(side=1,at=seq.Date(from=as.Date("2013-12-1"),to=as.Date("2014-6-1"),by="month"),tcl=0.5,cex.axis=1.3,
          labels=F)




#plot leaf water potential
plot.new()
plot.window(xlim=c(startdate,enddate),ylim=c(-1,0))
#legend("topleft",legend="Mid",bty="n",cex=1.4)
box()
axis(side=2,labels=T,cex.axis=1.5,tck=0.025,las=2)
axis(side=4,labels=T,cex.axis=1.5,tck=0.025,las=2)
legend("topright",legend=letters[4],bty="n",cex=1.4)
for (i in 1:length(lwp.list)){
  dat1 <- lwp.list[[i]]
  
  adderrorbars(x=dat1$Date,y=dat1$LWPpd.mean,SE=dat1$LWPpd.se,direction="updown",
               col=cols[i],add=T)
  abline(h=0.05,lty=2,lwd=2)
  
  points(LWPpd.mean~Date,data=dat1,pch=21,col=cols[i],bg=bgcols[i],add=T,cex=2,lwd=1.5,type="b",lty=i)
}
axis.Date(side=1,at=seq.Date(from=as.Date("2013-12-1"),to=as.Date("2014-6-1"),by="month"),tcl=0.5,cex.axis=1.6,
          labels=T)
title(xlab="Date",outer=T,cex.lab=2)



dev.copy2pdf(file="./output/VWC_drought.pdf")
}