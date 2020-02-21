#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- Try to understand if the soil was warmed by the treatment in WTC3
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------



source("R/loadLibraries.R")


#-----------------------------------------------------------------------------------------------------------
#- download the met data from HIEv
downloadHIEv(searchHIEv("WTC_TEMP_CM_WTCMET"),topath="data")
#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
# Read in the data, extract treatment values from chamber numbers
files <- list.files("WTC_TEMP_CM_WTCMET",path="data",full.names=T)
d.list <- list()
for (i in 1:length(files)){
  print(i)
  d.list[[i]] <- read.csv(files[i])
}
dat <- do.call(rbind,d.list)

dat$chamber_n <- as.factor(substr(dat$chamber,start=2,stop=3)) # extract the chamber number from the filename
dat$T_treatment <- as.factor(ifelse(as.numeric(dat$chamber_n) %% 2 == 0, "elevated","ambient"))
dat$DateTime <- as.POSIXct(dat$DateTime,format="%Y-%m-%d %T",tz="GMT")
dat$Date <- as.Date(dat$DateTime)
#-----------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------
# NA fill some bad data for probes 2 and 6.
dat[which(dat$SoilTempProbe_Avg.2.>35 | dat$SoilTempProbe_Avg.2.<0),"SoilTempProbe_Avg.2."] <- NA
dat[which(dat$SoilTempProbe_Avg.6.>35 | dat$SoilTempProbe_Avg.6.<0),"SoilTempProbe_Avg.6."] <- NA

# Data from depths 1, 3, 4, and 5 are good (that is, from 5-cm, 20-cm, 30-cm, and 50-cm)
#-----------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------
#- calculate daily median values for each chamber on each date at each depth
dat.m <- summaryBy(Tair_al+SoilTempProbe_Avg.1.+SoilTempProbe_Avg.3.+SoilTempProbe_Avg.4.+SoilTempProbe_Avg.5.~T_treatment+chamber+Date,
                   data=subset(dat,Date>as.Date("2013-02-13") & Date < as.Date("2014-05-26")),FUN=c(median),keep.names=T,na.rm=T)
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- calculate average for each treatment on each date
d.means <- summaryBy(Tair_al+SoilTempProbe_Avg.1.+SoilTempProbe_Avg.3.+SoilTempProbe_Avg.4.+SoilTempProbe_Avg.5.~Date+T_treatment,
                     data=dat.m,FUN=c(mean),keep.names=T,na.rm=T)
#d.means <- d.means[complete.cases(d.means),]

#- get average difference for air temperature
diffA <- diff(d.means$Tair_al)
depthA <- diffA[seq(1,length(diffA),2)]
mean(depthA,na.rm=T)
sd(depthA,na.rm=T)

#- get average differece for surface soil temperature (5-cm-depth)
diff1 <- diff(d.means$SoilTempProbe_Avg.1.)
depth1 <- diff1[seq(1,length(diff1),2)]
mean(depth1,na.rm=T)
sd(depth1,na.rm=T)

#- get average differece for mid soil temperature (20-cm-depth)
diff3 <- diff(d.means$SoilTempProbe_Avg.3.)
depth3 <- diff3[seq(1,length(diff3),2)]
mean(depth3,na.rm=T)
sd(depth3,na.rm=T)


#- get average differece for mid soil temperature (50-cm-depth)
diff5 <- diff(d.means$SoilTempProbe_Avg.5.)
depth5 <- diff5[seq(1,length(diff5),2)]
mean(depth5,na.rm=T)
sd(depth5,na.rm=T)

#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- make simple boxplots
windows(80,150)
par(mfrow=c(5,1),mar=c(4,4,0,0),oma=c(0,5,0,0))
ylims=c(15,35)
boxplot(SoilTempProbe_Avg.1.~T_treatment,data=d,col=c("blue","red"),ylim=ylims);legend("topleft","5-cm",bty="n")
legend("bottomright",bty="n",paste("Tdiff = ",round(d.treat[1],2)))
boxplot(SoilTempProbe_Avg.2.~T_treatment,data=d,col=c("blue","red"),ylim=ylims);legend("topleft","10-cm",bty="n")
legend("bottomright",bty="n",paste("Tdiff = ",round(d.treat[2],2)))
boxplot(SoilTempProbe_Avg.3.~T_treatment,data=d,col=c("blue","red"),ylim=ylims);legend("topleft","20-cm",bty="n")
legend("bottomright",bty="n",paste("Tdiff = ",round(d.treat[3],2)))
boxplot(SoilTempProbe_Avg.4.~T_treatment,data=d,col=c("blue","red"),ylim=ylims);legend("topleft","30-cm",bty="n")
legend("bottomright",bty="n",paste("Tdiff = ",round(d.treat[4],2)))
boxplot(SoilTempProbe_Avg.5.~T_treatment,data=d,col=c("blue","red"),ylim=ylims);legend("topleft","50-cm",bty="n")
legend("bottomright",bty="n",paste("Tdiff = ",round(d.treat[5],2)))

title(ylab="Tsoil (deg C)",outer=T,cex.lab=5,line=1)
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- plot treatment temperatures through time
windows(80,150)
par(mfrow=c(5,1),mar=c(4,4,0,0),oma=c(0,5,0,0))
ylims=c(5,30)
plotBy(Tair_al~Date|T_treatment,data=d.means,type="l",col=c("blue","red"),ylim=ylims,legendwhere="topright",
       ylab="Air temperature (deg C)")
plotBy(SoilTempProbe_Avg.1.~Date|T_treatment,data=d.means,type="l",col=c("blue","red"),ylim=ylims,
       ylab="Soil temperature (5-cm)",legend=F)
plotBy(SoilTempProbe_Avg.3.~Date|T_treatment,data=d.means,type="l",col=c("blue","red"),ylim=ylims,
       ylab="Soil temperature (20-cm)",legend=F)
plotBy(SoilTempProbe_Avg.4.~Date|T_treatment,data=d.means,type="l",col=c("blue","red"),ylim=ylims,
       ylab="Soil temperature (30-cm)",legend=F)
plotBy(SoilTempProbe_Avg.5.~Date|T_treatment,data=d.means,type="l",col=c("blue","red"),ylim=ylims,
       ylab="Soil temperature (50-cm)",legend=F)

#-----------------------------------------------------------------------------------------------------------