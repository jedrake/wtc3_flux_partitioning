#------------------------------------------------------------------------------------------------------------
# gapfill stem volume (for daily data)
gapfillvol <- function(dat){
  
 
  
  dat$chamber_n <- as.numeric(substr(dat$chamber,start=2,stop=3))
  
  # create dataframe for all days
  alldates <- rep(seq.Date(from=as.Date(range(dat$DateTime)[1]),to=as.Date(range(dat$DateTime)[2]),by="day"),12)
  chamber_n <- c(rep(1,length(alldates)/12),rep(2,length(alldates)/12),rep(3,length(alldates)/12),rep(4,length(alldates)/12),
                 rep(5,length(alldates)/12),rep(6,length(alldates)/12),rep(7,length(alldates)/12),rep(8,length(alldates)/12),
                 rep(9,length(alldates)/12),rep(10,length(alldates)/12),rep(11,length(alldates)/12),rep(12,length(alldates)/12))
  datedf <- data.frame(chamber_n=chamber_n,DateTime=alldates)                                                                                             
  
  #merge data in with dataframe of all days, sort it
  dat2 <- merge(dat,datedf,all=T,by=c("chamber_n","DateTime"))
  dat2 <- dat2[with(dat2,order(chamber_n,DateTime)),]
  
  #break across list, gapfill list
  dat3 <- zoo(dat2)
  dat3$Days_since_transplanting <- na.approx(dat3$Days_since_transplanting)
  dat3$vol <- na.approx(dat3$vol)
  dat3$vol_wood <- na.approx(dat3$vol_wood)
  dat3$vol_bark <- na.approx(dat3$vol_bark)
  dat3$mass_wood <- na.approx(dat3$mass_wood)
  dat3$mass_bark <- na.approx(dat3$mass_bark)
  
  
  
  # get it back to a normal dataframe
  dat4 <- numericdfr(fortify.zoo(dat3))
  dat4$DateTime <- as.Date(dat4$DateTime)
  dat4$Date <- dat4$DateTime
  
  # put some of the other bits back together
  dat4$chamber <- as.factor(paste0("C",sprintf("%02.0f",dat4$chamber_n)))
  dat4$T_treatment <- as.factor(ifelse(dat4$chamber_n %% 2 ==1,"ambient","elevated"))
  dat4$Index <- NULL
  dat4$Stem_number <- NULL
  
  #- clean up for exporting
  dat4_out <- dat4[,c("chamber","Date","T_treatment","Water_treatment","vol","vol_wood","vol_bark","mass_wood","mass_bark")]
  
  return(dat4_out)
  
}