#------------------------------------------------------------------------------------------------------------
# get an estimate of bole volume for every measurement day
getvol <- function(){
  
  
  # get the data. the "interpolated 38" csv was manual editied in excel to linearly interpolate the UPPER stem estimates
  #   on measurement date 38 as the mean of measurements #37 and #39.
  #downloadCSV(filename="Wdata/from HIEv/WTC_TEMP_CM_TREE-HEIGHT-DIAMETER_20121120-20140527_L1_V2.CSV,topath="data/from HIEv/")
  #size <- read.csv("data/from HIEv/WTC_TEMP_CM_TREE-HEIGHT-DIAMETER_20121120-20140527_L1_V2.CSV")
  size <- read.csv("WTC_TEMP_CM_TREE-HEIGHT-DIAMETER_20121120-20140527_L1_V2_interpolated38.csv")
  names(size)[15:47] <- paste("d",substr(names(size)[15:47],start=2,stop=5),sep="")
  size <- size[,1:49]
  size$DateTime <- as.Date(size$DateTime)
  
  #ignore the reference plots
  size2 <- subset(size,T_treatment !="reference")
  size2$T_treatment <- factor(size2$T_treatment)
  size2$Water_treatment <- factor(size2$Water_treatment)
  size2$chamber <- factor(size2$chamber)
  
  #melt into "long" format
  sLong <- reshape2::melt(size2,measure.vars=names(size)[15:47],value.name="diam",variable.name="height")
  sLong$height_n <- as.numeric(substr(sLong$height,start=2,stop=4))
  
  sLong <- subset(sLong,height_n>=65)
  
  
  
  # estimate volume
  # split long dataframe into a list for each chamber and each measurement date
  sLong$chamber_date <- paste(sLong$chamber,sLong$Measurement,sep="_")
  sLong.l <- split(sLong,sLong$chamber_date)
  
  #-- add a number for the ground (assume no taper) and a value for the maximum tree height (assume diameter of 0.1mm)
  for (i in 1:length(sLong.l)){
    # add a line to the dataframe for the diameter at floor height
    firstline <- sLong.l[[i]][1,]  
    firstline$height_n[1] <- 0 #. Edited to give Mike an estimate of total tree volume to the ground.
    
    # add a line to the dataframe for the tiny diameter at total plant height
    lastline <- sLong.l[[i]][nrow(sLong.l[[i]]),]
    lastline$height_n[1] <- lastline$Plant_height[1]
    lastline$diam[1] <- 0.1 
    sLong.l[[i]] <- rbind(firstline,sLong.l[[i]],lastline)
    sLong.l[[i]] <- sLong.l[[i]][which(is.na(sLong.l[[i]]$diam)==F),] # get rid of NA's
  }
  #treevol(sLong.l[[100]]) # example of volume calculation for a single observation of a single tree
  vols <- do.call(rbind,lapply(sLong.l,FUN=treevol))
  return(vols)
}
#------------------------------------------------------------------------------------------------------------
