#------------------------------------------------------------------------------------------------------------
#- Read and process the diameter and height dataset, return a dataframe of gapfilled size data.
returnd2h <- function(){
  
  #------------------------------------------------------------------------------------------------------------
  # get the tree size data
  size <- read.csv("data/WTC_TEMP_CM_TREE-HEIGHT-DIAMETER_20121120-20140527_L1_V2.CSV")
  size$chamber_n <- as.numeric(substr(size$chamber,start=2,stop=3))
  size$DateTime <- as.Date(size$DateTime)
  
  #do some processing to get d2h
  size2 <- base::subset(size,select=c("DateTime","Days_since_transplanting","chamber_n","chamber","T_treatment",
                                      "Water_treatment","Plant_height","Stem_number"))
  size2$diam <- size[,18]
  size2$diam_15 <- size[,16]
  size3 <- subset(size2,chamber_n<=12 & Stem_number==1)
  
  
  
  # tree 11 is different, because the height of stem 1 is not the actual true height. put the max height of the two stems in as the Plant_height
  tree11 <- subset(size2,chamber_n==11)
  tree11.max <- summaryBy(Plant_height~chamber_n+DateTime+Days_since_transplanting,data=tree11,FUN=max,na.rm=T,keep.names=T)
  size3[which(size3$chamber_n==11),"Plant_height"] <- tree11.max$Plant_height
  
  
  #- convert diameters to cm, heights to m
  size3$diam <- size3$diam/10
  size3$diam_15 <- size3$diam_15/10
  size3$Plant_height <- size3$Plant_height/100
  
  #get rid of NA's
  size4 <- subset(size3,diam>0)
  size4 <- size4[with(size4,order(DateTime,chamber_n)),]
  
  #get just the data with diameter at 15
  size_small <- subset(size3,diam_15>0 & year(DateTime)<2014)
  #------------------------------------------------------------------------------------------------------------
  
  
   
  #------------------------------------------------------------------------------------------------------------
  # gap fill diameter and height data
  
  # create dataframe for all days
  alldates <- rep(seq.Date(from=as.Date(range(size4$DateTime)[1]),to=as.Date(range(size4$DateTime)[2]),by="day"),12)
  chamber_n <- c(rep(1,length(alldates)/12),rep(2,length(alldates)/12),rep(3,length(alldates)/12),rep(4,length(alldates)/12),
                  rep(5,length(alldates)/12),rep(6,length(alldates)/12),rep(7,length(alldates)/12),rep(8,length(alldates)/12),
                  rep(9,length(alldates)/12),rep(10,length(alldates)/12),rep(11,length(alldates)/12),rep(12,length(alldates)/12))
  datedf <- data.frame(chamber_n=chamber_n,DateTime=alldates)                                                                                             
   
  #merge data in with dataframe of all days
  size5 <-merge(size4,datedf,all=T,by=c("chamber_n","DateTime"))
  
  #break across list, gapfill list
  size6 <- zoo(size5)
  size6$Days_since_transplanting <- na.approx(size6$Days_since_transplanting)
  size6$Plant_height <- na.approx(size6$Plant_height)
  size6$diam <- na.approx(size6$diam)
   
   
  # get it back to a normal dataframe
  size7 <- numericdfr(fortify.zoo(size6))
  size7$Date <- as.Date(size7$DateTime)
  size7$d2h <- with(size7,(diam/10)^2*Plant_height)
   
  # put some of the other bits back together
  size7$chamber <- as.factor(paste0("C",sprintf("%02.0f",size7$chamber_n)))
  size7$T_treatment <- as.factor(ifelse(size7$chamber_n %% 2 ==1,"ambient","elevated"))
  size7$Water_treatment <- "control"
  size7$Index <- NULL
  size7$Stem_number <- NULL
  
  #- establish a treatment key for the Water_treatment variable
  key <- data.frame(chamber=levels(size7$chamber),
                    Water_treatment=c("drydown","control","drydown","drydown","control","drydown",
                                      "control","drydown","control","control","drydown","control"))
  
  size_before <- subset(size7,Date<as.Date("2014-02-4"))
  size_after <- subset(size7,Date>=as.Date("2014-02-4"))
  size_after$Water_treatment <- NULL
  size_after2 <- merge(size_after,key,by="chamber")
  
  #- combined dataframes from before and after the drought began
  size8 <- rbind(size_before,size_after2)
  
  #- clean up dataframe for output
  size_out <- size8[,c("Date","chamber","T_treatment","Water_treatment","diam","Plant_height","d2h")]
  
  
  return(size_out)
}
#--------------------------------------------------------------------------------------------------