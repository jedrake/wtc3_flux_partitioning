#------------------------------------------------------------------------------------------------------------
#- function to return the measured stemwood and bark densities
getBarkWoodDensity <- function(){
  
  #--- Bark density data
  bark <- read.csv("data/WTC_TEMP_CM_BARKDENSITY_20140528_L1.csv")
  
  
  #--- wood density data
  wood <- read.csv("data/WTC_TEMP_CM_WOODDENSITY_20140528_L1.csv")
  
  wood$chamber_n <- as.numeric(substr(wood$chamber,start=2,stop=3))
  wood$T_treatment <- as.factor(ifelse(wood$chamber_n %% 2 == 1, "ambient","elevated"))
  wood2 <- subset(wood,Layer != "G Tip")
  wood2$Layer <- factor(wood2$Layer)
  wood2$position <- as.factor(ifelse(wood2$Layer == "Base","low",
                                     ifelse(wood2$Layer == "Middle","mid","top")))

  #-- merge wood and bark data
  dense <- merge(wood2,bark,by=c("chamber","position","T_treatment"))
  
  dense_out <- dense[,c("chamber","position","T_treatment","wooddensity","bark_density","diamoverbark","diamunderbark")]
  return(dense_out)
}
#------------------------------------------------------------------------------------------------------------