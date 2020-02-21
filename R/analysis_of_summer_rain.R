#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#- Analyze rain data from Hawkesbury, 1881 through 2014
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

#- read in rain data from 1881 through 2014, BOM station # 67021
#   I got this data from Sally Power
raindat1 <- read.csv("data/IDCJAC0009_067021_1800_Rain_Data.csv")

#- subset to just the summer months during which we implemented the drought
raindat2 <- subset(raindat1,Month %in% c(3,4,5))

#- sum rain (mm) over these months for each year
rainsums <- summaryBy(Rainfall.amount..millimetres.~Year,data=raindat2,FUN=sum,keep.names=T,na.rm=T)


#- three early years have no rain during these months
plot(rainsums[,1],rainsums[,2])
which(rainsums[,2]==0) # 3 years have no rain
which(rainsums[,2]<20) # 3 years have less than 20 mL of rain during these months


length(which(rainsums[,2]==0))/nrow(rainsums) # ~2% of years have no rain during this time

length(which(rainsums[,2]>100))/nrow(rainsums) # ~80% of years had >100 mm rain


boxplot(rainsums[,2],ylab="Summer rain (mm)",xlab=c("Rain from 1881-2014"))
