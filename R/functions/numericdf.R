# Function for converting all columns in a dataframe to numeric
numericdfr <- function(dfr){
  for(i in 1:ncol(dfr)){
    options(warn=-1)
    oldna <- sum(is.na(dfr[,i]))
    num <- as.numeric(as.character(dfr[,i]))
    if(sum(is.na(num)) > oldna)
      next
    else
      dfr[,i] <- num
  }
  options(warn=0)
  return(dfr)
}