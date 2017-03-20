#-------------------------------------------------------------------------------------------------------------------
#- Download the data associated with Drake et al. 2016 New Phyt.
#  This downloads the zipfile from figshare and extracts it to a folder named "data".

downloadNewPhytData <- function(){
if(!file.exists("data.zip")){
  download.file("https://ndownloader.figshare.com/files/4857112?private_link=cdc9a3caf5bffc0add94", "data.zip", mode="wb")
  unzip("data.zip",overwrite=F)
}
}
#-------------------------------------------------------------------------------------------------------------------
