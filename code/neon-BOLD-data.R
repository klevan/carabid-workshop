# Create your directory location
dirLocation <- 'C:/Users/klevan/Desktop/NEONphotos' # Put in the directory location where you want these to populate
dir.create(dirLocation)

if(file.exists(dirLocation)){
# Needed Libraries
library(XML)
library(bold)

# A dataframe with all the NEON uploaded specimens
allNEONspecimens <- bold_specimens(institutions = "National Ecological Observatory Network, United States")

# The Site location for each sample
specimenPage <- "http://www.boldsystems.org/index.php/Public_RecordView?processid="

# Grab image links for all samples 
noImages <- vector() #dim(allNEONspecimens)[1]
for(i in 1:10){
  tmp <- try(query <- getNodeSet(htmlParse(paste0(specimenPage,allNEONspecimens$processid[i]),encoding = "UTF-8"),"//img"))
  if (class(tmp)=="try-error"){
    noImages <- c(noImages,allNEONspecimens$processid[i])
  } else {
    a <- unlist(sapply(query,xmlToList)) # parse the data return
    if(length(as.character(a[grepl("http://www.boldsystems.org/pics/",a)]))>0){
      allNEONspecimens$image_urls[i] <- as.character(a[grepl("http://www.boldsystems.org/pics/",a)]) # choose the correct photo  
    } else{
      noImages <- c(noImages,allNEONspecimens$processid[i])
    }
  }
}

# Nice message
if (length(noImages)>0){
  print("Sorry. No images available for",paste(noImages, collapse = " "))
}

# Downloading the images 
allImages <- allNEONspecimens[is.na(allNEONspecimens$image_urls)==FALSE,c("processid","image_urls")]
for (i in allImages$image_urls){
  download.file(url=i,destfile = paste(dirLocation,paste0(basename(allImages$processid[match(i,allImages$image_urls)]),".jpg"),sep="/"), mode = 'wb')
}

}