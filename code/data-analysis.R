rm(list = ls())
###########
# LIBRARIES
###########
library(plyr)
library(dplyr)
library(maps)
library(mapproj)
library(foreign)
library(maptools)
library(raster)
library(sp)
library(graphics)
library(vegan)

##############
# LOADING DATA
##############
# The data in this github repository was downloaded on 8/7/2015
# Replace 'pathToData' with the location of the carabid data downloaded from NEON's web portal
pathToData <- 'C:/Users/klevan/Documents/GitHub/carabid-workshop/data/cleaned data'
setwd(pathToData)

bet_div <- read.csv(paste(pathToData,"carabidAbundanceData.csv",sep='/'),stringsAsFactors = FALSE,header = TRUE) 
bet_div1 <- read.csv(paste(pathToData,"carabidDiversityData.csv",sep='/'),stringsAsFactors = FALSE,header = TRUE) 
bet_field <- read.csv(paste(pathToData,"fieldData-cleaned.csv",sep='/'),stringsAsFactors = FALSE,header = TRUE); bet_field$beetleAbundance <- as.numeric(bet_field$beetleAbundance); bet_field$beetleRichness <- as.numeric(bet_field$beetleRichness) 
bet_sort <- read.csv(paste(pathToData,"sortData-cleaned.csv",sep='/'),stringsAsFactors = FALSE,header = TRUE) 
bet_pin <- read.csv(paste(pathToData,"pinData-cleaned.csv",sep='/'),stringsAsFactors = FALSE,header = TRUE) 
weather <- read.csv(paste(substr(pathToData,1,54),"NOAA weather data for 2014.csv",sep='/'),stringsAsFactors = FALSE,header = TRUE) 

# Inputting NEON Domain info
setwd(paste(substr(pathToData,1,54),'map data',sep='/'))
Ddbf<-read.dbf("NEON_Domains.dbf")
Dmap<-readShapePoly("NEON_Domains.shp")
crs(Dmap) <- "+proj=utm +units=m +ellps=WGS84"

#############################
# Treating the data correctly
#############################
bet_div$collectDate <- as.Date(bet_div$collectDate,format = '%m/%d/%Y'); bet_div$etOHChangeDate <- as.Date(bet_div$etOHChangeDate,format = '%m/%d/%Y'); bet_div$processingDate <- as.Date(bet_div$processingDate,format = '%m/%d/%Y'); bet_div$identifiedDate <- as.Date(bet_div$identifiedDate,format = '%m/%d/%Y'); bet_div$individualCount<- as.numeric(bet_div$individualCount)
bet_div1$collectDate <- as.Date(bet_div1$collectDate,format = '%m/%d/%Y')
bet_field$collectDate <- as.Date(bet_field$collectDate,format = '%m/%d/%Y'); bet_field$setDate <- as.Date(bet_field$setDate,format = '%m/%d/%Y')
bet_sort$collectDate <- as.Date(bet_sort$collectDate,format = '%m/%d/%Y'); bet_sort$etOHChangeDate <- as.Date(bet_sort$etOHChangeDate,format = '%m/%d/%Y'); bet_sort$processingDate <- as.Date(bet_sort$processingDate,format = '%m/%d/%Y'); bet_sort$identifiedDate <- as.Date(bet_sort$identifiedDate,format = '%m/%d/%Y')
bet_pin$collectDate <- as.Date(bet_pin$collectDate,format = '%m/%d/%Y'); bet_pin$processingDate <- as.Date(bet_pin$processingDate,format = '%m/%d/%Y'); bet_pin$identifiedDate <- as.Date(bet_pin$identifiedDate,format = '%m/%d/%Y')

for (i in 1:dim(bet_div1)[1]){
  bet_field %>% 
    filter(plotID==bet_div1$plotID[i],trapID==bet_div1$trapID[i],
           is.na(decimalLatitude)!=TRUE) -> a
  bet_div1$decimalLatitude[i] <- a$decimalLatitude[1]
  bet_div1$decimalLongitude[i] <- a$decimalLongitude[1]
  bet_div1$nlcdClass[i] <- a$nlcdClass[1]
  rm(a)
}

##########################################
# Creating data frames at different levels
##########################################
# bet_field; calculated at the plot level
bet_field_plot <- unique.data.frame(bet_field[c("domainID","siteID","plotID","nlcdClass",
                                                "setDate","collectDate",'eventID')])
for (i in unique(bet_field_plot$eventID)){
  bet_field %>% 
    filter(eventID==i)->a
  bet_div1 %>% 
    # select(-sampleID,-trapID) %>% 
    filter(plotID==substr(i,5,12),collectDate==bet_field_plot$collectDate[match(i,bet_field_plot$eventID)])-> div; div <- unique.data.frame(div[,c(1:3,5,7:10)])
  div %>% 
    filter(nchar(taxonID)<14)-> div1
  bet_field_plot$nlcdClass[match(i,bet_field_plot$eventID)] <- a$nlcdClass[1]
  bet_field_plot$decimalLatitude[match(i,bet_field_plot$eventID)] <- mean(a$decimalLatitude,na.rm = TRUE)
  bet_field_plot$decimalLongitude[match(i,bet_field_plot$eventID)]<- mean(a$decimalLongitude,na.rm = TRUE)
  bet_field_plot$geodeticDatum[match(i,bet_field_plot$eventID)]<- a$geodeticDatum[1]
  bet_field_plot$coordinateUncertainty[match(i,bet_field_plot$eventID)]<- a$coordinateUncertainty[1]
  bet_field_plot$elevation[match(i,bet_field_plot$eventID)]<- mean(a$elevation,na.rm = TRUE)
  bet_field_plot$elevationUncertainty[match(i,bet_field_plot$eventID)]<- a$elevationUncertainty[1]
  bet_field_plot$beetleAbundance[match(i,bet_field_plot$eventID)]<-sum(a$beetleAbundance)
  bet_field_plot$beetleRichness[match(i,bet_field_plot$eventID)] <- dim(div)[1]
  bet_field_plot$beetleRichnessNoMorphospecies[match(i,bet_field_plot$eventID)] <- dim(div1)[1]
  bet_field_plot$prcp[match(i,bet_field_plot$eventID)]<-mean(a$prcp)
  rm(a,div,div1)
}

# bet_field calculated at the bout level
bet_field_bout <- unique.data.frame(bet_field[,c("domainID","siteID","collectDate")])
for (i in 1:dim(bet_field_bout)[1]){
  bet_field %>% 
    filter(domainID==bet_field_bout$domainID[i],siteID==bet_field_bout$siteID[i],
           collectDate==bet_field_bout$collectDate[i]) -> a
  # Coordinates
  bet_field_bout$decimalLatitude[i] <- mean(a$decimalLatitude,na.rm=TRUE)
  bet_field_bout$decimalLongitude[i] <- mean(a$decimalLongitude,na.rm=TRUE)
  # Rainfall
  bet_field_bout$prcp[i]<-mean(a$prcp)
  # beetle Abundance
  bet_field_bout$beetleAbundance[i] <- sum(a$beetleAbundance)
  # beetle Richness
  bet_div1 %>% 
    #    select(-sampleID,-trapID,-plotID) %>% 
    filter(domainID==bet_field_bout$domainID[i],siteID==bet_field_bout$siteID[i],
           collectDate==bet_field_bout$collectDate[i]) -> a; a <- unique.data.frame(a[,c(1:2,5,7:10)])
  bet_field_bout$beetleRichness[i] <- dim(a)[1]
}

# Making a dataframe summarizing sites
siteID <- c("BART", "HARV", "SCBI", "DSNY", "OSBS", "JERC", "UNDE", "ORNL", "TALL", "WOOD", "CPER", "STER", "ONAQ")
locales <- c('Bartlett Experimental Forest, NH','Harvard Forest, MA','Smithsonian Conservation Biology Institute, VA',
             'Disney Wilderness Preserve, FL','Ordway-Swisher Biological Station, FL','Jones Ecological Research Center, GA',
             'University of Notre Dame Environmental Research Center, MI','Oak Ridge, TN','Talladega National Forest, AL',
             'Woodworth, ND','Central Plains Experimental Range, CO','North Sterling, CO','Onaqui-Ault, UT')
sites <- as.data.frame(cbind(siteID,locales)); rm(siteID)
# bet_field calculated at the site level
for (i in 1:dim(sites)[1]){
  bet_field %>% 
    filter(siteID==sites$siteID[i])-> a
  bet_div1 %>% 
    filter(siteID==sites$siteID[i])->div; div <- unique(div$taxonID)
  bet_div1 %>% 
    filter(siteID==sites$siteID[i],scientificName!='')->div1; div1 <- unique(div1$scientificName)
  # temporal info
  sites$beginSampling[i] <- min(as.character(a$setDate),na.rm = TRUE)
  sites$endSampling[i] <- max(as.character(a$collectDate),na.rm = TRUE)
  sites$decimalLatitude[i] <- mean(a$decimalLatitude,na.rm = TRUE)
  sites$decimalLongitude[i] <- mean(a$decimalLongitude,na.rm = TRUE)
  # precip data
  weather %>% 
    filter(siteID==sites$siteID[i],PRCP!='') -> w
  sites$prcp[i] <- sum(w$PRCP,na.rm = TRUE) # mm of rain; divide by 25.4 to convert to inches
  # trapping effort
  sites$numTrapsSampled[i] <- dim(a)[1]
  bet_field_plot %>% 
    filter(siteID==sites$siteID[i]) -> a1
  sites$numPlotsSampled[i] <- dim(a1)[1]
  # Organismal Data
  sites$numCarabidsCaught[i] <- sum(a$beetleAbundance)
  sites$beetleRichness[i] <- length(div)
  sites$beetleRichnessNoMorphospecies[i] <- length(div1)
  sites$numMammalsCaught[i] <- sum(a$numMammalsCaught,na.rm = TRUE)
  sites$numHerpsCaught[i] <- sum(a$numHerpsCaught,na.rm = TRUE)
  rm(w,a,a1,div,div1)
}


# bet_field divided by nlcdClass
types <- c("cultivatedCrops","pastureHay","grasslandHerbaceous","emergentHerbaceousWetlands","shrubScrub","deciduousForest","mixedForest","evergreenForest","woodyWetlands"); nlcd <- as.data.frame(types); rm(types)
for (i in 1:dim(nlcd)[1]){
  bet_field %>% 
    filter(nlcdClass==nlcd$types[i]) -> a
  bet_field %>% 
    #    select(plotID,nlcdClass) %>% 
    filter(nlcdClass==nlcd$types[i]) -> a1; a1 <- unique.data.frame(a1[c('plotID','nlcdClass')])
  bet_div1 %>% 
    filter(nlcdClass==nlcd$types[i])-> div; div <- unique.data.frame(div[c('taxonID','nlcdClass')])
  nlcd$numPlotsSampled[i] <- dim(a1)[1]  
  nlcd$numTrapsSampled[i] <- dim(a)[1]
  nlcd$numMammalsCaught[i] <- sum(a$numMammalsCaught,na.rm = TRUE)
  nlcd$numHerpsCaught[i] <- sum(a$numHerpsCaught,na.rm = TRUE)
  nlcd$beetleAbundance[i] <- sum(a$beetleAbundance,na.rm=TRUE)
  nlcd$beetleRichness[i] <- dim(div)[1]
  rm(a,a1)
}
nlcd$herpsPerTrapNight <- nlcd$numHerpsCaught/(nlcd$numTrapsSampled*14) # Amount of herptile bycatch per nlcdClass per trapnight
nlcd$mamPerTrapNight <- nlcd$numMammalsCaught/(nlcd$numTrapsSampled*14)   # Amount of mammal bycatch per nlcdClass per trapnight

# Look at the list of species
for(i in sites$siteID){
  bet_div1 %>% 
    filter(siteID==i,scientificName!='')->a 
  print(paste(i,'has',length(unique(a$scientificName)),'species',sep=' '))
  print(sort(unique(a$scientificName)))
}
rm(a)
##########################
# Initial data exploration
##########################
# Looking at data quality
par(mfrow=c(1,2))
# Traps were set out for 14 day intervals; deviations can be seen in the histogram
hist(bet_field$daysOfTrapping,xlim=c(0,30),col="black",main="Typical sampling interval",xlab="Days of trapping")

# 40 traps are deployed at each interval; occasionally traps are not deployed because of environmental or logistical reasons (i.e., plot flooding, staffing limitations)
bet_field %>% 
  count(siteID,boutNumber) -> numTrapsDeployed
hist(numTrapsDeployed$n,xlim=c(0,40),col="black",main="Typical number of traps deployed",xlab="Number of traps per site per bout")

# Calculating summary stats for typical trapping interval
trapCalc <- as.data.frame(unique(bet_field[c('siteID')])) # Create a new dataframe 'trapCalc' with the list of unique siteIDs
trapCalc$trappingInterval <- -1
trapCalc$numTrapsDeployed <- -1
for (j in 1:dim(trapCalc)[1]){
  bet_field %>% 
    filter(siteID==trapCalc$siteID[j])  -> a
  trapCalc$trappingInterval[j] <- mean(na.omit(a$daysOfTrapping))
  numTrapsDeployed %>% 
    filter(siteID==trapCalc$siteID[j]) -> a
  trapCalc$numTrapsDeployed[j] <- mean(a$n)
}
# Print summary stats of sampling effort
print(trapCalc)

# Latitudinal Gradients
par(mfrow=c(1,2))
plot(bet_field_bout$decimalLatitude,bet_field_bout$beetleAbundance,xlab='Latitude',ylab='Beetle Abundance',pch=21,bg="deepskyblue")
plot(bet_field_bout$decimalLatitude,bet_field_bout$beetleRichness,xlab='Latitude',ylab='Beetle Richness',pch=21,bg="deepskyblue")

plot(bet_field_plot$decimalLatitude,bet_field_plot$beetleAbundance,xlab='Latitude',ylab='Beetle Abundance',pch=21,bg="turquoise")
plot(bet_field_plot$decimalLatitude,bet_field_plot$beetleRichness,xlab='Latitude',ylab='Beetle Richness',pch=21,bg="turquoise")

plot(bet_field$decimalLatitude,bet_field$beetleAbundance,xlab='Latitude',ylab='Beetle Abundance',pch=21,bg="darkblue")
plot(bet_field$decimalLatitude,bet_field$beetleRichness,xlab='Latitude',ylab='Beetle Richness',pch=21,bg="darkblue")


# Weather effects on catch
plot(bet_field$prcp,bet_field$beetleAbundance,xlab='Precipitation (mm)',ylab='Beetle Abundance per trap',bg='blue',pch=21)
plot(bet_field_plot$prcp,bet_field_plot$beetleAbundance,xlab='Precipitation (mm)',ylab='Beetle Abundance per plot',bg='darkblue',pch=21)

# Habitat effect on catch
#boxplot(beetleAbundance~nlcdClass,data=bet_field_plot,ylim=c(0,100))
nlcd$color <- c('goldenrod',rep('darkolivegreen1',2),'darkolivegreen2','darkolivegreen3',rep('forestgreen',3),'darkgreen')
barplot(nlcd$beetleAbundance,names.arg=nlcd$types,col=nlcd$color,ylab="Beetle Abundance",xlab="NLCD Class",cex.lab=1.5,cex.axis=2.5)
barplot(nlcd$beetleRichness,names.arg=nlcd$types,col=nlcd$color,ylab="Beetle Richness",xlab="NLCD Class",cex.lab=1.5,cex.axis=2.5)



######
# MAPS
######
# Map of beetle abundance by site
plot(Dmap,col="#ADA96E",bg='#77BFC7',main='Beetle Abundance by Site',cex.main=3)
sites <- sites[order(sites[,'numCarabidsCaught']),]; sites$intensity <- heat.colors(13)[13:1]; sites$cex <- c(rep(1,4),rep(1.5,2),rep(2,4),rep(2.5,1),rep(3,1),rep(3.5,1))
points(sites$decimalLongitude,sites$decimalLatitude,bg=sites$intensity,cex=sites$cex,pch=21)

# Map of beetle Richness by site
plot(Dmap,col="#ADA96E",bg='#77BFC7',main='Beetle Richness by Site',cex.main=3)
sites <- sites[order(sites[,'beetleRichness']),]; sites$intensity <- heat.colors(13)[13:1]; sites$cex <- c(rep(1,4),rep(1.5,2),rep(2,4),rep(2.5,1),rep(3,1),rep(3.5,1))
points(sites$decimalLongitude,sites$decimalLatitude,bg=sites$intensity,cex=sites$cex,pch=21)

# Rainfall map
plot(Dmap,col="#ADA96E",bg='#77BFC7',main='Precipitation by Site',cex.main=3)
sites <- sites[order(sites[,'prcp']),]; sites$intensity <- c(rep('white',3),rep('lightblue',3),rep('turquoise',3),rep('deepskyblue',2),'blue','darkblue'); sites$cex <- c(rep(1,3),rep(1.5,3),rep(2,3),rep(2.5,2),rep(3,1),rep(3.5,1))
points(sites$decimalLongitude,sites$decimalLatitude,bg=sites$intensity,cex=sites$cex,pch=21)

# habitat map
plot(Dmap,col="#ADA96E",bg='#77BFC7',main='NLCD by Site',cex.main=3)
for(i in sites$siteID){
  bet_field %>% 
    filter(siteID==i,is.na(nlcdClass)==FALSE)-> a; a <- unique.data.frame(a[c('siteID','nlcdClass','plotID')]); a1 <- unique.data.frame(a[c('siteID','nlcdClass')])
  for(j in a1$nlcdClass){
    a %>% 
      filter(nlcdClass==j)-> x 
    a1$number[match(j,a1$nlcdClass)] <- dim(x)[1]
    a1$color[match(j,a1$nlcdClass)] <- nlcd$color[match(j,nlcd$types)]
  }
  pie(a1$number,col = a1$color,labels = '',main = i)
  rm(a,a1)
}

# Plot invasive species
plot(Dmap,col="#ADA96E",bg='#77BFC7',main='Invasive Species',cex.main=3); bet_div1 %>% filter(taxonID=='CARNEM')->CARNEM; bet_div1 %>% filter(taxonID=='TETLAE')->TETLAE
points(CARNEM$decimalLongitude,CARNEM$decimalLatitude,pch=21,bg='violet',cex=2.5)
points(TETLAE$decimalLongitude,TETLAE$decimalLatitude,pch=21,bg='orange',cex=2.5)

#Plot CARNEM through time
plot(Dmap,col="#ADA96E",bg='#77BFC7',main='Invasive Species',cex.main=3); car %>% filter(species=='Carabus nemoralis')->CARNEM1
points(CARNEM1$decimallongitude,CARNEM1$decimallatitude,pch=21,bg='red',cex=2)
points(CARNEM$decimalLongitude,CARNEM$decimalLatitude,pch=21,bg='violet',cex=2.5)
# Making maps of carabid occurences
#for (i in sort(unique(bet_div1$scientificName))[2:145]){
#  car %>% filter(species==i,is.na(Lat)==FALSE)-> a
#  bet_div1 %>% filter(scientificName==i)->bet_div1a
#  if (dim(a)[1]>0){
#    plot(Dmap,col="#ADA96E",bg='#77BFC7',main=i,cex.main=3)
#    points(a$Lon,a$Lat,col='#DC381F',pch=19,cex=2.5)
#    points(bet_div1a$decimalLongitude,bet_div1a$decimalLatitude,bg='#893BFF',pch=21,cex=4)
#  }
#}

par()
# Plotting at a single site: CPER
bet_div1 %>% filter(siteID=='CPER')-> C
plot(C$decimalLongitude,C$decimalLatitude)

for (i in sort(unique(C$collectDate))){
  C %>% 
    filter(collectDate==i,taxonID=="PASELO")-> p
  plot(p$decimalLongitude,p$decimalLatitude, col='black',
       main=as.Date(i,origin = as.Date('1970-01-01')),xlab="Longitude",ylab="Latitude",
       ylim=c(min(C$decimalLatitude),max(C$decimalLatitude)),
       xlim=c(max(C$decimalLongitude),min(C$decimalLongitude)))
  C %>% filter(collectDate==i,taxonID=='CRADUB')-> c
  points(c$decimalLongitude,c$decimalLatitude, bg='red',pch=21)
}
C %>% filter(taxonID=='PASELO')-> PASELO; C %>% filter(taxonID=='CRADUB')-> CRADUB
for (i in sort(unique(C$taxonID))){
  C %>% 
    filter(taxonID==i)->x 
  hist(x$collectDate,main=i,breaks=10,col='black')
}


j=1
for (i in sort(unique(C$taxonID))){
  C %>% 
    filter(taxonID==i)->x
  if(dim(x)[1]>1){
    if((density(x$julian)$n)>10){
    plot(density(x$julian),main=i,
         xlim=c(0,365),ylim=c(0,0.1))  
    polygon(density(x$julian),col=rainbow(13)[j], border="black")
    j=j+1
    }
  }
}



# Diving into site seasonal abundances
for (i in sort(unique(sites$siteID))){
  # Isolate the diversity within a site
  bet_div1 %>% 
    filter(siteID==i)-> x
  # Figure out the abundance of each taxon separated by date
  for (j in sort(unique(x$taxonID))){
    # get records for things identified in the sort stage
    bet_sort %>% filter(siteID==i,taxonID==j)->x_sort
    x_sort$sampleID <- substr(x_sort$associatedSampleID,1,19)
    
    # get records identified from the pin stage
    bet_pin %>% filter(siteID==i,taxonID==j,sampleType=='other carabid')->x_pin
    x_pin$sampleID <- substr(x_pin$sampleID,1,19)
    
    # summarize records from both sources
    x_all <- (rbind(x_pin[c('domainID','siteID','plotID','trapID','collectDate','sampleID','taxonID','individualCount','sampleType')],
                    x_sort[c('domainID','siteID','plotID','trapID','collectDate','sampleID','taxonID','individualCount','sampleType')]))
    x_all %>% 
      group_by(collectDate) %>% 
      summarise()-> x_date; x_date <- as.data.frame(sort(x_date$collectDate)); colnames(x_date)[1] <- 'collectDate'
    # Determine counts
    for (k in 1:dim(x_date)[1]){
      x_date$count[k] <- sum(subset(x_all,collectDate==x_date$collectDate[k])$individualCount,na.rm=TRUE)
    }
    x_date$siteID <- i; x_date$taxonID <- j; x_date$color <- rainbow(n=length(unique(x$taxonID)))[match(j,sort(unique(x$taxonID)))]
    # Create the list
    if (j==sort(unique(x$taxonID))[1]){
      a <- x_date
    } else {
      a <- rbind(a,x_date)
    }
  }
  plot(a$collectDate,a$count,bg=a$color,pch=21,cex=3,cex.axis=2,
       xlab='Date',ylab='Abundance',main=sites$locales[match(i,sites$siteID)])
}

############
# PERMANOVA
############
spp <- sort(unique(bet_div1$scientificName))[2:145]
species <- matrix(rep(0,length(spp)*length(sites$siteID)),ncol = length(spp),nrow = length(sites$siteID),byrow = TRUE,dimnames = list(sites$siteID,spp))
for (i in sites$siteID){
  for (j in spp){
    bet_div1 %>% 
      filter(siteID==i,scientificName==j) -> div
    species[match(i,sites$siteID),match(j,spp)] <- dim(div)[1]
  }
}
species <- species[c(1:8,10:12),] # Remove TALL and ONAQ, because they have no records in the matrix 'species'
adonis(species~decimalLatitude+prcp,data=sites[c(2:6,8:13),],permutations = 9999)
spp <- metaMDS(species); spp$domains <- c(rep('D01',2),'D02',rep('D03',3),'D05','D07','D09',rep('D10',2))
spp <- as.data.frame(cbind(as.numeric(spp$points[,1]),as.numeric(spp$points[,2]),spp$domains)); colnames(spp) <- c('MDS1','MDS2','domainID')
plot(spp$MDS1,spp$MDS2)
