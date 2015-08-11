##############################
########PREMIUM###############
##############################

setwd("C:\\Users\\Sean\\Documents\\GitHub\\Premium\\Test Data")
test_data<-read.csv(file.choose(), stringsAsFactors = FALSE)
attach(test_data)
require(ggmap)
require(RNCEP)
library("PReMiuM", lib.loc="~/R/win-library/3.2")
library(plyr)
library(dplyr)
library(tidyr)

Loc<-as.character(Loc[1:222]) # focus on southwestern for now
yield<-as.numeric(substr(yield_bu.A,1,5))
###codes lat/lon for the 32 unique locations that will be assigned to every observation
###geocoding works through google API, and it is much faster to query only as often as is
###absolutely necessary.
unique_gcode<-geocode(unique(Loc))
unique_gcode<-as.data.frame(cbind(as.character(unique(Loc)),unique_gcode))
gcode<-data.frame(Lon=0,Lat=0)
#plant_date<-vector(length=2289)
plant_date <- vector(length=nrow(test_data))
#harvest_date<-vector(length=2289)
harvest_date<-vector(length = nrow(test_data))

####Assigns geocodes to each observation
#for(i in 1:5){
for(i in 1:length(unique(Loc))){
  gcode[which(Loc==unique_gcode[i,1]),]<-unique_gcode[i,2:3]
  plant_date[which(Loc==unique_gcode[i,1])]<-as.character(planting[which(Loc==unique_gcode[i,1])])
  harvest_date[which(Loc==unique_gcode[i,1])]<-as.character(harvest[which(Loc==unique_gcode[i,1])])
}
Lon_Lat_Loc<-as.data.frame(cbind(gcode,Loc))
Lon_Lat_Loc$Lon <- Lon_Lat_Loc$Lon + 360

Loc_Date<-as.data.frame(cbind(Lon_Lat_Loc,plant_date,harvest_date))
loc_minmax<-c(min(gcode[,1]),max(gcode[,1]),min(gcode[,2]),max(gcode[,2]))
wx.test<-NCEP.gather(variable='air.sig995',level='surface',
            months.minmax = c(5,10),years.minmax = c(2014,2014),
            lat.southnorth = c(loc_minmax[3:4]), lon.westeast = c(loc_minmax[1:2]),
            reanalysis2 = FALSE, return.units = TRUE)
wx.ag<-NCEP.aggregate(wx.test, YEARS = TRUE, MONTHS = FALSE, DAYS = FALSE,
               HOURS = FALSE,fxn='mean')
detach(test_data)

## Function to round lon/lat
roundTo <- function(x, y){
#  which.min(abs(y - x))
  #rounded_x = vector(length = length(x))
  return(y[which.min(abs(y - x))])
}

## Apply function to Longitute and Latitude
Lon_Lat_Loc$Lon2 <- unlist(lapply(Lon_Lat_Loc[,1], function(x) roundTo(x, seq(275, 280, 2.5))))
Lon_Lat_Loc$Lat2 <- unlist(lapply(Lon_Lat_Loc[,2], function(x) roundTo(x, seq(37.5, 42.5, 2.5))))


wx.ag2 <- wx.ag %>% 
  unclass() %>%
  as.data.frame() %>%
  mutate(lat = row.names(.)) %>%
  gather(lon, yearly.avg, -lat) %>%
  mutate(lon = as.numeric(gsub(".2014_XX_XX_XX", '', lon)),
         lat = as.numeric(lat)) %>%
  rename(Lat2 = lat, 
         Lon2 = lon)

## Merge Lon_Lat_Loc to wx.ag2 
## also, merge that do test_data 

test_data2 <- test_data[1:222,]
test_data2$Loc <- as.character(test_data2$Loc)
Lon_Lat_Loc$Loc <- as.character(Lon_Lat_Loc$Loc)

# Becareful with duplicate rows after joining to test_data2
inputData <- Lon_Lat_Loc %>%
  inner_join(., wx.ag2, by=c('Lat2', 'Lon2')) %>%
  distinct(Lon, Lat, Loc, Lon2, Lat2, yearly.avg) %>%
  inner_join(., test_data2, by = 'Loc') %>%
  mutate(yield = yield_bu.A, 
         yield = as.numeric(gsub('\\*', '', yield)))
  

##
##
## Now, try to get the data into profRegr
##
## covariate: yearly.avg
## outcome: yield 
##
##

## create data frame with ONLY variables of interest

finalInput <- inputData %>%
  select(Loc, brand_hybrid, yearly.avg, yield)

## Using Prof Reg
covName <- names(finalInput[3])

mod <- profRegr(covName, outcome = 'yield', 
                yModel = 'Normal', xModel = "Normal",
                #fixedEffectsNames = 'yield',
                #discreteCovs = 
                data = finalInput)

# dissimilarity Matrix
calcDists <- calcDissimilarityMatrix(mod)
heatDissMat(calcDists)

# clustering
clusts <- calcOptimalClustering(calcDists)
