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

test_data$district <- tolower(test_data$district)
test_data$district <- gsub('^(southwester)$', 'southwestern', test_data$district)
Loc<-as.character(Loc) # focus on southwestern for now
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
wx.test<-NCEP.gather(variable=c('air.sig995','lftx.sfc','omega.sig995'),level='surface',
            months.minmax = c(5,10),years.minmax = c(2014,2014),
            lat.southnorth = c(loc_minmax[3:4]), lon.westeast = c(loc_minmax[1:2]),
            reanalysis2 = FALSE, return.units = TRUE)

wx.ag<-NCEP.aggregate(wx.test, YEARS = TRUE, MONTHS = FALSE, DAYS = FALSE,
               HOURS = FALSE,fxn='mean')
detach(test_data)


##  NCEP.array2df <- coverts array output into data frame
## create a for-loop to bring in weather data for several metrics
metrics <- c('air.sig995',
             #'lftx.sfc',
             'omega.sig995', 
             #'pottmp.sig995',
             #'pr_wtr.eatm',
             'pres.sfc',
             'rhum.sig995',
             #'slp',
             'uwnd.sig995',
             'vwnd.sig995')


             #'soilw.0-10cm', 'soilw.10-200cm', 'sfcr.sfc', 'prate.sfc')

for (i in 1:length(metrics)) {

loc_minmax<-c(min(gcode[,1]),max(gcode[,1]),min(gcode[,2]),max(gcode[,2]))
wx.test<-NCEP.gather(variable= metrics[i],level='surface',
                     months.minmax = c(5,10),years.minmax = c(2014,2014),
                     lat.southnorth = c(loc_minmax[3:4]), lon.westeast = c(loc_minmax[1:2]),
                     reanalysis2 = FALSE, return.units = TRUE)

wx.ag<-NCEP.aggregate(wx.test, YEARS = TRUE, MONTHS = FALSE, DAYS = FALSE,
                      HOURS = FALSE,fxn= 'mean')

wx.ag2<-NCEP.aggregate(wx.test, YEARS = TRUE, MONTHS = FALSE, DAYS = FALSE,
                      HOURS = FALSE,fxn= 'var')


  wx.df <- NCEP.array2df(wx.ag)
  wx.df2 <- NCEP.array2df(wx.ag2)
  
  colnames(wx.df)[4] <- paste0(metrics[[i]], '_mean')
  colnames(wx.df2)[4] <- paste0(metrics[[i]], '_variance')
  

if (i == 1 ) wx.output <- as.data.frame(cbind(wx.df, wx.df2[4]))
if (i > 1) wx.output <-as.data.frame(cbind(wx.output, wx.df[4], wx.df2[4]))
  
}

## Function to round lon/lat
roundTo <- function(x, y){
#  which.min(abs(y - x))
  #rounded_x = vector(length = length(x))
  return(y[which.min(abs(y - x))])
}

## Apply function to Longitute and Latitude
#Lon_Lat_Loc$Lon2 <- unlist(lapply(Lon_Lat_Loc[,1], function(x) roundTo(x, seq(275, 280, 2.5))))
#Lon_Lat_Loc$Lat2 <- unlist(lapply(Lon_Lat_Loc[,2], function(x) roundTo(x, seq(37.5, 42.5, 2.5))))
Lon_Lat_Loc$Lon2 <- unlist(lapply(Lon_Lat_Loc[,1], function(x) roundTo(x, seq(min(wx.output$longitude), max(wx.output$longitude), 2.5))))
Lon_Lat_Loc$Lat2 <- unlist(lapply(Lon_Lat_Loc[,2], function(x) roundTo(x, seq(min(wx.output$latitude), max(wx.output$latitude), 2.5))))


# wx.ag2 <- wx.ag %>% 
#   unclass() %>%
#   as.data.frame() %>%
#   mutate(lat = row.names(.)) %>%
#   gather(lon, yearly.avg, -lat) %>%
#   mutate(lon = as.numeric(gsub(".2014_XX_XX_XX", '', lon)),
#          lat = as.numeric(lat)) %>%
#   rename(Lat2 = lat, 
#          Lon2 = lon)

wx.ag2 <- wx.output %>%
  select(-datetime) %>%
  rename(Lat2 = latitude, 
         Lon2 = longitude)

## Merge Lon_Lat_Loc to wx.ag2 
## also, merge that do test_data 

#test_data2 <- test_data[1:222,]
#test_data2$Loc <- as.character(test_data2$Loc)
Lon_Lat_Loc$Loc <- as.character(Lon_Lat_Loc$Loc)

# Becareful with duplicate rows after joining to test_data2
inputData <- Lon_Lat_Loc %>%
  inner_join(., wx.ag2, by=c('Lat2', 'Lon2')) %>%
  distinct() %>%
  inner_join(., test_data, by = 'Loc') %>%
  mutate(yield = yield_bu.A, 
         yield = as.numeric(gsub('\\*', '', yield)))

# subset data to those where brand_hybrid > 3
inputData <- inputData %>% 
  group_by(brand_hybrid) %>%
  mutate(count = n()) %>%
  filter(count > 3) %>%
  select(-count) %>%
  ungroup() 


##
##
## Now, try to get the data into profRegr
##
## covariate: yearly.avg
## outcome: yield 
##
##

## create data frame with ONLY variables of interest
metrics_mean <- c(paste0(metrics, "_mean"), paste0(metrics, '_variance'))
finalInput <- inputData[c('yield', 'brand_hybrid', metrics_mean)]

## filter down to brand_hybrids > 3
finalInput <- finalInput %>% 
  group_by(brand_hybrid) %>%
  mutate(count = n()) %>%
  filter(count > 3) %>%
  select(-count) %>%
  ungroup() 

## Using Prof Reg
covName <- names(finalInput[2:length(finalInput)])

## determine numeric variables
numericVars <- which(sapply(finalInput, class)=='numeric' & names(finalInput) != 'yield')
categoricalVars <- which(sapply(finalInput, class)=='character' & names(finalInput) != 'yield')

system.time({

  mod <- profRegr(covName, outcome = 'Yield', 
                yModel = 'Normal', xModel = "Mixed",
                #nCovariates = 2,
                #fixedEffectsNames = 'yield',
                discreteCovs = c(names(finalInput[categoricalVars])),
                continuousCovs = c(names(finalInput[numericVars])),
                data = finalInput)
})

# dissimilarity Matrix
calcDists <- calcDissimilarityMatrix(mod)
#heatDissMat(calcDists)

# clustering
clusts <- calcOptimalClustering(calcDists)

# risk profile obj
riskProfileOb <- calcAvgRiskAndProfile(clusts)

# plot risk profile
s1 <- metrics_mean[1:6]
s2 <- metrics_mean[7:12]
#s3 <- metrics_mean[11:15]
#s4 <- metrics_mean[16:20]

plotRiskProfile(riskProfileOb, outFile = "summary1.png", whichCovariates = s1)
plotRiskProfile(riskProfileOb, outFile = "summary2.png", whichCovariates = s2)
#plotRiskProfile(riskProfileOb, outFile = "summary3.png", whichCovariates = s3)
#plotRiskProfile(riskProfileOb, outFile = "summary4.png", whichCovariates = s4)

# Pull out cluster membership and attached to data 

clusterData <- clusts$clustering
inputData$cluster <-as.character(clusterData)



#
#
# Compute cluster descriptives 
#
#

inputData %>% 
  group_by(cluster) %>%
  summarise_each(funs(mean, median, min, max, sd), air.sig995:vwnd.sig995, yield, -cluster) %>%
  gather(key, value, -cluster) %>%
  #separate(key, c('metric', 'statistic'), sep = '') %>%
  extract(key, c('metric', 'statistic'), "(.+)_(\\w+)$") %>%
  spread(statistic, value) %>%
  arrange(metric)

# deviation from average yield by cluster
inputData %>%
  group_by(cluster) %>%
  mutate(cluster_avg = mean(yield)) %>%
  ungroup() %>%
  group_by(brand_hybrid) %>%
  #filter(n() > 3) %>%
  mutate(deviation = yield/cluster_avg) %>%
  select(brand_hybrid, cluster, yield, cluster_avg, deviation)
  

# Plot cluster inputData
inputData %>% 
  gather(key, value, air.sig995_mean:vwnd.sig995_variance) %>%
  extract(key, c('metric', 'statistic'), "(.+)_(\\w+)$") %>%
  qplot(as.factor(cluster),value, data = ., size = yield) +
  facet_wrap(statistic~key, scales = 'free')

# Plot cluster inputData
inputData %>%
  gather(key, value, air.sig995_mean:vwnd.sig995_variance) %>%
  qplot(value, yield, data = ., color = as.factor(cluster)) +
  facet_wrap(~key, scales = 'free')

inputData %>% 
  qplot(as.factor(brand_hybrid), yield, inputData = ., color= as.factor(cluster)) +
  coord_flip()

## max percentage deviation from the brand_hybrid average

inputData %>% 
  group_by(brand_hybrid) %>%
  mutate(brand_hybrid_avg = mean(yield),
         deviation = (yield - brand_hybrid_avg)/brand_hybrid_avg,
         base = n()) %>%
  filter(base >= 4) %>%
  filter(abs(deviation) >= .10) %>%
  qplot(deviation, data = ., fill = as.factor(cluster))

inputData %>% 
  group_by(brand_hybrid) %>%
  mutate(brand_hybrid_avg = mean(yield),
         deviation = (yield - brand_hybrid_avg)/brand_hybrid_avg,
         base = n()) %>%
  filter(base >= 4) %>%
  filter(abs(deviation) >= .10) %>%
  qplot(Lat2, Lon2, data = ., size = deviation, color = as.factor(cluster))

# get map
map <- get_map(location = 'United States', zoom = 5)

mapPoints <- ggmap(map) +
  geom_point(aes(x=Lon, y=Lat, size = yield), data = inputData)

