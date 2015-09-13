# Data Input #

library(plyr)
library(dplyr)
library(tidyr)
library(xlsx)
library(XLConnect)
library(ggmap)
library(RNCEP)

# Set Working Directory
workingD <- setwd("C:\\Users\\Sean\\Documents\\GitHub\\Premium\\Input Data")

# read in xlsx 
wb <- loadWorkbook("C:\\Users\\Sean\\Documents\\GitHub\\Premium\\Input Data\\IA\\2014_ISU_CornSingleLocationData.xlsx")
lst = readWorksheet(wb, sheet = getSheets(wb))


# create empty list objects to be filled
iowaMeta <- list()
iowaData <- list()
lst2 <- lst
dims<-matrix(nrow=16,ncol=2)
#
# the following loop will split up the tabs in the workbook into two
# sections: 1) Meta data 
#           2) Data
#
for (i in 2:length(lst)) {
  
  lst2[[i]] = lst2[[i]][-7,]
  iowaMeta[[i-1]] = lst2[[i]][1:5,]
  iowaMeta[[i-1]] = as.vector(c())
  iowaData[[i-1]] = lst2[[i]][-(1:5), c(2, 6)]
  colnames(iowaData[[i-1]]) = iowaData[[i-1]][1,]
  iowaData[[i-1]] = iowaData[[i-1]][-1,]
  
  numberOfRows = nrow(iowaData[[i-1]])
  row.names(iowaData[[i-1]]) = 1:numberOfRows
  
  iowaData[[i -1]] = iowaData[[i-1]][1:(which(is.na(iowaData[[i-1]][,1]))-1),]
  
  colnames(iowaData[[i-1]]) = c('brand_hybrid', 'yield')
  }

names(iowaData) <- names(lst2)[-1]
Location<-paste(lst2[[1]]$Location,"IA",sep=", ")
lst2[[1]]$Location<-Location
attach(lst2[[1]])
unique_gcode<-geocode(unique(Location))
unique_gcode<-as.data.frame(cbind(as.character(unique(Location)),unique_gcode))
gcode<-data.frame(Lon=0,Lat=0)

for(i in 1:length(unique(Location))){
  gcode[which(Location==unique_gcode[i,1]),]<-unique_gcode[i,2:3]
  plant_date[which(Location==unique_gcode[i,1])]<-as.character(plant_date[which(Location==unique_gcode[i,1])])
  harvest_date[which(Location==unique_gcode[i,1])]<-as.character(harvest_date[which(Location==unique_gcode[i,1])])
}
Lon_Lat_Loc<-as.data.frame(cbind(gcode,Location))
Lon_Lat_Loc$Lon <- Lon_Lat_Loc$Lon + 360

Loc_Date<-as.data.frame(cbind(Lon_Lat_Loc,plant_date,harvest_date))
loc_minmax<-c(min(gcode[,1]),max(gcode[,1]),min(gcode[,2]),max(gcode[,2]))

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

roundTo <- function(x, y){
  #  which.min(abs(y - x))
  #rounded_x = vector(length = length(x))
  return(y[which.min(abs(y - x))])
}

Lon_Lat_Loc2<-as.data.frame(cbind(lst2[[1]][,1],Lon_Lat_Loc))
Lon_Lat_Loc2$Lon2 <- unlist(lapply(Lon_Lat_Loc[,1], function(x) roundTo(x, seq(min(wx.output$longitude), max(wx.output$longitude), 2.5))))
Lon_Lat_Loc2$Lat2 <- unlist(lapply(Lon_Lat_Loc[,2], function(x) roundTo(x, seq(min(wx.output$latitude), max(wx.output$latitude), 2.5))))

wx.ag2 <- wx.output %>%
  select(-datetime) %>%
  rename(Lat2 = latitude, 
         Lon2 = longitude)

Lon_Lat_Loc$Loc <- as.character(Lon_Lat_Loc$Loc)

inputData <- Lon_Lat_Loc2 %>%
  inner_join(., wx.ag2, by=c('Lat2', 'Lon2')) 

iowaData2<-iowaData
for(i in 1:27){
  iowaData2[[i]][,3:14]<-inputData[i,7:18]
}

iowaMASTER <- ldply(iowaData2, data.frame)
