setwd("C:\\Users\\Sean\\Documents\\GitHub\\Premium\\Master Data")
library(plyr)
library(dplyr)
library(tidyr)
library(xlsx)
library(XLConnect)
library(ggmap)
library(RNCEP)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
Loc<-vector(length=732)
for(i in 1:732){
  Loc[i]<-paste0(gsub("^(.*?),.*", "\\1", substr(as.character(KS$Location[i]),6,nchar(as.character(KS$Location[i])))),", Kansas")
}
KS$Location<-Loc
colnames(KS)<-c("brand_hybrid","Yield","Location")
input_data$Yield <- as.numeric(gsub('\\*', '', input_data$Yield))

attach(input_data)

unique_gcode<-geocode(unique(as.character(Location)))
unique_gcode<-as.data.frame(cbind(as.character(unique(Location)),unique_gcode))
gcode<-data.frame(Lon=0,Lat=0)

for(i in 1:length(unique(Location))){
  gcode[which(Location==unique_gcode[i,1]),]<-unique_gcode[i,2:3]
}

Lon_Lat_Loc<-as.data.frame(cbind(gcode,Location))
Lon_Lat_Loc$Lon <- Lon_Lat_Loc$Lon + 360

Loc_Date<-as.data.frame(Lon_Lat_Loc)
loc_minmax<-c(min(gcode[,1]),max(gcode[,1]),min(gcode[,2]),max(gcode[,2]))

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

for (i in 1:length(metrics)) {
  
  loc_minmax<-c(min(gcode[,1]),max(gcode[,1]),min(gcode[,2]),max(gcode[,2]))
  wx.test<-NCEP.gather(variable= metrics[i],level='surface',
                       months.minmax = c(5,10),years.minmax = c(2014,2014),
                       lat.southnorth = c(loc_minmax[3:4]), lon.westeast = c(loc_minmax[1:2]),
                       reanalysis2 = FALSE, return.units = TRUE)
  
  wx.ag<-NCEP.aggregate(wx.test, YEARS = TRUE, MONTHS = FALSE, DAYS = FALSE,
                        HOURS = FALSE,fxn= 'mean')
  
  #wx.ag2<-NCEP.aggregate(wx.test, YEARS = TRUE, MONTHS = FALSE, DAYS = FALSE,
   #                      HOURS = FALSE,fxn= 'var')
  
  
  wx.df <- NCEP.array2df(wx.ag)
  wx.df2 <- NCEP.array2df(wx.ag2)
  
  colnames(wx.df)[4] <- paste0(metrics[[i]], '_mean')
  #colnames(wx.df2)[4] <- paste0(metrics[[i]], '_variance')
  
  
  if (i == 1 ) wx.output <- as.data.frame(cbind(wx.df, wx.df2[4]))
  if (i > 1) wx.output <-as.data.frame(cbind(wx.output, wx.df[4], wx.df2[4]))
  
}

roundTo <- function(x, y){
  #  which.min(abs(y - x))
  #rounded_x = vector(length = length(x))
  return(y[which.min(abs(y - x))])
}

Lon_Lat_Loc2<-Lon_Lat_Loc
Lon_Lat_Loc2$Lon2 <- unlist(lapply(Lon_Lat_Loc[,1], function(x) roundTo(x, seq(min(wx.output$longitude), max(wx.output$longitude), 2.5))))
Lon_Lat_Loc2$Lat2 <- unlist(lapply(Lon_Lat_Loc[,2], function(x) roundTo(x, seq(min(wx.output$latitude), max(wx.output$latitude), 2.5))))

wx.ag2 <- wx.output %>%
  select(-datetime) %>%
  rename(Lat2 = latitude, 
         Lon2 = longitude)

Lon_Lat_Loc$Loc <- as.character(Lon_Lat_Loc$Loc)

inputData <- Lon_Lat_Loc2 %>%
  inner_join(., wx.ag2, by=c('Lat2', 'Lon2')) %>%
  inner_join(.,input_data,by="Location") %>%
  distinct()

Output<-inputData %>%
  select(brand_hybrid, Yield,air.sig995_mean:vwnd.sig995_mean)

finalInput$brand_hybrid<-as.character(finalInput$brand_hybrid)
numericVars <- which(sapply(finalInput, class)=='numeric' & names(finalInput) != 'Yield')
categoricalVars <- which(sapply(finalInput, class)=='character' & names(finalInput) != 'Yield')

system.time({
  
  mod <- profRegr(covName, outcome = 'Yield', 
                  yModel = 'Normal', xModel = "Mixed",
                  #nCovariates = 2,
                  #fixedEffectsNames = 'yield',
                  discreteCovs = c(names(finalInput[categoricalVars])),
                  continuousCovs = c(names(finalInput[numericVars])),
                  data = finalInput)
})

calcDists <- calcDissimilarityMatrix(mod)

clusts <- calcOptimalClustering(calcDists)

riskProfileOb <- calcAvgRiskAndProfile(clusts)

plotRiskProfile(riskProfileOb, outFile = "summary1.png", whichCovariates = metrics)
