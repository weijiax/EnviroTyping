##############################
########PREMIUM###############
##############################

setwd("C:\\Users\\Sean\\Documents\\GitHub\\Premium\\Test Data")
test_data<-read.csv(file.choose())
attach(test_data)
require(ggmap)
require(RNCEP)
library("PReMiuM", lib.loc="~/R/win-library/3.2")
Loc<-as.character(Loc)
yield<-as.numeric(substr(yield_bu.A,1,5))
###codes lat/lon for the 32 unique locations that will be assigned to every observation
###geocoding works through google API, and it is much faster to query only as often as is
###absolutely necessary.
unique_gcode<-geocode(unique(Loc))
unique_gcode<-as.data.frame(cbind(as.character(unique(Loc)),unique_gcode))
gcode<-data.frame(Lon=0,Lat=0)
plant_date<-vector(length=2289)
harvest_date<-vector(length=2289)
####Assigns geocodes to each observation
for(i in 1:32){
  gcode[which(Loc==unique_gcode[i,1]),]<-unique_gcode[i,2:3]
  plant_date[which(Loc==unique_gcode[i,1])]<-as.character(planting[which(Loc==unique_gcode[i,1])])
  harvest_date[which(Loc==unique_gcode[i,1])]<-as.character(harvest[which(Loc==unique_gcode[i,1])])
}
Lon_Lat_Loc<-as.data.frame(cbind(gcode,Loc))

Loc_Date<-as.data.frame(cbind(Lon_Lat_Loc,plant_date,harvest_date))
loc_minmax<-c(min(gcode[,1]),max(gcode[,1]),min(gcode[,2]),max(gcode[,2]))
wx.test<-NCEP.gather(variable='air.sig995',level='surface',
            months.minmax = c(5,10),years.minmax = c(2014,2014),
            lat.southnorth = c(loc_minmax[3:4]), lon.westeast = c(loc_minmax[1:2]),
            reanalysis2 = FALSE, return.units = TRUE)
wx.ag<-NCEP.aggregate(wx.test, YEARS = FALSE, MONTHS = TRUE, DAYS = TRUE,
               HOURS = FALSE,fxn='mean')

####Creating an X dataframe for each lon/lat cell (col) and each time (row)
x.mat<-matrix(nrow=184,ncol=32,byrow=T)
x.mat.colnames<-matrix(nrow=4,ncol=8)
for(i in 1:4){
  for(j in 1:8){
    x.mat.colnames[i,j]<-paste(dimnames(wx.ag)[[1]][i],dimnames(wx.ag)[[2]][j],sep="x")
  }
}
colnames(x.mat)<-as.vector(x.mat.colnames)
rownames(x.mat)<-dimnames(wx.ag)[[3]]
rownames(x.mat)<-substr(rownames(x.mat),6,10)
for(k in 1:184){
      x.mat[k,]<-as.vector(wx.ag[,,k])
}

runTest<-profRegr(yModel="Normal",xModel="Normal",nClusInit = 32)