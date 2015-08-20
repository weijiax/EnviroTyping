# Data Input #

library(plyr)
library(dplyr)
library(tidyr)
library(xlsx)
library(XLConnect)


# Set Working Directory
workingD <- setwd("C:\\Users\\Chuck\\Documents\\GitHub\\Premium\\Input Data")

# read in xlsx 
wb <- loadWorkbook("C:\\Users\\Chuck\\Documents\\GitHub\\Premium\\Input Data\\IA\\2014_ISU_CornSingleLocationData.xlsx")
lst = readWorksheet(wb, sheet = getSheets(wb))


# create empty list objects to be filled
iowaMeta <- list()
iowaData <- list()
lst2 <- lst

#
# the following loop will split up the tabs in the workbook into two
# sections: 1) Meta data 
#           2) Data
#
for (i in 2:length(lst)) {
  
  lst2[[i]] = lst2[[i]][-7,]
  iowaMeta[[i-1]] = lst2[[i]][1:5,]
  
  iowaData[[i-1]] = lst2[[i]][-(1:5), c(2, 6)]
  colnames(iowaData[[i-1]]) = iowaData[[i-1]][1,]
  iowaData[[i-1]] = iowaData[[i-1]][-1,]
  
  numberOfRows = nrow(iowaData[[i-1]])
  row.names(iowaData[[i-1]]) = 1:numberOfRows
  
  iowaData[[i -1]] = iowaData[[i-1]][1:(which(is.na(iowaData[[i-1]][,1]))-1),]
  
  colnames(iowaData[[i-1]]) = c('brand_hybrid', 'yield')
  
  iowaData[[i-1]]$Worksheet = 
  
}

names(iowaData) <- names(lst2)[-1]

##
##
## For next time... figure out how to bring in the meta data information
## to each data frame.
