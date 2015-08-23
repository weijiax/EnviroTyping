# Illinois Data Input #

library(plyr)
library(dplyr)
library(tidyr)
library(xlsx)


# Set Working Directory to IL folder
workingD <- setwd("C:\\Users\\Chuck\\Documents\\GitHub\\Premium\\Input Data\\IL")


# read in file names within the IL folder
temp = list.files(pattern="*.xls")

# remove the first element (cornentbl14) 
temp <- temp[-1]

## Bring in all the spreadsheets into a list
## (The actual data starts on row 7)
data <- list()
data <- lapply(temp, function(x) read.xlsx(x, sheetIndex = 1,
                                           stringsAsFactors = FALSE,
                                           header = FALSE,
                                           startRow = 7))

## read in the column names from each spreadsheet
## (all the info we need is in rows 3:5)
locationInfo <- list()
locationInfo <- lapply(temp, function(x) read.xlsx(x, sheetIndex = 1,
                                                   rowIndex = c(3:5),
                                                   stringsAsFactors=FALSE,
                                                   header=TRUE))
## variable names
## This will be used as the new variable names
varnames<-lapply(locationInfo, function(x) paste(as.character(x[1,]),
                                                 as.character(x[2,]),
                                                 sep = "_"))

## location info
## (pull out location information from each spreadsheet)
locationInfo2 <- lapply(locationInfo, function(x) grep('^[^N^X]', names(x), value=TRUE))



##
## Loop:
##
data2 <- data
for (i in 1:length(data)) {
  
# add variable names  
  colnames(data2[[i]]) <- paste(varnames[[i]])
  
# keep only the columns of interest (company, brand name, yield, moisture)
  data2[[i]] <- data2[[i]][,grep('NA_Company|(NA_Name)|Yield_bu/a|Moisture_', names(data2[[i]]))]
  
# omit the rows after the line break
  data2[[i]] <- data2[[i]][1:which.max(is.na(data[[i]][2]))-1,]

# rename the variables by appending the location information
  names(data2[[i]]) <- c('Company', 'brand_hyrbrid', paste(rep(locationInfo2[[i]], each =2), names(data2[[i]][-c(1,2)]), sep="_"))

# create a variable that specifies which spreadsheet is observation is from
  data2[[i]]$sheet <- temp[[i]]
}

## convert the list into one data frame
mergedData <- ldply(data2)


## Wrangle some data
output<- mergedData%>%
  select(Company, brand_hyrbrid, sheet, matches('Yield')) %>%
  select(-matches('Regional')) %>%
  gather(key, value, -Company, -brand_hyrbrid, -sheet) %>%
  mutate(key = gsub('_bu.+', '', key),
         key = gsub("\\.{2}", ".", key)) %>%
  separate(key, c('Location', 'Variable'), sep="_") %>%
  spread(Variable, value) %>%
  filter(!is.na(Yield)) %>%
  rename(brand_hybrid=brand_hyrbrid)

#
write.csv(output, 'IL data_clean.csv', row.names = FALSE)
