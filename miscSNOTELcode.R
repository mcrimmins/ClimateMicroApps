# download and process SNOTEL data
# MAC 11/22/22

library(snotelr)
library(dplyr)

# water year function
# https://stackoverflow.com/questions/27626533/r-create-function-to-add-water-year-column
wtr_yr <- function(dates, start_month=9) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}


# get station metadata
meta_data <- snotelr::snotel_info()

# add in station search based on area of interest

# get station 'chalendar' for USFS KNF
siteID<-meta_data$site_id[which((meta_data$site_name=="chalender ")==TRUE)]

snowDF <- snotel_download(site_id = c(siteID), internal = TRUE)

# add date fields
snowDF$date<-as.Date(snowDF$date)
snowDF$month<-as.numeric(format(snowDF$date, "%m"))
snowDF$year<-as.numeric(format(snowDF$date, "%Y"))
snowDF$wtrYear<-wtr_yr(snowDF$date, 10)

years<-unique(snowDF$year)

# full date range for missing days
#dates<-seq.Date(snowDF$date[1],snowDF$date[nrow(snowDF)], by="day")

# monthly summary
moSnow <- snowDF magrittr::%>%
  dplyr::group_by(month, year) magrittr::%>%
  dplyr::summarize(date=first(date),
            temp=mean(temperature_mean, na.rm=TRUE),
            swe=mean(snow_water_equivalent, na.rm=TRUE),
            precip=sum(precipitation, na.rm=TRUE))

# monthly climo
moClimo <- snowDF %>%
  group_by(month) %>%
  summarize(date=first(date),
            temp=mean(temperature_mean, na.rm=TRUE),
            swe=mean(snow_water_equivalent, na.rm=TRUE),
            precip=sum(precipitation, na.rm=TRUE)/length(years))


# date range/month range summary
subData<-subset(snowDF, month %in% c(10,11,12,1,2)) # put seas def in function, auto adjust year/water year based on seas

# convert SWE from mm to in
subData$snow_water_equivalent<-subData$snow_water_equivalent/25.4
subData$precipitation<-subData$precipitation/25.4

# max SWE summary
yearSWE<-subData %>%
          group_by(wtrYear) %>% # change to year
          summarise(swe=max(snow_water_equivalent, na.rm=TRUE),
                    nDays=sum(!is.na(snow_water_equivalent)),
                    nMiss=sum(is.na(snow_water_equivalent)))
# calculate anomalies
yearSWE<-yearSWE %>% 
          mutate(meanSWE=mean(swe, na.rm=TRUE)) %>%
          mutate(anomSWE=swe-meanSWE)

# PRECIP summary
yearPRECIP<-subData %>%
  group_by(wtrYear) %>% # change to year
  summarise(precip=sum(precipitation, na.rm=TRUE),
            nDays=sum(!is.na(precipitation)),
            nMiss=sum(is.na(precipitation)))
# calculate anomalies
yearPRECIP<-yearPRECIP %>% 
  mutate(meanPRECIP=mean(precip, na.rm=TRUE)) %>%
  mutate(anomPRECIP=precip-meanPRECIP)


#####
# write into function

getSnotel<-function(stationName, season){
  
  # water year function
  wtr_yr <- function(dates, start_month) {
    # Convert dates into POSIXlt
    dates.posix = as.POSIXlt(dates)
    # Year offset
    offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
    # Water year
    adj.year = dates.posix$year + 1900 + offset
    # Return the water year
    adj.year
  }
  
  # get station metadata
  meta_data <- snotelr::snotel_info()
  meta_data$site_name<-trimws(meta_data$site_name)
  
  # get site metadata
  siteMeta<-meta_data[which((meta_data$site_name==stationName)==TRUE),]
  
  # get specified station 
  siteID<-meta_data$site_id[which((meta_data$site_name==stationName)==TRUE)]
  # download data
  snowDF <- snotelr::snotel_download(site_id = c(siteID), internal = TRUE)
  
  # add date fields
  snowDF$date<-as.Date(snowDF$date)
  snowDF$month<-as.numeric(format(snowDF$date, "%m"))
  snowDF$year<-as.numeric(format(snowDF$date, "%Y"))
  # water year correction
  if(season[1]>season[length(season)]){
    snowDF$year<-wtr_yr(snowDF$date, season[1])
  }
  
  # date range/month range summary
  subData<-subset(snowDF, month %in% season) # put seas def in function, auto adjust year/water year based on seas
  
  # convert SWE from mm to in
  subData$snow_water_equivalent<-subData$snow_water_equivalent/25.4
  subData$precipitation<-subData$precipitation/25.4
  
  # max SWE summary
  yearSWE<-subData %>%
    group_by(year) %>% # change to year
    summarise(swe=max(snow_water_equivalent, na.rm=TRUE),
              nDays=sum(!is.na(snow_water_equivalent)),
              nMiss=sum(is.na(snow_water_equivalent)))
  # calculate anomalies
  yearSWE<-yearSWE %>% 
    mutate(meanSWE=mean(swe, na.rm=TRUE)) %>%
    mutate(anomSWE=swe-meanSWE)
  
  # PRECIP summary
  yearPRECIP<-subData %>%
    group_by(year) %>% # change to year
    summarise(precip=sum(precipitation, na.rm=TRUE),
              nDays=sum(!is.na(precipitation)),
              nMiss=sum(is.na(precipitation)))
  # calculate anomalies
  yearPRECIP<-yearPRECIP %>% 
    mutate(meanPRECIP=mean(precip, na.rm=TRUE)) %>%
    mutate(anomPRECIP=precip-meanPRECIP)
  
  # put into list
  snotelList<-list(yearSWE, yearPRECIP, siteMeta)
  names(snotelList)<-c("maxSWE","totalPrecip","siteMeta")
  
  # return data
  return(snotelList)
  
}


# testing of function source file
source('getSNOTELdata.R')

snotelData<-getSNOTELdata('chalender',c(10,11,12,1,2))



