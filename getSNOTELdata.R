#####
# Helper function to get SNOTEL data and summarize for specified station and season
# required libraries: 'dplyr', 'snotelr', 'magrittr' for pipe operator
# for package development importFrom(magrittr,"%>%")
#
# USAGE
# getSNOTELdata(stationName,season)
#
# Arguments
# stationName ...simple station name from snotel_info()
# season ...vector of month numbers to define season, for example c(10,11,12,1,2) for Oct-Feb
#
# Example
# snotelData<-getSNOTELdata('chalender',c(10,11,12,1,2))

library(magrittr)

getSNOTELdata<-function(stationName, season){
  
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
    dplyr::group_by(year) %>% # change to year
    dplyr::summarise(swe=max(snow_water_equivalent, na.rm=TRUE),
              nDays=sum(!is.na(snow_water_equivalent)),
              nMiss=sum(is.na(snow_water_equivalent)),
              minDate=min(date, na.rm = TRUE),
              maxDate=max(date, na.rm = TRUE))
  yearSWE$swe[yearSWE$swe<0]<-NA

  # calculate anomalies
  yearSWE<-yearSWE %>% 
    dplyr::mutate(meanSWE=mean(swe, na.rm=TRUE)) %>%
    dplyr::mutate(anomSWE=swe-meanSWE)
  
  # PRECIP summary
  yearPRECIP<-subData %>%
    dplyr::group_by(year) %>% # change to year
    dplyr::summarise(precip=sum(precipitation, na.rm=TRUE),
              nDays=sum(!is.na(precipitation)),
              nMiss=sum(is.na(precipitation)),
              minDate=min(date, na.rm = TRUE),
              maxDate=max(date, na.rm = TRUE))

  # calculate anomalies
  yearPRECIP<-yearPRECIP %>% 
    dplyr::mutate(meanPRECIP=mean(precip, na.rm=TRUE)) %>%
    dplyr::mutate(anomPRECIP=precip-meanPRECIP)
  
  # pull out latest year
  currData<-cbind.data.frame(siteMeta$description,round(siteMeta$elev*3.28084,0),
                             paste0(format(siteMeta$start,"%Y"),"-",format(siteMeta$end,"%Y")),
                             round(yearSWE$swe[nrow(yearSWE)],2),round(yearSWE$anomSWE[nrow(yearSWE)],2),
                             round(yearPRECIP$precip[nrow(yearPRECIP)],2),round(yearPRECIP$anomPRECIP[nrow(yearPRECIP)],2))
  colnames(currData)<-c('Station','Elev (ft)','POR','Max SWE (in)','Max SWE Anom (in)','Total Precip (in)','Precip Anom (in)')
  seasLabel<-paste0(yearPRECIP$minDate[nrow(yearPRECIP)]," to ",yearPRECIP$maxDate[nrow(yearPRECIP)])
  
  # put into list
  snotelList<-list(yearSWE, yearPRECIP, siteMeta, currData, seasLabel)
  names(snotelList)<-c("maxSWE","totalPrecip","siteMeta","current_seas","season_label")
  
  # return data
  return(snotelList)
  
}

