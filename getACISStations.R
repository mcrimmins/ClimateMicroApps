#####
# Helper function to get station data from RCC-ACIS and summarize for specified stations and season
# required libraries: 'dplyr', 'jsonlite', 'RCurl', 'magrittr' for pipe operator
# for package development importFrom(magrittr,"%>%")
#
# USAGE
# getACISStations(stations, season)
#
# Arguments
# stations ...station code 'sids' from https://www.rcc-acis.org/docs_webservices.html
# season ...vector of month numbers to define season, for example c(10,11,12,1,2) for Oct-Feb
# current year...specify year of interest
#
# Example
# stationData<-getACISStations(c("020678", "USW00003195", "USS0012P02S"),c(7,8,9), 2023)

library(magrittr)

getACISStations<-function(stations, season, currYr){
  
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
  
  # water year correction
  if(season[1]>season[length(season)]){
    yr<-as.numeric(format(Sys.Date(),"%Y"))+1
  }else{
    yr<-as.numeric(format(Sys.Date(),"%Y"))
  }
  
  # specify date range
  sdate<-"1900-01-01"
  edate<-as.Date(paste0(yr,"-",season[length(season)],"-01"))
  
  # format station vector for JSON string
  stations<-sprintf('"%s"', paste0(stations, collapse = '","'))
  
  # download data from RCC-ACIS in JSON format and convert
  jsonQuery=paste0('{"sids":[',stations,'],"sdate":"',sdate,'","edate":"',edate,'","elems":"1,2,43,4,10,11"}')
  out<-RCurl::postForm("http://data.rcc-acis.org/MultiStnData", 
                       .opts = list(postfields = jsonQuery, 
                                    httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  out<-jsonlite::fromJSON(out)
  
  # create date string
  dates<-seq.Date(as.Date(sdate),as.Date(edate), by="day")
  
  # get metadata
  ll<-data.frame(matrix(unlist(out$data$meta$ll), nrow=length(out$data$meta$ll), byrow=T))
  meta<-out$data$meta
  meta<-cbind.data.frame(meta, ll)
  
  # loop through stations in out list
  stationCLIM<-list()
  for(i in 1:nrow(meta)){
    
    # process station df
    temp<-out$data$data[[i]]
    temp<-cbind.data.frame(dates,temp)
    colnames(temp)<-c("date","t_max","t_min","t_mean","precip","snow","snowD")
    # replace T trace values
    temp$precip[temp$precip=="T"]<-"0.00"
    # convert columns to numeric
    unfactorize<-c("t_max","t_min","t_mean","precip","snow","snowD")
    temp[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(temp[,x])))
    
    # add date fields
    temp$date<-as.Date(temp$date)
    temp$month<-as.numeric(format(temp$date, "%m"))
    temp$year<-as.numeric(format(temp$date, "%Y"))
    
    # adjust to water year if needed
    if(season[1]>season[length(season)]){
      temp$year<-wtr_yr(temp$date, season[1])
    }
    
    # date range/month range summary
    subData<-subset(temp, month %in% season) # put seas def in function, auto adjust year/water year based on seas
    
    # Climate summary
    yearCLIM<-subData %>%
      dplyr::group_by(year) %>% # change to year
      dplyr::summarise(precipSum=sum(precip, na.rm=TRUE),
                       precipDays=sum(precip>0, na.rm = TRUE),
                       avgTemp=mean(t_mean, na.rm=TRUE),
                       freezeDays=sum(t_min<=32,na.rm=TRUE),
                       snowSum=sum(snow, na.rm = TRUE),
                       nDays=sum(!is.na(precip)),
                       nMiss=sum(is.na(precip)))
    
    # crop df to non-missing years, <10% missing
    yearCLIM$missing<-ifelse(yearCLIM$nMiss>max(yearCLIM$nDays)*0.1, NA, 1)
    por<-na.contiguous(yearCLIM$missing)
    por<-attr(por,"tsp")
    yearCLIM<-yearCLIM[por[[1]]:por[[2]],]  
    
    # calculate anomalies
    yearCLIM<-yearCLIM %>% 
      dplyr::mutate(meanPRECIP=mean(precipSum, na.rm=TRUE)) %>%
      dplyr::mutate(anomPRECIP=precipSum-meanPRECIP) %>%
      dplyr::mutate(meanTEMP=mean(avgTemp, na.rm=TRUE)) %>%
      dplyr::mutate(anomTEMP=avgTemp-meanTEMP) 
    
    # select year
    rowYr<-which(yearCLIM$year==currYr)
    
    # put latest year into list
    stationCLIM[[i]]<-cbind.data.frame(meta$name[i], meta$elev[i],
                                       paste0(min(yearCLIM$year),"-",max(yearCLIM$year)),
                                       yearCLIM$precipSum[rowYr],
                                       round(yearCLIM$anomPRECIP[rowYr],2),
                                       yearCLIM$precipDays[rowYr],
                                       yearCLIM$snowSum[rowYr],
                                       round(yearCLIM$avgTemp[rowYr],1),
                                       round(yearCLIM$anomTEMP[rowYr],2),
                                       yearCLIM$freezeDays[rowYr])
    
    # time period label
    if(season[1]>season[length(season)]){
      seasLabel<-paste0(month.abb[season[1]]," ",yearCLIM$year[rowYr]-1,"-",month.abb[season[length(season)]]," ",yearCLIM$year[rowYr])
    }else{
      seasLabel<-paste0(month.abb[season[1]]," ",yearCLIM$year[rowYr],"-",month.abb[season[length(season)]]," ",yearCLIM$year[rowYr])
    }
    
  }
  
  # combine list into df
  stationCLIM<-do.call(rbind, stationCLIM)
  # rename columns
  colnames(stationCLIM)<-c('Station','Elev (ft)','POR','Total Precip (in)','Precip Anom (in)','Days with Precip','Total Snow (in)',
                           'Avg Temp (F)','Temp Anom (F)','Freeze Days')
  
  # put into list
  stationList<-list(stationCLIM, seasLabel, meta)
  names(stationList)<-c("station_climate_summary","season_label","metaData")
  
  # return data
  return(stationList)
  
}
