# get stack of monthly TAVG PRISM from RCC ACIS, create raster stack
# MAC 12/8/22

# get monthly PRISM from RCC ACIS
library(RCurl)
library(jsonlite)
library(raster)    
# create current date
dateRangeStart="1900-01-01"
dateRangeEnd= "2021-12-31"

# generate dates -- keep with PRISM date
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),by="month")

# Set bounding box for PRISM extract
# AZ/NM bbox -115.004883,31.184609,-102.524414,37.387617
ACISbbox<- "-113.005371,34.642247,-111.132202,36.897194"

# ACIS query in JSON
jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"21","elems":"mly_avgt","meta":"ll,elev","output":"json"}') # or uid
#jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"2","elems":"pcpn","meta":"ll","output":"json"}') # or uid

out<-postForm("http://data.rcc-acis.org/GridData",
              .opts = list(postfields = jsonQuery,
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
out<-fromJSON(out)

# convert to list of matrices, flipud with PRISM
matrixList <- vector("list",length(out$data))
for(i in 1:length(out$data)){
  matrixList[[i]]<-apply(t(out$data[[i]][[2]]),1,rev)
}

# read into raster stack
rasterList<-lapply(matrixList, raster)
gridStack<-stack(rasterList)
gridExtent<-extent(min(out$meta$lon), max(out$meta$lon), min(out$meta$lat), max(out$meta$lat))
gridStack<-setExtent(gridStack, gridExtent, keepres=FALSE, snap=FALSE)
names(gridStack)<-allDates

## manage dates
allDates<-as.data.frame(allDates)
allDates$month<-as.numeric(format(allDates$allDates, "%m"))
allDates$year<-as.numeric(format(allDates$allDates, "%Y"))

# look at some data
plot(gridStack)