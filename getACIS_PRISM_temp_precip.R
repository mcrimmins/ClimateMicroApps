# get stack of monthly PRISM from RCC ACIS, create raster stack
# MAC 9/8/22

# get monthly PRISM from RCC ACIS
library(RCurl)
library(jsonlite)
library(raster)    
# create current date
dateRangeStart="1900-01-01"
dateRangeEnd= "2022-12-31"

# generate dates -- keep with PRISM date
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),by="month")

# Set bounding box for PRISM extract
# AZ/NM bbox -115.004883,31.184609,-102.524414,37.387617
ACISbbox<- "-113.005371,34.642247,-111.132202,36.897194"

ACISbbox<-"-113.046570,31.377089,-111.088257,32.976412"


# ACIS query in JSON
#jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"21","elems":"mly_pcpn","meta":"ll,elev","output":"json"}') # or uid
#jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"2","elems":"pcpn","meta":"ll","output":"json"}') # or uid
# precip and temp
jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"21","elems":[{"name":"mly_pcpn"},{"name":"mly_avgt"}],"meta":"ll,elev","output":"json"}') # or uid



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
# set 0 and neg to NA
gridStack[gridStack < 0] <- NA

#### get temp data #####
matrixList <- vector("list",length(out$data))
for(i in 1:length(out$data)){
  matrixList[[i]]<-apply(t(out$data[[i]][[3]]),1,rev)
}

# read into raster stack
rasterList<-lapply(matrixList, raster)
tempStack<-stack(rasterList)
gridExtent<-extent(min(out$meta$lon), max(out$meta$lon), min(out$meta$lat), max(out$meta$lat))
tempStack<-setExtent(tempStack, gridExtent, keepres=FALSE, snap=FALSE)
names(tempStack)<-allDates
# set 0 and neg to NA
tempStack[tempStack <= -999] <- NA
#####



## manage dates
allDates<-as.data.frame(allDates)
allDates$month<-as.numeric(format(allDates$allDates, "%m"))
allDates$year<-as.numeric(format(allDates$allDates, "%Y"))

# look at some data
plot(gridStack)

# calc SPI on stack using SPEI package
library(SPEI)
# set rasteroptions
rasterOptions(progress = 'text')

# Declare the function to use
funSPI <- function(x, scale=2, na.rm=TRUE,...) as.numeric((spi(x, scale=scale, na.rm=na.rm, ...))$fitted)

rstSPI <- calc(gridStack, fun = funSPI)


##### map plotting, time series extraction, zonal stats with polygons...
library(rgdal)
# get shapefiles from https://www.fs.usda.gov/detailfull/r3/landmanagement/gis/?cid=stelprdb5201889&width=full
download.file("https://www.fs.usda.gov/r3/gis/alp/AdministrativeForest.zip", destfile = "./shapes/forests.zip", method="curl") # created subdir 'shapes'
  unzip("./shapes/forests.zip", exdir = "./shapes",overwrite = TRUE)
  #ogrListLayers("./shapes/AdministrativeForest.gdb")
  forests<-readOGR("./shapes/AdministrativeForest.gdb")
  kaibab_forest<-subset(forests, FORESTNAME=="Kaibab National Forest")
  kaibab_forest<-spTransform(kaibab_forest, CRS("+proj=longlat +datum=WGS84")) # reproject to lat/lon
# districts  
download.file("https://www.fs.usda.gov/r3/gis/alp/RangerDistrict.zip", destfile = "./shapes/districts.zip", method="curl") # created subdir 'shapes'
  unzip("./shapes/districts.zip", exdir = "./shapes",overwrite = TRUE)
  #ogrListLayers("./shapes/AdministrativeForest.gdb")
  districts<-readOGR("./shapes/RangerDistrict.gdb")
  kaibab_districts<-subset(districts, FORESTNAME=="Kaibab National Forest")
  kaibab_districts<-spTransform(kaibab_districts, CRS("+proj=longlat +datum=WGS84")) # reproject to lat/lon
# RMU Units
download.file("https://www.fs.usda.gov/r3/gis/gisdata/Rangeland.zip", destfile = "./shapes/rmu.zip", method="curl") # created subdir 'shapes'
  unzip("./shapes/rmu.zip", exdir = "./shapes",overwrite = TRUE)
  ogrListLayers("./shapes/rmu.gdb")
  allotments<-readOGR("./shapes/rmu.gdb","GC_ALLOTMENTS_V")
  kaibab_allotments<-subset(allotments, ADMIN_FOREST_NUM=="07")
  kaibab_allotments<-spTransform(kaibab_allotments, CRS("+proj=longlat +datum=WGS84")) # reproject to lat/lon
  
 # make a map with polygon overlay
  library(rasterVis)
  
  pal <- colorRampPalette(RColorBrewer::brewer.pal(n=11, 'BrBG'))
  levelplot(rstSPI[[nlayers(rstSPI)]], margin=FALSE, col.regions=pal, at=seq(-3, 3, 0.25))+
    layer(sp.polygons(kaibab_allotments, fill = NA, col="black"))
 
 # extract a time series
  tsSPI<-t(raster::extract(rstSPI, cellFromXY(rstSPI, c(-112.2,36.4))))
  
#  extract average SPI for specific polygon
  poly<-subset(kaibab_districts, kaibab_districts$DISTRICTNAME=="Tusayan Ranger District")
  avgSPI <- t(extract(rstSPI, poly, fun='mean', na.rm=TRUE, df=TRUE, weights = FALSE))
  avgSPI <- avgSPI[2:nrow(avgSPI),] # drop that first ID row
  avgSPI <- cbind.data.frame(allDates, avgSPI)
  
  # plot leaflet of districts
  library(leaflet)
  
  leaflet(kaibab_forest) %>%  addTiles() %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5)
  
  
  
#####  
# plot with basemap

  library(basemaps)
  
  # set projections of existing data
  proj4string(kaibab_allotments) <- CRS("+init=epsg:4326")
  proj4string(rstSPI) <- CRS("+init=epsg:4326")
  
  # coordinate ref system for basemaps
  epsg3857<-"+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs +type=crs"
  
  # reproject layers to match basemaps
  kaibab_allotments_webMerc <- spTransform(kaibab_allotments, CRS(epsg3857))
  rstSPI_webMerc <- projectRaster(rstSPI[[nlayers(rstSPI)]], res=4000, raster(kaibab_allotments_webMerc, res=4000))
  
  # get bbox in right format based on raster layer
  ext<-extent(rstSPI_webMerc)
  ext<-sf::st_bbox(c(xmin = ext@xmin, xmax = ext@xmax, ymax = ext@ymax, ymin = ext@ymin), crs = sf::st_crs(rstSPI_webMerc))
  
  # or as plot:
  #basemap_plot(ext)
  
  # or as ggplot2:
  ggBase<-basemap_ggplot(ext, map_service = "carto", map_type = "light")
  
  # raster layer into df for ggplot
  r_spdf <- as(rstSPI_webMerc, "SpatialPixelsDataFrame")
  r_df <- as.data.frame(r_spdf)
  colnames(r_df) <- c("value", "x", "y") 
  
  # trying this https://stackoverflow.com/questions/53023183/overlay-multiple-geom-raster-plots-with-different-gradients
    
  ggBase+annotate(geom="raster", x=r_df$x, y=r_df$y, alpha=.5,
             fill = scales::colour_ramp(c("brown","white","green"))(r_df$value))+
    geom_polygon(data=kaibab_allotments_webMerc, aes(x=long, y=lat, group=group), 
                 fill=NA, color="grey50", size=0.25)+
    coord_sf()
   
  # need better mapping of colors and need legend, not sure if possible with annotation
  
