# get and map NASA SPORT LIS soil moisture data
# https://geo.ndc.nasa.gov/SPoRT/modeling/lis/conus3km/geotiff/vsm_percentiles/
# MAC 02/27/23

library(raster)
library(rasterVis)
library(jsonlite)

# load KNF forests
forests<-rgdal::readOGR("./shapes/AdministrativeForest.gdb")
kaibab_forest<-subset(forests, FORESTNAME=="Kaibab National Forest")
kaibab_forest<-spTransform(kaibab_forest, CRS("+proj=longlat +datum=WGS84")) # reproject to lat/lon

# get json listing of available files
fileList<-fromJSON("https://geo.ndc.nasa.gov/SPoRT/modeling/lis/conus3km/geotiff/vsm_percentiles/?format=json")
fileList<-fileList$directory_listing

# get filename
fileList<-tidyr::separate(data = fileList, col = filename, remove=FALSE,
                      into = c("date", "time", NA, NA, "var","type", "res","fileType","spatial","misc"), sep = "_")
fileList$date<-as.Date(fileList$date, format = "%Y%m%d")

fileName<-fileList$filename[which(fileList$date==max(fileList$date) & fileList$var=="vsm0-40cm" & fileList$fileType=="float" & fileList$spatial=="wgs84.tif")]

download.file(paste0("https://geo.ndc.nasa.gov/SPoRT/modeling/lis/conus3km/geotiff/vsm_percentiles/",fileName),
              destfile = "temp.tif")

mapSM<-raster("temp.tif")
mapSM[mapSM==9999]<-NA

mapSM<-crop(mapSM,extent(kaibab_forest))

at<-c(0,2,5,10,20,30,40,50,60,70,80,90,95,98,100)
#at<-c(seq(0,100,10))
#at<-c(seq(0,30,2.5))
#mapTheme <- rasterTheme(region = c("red","orange","yellow","white","white","lightblue","blue","darkblue"))
mapTheme <- rasterTheme(region = c("orangered4","orangered2","darkorange3","tan1","khaki1","gray75","gray75","gray75","gray75","azure1","lightskyblue","skyblue3","dodgerblue2","dodgerblue4"))

mapSMFig<-levelplot(mapSM, contour=FALSE, margin=FALSE, at=at,
                   par.settings=mapTheme,
                   #par.settings = list(region=c("lightblue", "blue","green","green4","yellow","red", "red4"),
                   #                   axis.line = list(col = textCol[panel.number()])),
                   #scales=list(draw=TRUE),
                   colorkey=list(at=at,
                                 labels=list(at=at,labels=at)),
                   main="NASA-LIS 0-40cm Vol Soil Moisture Percentile")+
  layer(sp.polygons(kaibab_forest, col = 'black', lwd=1))

png("SPORT_INT40_VSM.png", width = 10, height = 6, units = "in", res = 300L)
mapSMFig
dev.off()
