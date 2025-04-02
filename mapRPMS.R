# script to download and map USFS RPMS forage production data
# MAC 02/02/23

library(raster)
library(rgdal)
library(rasterVis)

yr<-2023

# link from https://fuelcast.net/downloads
download.file(paste0("https://fuelcast.net/rpms-download?date=",yr,"&file=rpms_",yr,".tif"),
              destfile = paste0("./data/",yr,"rpms.tif"), method = "wget", extra = "--no-verbose")

# # load KNF forests
# forests<-readOGR("./Kaibab National Forest/Data/shapes/AdministrativeForest.gdb")
# kaibab_forest<-subset(forests, FORESTNAME=="Kaibab National Forest")
# kaibab_forest<-spTransform(kaibab_forest, CRS("+proj=longlat +datum=WGS84")) # reproject to lat/lon

# Allotments level
ogrListLayers("./shapes/rmu.gdb")
allotments<-readOGR("./shapes/rmu.gdb")
kaibab_allotments<-subset(allotments, ADMIN_FOREST_NUM=="07")
kaibab_allotments<-spTransform(kaibab_allotments, CRS("+proj=longlat +datum=WGS84"))


# load into layer
rpms<-raster(paste0("./data/",yr,"rpms.tif"))
rpms<-crop(rpms,extent(kaibab_forest))

# plot map using RasterVis
at<-c(seq(0,quantile(rpms, 0.99),200)) # set values in color ramp
mapTheme <- rasterTheme(region = c("tan","orange","yellow","green","darkgreen")) # set coloramp

# plot figure
rpmsFig<-levelplot(rpms, contour=FALSE, margin=FALSE, at=at,
                   par.settings=mapTheme,
                   #par.settings = list(region=c("lightblue", "blue","green","green4","yellow","red", "red4"),
                   #                   axis.line = list(col = textCol[panel.number()])),
                   scales=list(draw=TRUE),
                   main=paste0(yr," Average Production (RPMS, lbs/ac)"))+
  layer(sp.polygons(kaibab_allotments, col = 'black', lwd=1))

# save figure
png(paste0("rpms",yr,".png"), width = 5.9, height = 5.9, units = "in", res = 300L)
  rpmsFig
dev.off()

# https://stackoverflow.com/questions/75328954/how-to-extract-the-mean-raster-value-for-a-polygon-based-on-matching-a-layer-fro

