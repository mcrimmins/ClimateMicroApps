# testing out NRCS MLRA/LRU shapefile from Emilio
# MAC 5/24/23

##### map plotting, time series extraction, zonal stats with polygons...
library(rgdal)
# get shapefiles from https://www.fs.usda.gov/detailfull/r3/landmanagement/gis/?cid=stelprdb5201889&width=full
download.file("https://cals.arizona.edu/climate/misc/AZ_MLRA.zip", destfile = "./shapes/AZ_MLRA.zip", method="curl") # created subdir 'shapes'
unzip("./shapes/AZ_MLRA.zip", exdir = "./shapes",overwrite = TRUE)
#ogrListLayers("./shapes/AdministrativeForest.gdb")
mlraAZ<-readOGR("./shapes/AZ_MLRA","mlra_a_az")
mlra41<-subset(mlraAZ, MLRA=="41")
mlra41<-spTransform(mlra41, CRS("+proj=longlat +datum=WGS84")) # reproject to lat/lon

# need to group polygons up to LRU
# Merge polygons by ID, https://gis.stackexchange.com/questions/63577/joining-polygons-in-r
mlra41agg <- maptools::unionSpatialPolygons(mlra41, mlra41@data$LRU)
# add back in grouped df
# Convert SpatialPolygons to data frame
mlra.df <- as(mlra41, "data.frame")
# Aggregate and sum desired data attributes by ID list
mlra41.df.agg <- aggregate(mlra.df[, 1:3], list(mlra.df$LRU), sum)
row.names(mlra41.df.agg) <- as.character(mlra41.df.agg$Group.1)
# Reconvert data frame to SpatialPolygons
mlra41.shp.agg <- SpatialPolygonsDataFrame(mlra41agg, mlra41.df.agg)
# test plot
spplot(mlra41.shp.agg, "Group.1", main = "AZ MLRA 41 - LRUs")