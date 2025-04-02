# get PhenoMap data
# 01/24/23 MAC
# https://manzanita.forestry.oregonstate.edu/arcgis/rest/services/WWETAC_PhenoMap
# https://www.fs.usda.gov/rmrs/tools/wwetac-phenomap
# https://kdvdecisions-blog.netlify.app/2020/04/18/obtaining-spatial-data-from-esri-rest-apis-in-r/

# trying https://andyarthur.org/how-to-access-wms-servers-in-r-programming-language.html

# obtain layer info from WMS server using sf built-in version of gdal_info
wms_url <- 'https://orthos.its.ny.gov/ArcGIS/services/wms/Latest/MapServer/WMSServer?'
wms_url<-'https://manzanita.forestry.oregonstate.edu/arcgis/rest/services/WWETAC_PhenoMap/PhenoMap_Current_Week/MapServer'
ginfo <- sf::gdal_utils('info', stringr::str_c('WMS:',wms_url), quiet=F)

# wfs https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/
# wms https://andyarthur.org/how-to-access-wms-servers-in-r-programming-language.html


library(raster)

download.file("https://manzanita.forestry.oregonstate.edu/arcgis/rest/services/WWETAC_PhenoMap/PhenoMap_Week_4/MapServer/export?bbox=-112%2C33%2C-111%2C36&bboxSR=4326&layers=0&layerDefs=&size=&imageSR=&historicMoment=&format=png32&transparent=false&dpi=1200&time=&layerTimeOptions=&dynamicLayers=&gdbVersion=&mapScale=&rotation=&datumTransformations=&layerParameterValues=&mapRangeValues=&layerRangeValues=&f=image", destfile = "test.png", method="curl") # created subdir 'shapes'

#test<-rgdal::readOGR("https://manzanita.forestry.oregonstate.edu/arcgis/rest/services/WWETAC_PhenoMap/PhenoMap_Week_4/MapServer/export?bbox=-112%2C30%2C-110%2C35&bboxSR=4326&layers=0&layerDefs=&size=&imageSR=&historicMoment=&format=png32&transparent=false&dpi=600&time=&layerTimeOptions=&dynamicLayers=&gdbVersion=&mapScale=&rotation=&datumTransformations=&layerParameterValues=&mapRangeValues=&layerRangeValues=&f=kmz")

test<-raster('test.png')

# see REST specifications
# https://gis.stackexchange.com/questions/373270/rest-queries-for-esri-map-service-raster-layers


# https://cran.r-project.org/web/packages/arcpullr/vignettes/raster_layers.html

library(arcpullr)
library(sf)

bbox<-st_sf(st_as_sfc(st_bbox(c(xmin = -110, xmax = -112, ymax = 36, ymin = 35), crs = sf::st_crs(4326))))

# PhenoMap Server
image_server <- "https://manzanita.forestry.oregonstate.edu/arcgis/rest/services/"

service<-"WWETAC_PhenoMap/PhenoMap_Current_Week/MapServer/0/"

url<-paste0(image_server,service)

#image <- get_image_layer(url, bbox)
image <- get_map_layer(url, bbox)


plot_layer(wi_landcover)


# Creating the aoi in `EPSG: 32145` and converting it into the right crs (i.e. `EPSG: 3857`)
aoi <- st_sf(st_as_sfc(st_bbox(c(xmin = 499682.2, xmax = 503271.3, ymin = 208467.7, ymax = 212056.7), crs = st_crs(32145)))) %>% 
  st_transform(., st_crs(3857))

# Extracting the raster corresponding to the `aoi`
endpoint <- "https://maps.vcgi.vermont.gov/arcgis/rest/services/EGC_services/IMG_VCGI_LIDARNDSM_WM_CACHE_v1/ImageServer/"
ndsm <- get_image_layer(url = endpoint, sf_object = aoi)

# example
# WDNR Server
image_server <- "https://dnrmaps.wi.gov/arcgis_image/rest/services/"

# WI Landcover Type URL
landcover_path <- "DW_Land_Cover/EN_Land_Cover2_Lev2/MapServer"
landcover_url <- paste0(image_server, landcover_path)

# WI Leaf-off Aerial Imagery URL
wi_leaf_off_path <- "DW_Image/EN_Image_Basemap_Leaf_Off/ImageServer"
wi_aerial_imagery_url <- paste0(image_server, wi_leaf_off_path)

wi_landcover <- get_map_layer(landcover_url, wis_poly)
plot_layer(wi_landcover)

get_layer_legend(landcover_url)
