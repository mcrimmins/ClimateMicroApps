# testing climate engine for plotting RAP data
# MAC 08/21/23
# links to info
# https://support.climateengine.org
# https://docs.climateengine.org/docs/build/html/overview.html
# https://api.climateengine.org/docs


library(raster)
library(httr) # HTTP API requests
library(httr2) # HTTP API requests
library(googleCloudStorageR)
library(tidyverse)

# load key, proj info
source('~/RProjects/ClimMicroApps/climEngKey.R')

# google bucket info
bucket<-"clim-engine"
# data directory
dataDir<-"climEng"

# load KNF forests
# forests<-rgdal::readOGR("./shapes/AdministrativeForest.gdb")
# kaibab_forest<-subset(forests, FORESTNAME=="Kaibab National Forest")
# kaibab_forest<-spTransform(kaibab_forest, CRS("+proj=longlat +datum=WGS84")) # reproject to lat/lon

# district level
#ogrListLayers("./shapes/RangerDistrict.gdb")
ShapeFile2<-rgdal::readOGR("./shapes/RangerDistrict.gdb")
Level2Data<-subset(ShapeFile2, FORESTNAME=="Kaibab National Forest")
Level2Data<-spTransform(Level2Data, CRS("+proj=longlat +datum=WGS84"))

# # rmu level
# # rgdal::ogrListLayers("./shapes/rmu.gdb")
# ShapeFile2<-rgdal::readOGR("./shapes/rmu.gdb")
# Level2Data<-subset(ShapeFile2, ADMIN_ORG_NAME=="WILLIAMS RANGER DISTRICT")
# Level2Data<-spTransform(Level2Data, CRS("+proj=longlat +datum=WGS84"))

# get bounding box coords
bbox<-paste0("[",extent(Level2Data)@xmin-0.1,",",extent(Level2Data)@ymin-0.1,
             ",",extent(Level2Data)@xmax+0.1,",",extent(Level2Data)@ymax+0.1,"]")

# set small bbox

bbox<-"[-111.809578,35.141283,-111.798720,35.149530]"
centroid<-paste0("[[",(-111.809578+-111.798720)/2,",",(35.141283+35.149530)/2,"]]")

##### get time series -----
# get timeseries of RAP data
# following https://github.com/Google-Drought/SupportSiteTutorials/blob/a038ee1e69fff32008d8619c0acc6db082d5795d/Timeseries/Blends_Example.Rmd
centroids<-rgeos::gCentroid(Level2Data,byid=TRUE)

# Define root url for Climate Engine API
root_url <- 'https://api.climateengine.org/'
# Define endpoint for initial data request
endpoint <- "timeseries/native/points"

# Define API arguments time-series endpoint to get long-term blend data 
query <- list(dataset = 'RAP_PRODUCTION_16DAY',
              variable = "herbaceousAGB",
              start_date = '1986-01-01',
              end_date = Sys.Date(),
              #buffer = '1000',
              coordinates = paste0("[[",centroids@coords[1,1],",",centroids@coords[1,2],"]]"),
              #coordinates = centroid,
              area_reducer = 'mean')

# Run GET request to get data
getTS <- GET(paste0(root_url, endpoint), config = add_headers(Authorization = key), query = query)
print(getTS)

# Parse JSON returned from API
rapTS <- content(getTS, "parsed")
rapTS <- enframe(unlist(rapTS[[1]]$Data))
rapTS_cols <- rapTS$name %>% unique()

# Define simple function to iterate over "name" values in parsed dataframe and return single column dataframe with those values
generate_data_frame <- function(col, df){
  # Filter for name from parsed API return
  df_val <- df %>%
    filter(name == col)
  # Create output data frame based on name from parsed API return
  out_df <- tibble(val = df_val$value)
  # Provide names from parsed API return as column name
  colnames(out_df) <- c(col)
  return(out_df)
}

# Map generate_data_frame over list of columns and clean up resulting data frame
rapTS <- map(rapTS_cols, generate_data_frame, rapTS) %>%
  bind_cols() %>%
  mutate(Date = as.Date(Date),
         Value = as.numeric(`herbaceousAGB (lbs/acre)`),
         Variable = "herbaceousAGB (lbs/acre)") %>%
  filter(Value > 0) %>%
  select(-`herbaceousAGB (lbs/acre)`)

# plot a time series
ggplot(rapTS, aes(Date,Value))+
  geom_line()+
  ggtitle(paste0("RAP herbAGB (lbs/ac) at ",centroids@coords[1,1],",",centroids@coords[1,2]))
#####

gcs_upload(rapTS, bucket=bucket)

##### download raw RAP raster ----
tempFile<-"rawRAP"
exportPath<-paste0(bucket,"/",tempFile)

print(paste0("Processing ", tempFile))

endpoint = '/raster/export/values'

##### q1 - raw data, latest period ----
query <- list(dataset = 'RAP_PRODUCTION_16DAY',
              variable = "herbaceousAGB",
              temporal_statistic = "mean",
              bounding_box = bbox,
              export_path = exportPath,
              start_date = rapTS$Date[nrow(rapTS)],
              end_date = rapTS$Date[nrow(rapTS)],
              start_year = '1986',
              end_year = '2022'
)
#####

##### q2 - total production since Jan 1st
query <- list(dataset = 'RAP_PRODUCTION_16DAY',
              variable = "herbaceousAGB",
              temporal_statistic = "total",
              bounding_box = bbox,
              export_path = exportPath,
              #export_resolution = 1000,
              start_date = paste0(format(rapTS$Date[nrow(rapTS)], "%Y"),"-01-01"),
              end_date = rapTS$Date[nrow(rapTS)]
)

# Run GET request to get data
get_raster <- GET(paste0(root_url, endpoint), config = add_headers(Authorization = key), query = query)
print(get_raster)

# download file from google cloud
objG<-gcs_list_objects(bucket)

# wait for processing
ptm <- proc.time()
  while(length(objG)==0){
    print("waiting for raster to process")
    objG<-gcs_list_objects(bucket)
    Sys.sleep(10)
  }
rawTime<-proc.time() - ptm

# download from bucket
gcs_get_object(objG$name[[which(objG$name==paste0(tempFile,".tif"))]], saveToDisk = paste0("./",dataDir,"/",tempFile,".tif"), bucket = bucket, overwrite = TRUE)
#####

##### download anom raster ----
tempFile<-"anomRAP"
exportPath<-paste0(bucket,"/",tempFile)

print(paste0("Processing ", tempFile))

endpoint = '/raster/export/anomalies'

##### q1 --- anomaly of most recent period -----
# query <- list(dataset = 'RAP_PRODUCTION_16DAY',
#               variable = "herbaceousAGB_mask",
#               temporal_statistic = "mean",
#               bounding_box = bbox,
#               export_path = exportPath,
#               start_date = rapTS$Date[nrow(rapTS)],
#               end_date = rapTS$Date[nrow(rapTS)],
#               start_year = '1986',
#               end_year = '2022',
#               calculation = 'anom'
#              )
#####

##### q2 -- anomaly of cumulative production ----
query <- list(dataset = 'RAP_PRODUCTION_16DAY',
              variable = "herbaceousAGB",
              temporal_statistic = "total",
              bounding_box = bbox,
              export_path = exportPath,
              start_date = paste0(format(rapTS$Date[nrow(rapTS)], "%Y"),"-01-01"),
              end_date = rapTS$Date[nrow(rapTS)],
              start_year = '1986',
              end_year = format(rapTS$Date[nrow(rapTS)], "%Y"),
              calculation = 'anom'
)
#####

# Run GET request to get data
get_raster <- GET(paste0(root_url, endpoint), config = add_headers(Authorization = key), query = query)
print(get_raster)

# download file from google cloud
objG<-gcs_list_objects(bucket)

# wait for processing
ptm <- proc.time()
  while(length(which(objG$name==paste0(tempFile,".tif")))==0){
    print("waiting for raster to process")
    objG<-gcs_list_objects(bucket)
    #gcs_list_objects(bucket)
    Sys.sleep(10)
  }
anomTime<-proc.time() - ptm
# download from bucket
gcs_get_object(objG$name[[which(objG$name==paste0(tempFile,".tif"))]], saveToDisk = paste0("./",dataDir,"/",tempFile,".tif"), bucket = bucket, overwrite = TRUE)
#####

##### download percentile raster
tempFile<-"percRAP"
exportPath<-paste0(bucket,"/",tempFile)

print(paste0("Processing ", tempFile))

endpoint = '/raster/export/percentiles'

##### q1 - percentiles of most recent period -----
# query <- list(dataset = 'RAP_PRODUCTION_16DAY',
#               variable = "herbaceousAGB_mask",
#               temporal_statistic = "mean",
#               bounding_box = bbox,
#               export_path = exportPath,
#               start_date = rapTS$Date[nrow(rapTS)],
#               end_date = rapTS$Date[nrow(rapTS)],
#               start_year = '1986',
#               end_year = '2022'
# )
#####

##### q2 - percentiles of cumulative total -----
query <- list(dataset = 'RAP_PRODUCTION_16DAY',
              variable = "herbaceousAGB",
              temporal_statistic = "total",
              bounding_box = bbox,
              export_path = exportPath,
              start_date = paste0(format(rapTS$Date[nrow(rapTS)], "%Y"),"-01-01"),
              end_date = rapTS$Date[nrow(rapTS)],
              start_year = '1986',
              end_year = format(rapTS$Date[nrow(rapTS)], "%Y")
)
#####

# Run GET request to get data
get_raster <- GET(paste0(root_url, endpoint), config = add_headers(Authorization = key), query = query)
print(get_raster)

# download file from google cloud
objG<-gcs_list_objects(bucket)

# wait for processing
ptm <- proc.time()
  while(length(which(objG$name==paste0(tempFile,".tif")))==0){
    print("waiting for raster to process")
    objG<-gcs_list_objects(bucket)
    Sys.sleep(10)
  }
percTime<-proc.time() - ptm
# download from bucket
gcs_get_object(objG$name[[which(objG$name==paste0(tempFile,".tif"))]], saveToDisk = paste0("./",dataDir,"/",tempFile,".tif"), bucket = bucket, overwrite = TRUE)
#####


####### PLOTTING/SPATIAL STATS ----

##### plot map
library(ggplot2)
library(rasterVis)
library(scales)
library(cowplot)

# load downloaded rasters
rawRAP<-raster("./climEng/rawRAP.tif")
anomRAP<-raster("./climEng/anomRAP.tif")
percRAP<-raster("./climEng/percRAP.tif")
# load shapefile for maps
poly<-fortify(Level2Data)

# use RPMS as mask
rpms<-raster("./data/2022rpms.tif")
  rpms<-crop(rpms,rawRAP)
rawRAP<-mask(rawRAP,rpms)
anomRAP<-mask(anomRAP,rpms)
percRAP<-mask(percRAP,rpms)

# make maps
pRAW<-rasterVis::gplot(rawRAP) + geom_tile(aes(fill = value)) +
  #facet_wrap(~ variable) +
  scale_fill_gradient2(low = 'brown', mid="yellow", high = 'forestgreen', midpoint=500, limits=c(0, 1000), oob=squish, name="lbs/ac") +
  #geom_polygon(data=poly, aes(x = long, y = lat, group = group), fill=NA, color="black")+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  coord_equal()+
  theme_bw()+
  ggtitle(paste0("Herbaceous Above-ground Biomass \n- RAP ",rapTS$Date[nrow(rapTS)]))

pAnom<-rasterVis::gplot(anomRAP) + geom_tile(aes(fill = value)) +
  #facet_wrap(~ variable) +
  scale_fill_gradient2(low = 'orange', mid="white", high = 'purple', limits=c(-50, 50), oob=squish, name="lbs/ac") +
  #scale_fill_gradient2(low = 'orange', mid="white", high = 'purple', limits=c(0, 100), midpoint = 50, oob=squish, name="Percentile") +
  geom_polygon(data=poly, aes(x = long, y = lat, group = group), fill=NA, color="black")+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  coord_equal()+
  theme_bw()+
  ggtitle(paste0("Herbaceous Above-ground Biomass \nAnomaly (lbs/ac) - RAP ",rapTS$Date[nrow(rapTS)]))

pPerc<-rasterVis::gplot(percRAP) + geom_tile(aes(fill = value)) +
  #facet_wrap(~ variable) +
  #scale_fill_gradient2(low = 'orange', mid="white", high = 'purple', limits=c(-50, 50), oob=squish, name="lbs/ac") +
  scale_fill_gradient2(low = 'orange', mid="white", high = 'purple', limits=c(0, 100), midpoint = 50, oob=squish, name="Percentile") +
  geom_polygon(data=poly, aes(x = long, y = lat, group = group), fill=NA, color="black")+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  coord_equal()+
  theme_bw()+
  ggtitle(paste0("Herbaceous Above-ground Biomass \nPercentile - RAP ",rapTS$Date[nrow(rapTS)]))

percRAP[percRAP >=16] <- NA

pPerc16<-rasterVis::gplot(percRAP) + geom_tile(aes(fill = value)) +
  #facet_wrap(~ variable) +
  #scale_fill_gradient2(low = 'orange', mid="white", high = 'purple', limits=c(-50, 50), oob=squish, name="lbs/ac") +
  scale_fill_gradient2(low = 'orange', mid="white", high = 'purple', limits=c(0, 100), midpoint = 50, oob=squish, name="Percentile") +
  geom_polygon(data=poly, aes(x = long, y = lat, group = group), fill=NA, color="black")+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  coord_equal()+
  theme_bw()+
  ggtitle(paste0("Herbaceous Above-ground Biomass \n<16th percentile - RAP ",rapTS$Date[nrow(rapTS)]))

p<-plot_grid(pRAW,pAnom,pPerc,pPerc16)

save_plot("RAP_percentiles_RPMSmask.png",p,base_height = 15, base_width = 11.5)

#####
# get zonal stats from districts
#test<-raster::extract(rapAnom, Level2Data, fun=mean, na.rm=TRUE, df=TRUE, weights=FALSE)

spatStats<-list()
for(i in 1:length(Level2Data@polygons)){
  clip1 <- crop(anomRAP, extent(Level2Data[i,])) #crop to extent of polygon
  clip2 <- rasterize(Level2Data[i,], clip1, mask=TRUE) #crops to polygon edge & converts to raster
  ext <- getValues(clip2)
  spatStats[[i]]<- cbind.data.frame(
    min(ext, na.rm = TRUE),
    max(ext, na.rm = TRUE),
    mean(ext, na.rm =TRUE),
    median(ext, na.rm = TRUE),
    IQR(ext, na.rm = TRUE),
    Level2Data[i,]@data)
}

spatStats<-do.call(rbind,spatStats)
colnames(spatStats)[1:5]<-c("min","max","mean","median","IQR")
spatStats[,1:5]<-round(spatStats[,1:5],1)

# make a table
library(kableExtra)

spatStats[,c(13,1:5)] %>%
  kbl(caption = paste0("RAP Anomaly (lbs/ac) Statistics - ",rapTS$Date[nrow(rapTS)])) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable("RAP_summary.png")
#####

######

##### CLEAN UP GOOGLE BUCKET ----
objG<-gcs_list_objects(bucket)

for(i in 1:nrow(objG)){
  gcs_delete_object(objG$name[i], bucket=bucket)
}
######


