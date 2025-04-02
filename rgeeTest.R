# trying out reticulate and rgee
# 06/24/23 MAC

# https://www.css.cornell.edu/faculty/dgr2/_static/files/R_html/ex_rgee.html
library(reticulate)
#Sys.which("python")
#Sys.which("python3")
#use_python(Sys.which("python3"))
py_config()

#library(reticulate)
#path_to_python <- "/home/crimmins/anaconda3/bin/python3.6"
#use_python(path_to_python)

# use the standard Python numeric library
np <- reticulate::import("numpy", convert = FALSE)
# do some array manipulations with NumPy
a <- np$array(c(1:4))
print(a)  # this should be a Python array

print(py_to_r(a)) 

(sum <- a$cumsum())
print(py_to_r(sum))

# follow https://github.com/r-spatial/rgee to install rgee

install.packages(c("remotes", "googledrive"))
remotes::install_github("r-spatial/rgee")

library(rgee)

# Get the username
HOME <- Sys.getenv("HOME")

# 1. Install miniconda
reticulate::install_miniconda()

# 2. Install Google Cloud SDK
system("curl -sSL https://sdk.cloud.google.com | bash")

# 3 Set global parameters
Sys.setenv("RETICULATE_PYTHON" = sprintf("%s/.local/share/r-miniconda/bin/python3", HOME))
Sys.setenv("EARTHENGINE_GCLOUD" = sprintf("%s/google-cloud-sdk/bin/", HOME))

# 4 Install rgee Python dependencies
ee_install()

# 5. Authenticate and init your EE session
ee_Initialize()

library(reticulate)
library(rgee)

# 1. Initialize the Python Environment
ee_Initialize()

# 2. Install geemap in the same Python ENV that use rgee
# did not work!!! 06/24/2023
py_install("geemap")
gm <- import("geemap")


library(rgee)
ee_Initialize()

createTimeBand <-function(img) {
  year <- ee$Date(img$get('system:time_start'))$get('year')$subtract(1991L) 
  ee$Image(year)$byte()$addBands(img)
}

collection <- ee$
  ImageCollection('NOAA/DMSP-OLS/NIGHTTIME_LIGHTS')$
  select('stable_lights')$
  map(createTimeBand)

col_reduce <- collection$reduce(ee$Reducer$linearFit())
col_reduce <- col_reduce$addBands(
  col_reduce$select('scale'))
ee_print(col_reduce)

Map$setCenter(9.08203, 47.39835, 3)
Map$addLayer(
  eeObject = col_reduce,
  visParams = list(
    bands = c("scale", "offset", "scale"),
    min = 0,
    max = c(0.18, 20, -0.18)
  ),
  name = "stable lights trend"
)

library(tidyverse)
library(rgee)
library(sf)

ee_Initialize()
rgee::ee_Initialize(drive = TRUE)

nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate("2001-01-01", "2002-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("pr")) %>% # Select only precipitation bands
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("PP_%02d",1:12)) # rename the bands of an image

ee_nc_rain <- ee_extract(x = terraclimate, y = nc["NAME"], sf = FALSE)

  ee_nc_rain %>%
  pivot_longer(-NAME, names_to = "month", values_to = "pr") %>%
  mutate(month, month=gsub("PP_", "", month)) %>%
  ggplot(aes(x = month, y = pr, group = NAME, color = pr)) +
  geom_line(alpha = 0.4) +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  theme_minimal()

  
  
  #projects/rap-data-365417/assets/npp-partitioned-16day-v3-provisional

  test <- ee$ImageCollection("  projects/rap-data-365417/assets/npp-partitioned-16day-v3-provisional") %>%
    ee$ImageCollection$filterDate("2023-06-13")

  # Load the image.
  rap <- ee$Image('projects/rap-data-365417/assets/npp-partitioned-16day-v3-provisional/2023145')
  ee_print(rap)
  #vizParams <- list( bands = c('treNPP'))
  vizParams <- list(
    bands = c('pfgAGB'),
    min = 0.01,
    max = 50,
    palette = c('00FFFF', '0000FF')
  )
  Map$setCenter(lon = -112, lat = 32, zoom = 10)
  Map$addLayer(rap, vizParams,'rap data')

# export to raster
  # Define an image.
  img <- ee$Image('projects/rap-data-365417/assets/npp-partitioned-16day-v3-provisional/2023145')$
      select('pfgNPP')
  
  # img <- ee$Image("LANDSAT/LC08/C01/T1_SR/LC08_038029_20180810")$
  #   select(c("B4", "B3", "B2"))$
  #   divide(10000)
  # 
  # OPTIONAL display it using Map
  Map$centerObject(eeObject = img)
  Map$addLayer(eeObject = img, visParams = list(max = 50))
  
  # Define an area of interest.
  geometry <- ee$Geometry$Rectangle(
    coords = c(-112.8, 34.56, -111.47, 36.83),
    proj = "EPSG:4326",
    geodesic = FALSE
  )
  
  img_02 <- ee_as_raster(
    image = img,
    region = geometry,
    via = "drive"
  )
