# download/process rpms data
# MAC 1/28/23

library(raster)
library(rgdal)

# link from Rob Lankston - each year is 5.2 GB file
# download.file("https://storage.googleapis.com/fuelcast-data/rpms/2022/rpms_2022.tif?x-goog-signature=12f16b5ff62eba1e3227a4fd89e4eb3a02a7c372c2e88f5e5acff56d3d736caf78fe1b4fbb7320567aecf8094b0d308ab69fcafdab596ecaaab90bd1607c94779d7b4c84227e0ec745f739ff6d26a6c647a2bb3f62e082d61d980a474f0ed432a9e9a9c1ef4dfe5f02096e4c8c0fb5bfd31ef8fc41591f29cf2f6027034c08dd3166d280c91f022b2ba3774eef142fc63b1e649d236fb1da01b0d0ee85249b5f400e4ecfb0d532c26c563258e04361ef44d1db352282da6efac66e13082e388843639332e5d197912d949d1900d35fd9cdfadfd56f7eb869f894b94c0b93643b66e2de0802fe890b8c24300c1df7ea6207b84a56900799975a84f7b745879f7c&x-goog-algorithm=GOOG4-RSA-SHA256&x-goog-credential=storage-sa%40fuelcast.iam.gserviceaccount.com%2F20230127%2Fus-west1%2Fstorage%2Fgoog4_request&x-goog-date=20230127T225518Z&x-goog-expires=604800&x-goog-signedheaders=host",
#               destfile = "./data/2022rpms.tif", method="curl")

# link from https://fuelcast.net/downloads
download.file("https://fuelcast.net/rpms-download?date=2023&file=rpms_2023.tif",
              destfile = "./data/2023rpms.tif", method = "wget", extra = "--no-verbose")

# load KNF forests
forests<-readOGR("./shapes/AdministrativeForest.gdb")
kaibab_forest<-subset(forests, FORESTNAME=="Kaibab National Forest")
kaibab_forest<-spTransform(kaibab_forest, CRS("+proj=longlat +datum=WGS84")) # reproject to lat/lon


# load into layer
rpms<-raster("./data/2023rpms.tif")
rpms<-crop(rpms,extent(kaibab_forest))


library(rasterVis)

at<-c(seq(0,quantile(rpms, 0.99),200))
#at<-c(seq(0,30,2.5))
mapTheme <- rasterTheme(region = c("tan","orange","yellow","green","darkgreen"))


rpmsFig<-levelplot(rpms, contour=FALSE, margin=FALSE, at=at,
          par.settings=mapTheme,
          #par.settings = list(region=c("lightblue", "blue","green","green4","yellow","red", "red4"),
          #                   axis.line = list(col = textCol[panel.number()])),
          scales=list(draw=TRUE),
          main="2023 Average Production (RPMS, lbs/ac)")+
  layer(sp.polygons(kaibab_forest, col = 'black', lwd=1))



png("KNFrpms2023.png", width = 10, height = 6, units = "in", res = 300L)
rpmsFig
dev.off()

# new download options https://fuelcast.net/downloads

# link from Rob Lankston - each year is 5.2 GB file
#download.file("https://fuelcast.net/download?date=2023-03-22&file=total_herb_ppa_2023-03-22.tif",
#              destfile = "./data/20230322rpms.tif", method="curl")





