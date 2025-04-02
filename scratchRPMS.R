# Process RPMS data to get period average and recent year anom
# MAC 10/19/23

library(raster)

# set rasteroptions
rasterOptions(progress = 'text')

#showTmpFiles()
#removeTmpFiles()


# load files into stack
file_list = list.files(path="/scratch/crimmins/USDA_NPP/v2023",pattern = ".tif", full.names = T, recursive = T)
  # drop 2012 from list, bad data?
  file_list<-file_list[-29]
rpmsStack<-stack(file_list[1:(length(file_list)-1)])

# calculate mean
meanRPMS <- calc(rpmsStack, fun = mean, na.rm = T)

library(cluster)
beginCluster(n=7)
  meanRPMS <- clusterR(rpmsStack,fun=calc,args=list(fun = function(x) mean(x, na.rm=TRUE)))
endCluster()

plot(meanRPMS)

writeRaster(meanRPMS, filename = paste0("/scratch/crimmins/USDA_NPP/v2023/swRPMS_1984_2022mean.tif"), format="GTiff", overwrite=TRUE)

# calculate anomaly
currYrRaster<-raster(file_list[39])
rpmsAnom<-currYrRaster-meanRPMS

writeRaster(rpmsAnom, filename = paste0("/scratch/crimmins/USDA_NPP/v2023/swRPMS_2023anom_lzw.tif"), format="GTiff", overwrite=TRUE)


