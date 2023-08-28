# download latest CPC outlooks and summarize for AOI
# MAC 3/11/23

library(raster)
library(RCurl)
#library(jsonlite)

# specify month/year
mo<-"12"
yr<-"2022"
currMoYr<-as.Date(paste0(yr,"-",mo,"-01"),format="%Y-%m-%d")

##### get extent from shapefile or specify directly ----
# load KNF forests
forests<-rgdal::readOGR("./shapes/AdministrativeForest.gdb")
kaibab_forest<-subset(forests, FORESTNAME=="Kaibab National Forest")
kaibab_forest<-spTransform(kaibab_forest, CRS("+proj=longlat +datum=WGS84")) # reproject to lat/lon
# get extent
aoi<-extent(kaibab_forest)
######

# create lookup tables
cpcProbs<-rbind.data.frame( c(33,"33-40%"),
                            c(40,"40-50%"),
                            c(50,"50-60%"),
                            c(60,"60-70%"),
                            c(70,"70-80%"),
                            c(80,"80-90%"),
                            c(90,"90-100%"))
colnames(cpcProbs)<-c("prob","label")

cpcCond<-rbind.data.frame(c("temperature","Above","warmer-than-average"),
                          c("temperature","Below","cooler-than-average"),
                          c("temperature","EC","equal-chances"),
                          c("precipitation","Above","wetter-than-average"),
                          c("precipitation","Below","drier-than-average"),
                          c("precipitation","EC","equal-chances"))
colnames(cpcCond)<-c("var","cat","label")

seasLabs<-rbind.data.frame(c("JFM","January-March"),
                            c("FMA","February-April"),
                            c("MAM","March-May"),
                            c("AMJ","April-June"),
                            c("MJJ","May-July"),
                            c("JJA","June-August"),
                            c("JAS","July-September"),
                            c("ASO","August-October"),
                            c("SON","September-November"),
                            c("OND","October-December"),
                            c("NDJ","November-January"),
                            c("DJF","December-February"))
colnames(seasLabs)<-c("abb","fullMonths")

monthLabs<-cbind.data.frame(month.abb,month.name)
  colnames(monthLabs)<-c("abb","name")

# create file names
if( format(currMoYr,"%m")==format(Sys.Date(),"%m") & format(currMoYr,"%Y")==format(Sys.Date(),"%Y")){
  fileNames<-c("monthupd_prcp_latest.zip",
               "monthupd_temp_latest.zip",
               "seasprcp_latest.zip",
               "seastemp_latest.zip")
}else{
  fileNames<-c(paste0("monthupd_prcp",format(currMoYr-1,"%Y%m"),".zip"),
               paste0("monthupd_temp",format(currMoYr-1,"%Y%m"),".zip"),
               paste0("seasprcp_",format(currMoYr-1,"%Y%m"),".zip"),
               paste0("seastemp_",format(currMoYr-1,"%Y%m"),".zip"))
}

# loop through file names
outList<-list()  
for(i in 1:length(fileNames)){
  # Create temp files
  temp <- tempfile()
  temp2 <- tempfile()
  # download files
  download.file(paste0("https://ftp.cpc.ncep.noaa.gov/GIS/us_tempprcpfcst/",fileNames[i]),
                destfile = temp)
  unzip(zipfile = temp, exdir = temp2)
  # read shapefile
  if( length(grep("seas",fileNames[i]))==0){
    outlook<-rgdal::readOGR(temp2)
  }else{
    outlook<-rgdal::readOGR(temp2,
                          rgdal::ogrListLayers(temp2)[grepl('lead1_', rgdal::ogrListLayers(temp2))])
  }
  # crop to aoi
  outlook<-crop(outlook, aoi)
  # get data from table, add labels
  outlook<-outlook@data[which.max(area(outlook)),c("Fcst_Date","Valid_Seas","Prob","Cat")]
  outlook$type<-ifelse(length(grep("seas",fileNames[i]))==0,"1-month","3-month")
  outlook$var<-ifelse(length(grep("prcp",fileNames[i]))==0,"temperature","precipitation")
  outlook$probLab<-cpcProbs[which(cpcProbs$prob==outlook$Prob),2]
  outlook$catLab<-ifelse(outlook$Prob>=50, paste0("Likely ",outlook$Cat),paste0("Leaning ",outlook$Cat))
  outlook$condLab<-cpcCond$label[which(outlook$var==cpcCond$var & outlook$Cat==cpcCond$cat)]
  
  if(outlook$type=="1-month"){
    outlook$timeLab<-monthLabs[which(substr(outlook$Valid_Seas,1,3)==monthLabs$abb),2]
  }else{
    outlook$timeLab<-seasLabs[which(substr(outlook$Valid_Seas,1,3)==seasLabs$abb),2]
  }

  # put df in list
  outList[[i]]<-outlook
    
}

# construct 1-month outlook report
if(outList[[1]]$Cat=="EC"){
  outPrecip<-paste0("The 1-month outlook for ",outList[[1]]$timeLab," predicts equal chances of above, below or normal precipitation and")
}else{
  outPrecip<-paste0("The 1-month outlook for ",outList[[1]]$timeLab," predicts a ",outList[[1]]$probLab," chance of ",outList[[1]]$condLab," precipitation amounts and")
}
if(outList[[2]]$Cat=="EC"){
  outTemp<-paste0(" equal chances of above, below or normal temperatures.")
}else{
  outTemp<-paste0(" a ",outList[[2]]$probLab," chance of ",outList[[2]]$condLab," temperatures.")
}
# paste outlook together
mo1outlook<-paste0(outPrecip,outTemp)

# construct 3-month outlook report
if(outList[[3]]$Cat=="EC"){
  outPrecip<-paste0("The 3-month seasonal outlook for ",outList[[3]]$timeLab," predicts equal chances of above, below or normal precipitation and")
}else{
  outPrecip<-paste0("The 3-month seasonal outlook for ",outList[[3]]$timeLab," predicts a ",outList[[3]]$probLab," chance of ",outList[[3]]$condLab," precipitation amounts and")
}
if(outList[[4]]$Cat=="EC"){
  outTemp<-paste0(" equal chances of above, below or normal temperatures.")
}else{
  outTemp<-paste0(" a ",outList[[4]]$probLab," chance of ",outList[[4]]$condLab," temperatures.")
}
# paste outlook together
seasOutlook<-paste0(outPrecip,outTemp)

# 
mo1outlook
seasOutlook

# output into list
outList<-list()
outList[[1]]<-mo1outlook
outList[[2]]<-seasOutlook







