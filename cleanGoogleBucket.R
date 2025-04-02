# clean up Google Cloud bucket
# MAC 1/24/24

library(googleCloudStorageR)

# set home dir
home<-"/home/crimmins/RProjects/ClimMicroApps"

# load key, proj info
source(paste0(home,'/climEngKey.R'))
# google bucket info
bucket<-"clim-engine"

##### CLEAN UP GOOGLE BUCKET ----
objG<-googleCloudStorageR::gcs_list_objects(bucket)

for(i in 1:nrow(objG)){
  googleCloudStorageR::gcs_delete_object(objG$name[i], bucket=bucket)
}
######

