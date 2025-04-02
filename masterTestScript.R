# test master script that uses helper functions
# MAC 12/08/22


library(kableExtra)


# set season
#season<-c(10,11,12,1,2,3,4,5,6,7,8,9)
season<-c(12,1,2)
currYr<-2023

stations<-c("028820","023668","027530")

# generate station table
source('getACISStations.R')
#stationCLIM<-getACISStations(c("020678", "USW00003195", "USS0012P02S"),season)
stationCLIM<-getACISStations(c("028820","023668","027530"),season, currYr)


stationCLIM$station_climate_summary %>%
  kbl(caption = paste0("Station climate summaries: ",stationCLIM$season_label)) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable("station_climate_summary.png")

# generate SNOTEL summary
source('getSNOTELdata.R')
snotelData<-getSNOTELdata('chalender',season)

snotelData$current_seas %>%
  dplyr::mutate(`Precip Anom (in)` = cell_spec(`Precip Anom (in)`, color = ifelse(`Precip Anom (in)` > 0, "green","brown"))) %>%
  #kbl(caption = paste0("SNOTEL station summary: ",snotelData$season_label)) %>%
  kable(escape=FALSE) %>%
  kable_styling(full_width = F, html_font = "Cambria") %>%
  save_kable("SNOTEL_climate_summary.png")