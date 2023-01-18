# myRAINge data summaries
# MAC 11/30/22

UID<-'771fb840-9ca0-11eb-b8f0-81aee0ecb927'
#gaugeID<-'a3232720-e775-11e8-a0f1-1fd3e8eee44c'
gaugeID<-'b8c617e0-e775-11e8-a0f1-1fd3e8eee44c'

URL<-paste0('https://n7pw03xtv3.execute-api.us-west-2.amazonaws.com/prod/public/gauges/',UID,'/',gaugeID)

jsonOUT<-jsonlite::fromJSON(URL)

logs<-jsonOUT$logs
  logs$logDateTime<-as.POSIXct(logs$logDateTime)
  logs$reading<-as.numeric(logs$reading)
  
# create observation table
  logList<-list()
  k=1
  for(i in 1:nrow(logs)){
    if(logs$isReset[i]==FALSE){
      logList[[k]]<-cbind.data.frame(logs$logDateTime[i-1],logs$logDateTime[i],logs$reading[i])
      k=k+1
    }
  }
  
logTable = do.call(rbind, logList)
  colnames(logTable)<-c("Reset Date","Reading Date","Total Precip")

library(kableExtra)
logTable %>%
  kbl(caption = jsonOUT$title) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable("myRAINge_precip_summary.png")

