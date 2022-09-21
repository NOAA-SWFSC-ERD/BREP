### sorry this is such a mess, apparently this was were I was at in 2018

anoms=read.csv("/Volumes/SeaGate/BREP/BREP/roms_anomalies/BREP_historical_SST_anomaly.txt")

contemp=anoms
contemp$Month=str_pad(contemp$Month,2,pad="0")
mean_anom_1month=contemp %>% mutate(date=paste0(Year,"-",Month)) %>% filter(date=="2014-07"|date=="2015-05"|date=="2016-05") %>% summarise(mean=min(SST_Anomaly)) %>% .[1,1] ## 1 month preceding closures
mean_anom_23=contemp %>% mutate(date=paste0(Year,"-",Month)) %>% filter(date=="2014-05"|date=="2014-06"|date=="2015-03"|date=="2015-04"|date=="2016-03"|date=="2016-04") %>% summarise(mean=min(SST_Anomaly))%>% .[1,1] ## months preceeding closures, 2nd and 3rd month as in registrar
mean_anom_6month=contemp %>% mutate(date=paste0(Year,"-",Month)) %>% filter(date=="2014-02"|date=="2014-03"|date=="2014-04"|date=="2014-05"|date=="2014-06"|date=="2014-07"|date=="2014-12"|date=="2015-01"|date=="2015-02"|date=="2015-03"|date=="2015-04"|date=="2015-05"|date=="2015-12"|date=="2016-01"|date=="2016-02"|date=="2016-03"|date=="2016-04"|date=="2016-05")%>% summarise(mean=min(SST_Anomaly))%>% .[1,1] ## 6 months preceding closures

contemp=contemp %>% dplyr::select(YR=Year,MON=Month,ANOM=SST_Anomaly)  #
contemp=contemp %>% dplyr::mutate(indicator_date=paste0(YR,"-",MON,"-16"))

contemp$one_month= mean_anom_1month
contemp$two_three= mean_anom_23
contemp$six_month= mean_anom_6month
contemp$one_month_value=NA
contemp$two_three_value=NA
contemp$six_month_value=NA 
contemp$one_month_status=NA
contemp$two_three_status=NA
contemp$six_month_status=NA 

# c. calc closure status
for(i in 7:nrow(contemp)){
  one_month_status=contemp$ANOM[i-1]
  two_three_status=mean(c(contemp$ANOM[i-2],contemp$ANOM[i-3]))
  six_month_status=mean(c(contemp$ANOM[i-1],contemp$ANOM[i-2],contemp$ANOM[i-3],contemp$ANOM[i-4],contemp$ANOM[i-5],contemp$ANOM[i-6]))
  
  contemp$one_month_value[i]=one_month_status
  contemp$two_three_value[i]=two_three_status
  contemp$six_month_value[i]=six_month_status
  
  if(one_month_status>=contemp$one_month[1]){contemp$one_month_status[i]="Closed"}
  if(one_month_status<contemp$one_month[1]){contemp$one_month_status[i]="Open"}
  
  if(two_three_status>=contemp$two_three[1]){contemp$two_three_status[i]="Closed"}
  if(two_three_status<contemp$two_three[1]){contemp$two_three_status[i]="Open"}
  
  if(six_month_status>=contemp$six_month[1]){contemp$six_month_status[i]="Closed"}
  if(six_month_status<contemp$six_month[1]){contemp$six_month_status[i]="Open"}
}

anom=contemp %>% dplyr::select(YR,MON,ANOM,indicator_date,contains("six")) %>% .[complete.cases(.),] %>% mutate(six_month_minus=six_month_value-six_month) %>% mutate(indicator_date=as.Date(indicator_date),zero=0)
anom$dt=strtrim(as.character(anom$indicator_date),8)
anom$dt=as.Date(paste0(anom$dt,"16"))
closures=anom %>% filter(indicator_date=="2014-08-16"|indicator_date=="2014-09-16"|indicator_date=="2015-06-16"|indicator_date=="2015-07-16"|indicator_date=="2015-08-16"|indicator_date=="2015-09-16"|indicator_date=="2016-06-16"|indicator_date=="2016-07-16"|indicator_date=="2016-08-16"|indicator_date=="2016-09-16") %>% group_by(YR)

ggplot()+
  geom_line(data=anom,aes(x=indicator_date,y=six_month_value),color="red")

