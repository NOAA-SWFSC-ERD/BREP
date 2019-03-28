## read in the correct packages
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}


pkgTest("stringr")
pkgTest("tidyverse")


anoms=read.csv("/Volumes/SeaGate/BREP/BREP/roms_anomalies/BREP_historical_SST_anomaly.txt") ### historical sst anomalies within extraction box from 1980 -> present (swith out file path)

# a. find anomalies for months proceeding historical closures

contemp=anoms
contemp$Month=str_pad(contemp$Month,2,pad="0") ## some junk to clean up the date format
threshold=0.77 ## threshold that determines whether closures are open or closed
contemp=contemp %>% dplyr::select(YR=Year,MON=Month,ANOM=SST_Anomaly)  ## some junk to clean up the date format
contemp=contemp %>% dplyr::mutate(indicator_date=paste0(YR,"-",MON,"-16")) ## some junk to clean up the date format

contemp$threshold= threshold
contemp$six_month_value=NA ## this column is going to be the indicator, i.e. the timeseries of anomalies smoothed by the preceeding six months
contemp$six_month_status=NA ## this column is going to be the closure status, i.e. if sixe_month_value is above or below 0.77, i.e. open or closed

# c. calc closure status
for(i in 7:nrow(contemp)){ ## for each row (starting at 7, because we need 6 preceeding months for each calculation)
  six_month_status=mean(c(contemp$ANOM[i-1],contemp$ANOM[i-2],contemp$ANOM[i-3],contemp$ANOM[i-4],contemp$ANOM[i-5],contemp$ANOM[i-6])) ## calculated the indicator, i.e. the mean of the preceeding 6 months
  contemp$six_month_value[i]=six_month_status
  
  if(six_month_status>=contemp$threshold[1]){contemp$six_month_status[i]="Closed"} ## if the indicator is above 0.77, write "closed" in the six_month_status column
  if(six_month_status<contemp$threshold[1]){contemp$six_month_status[i]="Open"} ## if the indicator is below 0.77, write "open" in the six_month_status column
}
contemp=contemp[7:nrow(contemp),] %>% dplyr::mutate(indicator_date=as.Date(indicator_date)) ## remove the first 6 rows that we couldn't work with and some junk to clean up the date format
write.csv(contemp,"/Volumes/SeaGate/BREP/BREP/operationalization_dale/indicators.csv") ## write out script
