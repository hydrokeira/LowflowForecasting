#### Required Libraries ####
library(dataRetrieval)
library(EGRET)
library("ggplot2")
library("sf")
library("dplyr")
library("tidyr")
library("stringr")

#### Conductivity / Dishcarge Data Retreival ####

#make list of states in Western US (WUS)
state_list<-c("WA", "OR", "CA", "MT", "NV", "UT", "CO", "AZ", "NM", "WY", "ID")

WUS_sites_94<-list()
WUS_sites_95<-list()

#retrieve conductivity sites in WUS w/ loop
for (i in 1:length(state_list)) {
  
  print(i)
  
  WUS_sites_94[[i]] <- whatNWISsites(stateCd = state_list[i], 
                                     parameterCd = "00094")
  
  WUS_sites_95[[i]] <- whatNWISsites(stateCd = state_list[i], 
                                     parameterCd = "00095")
}

#binding lab and field conductivity measurments 
WUS_94<-do.call(rbind, WUS_sites_94)

WUS_95<-do.call(rbind, WUS_sites_95)

WUS_sites<-bind_rows(WUS_94, WUS_95)

WUS_stream<-subset(WUS_sites, WUS_sites$site_tp_cd=="ST")

#retreiving discharge data
WUS_sites_disc <- list()

for (i in 1:length(state_list)) {
  
  print(i)
  
  WUS_sites_disc[[i]] <- whatNWISsites(stateCd = state_list[i], 
                                       parameterCd = "00060")
}

WUS_stream_disc <- do.call(rbind, WUS_sites_disc)

#binding discarge and conductivity dataframes
WUS_both <- intersect(WUS_stream$site_no, WUS_stream_disc$site_no)
WUS_stream_crop <- subset(WUS_stream, WUS_stream$site_no %in% WUS_both)
WUS_stream_crop<-WUS_stream_crop %>%
  distinct(site_no, .keep_all=TRUE)

#### conductivity filter loop ####
site_n_list <- (WUS_stream_crop$site_no)
site_quality_cond <- data.frame(matrix(ncol=2,nrow = length(site_n_list)))

#provide column names
colnames(site_quality_cond) <- c('gauge_code', "aData")

#i=1

for (i in 1:length(site_n_list)) {   
  print(i)
  site_n_i <- site_n_list[i]
  site_quality_cond$gauge_code[i]<-site_n_i
  conductivity <- readNWISqw(site_n_i, parameterCd = c("00094", "00095"))
  #conductivity <- aggregate(conductivity, by = list(conductivity$sample_dt), FUN = mean)
  #only keep sites that have more than 60 observations and whose last sample is after Jan 1, 1990
  site_quality_cond$aData[i] <- ifelse(length(unique(conductivity$sample_dt)) == 0, "Bad",
                                       ifelse(length(unique(conductivity$sample_dt))> 60,"Good", "Bad"))
  #site_quality_cond$aData[i]<-"Bad"
}

#### disharge filter loop ####

site_n_list <- unique(WUS_stream_crop$site_no)
site_quality_disc <- data.frame(matrix(ncol=2,nrow = length(site_n_list)))

#provide column names
colnames(site_quality_disc) <- c('gauge_code', "aData")

for (i in 1:length(site_n_list)) {   
  site_n_i <- site_n_list[i]
  site_quality_disc$gauge_code[i]<-site_n_i
  discharge <- readNWISdv(site_n_i, parameterCd = "00060")
  #if theres no discharge, bad, if there is good
  if(nrow(discharge) == 0){
    site_quality_disc$aData[i]<- "Bad"} else{
      site_quality_disc$aData[i] <- "Good"
    }
}

#subsetting by good data
Good_Cond <- subset(site_quality_cond, site_quality_cond$aData == "Good")
Good_Disc <- subset(site_quality_disc, site_quality_disc$aData == "Good")

#merge discharge and conductivity
Good_Sites <- merge(Good_Cond, Good_Disc)

setwd("C:\\Users\\johnkeir\\Box\\Hydrology_Lab\\Projects\\Low Flow\\Data")

#check if discharge is continuous for each "good site"
Good_Sites <- read.csv("GoodSites_CondQ.csv")

site_codes <- Good_Sites$gauge_code

site_codes <- ifelse((str_length(site_codes) < 8), paste0("0", site_codes), 
                                site_codes)


discharge_QAQC_list<-list()
for (i in 1:length(site_codes)) {
  
  print(i)
  
  good_site_one<- site_codes[i]
  
  Cond<-readNWISqw(good_site_one, parameterCd = c("00094", "00095"))
  
  Cond$sample_dt <- as.Date(Cond$sample_dt, "%Y-%m-%d")
  
  Cond <- Cond[complete.cases(Cond$sample_dt),]
  
  min_date<-min(as.Date(Cond$sample_dt))
  
  max_date<-max(as.Date(Cond$sample_dt))
  
  discharge <- readNWISdv(good_site_one, parameterCd = "00060")
  
  discharge_crop<-if(min_date > min(discharge$Date)){
    subset(discharge, discharge$Date >= min_date)
  } else{discharge}
  
  discharge_crop2<-if(max_date < max(discharge_crop$Date)){
    subset(discharge_crop, discharge_crop$Date <= max_date)
  } else{discharge_crop}
  
  discharge_QAQC_list[[i]] <- if(nrow(discharge_crop2) == 0){"Bad"} else{
      
    daily_TS<-as.data.frame(seq.Date(from = min(discharge_crop2$Date), to=max(discharge_crop2$Date), by="day"))
      
      colnames(daily_TS) <- "Date"
      
      discharge_daily<-merge(daily_TS, discharge_crop2, by="Date", all.x=TRUE)
      
      NA_lengths<-rle(is.na(discharge_daily$X_00060_00003))
      
      #ggplot(discharge_daily, aes(Date, X_00060_00003))+geom_line()
      
      if(max((NA_lengths$lengths)[NA_lengths$values==TRUE])>30){"Bad"} else{"Good"}
  }
}


QAQC_df <- do.call(rbind.data.frame, discharge_QAQC_list)
QAQC_df$site_codes<-site_codes
colnames(QAQC_df)[1]<-"Keep"
write.csv(QAQC_df, "QAQC_df.csv")

setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/Data")

QAQC_df <- read.csv("QAQC_df.csv")

#how many bads
length(which(QAQC_df$Keep == "Bad"))

#subset good
QAQC_good <- subset(QAQC_df, QAQC_df$Keep == "Good")
site_codes_good <- QAQC_good$site_codes
write.csv(site_codes_good, "site_codes_Q_good.csv")
