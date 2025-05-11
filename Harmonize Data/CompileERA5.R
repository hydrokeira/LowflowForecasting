##compile ERA5-Land data
require(googledrive)
require(reshape2)
require(EflowStats)
require(lubridate)
require(dplyr)

folder_links<-c("https://drive.google.com/drive/folders/1uzZGpKRqBhcgXMEO7Q_LT2hRViYWHJTR",
               "https://drive.google.com/drive/folders/1PvTtuqIPi7dG6CyUO6UYgjLp0L0p8ohJ",
               "https://drive.google.com/drive/folders/13fXlLjM0nT8aueF4nYlRf-6VKlltZMsI",
               "https://drive.google.com/drive/folders/13sWJhA20zFXG_28MOigOr7XNMy3CZdw3")

k=2
for (k in 1:4) {
 
  print(k)
  
  folder_id = drive_get(as_id(folder_links[k]))
  
  #download in entirety
  files = drive_ls(folder_id)
  
  setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/ERA5_Data")
  
  for (i in 1:length(files$name)) {
    
    print(i)
    
    drive_download(files$id[i], overwrite = TRUE)
    
  }
  
}

data_types<-c("EVAP", "Precip", "Temp", "SWE")

i=4

setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/ERA5_Data")

for (i in 1:4) {
  
  print(i)
  
  files_list<-list()
  
  files<-list.files(pattern = data_types[i])
  
  for (k in 1:length(files)) {
    
    dat<-read.csv(files[k])
    
    colnames_final<-colnames(dat)
    
    if(i==4){
      
    colnames(dat)<-colnames_final
      
    }
    
    files_list[[k]]<-dat
    
  }
  
  colnames(files_list[[6]])<-colnames(files_list[[7]]) #for a swe issue
  files_list[[6]]$dates<-as.Date(files_list[[6]]$dates, "%m/%d/%y")
  files_list[[6]]$dates<-as.character(files_list[[6]]$dates)
  
  dat_all<-do.call(bind_rows, files_list)
  
  write.csv(dat_all, paste0(data_types[i], "_AllSites.csv"))
}

setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/ERA5_Data/CompiledData")

test<-read.csv("SWE_1950_test.csv")
swe<-read.csv("SWE_AllSites.csv")

colnames_test<-colnames(test)
colnames_swe<-colnames(swe)

newcols<-setdiff(colnames_test, colnames_swe)
oldcols<-setdiff(colnames_swe, colnames_test)

test_cols<-test[,c("dates",newcols)]
swe_cols<-swe[,c("dates", oldcols)]

setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/ERA5_Data/CompiledData")

list.files()

evap<-read.csv("EVAP_AllSites.csv")

evap<-evap[,-c(1,6)]
evap_melt<-melt(evap, id.vars="dates")
evap_melt$site_no<-gsub('^.|.$', '', evap_melt$variable)
evap_melt$site_no<-substr(evap_melt$site_no, 1, nchar(evap_melt$site_no)-1)

evap_melt$site_no<-ifelse(nchar(evap_melt$site_no) < 8, paste0("0", evap_melt$site_no), evap_melt$site_no)

evap_melt$dates<-as.Date(evap_melt$dates)

evap_melt$waterYear<-get_waterYear(evap_melt$dates)

evap_annual<-evap_melt %>%
  group_by(site_no, waterYear) %>%
  summarise(annual_evap_mm=sum(value)*-1*1000)

write.csv(evap_annual, "Annual_Evaportation.csv")

precip<-read.csv("Precip_AllSites.csv")

precip<-precip[,-c(1,6)]
precip_melt<-melt(precip, id.vars="dates")
precip_melt$site_no<-gsub('^.|.$', '', precip_melt$variable)
precip_melt$site_no<-substr(precip_melt$site_no, 1, nchar(precip_melt$site_no)-1)

precip_melt$site_no<-ifelse(nchar(precip_melt$site_no) < 8, paste0("0", precip_melt$site_no), precip_melt$site_no)

precip_melt$dates<-as.Date(precip_melt$dates)

precip_melt$waterYear<-get_waterYear(precip_melt$dates)

precip_annual<-precip_melt %>%
  group_by(site_no, waterYear) %>%
  summarise(annual_precip_mm=sum(value)*1000)

write.csv(precip_annual, "Annual_Precipitation.csv")

temp<-read.csv("Temp_AllSites.csv")

temp<-temp[,-c(1,6)]
temp_melt<-melt(temp, id.vars="dates")
temp_melt$site_no<-gsub('^.|.$', '', temp_melt$variable)
temp_melt$site_no<-substr(temp_melt$site_no, 1, nchar(temp_melt$site_no)-1)

temp_melt$site_no<-ifelse(nchar(temp_melt$site_no) < 8, paste0("0", temp_melt$site_no), temp_melt$site_no)

temp_melt$dates<-as.Date(temp_melt$dates)

temp_melt$waterYear<-get_waterYear(temp_melt$dates)

temp_annual<-temp_melt %>%
  group_by(site_no, waterYear) %>%
  summarise(mean_temp_C=mean(value, na.rm = T)-273.15)

write.csv(temp_annual, "Annual_Temperature.csv")

swe<-read.csv("SWE_AllSites.csv")

swe<-swe[,-c(1)]

colnames(swe)[668:670]<-newcols

swe_melt<-melt(swe, id.vars="dates")
swe_melt$site_no<-gsub('^.|.$', '', swe_melt$variable)
swe_melt$site_no<-substr(swe_melt$site_no, 1, nchar(swe_melt$site_no)-1)

swe_melt$site_no<-ifelse(nchar(swe_melt$site_no) < 8, paste0("0", swe_melt$site_no), swe_melt$site_no)

swe_melt$dates<-as.Date(swe_melt$dates, "%m/%d/%y")
swe_melt$year<-year(swe_melt$dates)
swe_melt$year<-ifelse(swe_melt$year > 2023, swe_melt$year-100, swe_melt$year)
swe_melt <- swe_melt %>%
  mutate(dates = update(dates, year = year))

swe_melt$waterYear<-get_waterYear(swe_melt$dates)

swe_melt$value<-ifelse(swe_melt$value < 0, 0, swe_melt$value)

peak_swe_annual<-swe_melt %>%
  group_by(site_no, waterYear) %>%
  slice_max(value, n=1)

peak_swe_annual$unique<-paste(peak_swe_annual$site_no, peak_swe_annual$waterYear)

peak_swe_annual<-peak_swe_annual[!duplicated(peak_swe_annual$unique),]

peak_swe_annual$peak_swe_day<-get_waterYearDay(peak_swe_annual$dates)
peak_swe_annual$swe_mm<-peak_swe_annual$value*1000

peak_swe_annual<-peak_swe_annual[,c("waterYear", "site_no", "peak_swe_day", "swe_mm")]

swe_melt_sdd<-left_join(swe_melt, peak_swe_annual)

swe_melt_sdd_one_site<-subset(swe_melt_sdd, swe_melt_sdd$site_no=="05014500")

swe_sdd<-swe_melt_sdd %>%
  dplyr::group_by(site_no, waterYear) %>%
  dplyr::mutate(consecutive_zeros = cumsum(value == 0 & lag(value, default = 1) != 0)) %>%
  dplyr::mutate(wateryearday=get_waterYearDay(dates)) %>%
  dplyr::filter(value == 0 & wateryearday > peak_swe_day) %>%
  dplyr::group_by(site_no, waterYear, consecutive_zeros) %>%
  # Keep only the rows where there are at least 7 consecutive zeros
  dplyr::filter(n() >= 7) %>%
  # Find the first zero in each block
  dplyr::slice_min(order_by = dates, n = 1)

swe_sdd$unique<-paste(swe_sdd$site_no, swe_sdd$waterYear)

swe_sdd<-swe_sdd[!duplicated(swe_sdd$unique),]
swe_sdd$melt_days<-swe_sdd$wateryearday-swe_sdd$peak_swe_day

swe_sdd$melt_rate<-swe_sdd$swe_mm/swe_sdd$melt_days
  
swe_annual_v2<-swe_sdd[,c(4,6,7,8,10,13)]

colnames(swe_annual_v2)<-c("site_no", "waterYear", "peak_swe_day", "peak_swe_mm", "sdd", "melt_rate")
  
write.csv(swe_annual_v2, "Annual_SWE.csv")



