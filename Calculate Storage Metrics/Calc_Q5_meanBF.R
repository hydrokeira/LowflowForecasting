setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow")

BF_files_path<-"/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/HydroSepData/ContBaseflowFiles/"

groundwater_EM<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/HydroSepData/groundwater_EMs.csv")

sites<-unique(groundwater_EM$site_no)
sites_char<-ifelse(nchar(sites) < 8, paste0("0", sites), sites)

normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

skip_these<-c(32)

bf_list<-list()

flag_list_bf<-list()

lowflow_list<-list()

for (i in 1:length(sites_char)) {
  
  print(i)
  
  if(i %in% skip_these) next
  
  BF_file<-list.files(path = BF_files_path, pattern = sites_char[i])
  
  baseflow<-read.csv(paste0(BF_files_path, BF_file))
  baseflow$Date<-as.Date(baseflow$Date)
  
  BF_years_count<-baseflow %>%
    group_by(get_waterYear(Date)) %>%
    summarise(num_days=n_distinct(Date))
  
  keep_years<-subset(BF_years_count, BF_years_count$num_days > 364)
  
  baseflow<-subset(baseflow, get_waterYear(Date) %in% keep_years$`get_waterYear(Date)`)
  
  mean_basefow<-baseflow %>%
    group_by(get_waterYear(Date)) %>%
    summarise(mean_bf=median(bf_percent))
  
  mean_basefow$site<-sites_char[i]
 
  #mean_basefow$flag<-ifelse(mean_basefow$mean_bf < 0 | mean_basefow$mean_bf > 1, "remove", "keep")
  
  bf_list[[i]]<-mean_basefow
  
  #flag_list_bf[[i]]<-data.frame(ifelse("remove" %in% mean_basefow$flag, "flag", "fine"), sites_char[i])
  
  q5<-baseflow %>%
    filter(month(Date) < 11 & month(Date) > 7) %>%
    group_by(year(as.Date(Date))) %>%
    summarise(lowflow=quantile(Q, 0.05)) %>%
    mutate(norm_q5=normalize(lowflow))
  
  q5$site<-sites_char[i]
  
  lowflow_list[[i]]<-q5
   
}

bf_df<-do.call(bind_rows, bf_list)
colnames(bf_df)<-c("WY", "mean_bf_prop", "site")

bf_df_long_record<-bf_df %>%
  group_by(site) %>%
  filter(n() > 9)

lowflow_df<-do.call(bind_rows, lowflow_list)
colnames(lowflow_df)<-c("WY", "Q5", "norm_Q5", "site")

site_stats<-left_join(bf_df_long_record, lowflow_df, by=c("site", "WY"))

unique(site_stats$site)

write.csv(site_stats, "Site_Stats_Q5_baseflow.csv")

