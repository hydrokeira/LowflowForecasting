cond_files_path<-"/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/WRTDS_Outputs/DailyCondFiles"

groundwater_EM<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/HydroSepData/groundwater_EMs.csv")

runoff_EM<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/HydroSepData/runoff_EMs.csv")

sites<-unique(groundwater_EM$site_no)
sites_char<-ifelse(nchar(sites) < 8, paste0("0", sites), sites)

pdf("BaseflowProp.pdf")

skip_these<-c(32)

for (i in 32:length(sites_char)) {
  
  print(i)
  
  if(i %in% skip_these) next
  
  gw<-subset(groundwater_EM, groundwater_EM$site_no==sites[i])[1,3]
  ro<-subset(runoff_EM, runoff_EM$site_no==sites[i])[1,3]
  
  con_file<-list.files(path = cond_files_path, pattern = sites_char[i])
  
  continuous<-read.csv(paste0("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/WRTDS_Outputs/DailyCondFiles/",con_file))
  
  continuous<-continuous[,c("Date", "Q", "GenConc")]
  
  continuous$bf<-continuous$Q*((continuous$GenConc-ro)/(gw-ro))
  
  continuous$bf_percent<-continuous$bf/continuous$Q
  
  continuous$Date<-as.Date(continuous$Date)
  
  p1<-ggplot(continuous, aes(x=Date))+geom_line(aes(y=Q), group=1, col="blue")+
    geom_line(aes(y=bf), col="black")+theme_classic()+ylim(0,max(continuous$Q))+ggtitle(sites_char[i])+
    theme(text = element_text(size = 20))+labs(y="Discharge")
  
  p2<-ggplot(continuous, aes(x=Date))+geom_line(aes(y=log(Q)), group=1, col="black")+
    geom_line(aes(y=log(bf)), col="blue")+theme_bw()

  p3<-ggarrange(p1, p2, nrow = 2)
  
  pdf("BaseflowProp_DefenseExample.pdf", width = 8, height = 5)
  
  print(p1)
  
  dev.off()
  
  setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/HydroSepData/ContBaseflowFiles")
  
  write.csv(continuous, paste0("BaseflowProportion_", sites_char[i],".csv"))
  
}

dev.off()

BF_files_path<-"/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/HydroSepData/ContBaseflowFiles/"

groundwater_EM<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/HydroSepData/groundwater_EMs.csv")

sites<-unique(groundwater_EM$site_no)
sites_char<-ifelse(nchar(sites) < 8, paste0("0", sites), sites)

normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

WY_stats_list<-list()

flag_list<-list()

skip_these<-c(32)

setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/HydroSepData")

#pdf("BaseflowPropHistograms.pdf")

for (i in 1:length(sites)) {
  
  print(i)
  
  if(i %in% skip_these) next
  
  BF_file<-list.files(path = BF_files_path, pattern = sites_char[i])
  
  baseflow<-read.csv(paste0(BF_files_path, BF_file))
  
  BF_years_count<-baseflow %>%
    group_by(get_waterYear(as.Date(Date))) %>%
    summarise(num_days=n_distinct(Date))
  
  keep_years<-subset(BF_years_count, BF_years_count$num_days > 364)
  
  baseflow<-subset(baseflow, get_waterYear(as.Date(Date)) %in% keep_years$`get_waterYear(as.Date(Date))`)
  
  #p1<-ggplot(baseflow, aes(bf_percent))+geom_histogram(fill="black")+theme_bw()+ggtitle(sites_char[i])
  
  #print(p1)
  
  bf_WY<-baseflow %>%
    group_by(get_waterYear(as.Date(Date))) %>%
    summarise(mean_bf=median(bf_percent))
  
  bf_WY_flag<-bf_WY
  
  bf_WY_flag$flag<-ifelse(bf_WY_flag$mean_bf < 0 | bf_WY_flag$mean_bf > 1, "remove", "keep")
  
  flag_list[[i]]<-data.frame(ifelse("remove" %in% bf_WY_flag$flag, "flag", "fine"), sites_char[i])
  
  colnames(bf_WY)[1]<-"year"
  
  q5_WY<-baseflow %>%
    filter(month(Date) < 12 & month(Date) > 4) %>%
    group_by(year(as.Date(Date))) %>%
    summarise(lowflow=quantile(Q, 0.05))
  
  colnames(q5_WY)[1]<-"year"
  
  all_WY_stats<-left_join(bf_WY, q5_WY, by="year")
  all_WY_stats<-all_WY_stats[complete.cases(all_WY_stats),]
  
  all_WY_stats$mean_lowflow<-mean(all_WY_stats$lowflow)
  
  all_WY_stats$diff_mean_lowflow<-(all_WY_stats$lowflow-all_WY_stats$mean_lowflow)/all_WY_stats$mean_lowflow
  
  all_WY_stats$norm_lowflow_minmax<-normalize(all_WY_stats$lowflow)
  
  all_WY_stats$norm_lowflow_zscore<-scale(all_WY_stats$lowflow)
  
  all_WY_stats$site<-sites_char[i]
  
  WY_stats_list[[i]]<-all_WY_stats
  
}

#dev.off()

all_sites_WY_stats<-do.call(bind_rows, WY_stats_list)

flag_df<-do.call(bind_rows, flag_list)

remove_these<-subset(flag_df, flag_df$ifelse..remove...in..bf_WY_flag.flag...flag....fine..=="flag")

all_sites_WY_stats_cropped<-all_sites_WY_stats %>%
  filter(!(site %in% remove_these$sites_char.i.))

all_sites_WY_stats<-all_sites_WY_stats %>%
  group_by(site) %>%
  mutate(min_year=min(year))

dry_year<-all_sites_WY_stats %>%
  filter(norm_lowflow_minmax==0) %>%
  group_by(year) %>%
  tally()

p1<-ggplot(all_sites_WY_stats, aes(year, site))+geom_tile(aes(fill=norm_lowflow_minmax))+theme_classic()+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), text = element_text(size=20),
        legend.position = "bottom")+
  scale_y_discrete(limits=unique(rev(all_sites_WY_stats$site[order(all_sites_WY_stats$min_year)])))+
  scale_fill_gradientn(colors = c("#D7191C", "#FDAE61", "#ABD9E9", "#2C7BB6"), breaks=c(0,.5,1),
                       labels=c(0,.5,1))+labs(x="Year", fill="Normalized Annual Q5", y="")

p2<-ggplot(all_sites_WY_stats_cropped, aes(year, site))+geom_tile(aes(fill=mean_bf))+theme_classic()+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), text = element_text(size=20),
        legend.position = "bottom")+
  scale_y_discrete(limits=unique(rev(all_sites_WY_stats$site[order(all_sites_WY_stats$min_year)])))+
  scale_fill_gradientn(colors = c("#F1EEF6", "#D4B9DA", "#E7298A", "#67001F"), breaks=c(0,.5,1),
                       labels=c(0,.5,1))+
  labs(x="Year", fill="Mean Annual Proportional GW Contribution", y="")

pdf("Lowflow_GWProp.pdf", width = 14, height = 8)

ggarrange(p1, p2)

dev.off()

brewer.pal(n=9, "PuRd")

