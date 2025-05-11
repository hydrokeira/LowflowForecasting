require(EflowStats)
require(dplyr)

setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/WUS_Lowflow")

wrtds_input_breaks<-read.csv("WRTDS_Cond_Input_Breaks.csv")
wrtds_q_input_breaks<-read.csv("WRTDS_Q_Input_Breaks.csv")

wrtds_input_nobreaks<-read.csv("WRTDS_Cond_Input.csv")
wrtds_q_input_nobreaks<-read.csv("WRTDS_Q_Input.csv")

wrtds_input_all<-bind_rows(wrtds_input_nobreaks, wrtds_input_breaks)

wrtds_q_input_all<-bind_rows(wrtds_q_input_nobreaks, wrtds_q_input_breaks)

wrtds_input_all$sample_dt<-as.Date(wrtds_input_all$sample_dt)
wrtds_q_input_all$Date<-as.Date(wrtds_q_input_all$Date)

breaks_sites<-unique(wrtds_input_all$site_no)

wrtds_q_input_all<-subset(wrtds_q_input_all, wrtds_q_input_all$site_no %in% breaks_sites)

# pdf("wrtds_input_allsites.pdf")
# 
# for (i in 1:length(breaks_sites)) {
#   
#   print(i)
#   
#   one_site<-subset(wrtds_input_all, wrtds_input_all$site_no==breaks_sites[i])
#   one_site_q<-subset(wrtds_q_input_all, wrtds_q_input_all$site_no==breaks_sites[i])
#   
#   p1<-ggplot()+geom_point(one_site, mapping=aes(sample_dt, result_va), col="red")+
#     geom_line(one_site_q, mapping=aes(Date, X_00060_00003))+
#     ggtitle(breaks_sites[i])
#   
#   print(p1)
#   
# }
# 
# dev.off()

Qshort_list<-list()
condshort_list<-list()

#cropped_Q_list<-c(6090300, 6287000, 6704500, 7103970, 9118450, 9183600, 
                  #12040500, 12324400, 12331800, 13050500, 13118700, 13159800)

for (i in 1:length(breaks_sites)) {
  
  print(i)
  
  df<-subset(wrtds_input_all, wrtds_input_all$site_no==breaks_sites[i])
  Q<-subset(wrtds_q_input_all, wrtds_q_input_all$site_no==breaks_sites[i])
  
  #find minimum date of Si file
  cond_min<-min(df$sample_dt)
  
  #convert to days since 1970
  cond_min_julian<-as.numeric(cond_min)
  
  # if(breaks_sites[i] %in% cropped_Q_list){
  #   
  #   Qmin<-cond_min_julian
  #   
  # }else{
    
    #subtract 10 years from Si min to get Q min
  Qmin<-(cond_min_julian-10*365.25)-1
    
  #}
  
  cond_max<-max(df$sample_dt)
  
  cond_max_julian<-as.numeric(cond_max)
  
  #subset Q file associated with Si file starting 10 years before Si file starts 
  #and ending when the Q file ends
  #extra space of Q file on ends of Si help moving flow weighted average for flux perform better
  Qshort<-Q[Q$Date >= Qmin & Q$Date <= cond_max_julian,]
  Qshort_list[[i]] <- Qshort
  
  #find minimum date of Q file
  Qmin<-min(Q$Date)
  
  #convert to day of water year
  QMinDay<-as.numeric(get_waterYearDay(Qmin))
  
  #find maximum date of Si file
  Qmax<-max(Qshort$Date)
  
  #convert to day of water year
  QMaxDay<-as.numeric(get_waterYearDay(Qmax))
  
  #subset Q file associated with Si file starting at beginning of water year of start of Si file and ending at end
  #of water year of last Si file date
  condShort<-df[df$sample_dt >= (Qmin) & df$sample_dt <= (Qmax),]
  
  condShort<-df[df$result_va <= mean(df$result_va)+4*sd(df$result_va),]
  
  condshort_list[[i]] <- condShort
  
}

wrtds_all_cropped<-bind_rows(condshort_list)
wrtds_q_all_cropped<-bind_rows(Qshort_list)

num_years<-wrtds_all_cropped %>%
  dplyr::group_by(site_no) %>%
  dplyr::summarise(n_year = n_distinct(WY))

num_obs<-wrtds_all_cropped %>%
  dplyr::group_by(site_no) %>%
  tally()

remove_these_obs<-subset(num_obs, num_obs$n < 60)

remove_these_years<-subset(num_years, num_years$n_year < 10)

remove_these_all<-c(remove_these_obs$site_no, remove_these_years$site_no)

wrtds_all_cropped2<-wrtds_all_cropped[!wrtds_all_cropped$site_no %in% remove_these_all,]

wrtds_all_cropped2<-wrtds_all_cropped2[,c("site_no", "sample_dt", "result_va")]

wrtds_q_all_cropped2<-wrtds_q_all_cropped[!wrtds_q_all_cropped$site_no %in% remove_these_all,]

wrtds_q_all_cropped2<-wrtds_q_all_cropped2[,c("site_no", "Date", "X_00060_00003")]

write.csv(wrtds_all_cropped2, "WRTDS_Cond_Input_08122024.csv")

write.csv(wrtds_q_all_cropped2, "WRTDS_Q_Input_08122024.csv")

##add line here to save out final WRTDS input files

final_sites<-unique(wrtds_all_cropped2$site_no)

pdf("wrtds_input_sites_final.pdf")

for (i in 1:length(final_sites)) {
  
  print(i)
  
  one_site<-subset(wrtds_all_cropped2, wrtds_all_cropped2$site_no==final_sites[i])
  one_site_q<-subset(wrtds_q_all_cropped2, wrtds_q_all_cropped2$site_no==final_sites[i])
  
  p1<-ggplot()+geom_point(one_site, mapping=aes(sample_dt, result_va), col="red")+
    geom_line(one_site_q, mapping=aes(Date, X_00060_00003))+
    ggtitle(breaks_sites[i])
  
  print(p1)
  
}

dev.off()


min_max_date<-wrtds_all_cropped2 %>%
  group_by(site_no) %>%
  summarise(min_date=min(sample_dt), max_date=max(sample_dt))

min_max_date$num_years<-difftime(min_max_date$max_date, min_max_date$min_date, units = "weeks")/52.25

mean_q<-wrtds_q_all_cropped2 %>%
  group_by(site_no) %>%
  summarise(median_q=median(X_00060_00003, na.rm=T))

wrtds_all_cropped2<-merge(wrtds_all_cropped2, mean_q, by="site_no")

png("USGS_sites_POR.png", width = 7, height = 10, units = "in", res = 300)

ggplot(wrtds_all_cropped2)+geom_point(aes(x=sample_dt, y=as.character(site_no), col=log(median_q)), size=.5)+
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(), text = element_text(size = 20))+
  scale_color_gradientn(colors=c("#f7fbff", "#6baed6","#08306b"))+
  scale_y_discrete(limits=unique(rev(min_max_date$site_no[order(min_max_date$min_date)])))+
  theme_bw()+labs(x="Date", col="Log (Discharge)", y="")

dev.off()

final_sites<-ifelse((str_length(final_sites) < 8), paste0("0", final_sites), 
                    final_sites)

site_info<-readNWISsite(final_sites)

getwd()

site_info$drain_area_va<-as.numeric(site_info$drain_area_va)

write.csv(site_info, "WRTDS_site_info.csv")




