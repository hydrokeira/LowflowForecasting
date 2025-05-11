require(dplyr)
require(ggplot2)
require(zoo)

setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/WUS_Lowflow")

q_file<-read.csv("WRTDS_Q_Input_08122024.csv")

q_file$site_no<-as.character(q_file$site_no)

q_file$site_no<-ifelse(nchar(q_file$site_no) < 8, paste0("0", q_file$site_no), q_file$site_no)

q_file$Date<-as.Date(q_file$Date)

q_file<-q_file[!(q_file$site_no=="15049900"),]

sites<-unique(q_file$site_no)

missing_q<-list()

for(i in 1:length(sites)){
  
  print(i)
  
  one_q<-subset(q_file, q_file$site_no==sites[i])
  
  complete_dates<-seq(min(one_q$Date), max((one_q$Date)), by=1)
  
  missing_q[[i]]<-ifelse(length(complete_dates)==nrow(one_q), "no", "yes")
  
}

sites_missing_q<-data.frame(unlist(missing_q))
sites_missing_q$site_no<-sites

sites_missing_q<-subset(sites_missing_q, sites_missing_q$unlist.missing_q.=="yes")

filled_q<-list()

interp_length<-list()

pdf("Interpolated_Q.pdf")

for(i in 1:nrow(sites_missing_q)){
  
  print(i)
  
  one_q<-subset(q_file, q_file$site_no==sites_missing_q[i,2])
  
  complete_dates<-data.frame(seq(min(one_q$Date), max((one_q$Date)), by=1))
  colnames(complete_dates)<-"Date"
  
  all_q_comp_date<-merge(complete_dates, one_q[,2:4], by="Date", all.x=TRUE)
  all_q_comp_date$site_no<-sites_missing_q[i,2]
  
  if(i==24){
    
    all_q_comp_date<-all_q_comp_date[-nrow(all_q_comp_date),]
    
  }else(
    
    all_q_comp_date<-all_q_comp_date
    
  )
  
  all_q_comp_date <- all_q_comp_date %>%
    mutate(filled_q=na.approx(X_00060_00003)) %>%
    mutate(interp_flag = ifelse(is.na(X_00060_00003), "yes", "no"))
  
  interp_length[[i]]<-all_q_comp_date %>%
    summarise(max=max(rle(interp_flag)$lengths[rle(interp_flag)$values=="yes"])) %>%
    filter(max!="-Inf")
  
  p1<-ggplot(all_q_comp_date, aes(Date, filled_q, col=interp_flag))+geom_point()+
    scale_color_manual(values=c("yes"="red", "no"="black"))+ggtitle(sites_missing_q[i,2])
  
  print(p1)
  
  filled_q[[i]]<-all_q_comp_date
  
}

dev.off()


interp_length_df<-do.call(bind_rows, interp_length)
interp_length_df$site_no<-sites_missing_q$site_no

interp_long_df<-subset(interp_length_df, interp_length_df$max > 49)

filled_q_df<-do.call(bind_rows, filled_q)

filled_q_df_long<-subset(filled_q_df, filled_q_df$site_no %in% interp_long_df$site_no)

ggplot(filled_q_df_long, aes(Date, filled_q, col=interp_flag))+geom_point()+facet_wrap(~site_no, scales="free")+
  scale_color_manual(values=c("yes"="red", "no"="black"))

cond<-read.csv("WRTDS_Cond_Input_08122024.csv")

cond$site_no<-as.character(cond$site_no)

cond$site_no<-ifelse(nchar(cond$site_no) < 8, paste0("0", cond$site_no), cond$site_no)

cond_missing_q<-subset(cond, cond$site_no %in% interp_long_df$site_no)

colnames(cond_missing_q)<-c("X", "site_no", "Date", "cond")

all_cond_q_missing<-merge(filled_q_df_long, cond_missing_q, all.x = T, by=c("Date", "site_no"))

ggplot(all_cond_q_missing)+geom_point(mapping=aes(Date, filled_q, col=interp_flag))+
  geom_point(mapping = aes(Date, cond), col="blue")+
  facet_wrap(~site_no, scales="free")+
  scale_color_manual(values=c("yes"="red", "no"="black"))

remove_q_before_sites<-data.frame(interp_long_df$site_no)

remove_from_analysis<-c("06140500", "06154100", "06205000", "07094500", "13210050")

remove_q_before_sites<-setdiff(remove_q_before_sites$interp_long_df.site_no, remove_from_analysis)

cond_revised<-cond[!(cond$site_no %in% remove_from_analysis),]
cond_revised<-cond_revised[!(cond_revised$site_no=="15049900"),]

filled_q_revised<-filled_q_df[!(filled_q_df$site_no %in% remove_from_analysis),]

q_remove_missing<-q_file[!(q_file$site_no %in% sites_missing_q$site_no),]

filled_q_revised_fine<-filled_q_revised[!(filled_q_revised$site_no %in% remove_q_before_sites),]

cropped_q_list<-list()

for (i in 1:length(remove_q_before_sites)) {
  
  filled_q_revised_one<-subset(filled_q_revised, filled_q_revised$site_no==remove_q_before_sites[i])
  
  cond_one<-subset(cond_revised, cond_revised$site_no==remove_q_before_sites[i])
  
  min_cond<-min(cond_one$sample_dt)
  
  filled_q_revised_one_cropped<-subset(filled_q_revised_one, filled_q_revised_one$Date > min_cond)
  
  cropped_q_list[[i]]<-filled_q_revised_one_cropped
  
}

cropped_q_df<-do.call(bind_rows, cropped_q_list)

all_interpolated_q<-bind_rows(filled_q_revised_fine, cropped_q_df)

write.csv(all_interpolated_q, "Interpolated_Q_WRL.csv")

all_interpolated_q<-all_interpolated_q[,c("Date", "site_no", "filled_q")]

colnames(all_interpolated_q)<-c("Date", "site_no", "discharge")

q_remove_missing<-q_remove_missing[,-1]

colnames(q_remove_missing)<-c("site_no", "Date", "discharge")

q_revised<-bind_rows(all_interpolated_q, q_remove_missing)

write.csv(cond_revised, "WRTDS_Cond_Input_Final_08132024.csv")

write.csv(q_revised, "WRTDS_Q_Input_Final_08132024.csv")
