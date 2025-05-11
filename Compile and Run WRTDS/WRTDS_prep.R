#import data 
setwd("C:/Users/gregorre/Box/Hydrology_Lab/Projects/Low Flow/Data")
filtered_cond_data <- read.csv("filtered_cond_data.csv")
#switching site_no to chr
#rename columns
names(filtered_cond_data)[names(filtered_cond_data) == "x"] <- "site_no"
filtered_cond_data$site_no <- ifelse((str_length(filtered_cond_data$site_no) < 8), paste0("0", filtered_cond_data$site_no), filtered_cond_data$site_no)

#packages 
# devtools::install_github("USGS-R/EflowStats@v5.0.3")
# install.packages("ggplot2")
library(dataRetrieval)
library(EGRET)
library(EflowStats)
library(lubridate)
library(stringr)
library(devtools)
library(dplyr)
library(ggplot2)

#make list of unique site no (unique()) RUN THIS
site_numbers <- unique(filtered_cond_data$site_no)

#### identify which sites have > 2 breaks and remove them  ####
list_breaks <- list()
for (i in 1:length(site_numbers)) {
  df <- subset(filtered_cond_data, filtered_cond_data$site_no == site_numbers[i]) 
  if (nrow(df) == 1) {
    
    list_breaks[[i]]<-"yes"
    
  }else{
    
    df$sample_dt <- as.Date(df$sample_dt)
    diff2 <- difftime(df$sample_dt, lag(df$sample_dt), units = "days")
    
    BPdf <- cbind(df, diff2)
    
    #add a column on the breakpoints of data clusters (two years)
    BPdf$BP<-ifelse(BPdf$diff > 730, "BP", "ok")
    
    BPdf[1,"BP"]<-"BP"
    
    BPdf[nrow(BPdf),"BP"]<-"BP"
    
    n_obs <- as.data.frame(which(BPdf$BP == "BP"))
    colnames(n_obs)<-"row_num"
    diff_num2<-as.data.frame(diff(n_obs$row_num, k=1))
    colnames(diff_num2) <- "n_obs"
    dates <- BPdf[n_obs$row_num,"sample_dt"]
    diff_num2$min_date <- dates[1:length(dates) - 1]
    diff_num2$max_date <- dates[2:length(dates)]
    diff_num_long2 <- subset(diff_num2, diff_num2$n_obs >= 60) 
    list_breaks[[i]] <- ifelse(nrow(diff_num_long2) > 2, "yes", "no")
    
  }
  
  print(i)
}

breaks_df <- unlist(list_breaks)
two_breaks<- which(breaks_df == "yes")
site_numbers_1B <-site_numbers[-two_breaks]

#identifying sites with 1 break
list_breaks <- list()
for (i in 1:length(site_numbers_1B)) {
  df <- subset(filtered_cond_data, filtered_cond_data$site_no == site_numbers_1B[i])
  df$sample_dt <- as.Date(df$sample_dt)
  diff2 <- difftime(df$sample_dt, lag(df$sample_dt), units = "days")
  
  BPdf <- cbind(df, diff2)
  
  #add a column on the breakpoints of data clusters (two years)
  BPdf$BP<-ifelse(BPdf$diff > 730, "BP", "ok")
  
  BPdf[1,"BP"]<-"BP"
  
  BPdf[nrow(BPdf),"BP"]<-"BP"
  
  n_obs <- as.data.frame(which(BPdf$BP == "BP"))
  colnames(n_obs)<-"row_num"
  diff_num2<-as.data.frame(diff(n_obs$row_num, k=1))
  colnames(diff_num2) <- "n_obs"
  dates <- BPdf[n_obs$row_num,"sample_dt"]
  diff_num2$min_date <- dates[1:length(dates) - 1]
  diff_num2$max_date <- dates[2:length(dates)]
  diff_num_long2 <- subset(diff_num2, diff_num2$n_obs >= 60) 
  list_breaks[[i]] <- ifelse(nrow(diff_num_long2) > 1, "yes", "no")
  print(i)
}

breaks_df <- unlist(list_breaks)
one_breaks<- which(breaks_df == "yes")
site_numbers_no_break <-site_numbers_1B[-one_breaks]
site_numbers_one_break <-site_numbers_1B[one_breaks]
write.csv(site_numbers_one_break, "C:/Users/gregorre/Box/Hydrology_Lab/Projects/Low Flow/Data/site_numbers_one_break.csv")
write.csv(site_numbers_no_break, "C:/Users/gregorre/Box/Hydrology_Lab/Projects/Low Flow/Data/site_numbers_no_break.csv")
#plotting one_breaks
site_numbers_one_break_data <- ifelse((str_length(site_numbers_one_break) < 8), paste0("0", site_numbers_one_break), 
                                      site_numbers_one_break)
setwd("C:/Users/gregorre/Box/Hydrology_Lab/Projects/Low Flow/Graphs")


#subset good data with one break
OB_good <- subset(filtered_cond_data, filtered_cond_data$site_no %in% site_numbers_one_break)
OB_good$sample_dt <- as.Date(OB_good$sample_dt, "%Y-%m-%d")

#graphing loop
OB_site_vec <- list()
pdf("one_breaks_graphs.pdf")
for(i in unique(OB_good$site_no)) {
  p2 <- ggplot() + 
    geom_point(subset(OB_good, site_no == i), mapping = aes(sample_dt, result_va), col = "darkred") +
    theme_bw() +
    labs(x = "Date", y = "Conductivity", title = paste(i))
  print(p2)
}
dev.off()



#subset cond data to each site
Qshort_list <- list()
condshort_list <- list()
site_numbers_no_break <- ifelse((str_length(site_numbers_no_break) < 8), paste0("0", site_numbers_no_break), 
                                site_numbers_no_break)
                                
for(i in 1:length(site_numbers_no_break)) {
  df <- subset(filtered_cond_data, filtered_cond_data$site_no == site_numbers_no_break[i])
  df$sample_dt <- as.Date(df$sample_dt)
  if(length(unique(month(df$sample_dt))) < 12) next
  Q <-readNWISdv(siteNumbers = site_numbers_no_break[i], parameterCd = "00060")
  
  #find minimum date of Si file
  cond_min<-min(df$sample_dt)
  
  #convert to days since 1970
  cond_min_julian<-as.numeric(cond_min)
  
  #subtract 10 years from Si min to get Q min
  Qmin<-(cond_min_julian-10*365.25)-1
  
  #subset Q file associated with Si file starting 10 years before Si file starts 
  #and ending when the Q file ends
  #extra space of Q file on ends of Si help moving flow weighted average for flux perform better
  Qshort<-Q[Q$Date > Qmin,]
  Qshort_list[[i]] <- Qshort
  # #convert to day of water year
  # MinDay<-as.numeric(hydro.day.new(Simin))
  # 
  # #find maximum date of Si file
  # Simax<-max(Si$Date)
  # #convert to day of water year
  # MaxDay<-as.numeric(hydro.day.new(Simax))
  # 
  # #find difference between beginning of next water year and end of Si file
  # si_water_year_diff<-365-MaxDay
  
  #subset Q file associated with Si file starting at beginning of water year of start of Si file and ending at end
  #of water year of last Si file date
  # Qshort<-Q[Q$Date > (Simin - MinDay) & Q$Date < (Simax + si_water_year_diff),]
  
  #extract date and discharge columns
  # Qshort<-Qshort %>%
  #   dplyr::select(Date, Qcms)
  
  # #write to new folder
  # setwd("/Users/reecegregory/Library/CloudStorage/Box-Box/Reece Gregory/Low Flow")
  # 
  # #write csv of discharge file
  # write.csv(Qshort, paste0(site_numbers[i], "_Q_WRTDS.csv"), row.names = FALSE)
  # 
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
  condShort<-df[df$sample_dt > (Qmin) & df$sample_dt < (Qmax),]
  condshort_list[[i]] <- condShort
  # write.csv(dfShort, paste0(site_numbers[i], "_cond_WRTDS.csv"), row.names = FALSE)
  print(i)
}

WRTDS_Q_Input <- do.call(bind_rows, Qshort_list)
WRTDS_Cond_Input <- do.call(bind_rows, condshort_list)

#set working directory to somewhere other than box because the file size it too big
setwd("C:/Users/gregorre/Desktop/Low_Flow_Data")
write.csv(WRTDS_Q_Input, "WRTDS_Q_Input.csv")
write.csv(WRTDS_Cond_Input, "WRTDS_Cond_Input.csv")


####find break points for all filtered data####
list_breaks <- list()
for (i in 1:length(site_numbers)) {
df <- subset(filtered_cond_data, filtered_cond_data$site_no == site_numbers[i])
df$sample_dt <- as.Date(df$sample_dt)
diff2 <- difftime(df$sample_dt, lag(df$sample_dt), units = "days")

BPdf <- cbind(df, diff2)

#add a column on the breakpoints of data clusters (two years)
BPdf$BP<-ifelse(BPdf$diff > 730, "BP", "ok")

BPdf[1,"BP"]<-"BP"

BPdf[nrow(BPdf),"BP"]<-"BP"

n_obs <- as.data.frame(which(BPdf$BP == "BP"))
colnames(n_obs)<-"row_num"
diff_num2<-as.data.frame(diff(n_obs$row_num, k=1))
colnames(diff_num2) <- "n_obs"
dates <- BPdf[n_obs$row_num,"sample_dt"]
diff_num2$min_date <- dates[1:length(dates) - 1]
diff_num2$max_date <- dates[2:length(dates)]
diff_num_long2 <- subset(diff_num2, diff_num2$n_obs >= 60) 
list_breaks[[i]] <- ifelse(nrow(diff_num_long2) > 2, "yes", "no")
print(i)
}

breaks_df <- unlist(list_breaks)
two_breaks<- which(breaks_df == "yes")
site_numbers_1B <-site_numbers[-two_breaks]

#### 