#testing difftime to create a new column in the dataframe with the difference between sequential rows
setwd("C:/Users/gregorre/Box/Hydrology_Lab/Projects/Low Flow/Data")
#load in packages
library(dataRetrieval)
library(EGRET)
library(dplyr)
library(ggplot2)
library(stringr)
require(lubridate)
require(EflowStats)

# #### Example Site 1 ####
# 
# #pull example site and make df with just dates for ease
# ex_site <- readNWISqw(siteNumbers = "06720500", parameterCd = c("00094", "00095"))
# 
# ex_site <- readNWISqw(siteNumbers = "09365000", parameterCd = c("00094", "00095"))
# #create diff column in the dataframe
# diff <- difftime(ex_site$sample_dt, lag(ex_site$sample_dt), units = "days")
# 
# ex_site <- cbind(ex_site, diff)
# 
# # summary(as.numeric(ex_site_dates$Diff))
# 
# #remove duplicate sample dates
# ex_site_nodups<-subset(ex_site, ex_site$diff > 0)
# 
# #add water year to data frame
# ex_site_nodups$WY<-get_waterYear(ex_site_nodups$sample_dt)
# 
# #add a column on the breakpoints of data clusters (two years)
# ex_site_nodups$BP<-ifelse(ex_site_nodups$diff > 730, "BP", "ok")
# 
# ex_site_nodups[1,"BP"]<-"BP"
# 
# ex_site_nodups[nrow(ex_site_nodups),"BP"]<-"BP"
# 
# n_obs <- as.data.frame(which(ex_site_nodups$BP == "BP"))
# colnames(n_obs)<-"row_num"
# diff_num<-as.data.frame(diff(n_obs$row_num, k=1))
# colnames(diff_num) <- "n_obs"
# dates <- ex_site_nodups[n_obs$row_num,"sample_dt"]
# diff_num$min_date <- dates[1:length(dates) - 1]
# diff_num$max_date <- dates[2:length(dates)]
# diff_num_long <- subset(diff_num, diff_num$n_obs >= 60)
#loop to pull data from min and max dates of clusters and put them in a list 
good_data <- list()
for(i in 1:nrow(diff_num_long)) {
  good_data[[i]] <- subset(ex_site_nodups, ex_site_nodups$sample_dt >= diff_num[i,2] & ex_site_nodups$sample_dt < diff_num[i, 3])
}

#finding number of samples per year
# year_count<-ex_site_nodups %>%
#   dplyr::group_by(WY) %>%
#   count()


# BP<-subset(ex_site_nodups, ex_site_nodups$diff > 730) 
# BP[nrow(BP) + 1,] <- tail(ex_site_nodups, n=1)
# 
# BP<-BP[!duplicated(BP$sample_dt),]
# 
# 
# i=1
# 
# BP_nrow<-list()
# min_date<-list()
# max_date<-list()
# 
# for (i in 1:nrow(BP)) {
#   
#   df<-subset(ex_site_nodups, ex_site_nodups$sample_dt < BP[i,3] & ex_site_nodups$sample_dt > BP[i-1,3])
#   min_date[[i]]<-min(df$sample_dt)
#   max_date[[i]]<-max(df$sample_dt)
#   BP_nrow[[i]]<-nrow(df)
#   
# }
# 
# BP_nrow<-unlist(BP_nrow)
# min_date<-unlist(min_date)
# max_date<-unlist(max_date)
# 
# BP$sample_num<-BP_nrow
# BP$min_date<-min_date
# BP$max_date<-max_date
# 
# BP_cut<-subset(BP, BP$samples > 59)
# 
# min_date<-min($sample_dt)

ggplot(ex_site_nodups, aes(sample_dt, result_va))+geom_point()+theme_classic()

ggplot2::ggplot(ex_site, aes(x=seq(1,nrow(ex_site), 1), y=diff))+geom_line()+geom_hline(yintercept = 365*2)

#### Looping for All Good Sites ####

getwd()

#read in good sites
good_sites <- read.csv("site_codes_Q_good.csv")
colnames(good_sites) <- c("X", "gauge_code")
good_sites$gauge_code2 <- ifelse((str_length(good_sites$gauge_code) < 8), paste0("0", good_sites$gauge_code), good_sites$gauge_code)
#good_sites[715, 4] <- "094196783"
good_data_list <- list()
#we removed 1148, 1149, 1152, 1158 (Wyoming)
#good_sites <- good_sites[-c(1148, 1149, 1152, 1158),]
#good_sites <- good_sites[-c(758),]
good_loops <- for(j in 1:nrow(good_sites)) {
 
    print(j)
  
    ex_site <- readNWISqw(siteNumbers = good_sites[j,3], parameterCd = c("00094", "00095")) 
    
    ex_site$sample_dt <- as.Date(ex_site$sample_dt, "%Y-%m-%d") 
    
    ex_site <- ex_site[complete.cases(ex_site$sample_dt),]
    
    diff <- difftime(ex_site$sample_dt, lag(ex_site$sample_dt), units = "days")
    
    ex_site <- cbind(ex_site, diff)
    
    #remove duplicate sample dates
    ex_site_nodups<-subset(ex_site, ex_site$diff > 0)
    
    #add water year to data frame
    ex_site_nodups$WY<-get_waterYear(ex_site_nodups$sample_dt)
    
    #add a column on the breakpoints of data clusters (two years)
    ex_site_nodups$BP<-ifelse(ex_site_nodups$diff > 730, "BP", "ok")
    
    ex_site_nodups[1,"BP"]<-"BP"
    
    ex_site_nodups[nrow(ex_site_nodups),"BP"]<-"BP"
    
    #pull out rows where there is a BP
    n_obs <- as.data.frame(which(ex_site_nodups$BP == "BP"))
    colnames(n_obs)<-"row_num"
    #get number of observations between each break point
    diff_num<-as.data.frame(diff(n_obs$row_num, k=1))
    colnames(diff_num) <- "n_obs"
    
    dates <- ex_site_nodups[n_obs$row_num,"sample_dt"]
    diff_num$min_date <- dates[1:length(dates) - 1]
    diff_num$max_date <- dates[2:length(dates)]
    diff_num_long <- subset(diff_num, diff_num$n_obs >= 60)
    
    if(nrow(diff_num_long) == 0) next
    
    #loop to pull data from min and max dates of clusters and put them in a list 
    good_data <- list()
    for(i in 1:nrow(diff_num_long)) {
      good_data[[i]] <- subset(ex_site_nodups, ex_site_nodups$sample_dt >= diff_num_long[i,2] & ex_site_nodups$sample_dt < diff_num_long[i, 3])
    }
    
    good_data_df <- do.call(what = bind_rows, good_data)
    
    good_data_list[[j]] <- good_data_df
   
}    

#Remove NULL in good_data_list 
is.null(good_data_list)
null_data<-which(sapply(good_data_list, is.null))

no_null_sites<-good_sites[-null_data,]

master_dataframe <- do.call(bind_rows, good_data_list)
length(unique(master_dataframe$site_no))

#saving master data 
write.csv(master_dataframe, "filtered_cond_data.csv")

