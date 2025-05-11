require(ggpubr)
require(dplyr)
require(data.table)
require(reshape2)
require(EflowStats)
require(zoo)
require(ggplot2)

setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow")

BF_files_path<-"/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/HydroSepData/ContBaseflowFiles/"

groundwater_EM<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/HydroSepData/groundwater_EMs.csv")

sites<-unique(groundwater_EM$site_no)
sites_char<-ifelse(nchar(sites) < 8, paste0("0", sites), sites)

skip_these<-c(32)

slope_list<-list()
rbfi_list<-list()

#pdf("RecessionCoefficent.pdf", width = 15, height = 7)

for (i in 1:length(sites_char)) {
  
  print(i)
  
  if(i %in% skip_these) next
  
  BF_file<-list.files(path = BF_files_path, pattern = sites_char[i])
  
  df<-read.csv(paste0(BF_files_path, BF_file))
  df$Date<-as.Date(df$Date)
  
  df <- df %>%
    arrange(Date) %>% #order by date
    mutate(dQ = Q - lag(Q),  # Change in discharge
           change_dQ = Q/lag(Q), #find change in discharge relative to previous day
           dQ_dt = dQ / as.numeric(Date - lag(Date))) %>%  # Daily rate of change
    filter(!is.na(dQ_dt)) %>% # Remove NA values (first row)
    filter(!change_dQ < 0.7) #remove anything where the relative difference is < 0.7, essentially where the change in discharge is small
  
  # Calculate the recession slope (-dQ/dt)
  recession_data <- df %>%
    filter(dQ < 0) %>%  # Keep only recession periods
    mutate(recession_slope = -dQ_dt)  # Make it positive for the slope
  
  # Fit a linear model to the recession data
  lm_model <- lm(recession_slope ~ Q, data = recession_data)
  model_summary <- summary(lm_model)
  
  # Extract statistics
  slope <- coef(lm_model)[2]
  p_value <- model_summary$coefficients[2, 4]
  r_squared <- model_summary$r.squared
  
  # Create a recession-slope plot
  p2<-ggplot(recession_data, aes(x = log(Q), y = log(recession_slope))) +
    geom_point() +
    stat_smooth(method = "lm", se = F, color = "blue", linetype = "dashed") +
    labs(title = sites_char[i],
         x = "Discharge",
         y = "Recession Slope (-dQ/dt)") +
    theme_classic() +
    #labs(subtitle = paste("Slope:", round(slope, 3), "\n",
     #                     "p-value:", format.pval(p_value, digits = 3, format = "f"), "\n",
      #                    "R²:", round(r_squared, 3)))+
    theme(text = element_text(size=25))


  p1<-ggplot() +
    geom_line(df, mapping=aes(x = Date, y = Q)) +
    geom_point(recession_data, mapping=aes(x=Date, y=Q), col="blue", size=0.5)+
    labs(title = sites_char[i], x = "Date", y = "Discharge") +
    theme_classic()+theme(text = element_text(size=20))

  p3<-ggarrange(p1, p2, widths = c(0.65, 0.35))

  pdf("RecessionCurve_ForPrez.pdf", width = 12, height=5)
  
  print(p3)

  dev.off()
  
  slope_list[[i]]<-data.frame(slope, sites_char[i])
  
  
  # Calculate daily changes in discharge
  df <- df %>%
    arrange(Date) %>%
    mutate(dQ = Q - lag(Q),  # Daily change in discharge
           abs_dQ = abs(dQ)) %>%              # Absolute change in discharge
    filter(!is.na(abs_dQ))  # Remove NA values (first row)
  
  # Calculate the total discharge over the period
  total_discharge <- sum(df$Q)
  
  # Calculate the Richards-Baker Flashiness Index
  RBFI <- sum(df$abs_dQ) / total_discharge
  
  rbfi_list[[i]]<-data.frame(RBFI, sites_char[i])
  
  
}
  

#dev.off()


recession_curve_slope<-do.call(bind_rows, slope_list)
colnames(recession_curve_slope)<-c("recession_curve_slope", "site_no")

RBFI_df<-do.call(bind_rows, rbfi_list)
colnames(RBFI_df)<-c("rbfi", "site_no")

write.csv(recession_curve_slope, "Recession_Curve.csv")

write.csv(RBFI_df, "RBFI.csv")

####Dynamic Storage####
setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/Data/WRTDS_Inputs")

Q<-read.csv("WRTDS_Q_Input_Final_08132024.csv")
Q$dates<-as.Date(Q$Date)
info<-read.csv("WRTDS_site_info.csv")

Q<-left_join(Q, info[,c(3,31)])
Q$drain_area_va<-Q$drain_area_va*2.59e+6
Q$Q_mm<-(Q$discharge*86400*1000)/Q$drain_area_va

setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/ERA5_Data/CompiledData")

ET<-read.csv("EVAP_AllSites.csv")
ET<-ET[,-c(1,6)]
ET_melt<-melt(ET, id.vars="dates")
ET_melt$site_no<-gsub('^.|.$', '', ET_melt$variable)
ET_melt$site_no<-substr(ET_melt$site_no, 1, nchar(ET_melt$site_no)-1)
ET_melt$site_no<-as.numeric(ET_melt$site_no)
ET_melt$dates<-as.Date(ET_melt$dates)

precip<-read.csv("Precip_AllSites.csv")
precip<-precip[,-c(1,6)]
precip_melt<-melt(precip, id.vars="dates")
precip_melt$site_no<-gsub('^.|.$', '', precip_melt$variable)
precip_melt$site_no<-substr(precip_melt$site_no, 1, nchar(precip_melt$site_no)-1)
precip_melt$site_no<-as.numeric(precip_melt$site_no)
precip_melt$dates<-as.Date(precip_melt$dates)

all_vars<-left_join(Q[,c(3,6,7)], ET_melt[,c(1,3,4)], by=c("site_no", "dates"))
colnames(all_vars)[4]<-"ET"

all_vars<-left_join(all_vars, precip_melt[,c(1,3,4)], by=c("site_no", "dates"))
colnames(all_vars)[5]<-"precip"

all_vars$waterYear<-get_waterYear(all_vars$dates)

all_vars <- all_vars %>%
  dplyr::group_by(waterYear, site_no) %>%
  dplyr::filter(n_distinct(dates) == 365)

all_vars<-all_vars %>%
  dplyr::group_by(site_no) %>%
  dplyr::mutate(Q_smoothed = rollmean(Q_mm, k = 7, fill = NA, align = "right"))

all_vars<-all_vars %>%
  dplyr::group_by(waterYear, site_no) %>%
  dplyr::mutate(hydro_flux=cumsum(Q_mm), hydro_flux_prop=hydro_flux/max(hydro_flux))

all_vars <- all_vars %>%
  dplyr::group_by(waterYear, site_no) %>%
  dplyr::filter(hydro_flux_prop > .40) %>%
  dplyr::mutate(tag = if_else(Q_smoothed == max(Q_smoothed), "peak", NA_character_))

all_vars_filtered <- all_vars %>%
  dplyr::group_by(waterYear, site_no) %>%
  dplyr::mutate(peak_occurred = cumsum(!is.na(tag)) > 0) %>%  # Create a logical column to indicate if peak has occurred
  dplyr::filter(peak_occurred)

all_vars_filtered<-all_vars_filtered %>%
  dplyr::group_by(waterYear, site_no) %>%
  dplyr::mutate(dS=precip*1000-Q_mm-ET*-1000) %>%
  dplyr::mutate(dS_sum=cumsum(dS)) %>%
  dplyr::mutate(WY_day = get_waterYearDay(dates))

sites<-unique(all_vars_filtered$site_no)

pdf("DS_drawdown.pdf")

for (i in 1:length(sites)) {
  
  one_site<-all_vars_filtered %>%
    filter(site_no==sites[i])
  
  p1<-ggplot(one_site, aes(WY_day, dS_sum, col=waterYear))+geom_line(aes(group=waterYear))+
    theme_classic()+theme(text = element_text(size = 20))+
    scale_color_gradientn(colors = c("red", "purple", "blue"))+
    ggtitle(sites[i])+labs(x="Water Year Day", y="DS drawdown (mm)", col="Water Year")
  
  print(p1)
  
}

dev.off()

ds_df<-all_vars_filtered[,c(1,3,6,12,13,14)]

write.csv(ds_df, "DS_drawdown.csv")


