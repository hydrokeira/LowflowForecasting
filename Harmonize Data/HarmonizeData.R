require(kgc)
require(dplyr)
require(data.table)

##compile all data

setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/Data/WRTDS_Inputs")

info<-read.csv("WRTDS_site_info.csv")

#format data to pull using KGC
coord_data<-data.frame(info, rndCoord.lon=RoundCoordinates(info$dec_long_va), 
                       rndCoord.lat=RoundCoordinates(info$dec_lat_va))

#get KG for each site
data <- data.frame(coord_data,ClimateZ=LookupCZ(coord_data))

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

KG_name<-read.csv("KG_Clim_Name.csv")

info<-merge(data, KG_name, by="ClimateZ")

#read in Q5/RLWG
setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow")

stream_met<-read.csv("Site_Stats_Q5_baseflow.csv")
stream_met<-stream_met[,c("WY", "site", "mean_bf_prop","Q5")]
colnames(stream_met)<-c("waterYear", "site_no", "bf_prop", "Q5")

recession_curve<-read.csv("Recession_Curve.csv")

#RBFI<-read.csv("RBFI.csv")

stream_met_all<-left_join(stream_met, info[,c(1,4,5,9,10,32,48)], by="site_no")

stream_met_all<-left_join(stream_met_all, recession_curve[,-1], by="site_no")

#stream_met_all<-left_join(stream_met_all, RBFI[,-1], by="site_no")

#read in climate data
setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/ERA5_Data/CompiledData")

evap<-read.csv("Annual_Evaportation.csv")
precip<-read.csv("Annual_Precipitation.csv")
swe<-read.csv("Annual_SWE.csv")
temp<-read.csv("Annual_Temperature.csv")

dat_all<-left_join(stream_met_all, evap[,-1], by=c("waterYear", "site_no"))

dat_all<-left_join(dat_all, precip[,-1], by=c("waterYear", "site_no"))

dat_all<-left_join(dat_all, temp[,-1], by=c("waterYear", "site_no"))

dat_all<-left_join(dat_all, swe[,-1], by=c("waterYear", "site_no"))

dat_all %>%
  dplyr::group_by(Name) %>%
  dplyr::summarize(unique_site_count = n_distinct(site_no)) 

##add in geo, land use, and gages2, elevation, basin slope
elevation<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/Basin_Elevation.csv")
elevation_melt<-melt(elevation)
elevation_melt$site_no<-gsub('^.|.$', '', elevation_melt$variable)
elevation_melt$site_no<-substr(elevation_melt$site_no, 1, nchar(elevation_melt$site_no)-1)
colnames(elevation_melt)[3]<-"mean_elevation"
elevation_melt$site_no<-as.numeric(elevation_melt$site_no)

slope<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/Basin_Slope.csv")
slope_melt<-melt(slope)
slope_melt$site_no<-gsub('^.|.$', '', slope_melt$variable)
slope_melt$site_no<-substr(slope_melt$site_no, 1, nchar(slope_melt$site_no)-1)
colnames(slope_melt)[3]<-"mean_slope"
slope_melt$site_no<-as.numeric(slope_melt$site_no)

dat_all<-left_join(dat_all, elevation_melt[,c(3,4)], by="site_no")
dat_all<-left_join(dat_all, slope_melt[,c(3,4)], by="site_no")

dat_all<-dat_all %>%
  dplyr::group_by(site_no) %>%
  dplyr::mutate(mean_bf = mean(bf_prop))

gages_dat<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/GAGES2_hydromod_data.csv")
gages_dat<-gages_dat %>%
  dplyr::mutate(across(2:7, ~ na_if(.x, -999)))

dat_all<-left_join(dat_all, gages_dat, by="site_no")

geo_data<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/Geo/Geo_Generalized_AllSites.csv")
geo_data$sum<-as.numeric(rowSums(geo_data[3:32], na.rm = T))
geo_data<-subset(geo_data, geo_data$sum > 0.99)

dat_all<-left_join(dat_all, geo_data[2:32], by="site_no")

lulc<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/LULC/LULC_AllSites.csv")

lulc<-left_join(dat_all[c(1,2)], lulc[2:29], by=c("site_no", "waterYear"))

lulc_filled <- lulc %>%
  dplyr::group_by(site_no) %>%
  dplyr::mutate(across(2:27, ~ ifelse(is.na(.), zoo::na.locf(., fromLast = TRUE), .)))

dat_all<-left_join(dat_all, lulc_filled, by=c("site_no", "waterYear"))

setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow")

write.csv(dat_all, "AllDataHarmonized_Simple_01252025.csv")



