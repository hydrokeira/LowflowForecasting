setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/Data/WRTDS_Inputs")

cond<-read.csv("WRTDS_Cond_Input_Final_08132024.csv")
cond$wy<-get_waterYear(as.Date(cond$sample_dt))

runoff_EM<-cond %>%
  group_by(site_no) %>%
  summarise(runoff=quantile(result_va, 0.01))

gw_EM<-cond %>%
  group_by(site_no) %>%
  summarise(runoff=quantile(result_va, 0.99))

setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/HydroSepData")

write.csv(runoff_EM, "runoff_EMs.csv")
write.csv(gw_EM, "groundwater_EMs.csv")
