require(dplyr)
require(tidyr)
setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/LULC")

five_year<-read.csv("GLC_fiveyear_data.csv")
annual<-read.csv("GLC_annual_data.csv")

all_lulc<-bind_rows(five_year, annual)

all_lulc<-all_lulc %>%
  dplyr::group_by(Year, Basin_ID) %>%
  dplyr::mutate(total_area=sum(Area_m2))

all_lulc<-all_lulc %>%
  dplyr::mutate(prop_area=Area_m2/total_area)


all_lulc_wide<-pivot_wider(data = all_lulc, id_cols = c("Basin_ID","Year"), names_from = "LandClass",
                           values_from = "prop_area")

write.csv(all_lulc_wide, "LULC_AllSites.csv")

# sites<-unique(all_lulc$Basin_ID)
# 
# pdf("LULC_change_time.pdf")
# 
# for (i in 1:length(sites)) {
#   
#   one_site<-all_lulc %>%
#     filter(Basin_ID==sites[i])
#   
#   ggplot(one_site, aes(Year, prop_area, fill=LandClass))+geom_bar(stat = "identity", position = "stack")+
#     theme_bw()+theme(text = element_text(size = 20))+ggtitle(paste(sites[i]))
#   
#   
# }