setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow/Geo")

geo<-read.csv("watershed_geologic_proportions9.csv")

geo_sum_generalized<-geo %>%
  dplyr::group_by(over_site_no, GENERALIZED_LITH) %>%
  dplyr::summarise(prop_sum=sum(proportion, na.rm = T))

geo_sum_generalized_cast<-dcast(geo_sum_generalized, formula = over_site_no~GENERALIZED_LITH)
colnames(geo_sum_generalized_cast)[1]<-"site_no"

write.csv(geo_sum_generalized_cast, "Geo_Generalized_AllSites.csv")

