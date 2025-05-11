setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow")

shap_df<-read.csv("Shap_values_Q5norm_WRL_RF_V9.csv")

shap_df$unique<-paste0(shap_df$site_no, "_", shap_df$waterYear)

dat<-read.csv("AllDataHarmonized_Simple_01252025.csv")

dat<-dat %>%
  dplyr::filter(!site_no %in% c(9153290, 13075983))

dat$Q5_norm<-dat$Q5/dat$drain_area_va

dat$unique<-paste0(dat$site_no, "_", dat$waterYear)

dat<-subset(dat, dat$unique %in% unique(shap_df$unique))

dat_stats<-dat %>%
  group_by(site_no) %>%
  summarise(DA=mean(drain_area_va), num_years=n_distinct(waterYear), min_year=min(waterYear), max_year=max(waterYear))

dat_Name<-dat[!duplicated(dat$site_no),]

pdf("DA_Count.pdf", width = 4, height = 4)

ggplot(dat_stats, aes(log(DA)))+geom_histogram(fill="black")+theme_classic()+
  labs(x="Log(Drainage Area (km2))", y="Number of Sites")+theme(text = element_text(size = 15))

dev.off()

p2<-ggplot(dat_stats, aes(num_years))+geom_histogram(fill="black")+theme_classic()+
  labs(x="Period of Record (years)", y="Number of Sites")+theme(text = element_text(size = 15))

p3<-ggplot(dat_stats, aes(min_year))+geom_histogram(fill="black")+theme_classic()+
  labs(x="First Year of Data", y="Number of Sites")+theme(text = element_text(size = 15))

p4<-ggplot(dat_stats, aes(max_year))+geom_histogram(fill="black")+theme_classic()+
  labs(x="Last Year of Data", y="Number of Sites")+theme(text = element_text(size = 15))

pdf("Site_Stats_POR.pdf", width = 9, height = 3)

ggarrange(p2, p3, p4, nrow = 1)

dev.off()

color_pal<-c("#E69F00","#56B4E9","#009E73","#0072B2","#D55E00","#CC79A7")

pdf("CZ_Count.pdf", width = 6, height = 4)

ggplot(dat_Name, aes(Name))+geom_bar(stat = "count", aes(fill=Name))+theme_classic()+
  labs(x="", y="Number of Sites")+
  theme(text = element_text(size = 15), legend.position = "null", axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = color_pal)

dev.off()


ERA_5_dat<-dat[,c(2,3,13:19)]

era_melt<-melt(ERA_5_dat, id.vars=c("site_no", "waterYear"))

era_melt <- era_melt %>%
  mutate(variable=case_when(
    variable=="annual_evap_mm"~"Annual Evaporation (mm)",
    variable=="annual_precip_mm"~"Annual Precipitation (mm)",
    variable=="mean_temp_C"~"Mean Temperature (C)",
    variable=="peak_swe_day"~"Peak SWE Day (DOY)",
    variable=="peak_swe_mm"~"Peak SWE (mm)",
    variable=="sdd"~"Snow Disappearance Day (DOY)",
    variable=="melt_rate"~"Melt Rate (mm/day)"
  ))

pdf("Climate_Histograms.pdf", width = 13.5, height = 6)

ggplot(era_melt, aes(value))+geom_histogram(fill="black")+facet_wrap(~variable, scales = "free", nrow = 2)+
  theme_classic()+theme(text = element_text(size = 15))+
  labs(x="Climate Variable Value", y="Number of Years")

dev.off()

cst_dat<-dat[,c(2,3,12,20:22)]

cst_dat<-cst_dat[!duplicated(cst_dat$site_no),]

cst_melt<-melt(cst_dat, id.vars="site_no")

cst_melt<-cst_melt %>%
  mutate(variable=case_when(
    variable=="mean_slope"~"Basin Slope (degrees)",
    variable=="mean_elevation"~"Mean Elevation (m)",
    variable=="mean_bf"~"Mean Baseflow Proportion",
    variable=="recession_curve_slope"~"Recession Curve Slope"
  ))

pdf("ST_Histograms.pdf", width = 6, height = 5)

ggplot(cst_melt, aes(value))+geom_histogram(fill="black")+facet_wrap(~variable, scales = "free")+
  theme_classic()+theme(text = element_text(size = 15))+
  labs(x="Subsurface/Topography Variable Value", y="Number of Sites")

dev.off()

model_input_data<-left_join(cst_dat, ERA_5_dat)

colnames(model_input_data)<-c("waterYear", "site_no", "Recession Curve Slope", "Mean Elevation (m)", "Basin Slope (degrees)",
                              "Mean Baseflow Proportion", "Annual Evaporation (mm)","Annual Precipitation (mm)",
                              "Mean Temperature (C)", "Peak SWE Day (DOY)", "Peak SWE (mm)", "Snow Disappearance Day (DOY)",
                              "Melt Rate (mm/day)")

dat_cor<-cor(model_input_data[3:13])

pdf("Corr_Plot.pdf", width = 8, height = 8)

ggcorrplot::ggcorrplot(dat_cor, hc.order = TRUE,type = "upper",
                       outline.col = "white", lab = T)

dev.off()

