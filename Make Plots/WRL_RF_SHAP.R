require(dplyr)
require(ggplot2)

setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow")

shap_df<-read.csv("Shap_values_Q5norm_WRL_RF_V10.csv")

shap_df$unique<-paste0(shap_df$site_no, "_", shap_df$waterYear)

dat<-read.csv("AllDataHarmonized_Simple_01252025.csv")

dat<-dat %>%
  dplyr::filter(!site_no %in% c(9153290, 13075983))

dat$Q5_norm<-(dat$Q5/(dat$drain_area_va*2.58999*1000000))*86400000

dat_Name<-dat[,c(3,11)]
dat_Name<-dat_Name[!duplicated(dat_Name$site_no),]

dat$unique<-paste0(dat$site_no, "_", dat$waterYear)

dat<-subset(dat, dat$unique %in% unique(shap_df$unique))

shap_df<-left_join(shap_df, dat_Name)

res_all<-read.csv("Residuals_char.csv")

shap_df<-left_join(shap_df, res_all[3:13])

shap_mean<-shap_df %>%
  #filter(feature %in% c("mean_elevation", "mean_slope", "annual_precip_mm", "peak_swe_mm", "melt_rate",
  #                      "recession_curve_slope")) %>%
  dplyr::group_by(feature, clim_prediction_char) %>%
  dplyr::summarise(mean_abs_shap=mean(abs(phi), na.rm=T))

shap_mean_simple<-shap_df %>%
  #filter(feature %in% c("mean_elevation", "mean_slope", "annual_precip_mm", "peak_swe_mm", "melt_rate",
  #                      "recession_curve_slope")) %>%
  dplyr::group_by(feature) %>%
  dplyr::summarise(mean_abs_shap=mean(abs(phi), na.rm=T))

features_df<-read.csv("All_Features_df.csv")
colnames(features_df)[1]<-"feature"

shap_mean<-left_join(shap_mean, features_df)

feature_pal=c("Climate"="dodgerblue", "Subsurface"="#ffb7a1","Topography"="#e85d04")

set2<-ggplot(shap_mean, aes(mean_abs_shap, feature, fill=class))+geom_bar(stat="identity", position = "dodge")+theme_classic()+
  scale_y_discrete(limits=unique(shap_mean_simple$feature[order(shap_mean_simple$mean_abs_shap)]),
                   labels=c("Snow Disappearance Day", "Day of Peak SWE","Annual Evaporation", "Mean Baseflow Proportion", "Mean Elevation","Melt Rate",
                            "Mean Annual Temperature","Recession Curve Slope","Peak SWE", "Mean Slope","Annual Precipitation"))+
  theme(text = element_text(size=25),legend.position = "bottom",legend.box = "verticle",
        axis.text.x = element_text(angle = 45, hjust=1, size = 20),
        axis.text.y = element_text(color = "black"))+
  scale_fill_manual(values = feature_pal)+
  labs(x="Mean Absolute SHAP value", y="", fill="", alpha="", tag="a")

set2

dat<-read.csv("AllDataHarmonized_Simple_01252025.csv")

dat$Q5_norm<-(dat$Q5/(dat$drain_area_va*2.58999*1000000))*86400000

shap_features<-unique(shap_df$feature)

dat<-dat[,colnames(dat) %in% c(shap_features, "waterYear", "site_no", "Q5", "drain_area_va")]

dat<-dat %>%
  dplyr::filter(!site_no %in% c(9153290, 13075983))

rcs<-dat %>%
  dplyr::group_by(site_no) %>%
  dplyr::summarise(mean_rcs=mean(mean_slope))

rcs_crop<-dat %>%
  dplyr::group_by(site_no) %>%
  dplyr::summarise(mean_rcs=mean(mean_slope)) %>%
  dplyr::filter(mean_rcs > 13)

shap_df<-left_join(shap_df, dat)

color_pal<-c("#E69F00","#56B4E9","#009E73","#0072B2","#D55E00","#CC79A7")

one_feature<-shap_df %>%
  dplyr::filter(feature=="peak_swe_mm")

pdf("SHAP_peak_SWE_facet_updatedUnits.pdf", width = 10, height = 6)

ggplot(one_feature, aes(peak_swe_mm, phi, col=Name))+geom_point(alpha=0.5)+theme_classic()+
  geom_abline(y=0, slope = 0, col="black")+
  labs(y="SHAP Value", x="Peak SWE (mm)")+
  theme(text = element_text(size=20), legend.position = "null")+facet_wrap(~Name)+
  scale_color_manual(values = color_pal)

dev.off()

p1<-ggplot(one_feature, aes(annual_precip_mm, phi))+geom_point(alpha=0.5)+theme_classic()+
  geom_abline(y=0, slope = 0, col="black")+
  labs(y="SHAP Value", x="Annual Precipitation (mm)", tag="b")+
  theme(text = element_text(size=25), legend.position = "null")

p1


p2<-ggplot(one_feature, aes(mean_slope, phi))+geom_point(alpha=0.5)+theme_classic()+
  geom_abline(y=0, slope = 0, col="black")+
  labs(y="SHAP Value", x="Basin Slope (degrees)", tag="d")+
  theme(text = element_text(size=25), legend.position = "null")

p2


p3<-ggplot(one_feature, aes(peak_swe_mm, phi))+geom_point(alpha=0.5)+theme_classic()+
  geom_abline(y=0, slope = 0, col="black")+
  labs(y="SHAP Value", x="Peak SWE (mm)", tag="c")+
  theme(text = element_text(size=25))

p3


p4<-ggplot(one_feature, aes(recession_curve_slope, phi))+geom_point(alpha=0.5)+theme_classic()+
  geom_abline(y=0, slope = 0, col="black")+
  labs(y="SHAP Value", x="Recession Curve Slope", tag="e", col="")+
  theme(text = element_text(size=25))

p4

set1<-ggarrange(p1, p3, p2, p4, align = "hv")

pdf("SHAP_res_model_updatedUnits.pdf", width = 22, height = 10)

ggarrange(set2, set1, widths = c(0.44, 0.65))

dev.off()
