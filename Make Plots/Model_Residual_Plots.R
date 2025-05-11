require(dplyr)
require(reshape2)
require(ggplot2)
require(ggpubr)

setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow")

res1<-read.csv("Residuals_Climate_Full.csv")
res2<-read.csv("Residuals_CST_Full_withBF.csv")

res_all<-left_join(res1, res2)

res_all$threshold<-res_all$Q5_norm*0.5

res_all$res_diff<-abs(res_all$climate_residual)-abs(res_all$cst_residual)

res_all$clim_prediction_char<-ifelse(abs(res_all$climate_residual) >= res_all$threshold, "poor prediction", "good prediction")

res_all$cst_prediction_char<-ifelse(abs(res_all$cst_residual) >= res_all$threshold, "poor prediction", "good prediction")

write.csv(res_all, "Residuals_char.csv")

#proportion poorly predicted in climate vs cst model
table(res_all$cst_prediction_char)

table(res_all$clim_prediction_char)

10293/nrow(res_all)

6859/nrow(res_all)

res_all$res_diff<-abs(res_all$climate_residual)-abs(res_all$cst_residual)

p1<-ggplot(res_all, aes(climate_pred, Q5_norm, col=clim_prediction_char))+geom_point(alpha=0.5)+
  geom_abline(slope = 1, intercept = 0, col="red", lty="dashed", size=1)+
  #geom_abline(slope = 1.5, intercept = 0, col="red", lty="dashed", alpha=0.8)+
  #geom_abline(slope = 0.5, intercept = 0, col="red", lty="dashed", alpha=0.8)+
  theme_classic()+labs(x=expression(atop("Climate Model", paste("Predicted Q5" [norm], "(mm/day)"))), y=expression(Observed~Q5[norm]~(mm/day)), tag="a")+
  theme(text = element_text(size=22), legend.position = "null")+
  scale_color_manual(values=c("good prediction"="black", "poor prediction"="darkgrey"))
p1


p2<-ggplot(res_all, aes(cst_pred, Q5_norm, col=cst_prediction_char))+geom_point(alpha=0.5)+
  geom_abline(slope = 1, intercept = 0, col="red", lty="dashed", size=1)+
  theme_classic()+labs(x=expression(atop("CST Model", paste("Predicted Q5" [norm], "(mm/day)"))), y=expression(Observed~Q5[norm]~(mm/day)), tag="b")+
  theme(text = element_text(size=22), legend.position = "null")+
  scale_color_manual(values=c("good prediction"="black", "poor prediction"="darkgrey"))
p2

p3<-ggplot(res_all, aes(res_diff, fill=cst_prediction_char))+geom_histogram(binwidth = 0.001)+
  geom_vline(xintercept = 0, col="red", lty="dashed", size=1)+
  theme_classic()+labs(x="Difference in Climate\n and CST Residual (mm/day)", y="Count", tag="c")+
  theme(text = element_text(size=22),legend.position = "null")+
  scale_fill_manual(values=c("good prediction"="black", "poor prediction"="darkgrey"))+
  xlim(-0.2, 0.5)

p3

#get percent of sites that fall between the xaxis on panel c in figure above (p3)
res_all %>%
  filter(res_diff <= 0.5 & res_diff >= -0.2) %>%
  tally()

18841/19077

pdf("Pred_Observed_NoBFmodel.pdf")

ggplot(res_all, aes(cst_pred, Q5_norm, col=cst_prediction_char))+geom_point(alpha=0.5)+
  geom_abline(slope = 1, intercept = 0, col="red", lty="dashed", size=1)+
  theme_classic()+labs(x=expression(atop("CST Model No Baseflow", paste("Predicted Q5" [norm], "(mm/day)"))), y=expression(Observed~Q5[norm]~(mm/day)), tag="b")+
  theme(text = element_text(size=22), legend.position = "null")+
  scale_color_manual(values=c("good prediction"="black", "poor prediction"="darkgrey"))

dev.off()

pdf("Histogram_Full.pdf", width = 6, height = 6)

ggplot(res_all, aes(res_diff, fill=cst_prediction_char))+geom_histogram(binwidth = 0.005)+
  geom_vline(xintercept = 0, col="red", lty="dashed")+
  theme_classic()+labs(x="Difference in Climate and CST Residual (mm/day)", y="Count")+
  theme(text = element_text(size=15),legend.position = "null")+
  scale_fill_manual(values=c("good prediction"="black", "poor prediction"="darkgrey"))

dev.off()

pdf("Model_Performance_Stats_Full_ColoredRes_BigText_UpdatedUnits.pdf", width = 18, height = 6)

ggarrange(p1, p2, p3, nrow = 1, align = "v")

dev.off()

#total improvement across all sites with inclusion of CST
res_all$pos_neg<-ifelse(res_all$res_diff > 0, "positive", "negative")

table(res_all$pos_neg)
14711/nrow(res_all)

#mean residual of climate model
res_all %>%
  filter(clim_prediction_char=="poor prediction") %>%
  summarise(mean(abs(climate_residual)))

#mean residual of cst model
res_all %>%
  filter(cst_prediction_char=="poor prediction") %>%
  summarise(mean(abs(cst_residual)))

test_dat<-read.csv("AllDataHarmonized_Simple_01132025.csv")

test_res<-left_join(res_all[,2:13], test_dat[,2:22])

test_res$snow_frac<-test_res$peak_swe_mm/test_res$annual_precip_mm

test_res$aridity<-test_res$annual_precip_mm/test_res$annual_evap_mm

#create low, median, high flow conditions
test_res<-test_res %>%
  dplyr::group_by(site_no) %>%
  dplyr::mutate(median_Q5=median(Q5), high_Q5=1.25*median_Q5, low_Q5=0.75*median_Q5)

test_res$flow<-ifelse(test_res$Q5 > test_res$high_Q5, "high",
                      ifelse(test_res$Q5 < test_res$low_Q5, "low", "median"))

table(test_res$flow)

test_res$norm_res_diff<-test_res$res_diff/test_res$Q5_norm

test_res_nodups_prop<-test_res %>%
  dplyr::group_by(site_no) %>%
  dplyr::mutate(num_obs=n()) %>%
  dplyr::mutate(good_pre=sum(clim_prediction_char=="poor prediction" & pos_neg=="positive"), good_pre_prop=good_pre/num_obs) %>%
  #dplyr::mutate(mean_norm_res_diff=mean(norm_res_diff)) %>%
  dplyr::filter(!duplicated(site_no))

flow_prop<-test_res %>%
  dplyr::group_by(clim_prediction_char, Name) %>%
  dplyr::summarise(tot_sites=n())

flow_prop_cast<-dcast(flow_prop, Name~clim_prediction_char, value.var = "tot_sites")

flow_prop_cast$prop<-flow_prop_cast$`poor prediction`/(flow_prop_cast$`good prediction`+flow_prop_cast$`poor prediction`)

flow_prop<-test_res %>%
  dplyr::group_by(flow, Name) %>%
  dplyr::summarise(tot_sites=n())

flow_prop_cast<-dcast(flow_prop, Name~flow, value.var = "tot_sites")

flow_prop_cast$sum<-rowSums(flow_prop_cast[2:4])
flow_prop_cast[2:4]<-flow_prop_cast[2:4]/flow_prop_cast$sum

flow_prop_cast$prop<-flow_prop_cast$`poor prediction`/(flow_prop_cast$`good prediction`+flow_prop_cast$`poor prediction`)

flow_prop<-test_res %>%
  dplyr::filter(clim_prediction_char=="poor prediction" & pos_neg=="positive") %>%
  dplyr::group_by(Name, flow) %>%
  dplyr::summarise(tot_sites=n()) %>%
  dplyr::ungroup()
  
flow_prop2<-test_res %>%
  dplyr::group_by(Name, flow) %>%
  dplyr::summarise(tot_Name=n())

test_res %>%
  dplyr::filter(clim_prediction_char=="poor prediction" & pos_neg=="positive") %>%
  nrow()

flow_prop<-left_join(flow_prop, flow_prop2)

flow_prop$flow_prop_tot<-flow_prop$tot_sites/flow_prop$tot_Name

flow_prop<-flow_prop %>%
  mutate(flow=case_when(
    flow=="low"~"Low Flow",
    flow=="median"~"Median Flow",
    flow=="high"~"High Flow",
  ))

flow_prop$flow<-factor(flow_prop$flow, levels = c("Low Flow", "Median Flow", "High Flow"))

flow_prop$Name<-factor(flow_prop$Name, levels = c("Arid", "Mediterranean", "Semi-Arid", "Humid Temperate", 
                                                  "Humid Continental", "Subarctic"))

color_pal<-c("#E69F00", "#0072B2", "#D55E00", "#009E73", "#56B4E9","#CC79A7")


pdf("Prop_CST_Imp_ClimateZone.pdf", width = 13, height = 5)

ggplot(flow_prop, aes(Name, flow_prop_tot, fill=Name, alpha=flow))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_classic()+theme(text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust=1, size = 15),
                        legend.position = "left")+
  labs(x="", y="Proportion of Predictions\n Improved by CST", alpha="", fill=" ")+
  scale_fill_manual(values=color_pal, guide = "none")+
  scale_alpha_manual(values = c("Low Flow"=1, "Median Flow"=0.7, "High Flow"=0.4))

dev.off()

test_res_crop<-subset(test_res, test_res$clim_prediction_char=="poor prediction")

table(test_res_crop$pos_neg)
9519/nrow(test_res_crop)
9519/nrow(test_res)

residual_CZ<-test_res %>%
  dplyr::group_by(Name) %>%
  dplyr::summarise(tot_Name=n()) %>%
  dplyr::ungroup()

residual_CZ2<-test_res %>%
  dplyr::filter(clim_prediction_char=="poor prediction" & pos_neg=="positive") %>%
  dplyr::group_by(Name) %>%
  tally()

residual_CZ<-left_join(residual_CZ, residual_CZ2)
residual_CZ$prop_inc<-residual_CZ$n/residual_CZ$tot_Name

residual_CZ$Name<-factor(residual_CZ$Name, levels = c("Arid", "Mediterranean", "Semi-Arid", "Humid Temperate", 
                                                  "Humid Continental", "Subarctic"))

pdf("Overall_Prop_Inc_CZ.pdf", width = 5, height = 7)

ggplot(residual_CZ, aes(Name, prop_inc, fill=Name))+geom_bar(position = "dodge", stat = "identity")+
  theme_classic()+theme(text = element_text(size=15), axis.text.x = element_text(angle=45, hjust=1, size = 15), legend.position = "null",
                        axis.ticks.x = element_blank())+
  labs(x="", y="Proprtion of Predictions Improved by CST")+scale_fill_manual(values=color_pal)

dev.off()

#write.csv(test_res_crop, "Improved_Sites_CST.csv")

test_res_crop$snow_frac<-test_res_crop$peak_swe_mm/test_res_crop$annual_precip_mm

test_res_crop$aridity<-test_res_crop$annual_precip_mm/test_res_crop$annual_evap_mm

test_res_crop %>%
  dplyr::filter(pos_neg=="positive") %>%
  dplyr::group_by(Name) %>%
  dplyr::summarise(median(res_diff/Q5_norm))

test_res %>%
  dplyr::group_by(Name) %>%
  dplyr::summarise(median(mean(res_diff)/mean(Q5_norm)))

color_pal<-c("#E69F00","#56B4E9","#009E73","#0072B2","#D55E00","#CC79A7")

pdf("SnowFraction_Res_CZ.pdf", width = 7, height = 5.1)

ggplot(test_res_crop, aes(snow_frac, log(res_diff/Q5_norm)))+geom_point(aes(col=Name), alpha=0.7)+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="Snow Fraction", y="", col="Climate Zone")+
  scale_color_manual(values = color_pal)

dev.off()

pdf("SnowFraction_Res_CZ_facet.pdf", width = 8, height = 5)
ggplot(test_res_crop, aes(snow_frac, log(res_diff/Q5_norm)))+geom_point(aes(col=Name), alpha=0.7)+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="Snow Fraction", y="Log(Normalized Residual Difference)", col="Climate Zone")+
  scale_color_manual(values = color_pal)+facet_wrap(~Name)
dev.off()

pdf("Aridity_Res_CZ_facet.pdf", width = 8, height = 5)
ggplot(test_res_crop, aes(aridity, log(res_diff/Q5_norm)))+geom_point(aes(col=Name), alpha=0.7)+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="P/PET", y="Log(Normalized Residual Difference)", col="Climate Zone")+
  scale_color_manual(values = color_pal)+facet_wrap(~Name)
dev.off()

pdf("Aridity_Res_CZ.pdf", width = 7, height = 5.1)

ggplot(test_res_crop, aes(aridity, log(res_diff/Q5_norm)))+geom_point(aes(col=Name), alpha=0.7)+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="P/PET", y="Log(Normalized Residual Difference)", col="Climate Zone")+
  scale_color_manual(values = color_pal)

dev.off()

