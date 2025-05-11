install.packages("randomForest")
install.packages("Metrics")
install.packages("kernelshap")
install.packages("iml")
install.packages("shapviz")
install.packages("caret")

require(tidyr)
require(dplyr)
require(randomForest)
library(ggplot2)
library(kernelshap)
library(shapviz)
library(iml)
library(caret)
library(Metrics)
library(car)


#install.packages("caret")

setwd("C:/Users/johnkeir/Box/Hydrology_Lab/Projects/Low Flow")

dat<-read.csv("AllDataHarmonized_Simple_01252025.csv")

dat$Q5_norm<-(dat$Q5/(dat$drain_area_va*2.58999*1000000))*86400000

dat<-dat[,c(2:5,10:23)]

dat<-dat[complete.cases(dat),]

dat<-dat[,c(1:6,18,7:17)]

dat<-dat %>%
  dplyr::filter(!site_no %in% c(9153290, 13075983))

colSums(is.na(dat))

unique(dat$site_no)

year_count<-dat %>%
  group_by(site_no) %>%
  tally()

#for climate and subsurface model
drivers_df<-dat[,c(7:18)]

#for climate and subsurface model, no baseflow
drivers_df<-dat[,c(7:17)]

#for climate model
drivers_df<-dat[,c(7, 9:15)]

drivers_df<-as.data.frame(sapply(drivers_df, as.numeric))

#run intial RF using tuned parameters
set.seed(123)
rf_model1<-randomForest(Q5_norm~.,
                        data=drivers_df, importance=TRUE, proximity=TRUE, ntree=400)


#visualize output
rf_model1
plot(rf_model1)

obs_pred<-bind_cols(dat[,c(1,2,7)], rf_model1$predicted)

obs_pred$cst_residual<-obs_pred$Q5_norm-obs_pred$...4

colnames(obs_pred)[4]<-"cst_pred"

write.csv(obs_pred, "Residuals_CST_Full_noBaseflow.csv")

rmse(obs_pred$cst_pred, obs_pred$Q5_norm)/mean(obs_pred$Q5_norm)

summary(lm(cst_pred~Q5_norm, obs_pred))

predictor <- Predictor$new(rf_model1, data = drivers_df[, -1], y = drivers_df$Q5_norm)

#i=5

site_no_list<-dat$site_no
wy_list<-dat$waterYear

shap_list<-list()

for (i in 1:nrow(drivers_df)) {
  
  print(i)
  
  set.seed(123)
  shapley <- iml::Shapley$new(predictor, x.interest = drivers_df[i,-1])
  
  # Extract SHAP values
  shap_values <- shapley$results
  shap_values$site_no<-site_no_list[i]
  shap_values$waterYear<-wy_list[i]
  
  shap_list[[i]]<-shap_values
  
}

shap_df<-do.call(bind_rows, shap_list)

write.csv(shap_df, "Shap_values_Q5norm_WRL_RF_V10.csv")



