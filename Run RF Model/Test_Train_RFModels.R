require(tidyr)
require(dplyr)
require(randomForest)


setwd("C:/Users/johnkeir/Box/Hydrology_Lab/Projects/Low Flow")

dat<-read.csv("AllDataHarmonized_Simple_01252025.csv")

dat$Q5_norm<-dat$Q5/dat$drain_area_va

dat<-dat[,c(2:5,10:23)]

dat<-dat[complete.cases(dat),]

dat<-dat[,c(1:6,18,7:17)]

dat<-dat %>%
  dplyr::filter(!site_no %in% c(9153290, 13075983))

set.seed(123)
ind <- sample(2, nrow(dat), replace = TRUE, prob = c(0.7, 0.3))
train <- dat[ind==1,]
test <- dat[ind==2,]

#for climate and subsurface model
drivers_df<-train[,c(7:18)]

#for climate model
drivers_df<-train[,c(7, 9:15)]

drivers_df<-as.data.frame(sapply(drivers_df, as.numeric))

#run intial RF using tuned parameters
set.seed(123)
rf_model1<-randomForest(Q5_norm~.,
                        data=drivers_df, importance=TRUE, proximity=TRUE, ntree=400)


#visualize output
rf_model1
plot(rf_model1)

p1<-predict(rf_model1, train)

obs_pred<-bind_cols(train[,c(1,2,7)], p1)

p2<-predict(rf_model1, test)

obs_pred<-bind_cols(test[,c(1,2,7)], p2)

obs_pred$climate_residual<-obs_pred$Q5_norm-obs_pred$...4

rmse(obs_pred$...4, obs_pred$Q5_norm)/mean(obs_pred$Q5_norm)

summary(lm(...4~Q5_norm, obs_pred))
