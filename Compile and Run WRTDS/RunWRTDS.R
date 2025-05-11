##run WRTDS for conductivity data
install.packages("EGRET")
install.packages("dataRetrieval")
require(EGRET)
require(dataRetrieval)
require(dplyr)

setwd("C:/Users/johnkeir/Box/Hydrology_Lab/Projects/Low Flow/Data/WRTDS_Inputs/")

cond<-read.csv("WRTDS_Cond_Input_Final_08132024.csv")

cond$site_no<-as.character(cond$site_no)

cond$site_no<-ifelse(nchar(cond$site_no) < 8, paste0("0", cond$site_no), cond$site_no)

q_file<-read.csv("WRTDS_Q_Input_Final_08132024.csv")

q_file$site_no<-as.character(q_file$site_no)

q_file$site_no<-ifelse(nchar(q_file$site_no) < 8, paste0("0", q_file$site_no), q_file$site_no)

sites<-unique(cond$site_no)

egret_error<-list()

flux_bias<-list()

skip_these<-c(29, 613)

for (i in 1:length(sites)) {
  
  print(i)
  
  if(i %in% skip_these) next

  setwd("C:/Users/johnkeir/Box/Hydrology_Lab/Projects/Low Flow/Data/WRTDS_Inputs/TempWRTDSInputFiles/")
  
  one_q<-subset(q_file, q_file$site_no==sites[i])
  
  one_q<-one_q[,c("Date", "discharge")]
  
  one_q$Date<-as.Date(one_q$Date)
  
  write.csv(one_q, "discharge.csv", row.names = F)
  
  one_cond<-subset(cond, cond$site_no==sites[i])
  
  one_cond<-one_cond[,c("sample_dt", "result_va")]
  
  colnames(one_cond)<-c("Date", "conductivity")
  
  one_cond$remarks<-""
  
  one_cond<-one_cond[,c(1,3,2)]
  
  write.csv(one_cond, "chemistry.csv", row.names = F)
  
  daily<-EGRET::readUserDaily(filePath = "C:/Users/johnkeir/Box/Hydrology_Lab/Projects/Low Flow/Data/WRTDS_Inputs/TempWRTDSInputFiles/",
                       fileName = "discharge.csv")
  
  sample<-EGRET::readUserSample(filePath = "C:/Users/johnkeir/Box/Hydrology_Lab/Projects/Low Flow/Data/WRTDS_Inputs/TempWRTDSInputFiles/",
                               fileName = "chemistry.csv")
  
  if(min(daily$Date) > min(sample$Date)){
    
    sample<-subset(sample, sample$Date > min(daily$Date))
    
  }else{
    
    sample<-sample
    
  }
  
  if(max(daily$Date) < max(sample$Date)){
    
    sample<-subset(sample, sample$Date < max(daily$Date))
    
  }else{
    
    sample<-sample
    
  }
  
  info<-EGRET::readNWISInfo(siteNumber = sites[i], parameterCd = "00095", interactive = F)
  
  # Create a list of the discharge, chemistry, and information files
  egret_list <- EGRET::mergeReport(INFO = info, Daily = daily, Sample = sample, verbose = F)
  
  # Run series
  egret_list_out <- EGRET::runSeries(eList = egret_list, windowSide = 11, minNumObs = 45, minNumUncen=45,
                                     verbose = F, windowS = 0.5)
  
  # Fit original model
  egret_estimation <- EGRET::modelEstimation(eList = egret_list, windowS = 0.5,
                                             minNumObs = 45, minNumUncen =45, verbose = F)
  
  # Fit WRTDS Kalman
  egret_kalman <- EGRET::WRTDSKalman(eList = egret_estimation, niter = 200, verbose = T)
  
  # Identify error statistics
  egret_error[[i]] <- EGRET::errorStats(eList = egret_kalman)
  
  flux_bias[[i]] <- EGRET::fluxBiasStat(localSample = EGRET::getSample(x = egret_kalman))
  
  setwd("C:/Users/johnkeir/Box/Hydrology_Lab/Projects/Low Flow/WRTDS_Outputs/FluxBiasPlots/")
  
  pdf(paste0("FluxBias_", sites[i], ".pdf"))
  
  EGRET::fluxBiasMulti(egret_kalman)
  
  dev.off()
  
  setwd("C:/Users/johnkeir/Box/Hydrology_Lab/Projects/Low Flow/WRTDS_Outputs/ContCondPlots/")
  
  pdf(paste0("DailyCond_", sites[i], ".pdf"))
  
  EGRET::plotConcTimeDaily(egret_kalman)
  
  dev.off()
  
  setwd("C:/Users/johnkeir/Box/Hydrology_Lab/Projects/Low Flow/WRTDS_Outputs/DailyCondFiles/")
  
  egret_conc_kalman <- egret_kalman$Daily
  
  write.csv(egret_conc_kalman, paste0("DailyCondFile_", sites[i], ".csv"))
  
}

i=105

sites_remove_missing<-sites[-c(29,613)]

egret_error_df<-do.call(bind_rows, egret_error)
egret_error_df$site_no<-sites_remove_missing

flux_bias_df<-do.call(bind_rows, flux_bias)
flux_bias_df$site_no<-sites_remove_missing


setwd("C:/Users/johnkeir/Box/Hydrology_Lab/Projects/Low Flow/WRTDS_Outputs")
write.csv(egret_error_df, "Egret_Errors.csv")
write.csv(flux_bias_df, "Flux_Bias.csv")

