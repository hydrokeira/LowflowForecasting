setwd("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/Low Flow")

dat<-read.csv("AllDataHarmonized_Simple_01252025.csv")

dat<-dat %>%
  dplyr::filter(!site_no %in% c(9153290, 13075983))

dat<-subset(dat, dat$waterYear > 1949)

dat<-dat[complete.cases(dat),]

dat<-dat[!duplicated(dat$site_no),]

range(dat$drain_area_va)

write.csv(dat, "WRL_sites_final_jan2025.csv")

dat<-dat %>%
  dplyr::group_by(site_no) %>%
  dplyr::mutate(median_Q5=median(Q5), high_Q5=1.25*median_Q5, low_Q5=0.75*median_Q5)

dat$flow<-ifelse(dat$Q5 > dat$high_Q5, "high",
                     ifelse(dat$Q5 < dat$low_Q5, "low", "median"))

dat_year<-dat %>%
  dplyr::group_by(site_no) %>%
  dplyr::summarise(min(waterYear))

dat$flow<-factor(dat$flow, levels = c("low", "median", "high"))

pdf("POR_flow.pdf", width = 5, height = 9)

ggplot(dat, aes(waterYear, as.character(site_no), fill=flow))+geom_tile()+
  theme_classic()+
  scale_y_discrete(limits=unique(factor(rev(dat_year$site_no[order(dat_year$`min(waterYear)`)]))))+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), text = element_text(size=20),
        legend.position = c(0.27,0.1))+
  labs(y="", x="Date", fill="Flow Condition")+
  scale_fill_manual(values = c("low"="lightskyblue", "median"="dodgerblue3", high="midnightblue"))

dev.off()
