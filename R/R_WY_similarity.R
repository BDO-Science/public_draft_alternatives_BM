library(tidyverse)
library(lubridate)
library(viridis)
library(vegan)

#Path to local drive
root <- "~/GitHub/public_draft_alternatives_BM"
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")

#################### Read dayflow data
#data_dayflow_1929_1939<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1929-1939.csv"))
#data_dayflow_1940_1949<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1940-1949.csv"))
#data_dayflow_1950_1955<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1950-1955.csv"))

data_dayflow_1956_1969<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1956-1969.csv"))
data_dayflow_1970_1983<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1970-1983.csv"))
data_dayflow_1984_1996<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1984-1996.csv"))
data_dayflow_1997_2020<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1997-2020.csv")) %>% mutate(EXPORT=EXPORTS)
data_dayflow_2021<-read.csv(file.path(data_root, "Dayflow", "dayflowcalculations2021.csv"))%>% mutate(EXPORT=EXPORTS)

data_dayflow<-bind_rows(#data_dayflow_1929_1939,data_dayflow_1940_1949,data_dayflow_1950_1955,
  data_dayflow_1956_1969,data_dayflow_1970_1983,data_dayflow_1984_1996,data_dayflow_1997_2020,data_dayflow_2021)
data_dayflow$Date <- as.Date(data_dayflow$Date,"%m/%d/%Y")

#Add water year to dayflow
data_dayflow$WY<-as.numeric(ifelse(month(data_dayflow$Date)>9,data_dayflow$Year+1,data_dayflow$Year))

data_dayflow$Month<-month(data_dayflow$Date)

#Add julian day
data_dayflow$julianday<-yday(data_dayflow$Date)

#################### Add Water Year information

#Data from https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST
#Copy and pasted into excel
data_wateryear<-read.csv(file.path(data_root, "Dayflow", "DWR_WSIHIST.csv"))

#Add WY type information
data_dayflow_added<-left_join(data_dayflow,data_wateryear[,c("WY","Sac_WY")])

##########
data_dayflow_edit<-data_dayflow_added %>% group_by(WY,Month) %>% summarise(SAC=mean(SAC),SJR=mean(SJR),YOLO=mean(YOLO),SWP=mean(SWP),CVP=mean(CVP)) %>%
  pivot_wider(names_from = Month, values_from = c(SAC,SJR,YOLO,SWP,CVP), names_sep="_")

WY_list<-data_dayflow_edit$WY
data_dayflow_edit$WY<-NULL

wy_dist<-as.matrix(vegdist(data_dayflow_edit,method="euclidean"))

wy_dist

row.names(wy_dist)<-WY_list
colnames(wy_dist)<-WY_list

postCALSIM<-c(2003:2021)

#Remove all post-CALSIM years
wy_dist[rownames(wy_dist) %in% postCALSIM,]<-0
wy_dist[wy_dist == 0] <- NA

wy_comparison<-data.frame(WaterYear=c(2003:2021),SimilarWY=NA)

similarwy<-vector()
similarwy[1]<-data.frame(sort(wy_dist[,"2003"])) %>% top_n(-1) %>% row.names()
similarwy[2]<-data.frame(sort(wy_dist[,"2004"])) %>% top_n(-1) %>% row.names()
similarwy[3]<-data.frame(sort(wy_dist[,"2005"])) %>% top_n(-1) %>% row.names()
similarwy[4]<-data.frame(sort(wy_dist[,"2006"])) %>% top_n(-1) %>% row.names()
similarwy[5]<-data.frame(sort(wy_dist[,"2007"])) %>% top_n(-1) %>% row.names()
similarwy[6]<-data.frame(sort(wy_dist[,"2008"])) %>% top_n(-1) %>% row.names()
similarwy[7]<-data.frame(sort(wy_dist[,"2009"])) %>% top_n(-1) %>% row.names()
similarwy[8]<-data.frame(sort(wy_dist[,"2010"])) %>% top_n(-1) %>% row.names()
similarwy[9]<-data.frame(sort(wy_dist[,"2011"])) %>% top_n(-1) %>% row.names()
similarwy[10]<-data.frame(sort(wy_dist[,"2012"])) %>% top_n(-1) %>% row.names()
similarwy[11]<-data.frame(sort(wy_dist[,"2013"])) %>% top_n(-1) %>% row.names()
similarwy[12]<-data.frame(sort(wy_dist[,"2014"])) %>% top_n(-1) %>% row.names()
similarwy[13]<-data.frame(sort(wy_dist[,"2015"])) %>% top_n(-1) %>% row.names()
similarwy[14]<-data.frame(sort(wy_dist[,"2016"])) %>% top_n(-1) %>% row.names()
similarwy[15]<-data.frame(sort(wy_dist[,"2017"])) %>% top_n(-1) %>% row.names()
similarwy[16]<-data.frame(sort(wy_dist[,"2018"])) %>% top_n(-1) %>% row.names()
similarwy[17]<-data.frame(sort(wy_dist[,"2019"])) %>% top_n(-1) %>% row.names()
similarwy[18]<-data.frame(sort(wy_dist[,"2020"])) %>% top_n(-1) %>% row.names()
similarwy[19]<-data.frame(sort(wy_dist[,"2021"])) %>% top_n(-1) %>% row.names()


similarwy

wy_comparison$SimilarWY<-similarwy

write.csv(wy_comparison,file=file.path(output_root,"WaterYear_Calsim_comparison.csv"),row.names=F)

          