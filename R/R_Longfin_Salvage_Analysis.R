library(tidyverse)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)
library(readxl)
library(grid)
library(gridExtra)

#Path to local drive
root <- "~/GitHub/public_draft_alternatives_BM"
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")

##################### Load salvage data

#Assume NS = no salvage
salvage_data_longfin<-read.csv(file.path(data_root, "Abundance Data", "SalvageExport_LongfinSmelt.csv")) %>% mutate(Sample.Date=as.Date(Sample.Date)) %>%
  mutate(Salvage = as.numeric(ifelse(Salvage=="NS",0,Salvage))) %>% group_by(Sample.Date) %>% 
  summarise(Salvage=sum(Salvage)) %>%
  mutate(WY=ifelse(month(Sample.Date)>9,year(Sample.Date)+1,year(Sample.Date)))

#Split data by WY to calculate cumsum
lfs_data <- split(salvage_data_longfin, f = salvage_data_longfin$WY)

lfs_data <- lapply(lfs_data, function(x) {x %>% mutate(CumulativeSalvage = cumsum(Salvage))} )

#Re-merge data
lfs_data <- do.call("rbind", lfs_data)

#Create dummy date
lfs_data$dummydate <- as.Date(lfs_data$Sample.Date)

lfs_data <- lfs_data %>% mutate(dummydate = case_when(
  month(Sample.Date)>9 ~ as.Date(paste(1999,strftime(Sample.Date,format="%m-%d"),sep="-")),
  month(Sample.Date)<=9 ~as.Date(paste(2000,strftime(Sample.Date,format="%m-%d"),sep="-"))
)) 


#Plot by water year
  plot_longfin <- ggplot2::ggplot(data=lfs_data %>% filter(WY>2008) ,aes(x=dummydate,y=CumulativeSalvage,group=WY,color=WY))+
  ggplot2::theme_bw()+
  ggplot2::geom_line(alpha=0.5)+
  #ggplot2::scale_colour_manual(values = c("#003E51","#007396","#C69214","#FF671F","#9A3324")) +
  #ggplot2::scale_size_manual(values = c(0.5,0.5,0.5,0.5,0.5,1),name="",labels=c(2017:2022)) +
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=7, color="black",angle=90), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 9),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::ylab("Cumulative Loss")+ggplot2::xlab(NULL)+
  ggplot2::scale_x_date(date_labels = "%b",date_breaks  ="1 month")+
  ggplot2::geom_hline(yintercept=576, linetype="dashed", color = "red")+
  ylim(0,1000)

plot_longfin


annual_data <- lfs_data %>% group_by(WY) %>% summarise(salvage=sum(Salvage))

test <- lfs_data %>% filter(WY==2020)
