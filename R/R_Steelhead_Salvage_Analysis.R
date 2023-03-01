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
salvage_data_steelhead<-read.csv(file.path(data_root, "Abundance Data", "SalvageExport_Steelhead.csv")) %>% mutate(Sample.Date=as.Date(Sample.Date)) %>% group_by(Sample.Date) %>% 
  summarise(Loss=sum(Loss)) %>%
  mutate(WY=ifelse(month(Sample.Date)>9,year(Sample.Date)+1,year(Sample.Date)))

#Split data by WY to calculate cumsum
sth_data <- split(salvage_data_steelhead, f = salvage_data_steelhead$WY)

sth_data <- lapply(sth_data, function(x) {x %>% mutate(CumulativeLoss = cumsum(Loss))} )

#Re-merge data
sth_data <- do.call("rbind", sth_data)

#Create dummy date
sth_data$dummydate <- as.Date(sth_data$Sample.Date)

sth_data <- sth_data %>% mutate(dummydate = case_when(
  month(Sample.Date)>9 ~ as.Date(paste(1999,strftime(Sample.Date,format="%m-%d"),sep="-")),
  month(Sample.Date)<=9 ~as.Date(paste(2000,strftime(Sample.Date,format="%m-%d"),sep="-"))
)) 

#Plot by water year
plot_steelhead <- ggplot2::ggplot(data=sth_data %>% filter(WY>2008),aes(x=dummydate,y=CumulativeLoss,group=WY,color=WY))+
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
  ggplot2::geom_hline(yintercept=50, linetype="dashed", color = "red")+
  ylim(0,2000)

plot_steelhead
