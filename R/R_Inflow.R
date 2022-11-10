library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
library(gtable)
library(sharpshootR)
library(RcppRoll)
library(ggpubr)

#Path to local drive
root <- "~/GitHub/public_draft_alternatives_BM"
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")

#################### Read dayflow data
data_dayflow_1970_1983<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1970-1983.csv"))
data_dayflow_1984_1996<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1984-1996.csv"))
data_dayflow_1997_2020<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1997-2020.csv")) %>% mutate(EXPORT=EXPORTS)
data_dayflow_2021<-read.csv(file.path(data_root, "Dayflow", "dayflowcalculations2021.csv"))%>% mutate(EXPORT=EXPORTS)

data_dayflow<-bind_rows(data_dayflow_1970_1983,data_dayflow_1984_1996,data_dayflow_1997_2020,data_dayflow_2021)
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

#Add time period  information
data_dayflow_added<-data_dayflow_added %>% mutate(TimePeriod= case_when(
  WY<2000 ~ "1970-1999",
  WY>=2000&WY<2011 ~ "2000-2010",
  WY>=2011 ~ "2011-2021"
))

#2010-present (back to 8/9 opinion)
#pre 8/9, 2000-2010, 
#pre-D-1641 (2000)

str(data_dayflow_added)
data_dayflow_added$Sac_WY<-factor(data_dayflow_added$Sac_WY, levels = c("W", "AN", "BN","D","C"))

#Set up dummy date
#2000 was a leap year
data_dayflow_added <- data_dayflow_added %>% mutate(dummyDate = case_when(
  month(Date)>9 ~ as.Date(paste(1999,strftime(Date,format="%m-%d"),sep="-")),
  month(Date)<=9 ~as.Date(paste(2000,strftime(Date,format="%m-%d"),sep="-"))
))


#Plot for INFLOW
plot_inflow <- ggplot2::ggplot(data=data_dayflow_added,aes(x=dummyDate,y=TOT,group=WY,color=Sac_WY))+
  ggplot2::theme_bw()+
  ggplot2::geom_line(alpha=0.5)+
  facet_grid(TimePeriod~Sac_WY)+
  geom_smooth(aes(group=Sac_WY),se=F)+
  ggplot2::scale_colour_manual(values = c("#003E51","#007396","#C69214","#FF671F","#9A3324")) +
  ggplot2::scale_size_manual(values = c(0.5,0.5,0.5,0.5,0.5,1),name="",labels=c(2017:2022)) +
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=7, color="black",angle=90), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 9),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::ylab(bquote("Delta"~inflow~"("*ft^3*"/s)"))+ggplot2::xlab(NULL)+
  ggplot2::scale_x_date(date_labels = "%b",date_breaks  ="1 month")

plot_inflow


#Print figure
tiff(filename=file.path(output_root,"Figure_Inflow_by_TimePeriod.tiff"),
     type="cairo",
     units="in", 
     width=14, #10*1, 
     height=8, #22*1, 
     pointsize=5, #12, 
     res=600,
     compression="lzw")
print(plot_inflow)
dev.off()


#Plot for CVP and SWP EXPORT
plot_cvp <- ggplot2::ggplot(data=data_dayflow_added,aes(x=dummyDate,y=CVP,group=WY,color=Sac_WY))+
  ggplot2::theme_bw()+
  ggplot2::geom_line(alpha=0.5)+
  facet_grid(TimePeriod~Sac_WY)+
  geom_smooth(aes(group=Sac_WY),se=F)+
  ggplot2::scale_colour_manual(values = c("#003E51","#007396","#C69214","#FF671F","#9A3324")) +
  ggplot2::scale_size_manual(values = c(0.5,0.5,0.5,0.5,0.5,1),name="",labels=c(2017:2022)) +
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=7, color="black",angle=90), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 9),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::ylab(bquote("CVP"~export~"("*ft^3*"/s)"))+ggplot2::xlab(NULL)+
  ggplot2::scale_x_date(date_labels = "%b",date_breaks  ="1 month")

plot_cvp

plot_swp <- ggplot2::ggplot(data=data_dayflow_added,aes(x=dummyDate,y=SWP,group=WY,color=Sac_WY))+
  ggplot2::theme_bw()+
  ggplot2::geom_line(alpha=0.5)+
  facet_grid(TimePeriod~Sac_WY)+
  geom_smooth(aes(group=Sac_WY),se=F)+
  ggplot2::scale_colour_manual(values = c("#003E51","#007396","#C69214","#FF671F","#9A3324")) +
  ggplot2::scale_size_manual(values = c(0.5,0.5,0.5,0.5,0.5,1),name="",labels=c(2017:2022)) +
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=7, color="black",angle=90), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 9),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::ylab(bquote("SWP"~export~"("*ft^3*"/s)"))+ggplot2::xlab(NULL)+
  ggplot2::scale_x_date(date_labels = "%b",date_breaks  ="1 month")

plot_swp

#Print figure
tiff(filename=file.path(output_root,"Figure_CVP_by_TimePeriod.tiff"),
     type="cairo",
     units="in", 
     width=14, #10*1, 
     height=8, #22*1, 
     pointsize=5, #12, 
     res=600,
     compression="lzw")
print(plot_cvp)
dev.off()

#Print figure
tiff(filename=file.path(output_root,"Figure_SWP_by_TimePeriod.tiff"),
     type="cairo",
     units="in", 
     width=14, #10*1, 
     height=8, #22*1, 
     pointsize=5, #12, 
     res=600,
     compression="lzw")
print(plot_swp)
dev.off()
