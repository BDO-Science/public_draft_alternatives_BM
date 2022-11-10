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

##################### LOad abundance index data

abundance_data<-read.csv(file.path(data_root, "Abundance Data", "MasterFishAbundance.csv"))

# add rows with NA info
abundance_data$Text <- if_else(is.na(abundance_data$Abundance),"N/A","")

#Read chinook salmon and steelhead data
salvage_data_salmon_WR<-read.csv(file.path(data_root, "Abundance Data", "SalvageExport_ChinookSalmon_WR.csv")) %>% mutate(Species="Chinook Salmon - winter-run")
salvage_data_salmon_SR<-read.csv(file.path(data_root, "Abundance Data", "SalvageExport_ChinookSalmon_SR.csv")) %>% mutate(Species="Chinook Salmon - spring-run")
salvage_data_steelhead<-read.csv(file.path(data_root, "Abundance Data", "SalvageExport_Steelhead.csv")) %>% mutate(Species="Steelhead")

salvage_data_salmon_WR$Date<-as.Date(salvage_data_salmon_WR$Sample.Date)
salvage_data_salmon_WR$wateryear<-ifelse(month(salvage_data_salmon_WR$Date)>9,year(salvage_data_salmon_WR$Date)+1,year(salvage_data_salmon_WR$Date))

salvage_data_salmon_SR$Date<-as.Date(salvage_data_salmon_SR$Sample.Date)
salvage_data_salmon_SR$wateryear<-ifelse(month(salvage_data_salmon_SR$Date)>9,year(salvage_data_salmon_SR$Date)+1,year(salvage_data_salmon_SR$Date))

salvage_data_steelhead$Date<-as.Date(salvage_data_steelhead$Sample.Date)
salvage_data_steelhead$wateryear<-ifelse(month(salvage_data_steelhead$Date)>9,year(salvage_data_steelhead$Date)+1,year(salvage_data_steelhead$Date))

sum_salmon_wr <- salvage_data_salmon_WR %>% group_by(wateryear, Species) %>% summarise(Loss=sum(Loss)) %>% left_join(.,abundance_data) %>%
  filter(wateryear>1993&wateryear<2023)

sum_salmon_sr <- salvage_data_salmon_SR %>% group_by(wateryear, Species) %>% summarise(Loss=sum(Loss)) %>%
  filter(wateryear>1993&wateryear<2023)

sum_salmon_sth <- salvage_data_steelhead %>% group_by(wateryear, Species) %>% summarise(Loss=sum(Loss)) %>%
  filter(wateryear>1993&wateryear<2023)

annual_loss_winterrun<-ggplot(data=sum_salmon_wr, aes(x=wateryear, y=Loss)) +
  ggplot2::geom_bar(stat="identity")+ facet_grid(rows = vars(Species), scales = "free")+
  theme_bw()+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 10),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::scale_x_continuous(breaks=seq(1994,2022,2)) +
  ggplot2::labs(title=NULL,
       x ="Water year", y = "Loss at the salvage facilities")

annual_loss_winterrun

annual_JPE_winterrun<-ggplot(data=sum_salmon_wr,aes(x=wateryear, y=Abundance)) +
  ggplot2::geom_bar(stat="identity")+ facet_grid(rows = vars(Species), scales = "free")+
  theme_bw()+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 10),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::scale_x_continuous(breaks=seq(1994,2022,2), limits=c(1994,2022)) +
  ggplot2::labs(title=NULL,
                x ="Water year", y = "JPE")+
  ggplot2::geom_text(
    aes(x=wateryear,y=0,label = Text),
    vjust = 0.1,
    hjust = 0.1, angle=90)

annual_JPE_winterrun

annual_loss_springrun<-ggplot(data=sum_salmon_sr, aes(x=wateryear, y=Loss)) +
  ggplot2::geom_bar(stat="identity")+ facet_grid(rows = vars(Species), scales = "free")+
  theme_bw()+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 10),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::scale_x_continuous(breaks=seq(1994,2022,2)) +
  ggplot2::labs(title=NULL,
                x ="Water year", y = "Loss at the salvage facilities")

annual_loss_springrun

#Print figure
tiff(filename=file.path(output_root,"Figure_ChinookSalmon_salvage.tiff"),
     type="cairo",
     units="in", 
     width=10, #10*1, 
     height=8, #22*1, 
     pointsize=5, #12, 
     res=500,
     compression="lzw")
grid.arrange(annual_JPE_winterrun,annual_loss_winterrun,annual_loss_springrun, ncol=1,nrow=3)

dev.off()

##Steelhead

annual_loss_steelhead<-ggplot(data=sum_salmon_sth, aes(x=wateryear, y=Loss)) +
  ggplot2::geom_bar(stat="identity")+ facet_grid(rows = vars(Species), scales = "free")+
  theme_bw()+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 10),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::scale_x_continuous(breaks=seq(1994,2022,2)) +
  ggplot2::labs(title=NULL,
                x ="Water year", y = "Loss at the salvage facilities")

annual_loss_steelhead

#Print figure
tiff(filename=file.path(output_root,"Figure_Steelhead_salvage.tiff"),
     type="cairo",
     units="in", 
     width=10, #10*1, 
     height=4, #22*1, 
     pointsize=5, #12, 
     res=500,
     compression="lzw")
annual_loss_steelhead
dev.off()


##############
#Read Delta Smelt, Longfin Smelt, and Green Sturgeon data

salvage_data_longfin<-read.csv(file.path(data_root, "Abundance Data", "SalvageExport_LongfinSmelt.csv")) %>% mutate(Species="Longfin Smelt")
salvage_data_deltasmelt<-read.csv(file.path(data_root, "Abundance Data", "SalvageExport_DeltaSmelt.csv")) %>% mutate(Species="Delta Smelt")
salvage_data_greensturgeon<-read.csv(file.path(data_root, "Abundance Data", "SalvageExport_GreenSturgeon.csv")) %>% mutate(Species="Green Sturgeon")

salvage_data_nonsalmonids<-bind_rows(salvage_data_longfin,salvage_data_deltasmelt,salvage_data_greensturgeon)

remove(salvage_data_longfin,salvage_data_deltasmelt,salvage_data_greensturgeon)

salvage_data_nonsalmonids$Date<-as.Date(salvage_data_nonsalmonids$Sample.Date)
salvage_data_nonsalmonids$wateryear<-ifelse(month(salvage_data_nonsalmonids$Date)>9,year(salvage_data_nonsalmonids$Date)+1,year(salvage_data_nonsalmonids$Date))

salvage_data_nonsalmonids$Salvage<-ifelse(salvage_data_nonsalmonids$Salvage=="NS",0,salvage_data_nonsalmonids$Salvage)
unique(salvage_data_nonsalmonids$Salvage)
salvage_data_nonsalmonids$Salvage<-as.numeric(salvage_data_nonsalmonids$Salvage)

sum_nonsalmonids <- salvage_data_nonsalmonids %>% group_by(wateryear, Species) %>% summarise(Salvage=sum(Salvage)) %>% left_join(.,abundance_data) %>%
  filter(wateryear>1993&wateryear<2023)

##Longfin Smelt

annual_loss_longfinsmelt<-ggplot(data=sum_nonsalmonids %>% filter(Species=="Longfin Smelt"), aes(x=wateryear, y=Salvage)) +
  ggplot2::geom_bar(stat="identity")+ facet_grid(rows = vars(Species), scales = "free")+
  theme_bw()+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 10),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::scale_x_continuous(breaks=seq(1994,2022,2)) +
  ggplot2::labs(title=NULL,
                x ="Water year", y = "Expanded salvage at the salvage facilities")

annual_loss_longfinsmelt

annual_index_longfinsmelt<-ggplot(data=sum_nonsalmonids %>% filter(Species=="Longfin Smelt"),aes(x=wateryear, y=Abundance)) +
  ggplot2::geom_bar(stat="identity")+ facet_grid(rows = vars(Species), scales = "free")+
  theme_bw()+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 10),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::scale_x_continuous(breaks=seq(1994,2022,2), limits=c(1994,2022)) +
  ggplot2::labs(title=NULL,
                x ="Water year", y = "FMWT Index")+
  ggplot2::geom_text(
    aes(x=wateryear,y=0,label = Text),
    vjust = 0.1,
    hjust = 0.1, angle=90)

annual_index_longfinsmelt

#Print figure
tiff(filename=file.path(output_root,"Figure_LongfinSmelt_salvage.tiff"),
     type="cairo",
     units="in", 
     width=10, #10*1, 
     height=8, #22*1, 
     pointsize=5, #12, 
     res=500,
     compression="lzw")
grid.arrange(annual_index_longfinsmelt,annual_loss_longfinsmelt, ncol=1,nrow=2)

dev.off()


##Green Sturgeon

annual_loss_greensturgeon<-ggplot(data=sum_nonsalmonids %>% filter(Species=="Green Sturgeon"), aes(x=wateryear, y=Salvage)) +
  ggplot2::geom_bar(stat="identity")+ facet_grid(rows = vars(Species), scales = "free")+
  theme_bw()+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 10),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::scale_x_continuous(breaks=seq(1994,2022,2)) +
  ggplot2::labs(title=NULL,
                x ="Water year", y = "Expanded salvage at the salvage facilities")

annual_loss_greensturgeon

#Print figure
tiff(filename=file.path(output_root,"Figure_GreenSturgeon_salvage.tiff"),
     type="cairo",
     units="in", 
     width=10, #10*1, 
     height=4, #22*1, 
     pointsize=5, #12, 
     res=500,
     compression="lzw")
annual_loss_greensturgeon
dev.off()


##Delta Smelt

annual_loss_deltasmelt<-ggplot(data=sum_nonsalmonids %>% filter(Species=="Delta Smelt") %>% group_by(wateryear,Species) %>%
                                 summarise(Salvage=mean(Salvage)), aes(x=wateryear, y=Salvage)) +
  ggplot2::geom_bar(stat="identity")+ facet_grid(rows = vars(Species), scales = "free")+
  theme_bw()+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 10),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::scale_x_continuous(breaks=seq(1994,2022,2)) +
  ggplot2::labs(title=NULL,
                x ="Water year", y = "Expanded salvage at the salvage facilities")

annual_loss_deltasmelt

#Print figure
tiff(filename=file.path(output_root,"Figure_DeltaSmelt_salvage.tiff"),
     type="cairo",
     units="in", 
     width=10, #10*1, 
     height=4, #22*1, 
     pointsize=5, #12, 
     res=500,
     compression="lzw")
annual_loss_deltasmelt
dev.off()

annual_index_deltasmelt<-ggplot(data=sum_nonsalmonids %>% filter(Species=="Delta Smelt"),aes(x=wateryear, y=Abundance)) +
  ggplot2::geom_bar(stat="identity")+ facet_grid(rows = vars(Survey), scales = "free")+
  theme_bw()+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 10),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::scale_x_continuous(breaks=seq(1994,2022,2), limits=c(1994,2022)) +
  ggplot2::labs(title=NULL,
                x ="Water year", y = "Delta Smelt abundance indices")+
  ggplot2::geom_text(
    aes(x=wateryear,y=0,label = Text),
    vjust = 0.1,
    hjust = 0.1, angle=90)

annual_index_deltasmelt

#Print figure
tiff(filename=file.path(output_root,"Figure_DeltaSmelt_indices.tiff"),
     type="cairo",
     units="in", 
     width=10, #10*1, 
     height=8, #22*1, 
     pointsize=5, #12, 
     res=500,
     compression="lzw")
annual_index_deltasmelt
dev.off()