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
##Longfin
salvage_data_longfin<-read.csv(file.path(data_root, "Abundance Data", "SalvageExport_LongfinSmelt.csv")) %>% mutate(Sample.Date=as.Date(Sample.Date)) %>%
  mutate(Salvage = as.numeric(ifelse(Salvage=="NS",0,Salvage))) %>% group_by(Sample.Date) %>% 
  summarise(Salvage=sum(Salvage)) %>%
  mutate(WY=ifelse(month(Sample.Date)>9,year(Sample.Date)+1,year(Sample.Date)))

longfin_date <- salvage_data_longfin %>% filter(Salvage>0) %>% group_by(WY) %>% summarise(Longfin_Salvage=min(Sample.Date))

##Delta Smelt
salvage_data_dsm <-read.csv(file.path(data_root, "Abundance Data", "SalvageExport_DeltaSmelt.csv")) %>% mutate(Sample.Date=as.Date(Sample.Date)) %>%
  mutate(Salvage = as.numeric(ifelse(Salvage=="NS",0,Salvage))) %>% group_by(Sample.Date) %>% 
  summarise(Salvage=sum(Salvage)) %>%
  mutate(WY=ifelse(month(Sample.Date)>9,year(Sample.Date)+1,year(Sample.Date)))

deltasmelt_date <- salvage_data_dsm %>% filter(Salvage>0) %>% group_by(WY) %>% summarise(DeltaSmelt_Salvage=min(Sample.Date))


##Steelhead

salvage_data_steelhead<-read.csv(file.path(data_root, "Abundance Data", "SalvageExport_Steelhead.csv")) %>% mutate(Sample.Date=as.Date(Sample.Date)) %>% group_by(Sample.Date) %>% 
  summarise(Loss=sum(Loss)) %>%
  mutate(WY=ifelse(month(Sample.Date)>9,year(Sample.Date)+1,year(Sample.Date)))

#Split data by WY to calculate cumsum
sth_data <- split(salvage_data_steelhead, f = salvage_data_steelhead$WY)

sth_data <- lapply(sth_data, function(x) {x %>% mutate(CumulativeLoss = cumsum(Loss))} )

#Re-merge data
sth_data <- do.call("rbind", sth_data)

steelhead_date<- sth_data %>% filter(CumulativeLoss >= 3000*0.05) %>% group_by(WY) %>% summarise(Steelhead_Salvage=min(Sample.Date))


##Genetic Winter-run
### Read Kevin Reece's Jan 13, 2023 genetic data

genetic_data_WR <-read_excel(file.path(data_root,"Abundance Data","WY1996-2022 Genetically Confirmed WR_CVP and SWP salvage and loss for OMR analysis_20230113Final.xlsx"), sheet = "WY1996-2022 JUVWR Genetic_ID", range = cell_cols("A:K")) %>% 
  rename(SampleDate='Sample Date',SampleTime='Sample Time',FL=Forklength,Facility=Loc,Salvage_KevinReece=Salvage,Loss_KevinReece=Loss,LAD_KevinReece='Model Race',GeneticAssignment=Assignment) %>%
  mutate(SampleTime=format(SampleTime, "%H:%M:%S")) %>% mutate(SampleDate=as.Date(SampleDate)) %>%
  mutate(SampleDateTime=as.POSIXct(paste(SampleDate, SampleTime), format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")) %>% select(-Catch) %>%
  rename(WY=WaterYear)

wr_genetic_date<- genetic_data_WR %>% group_by(WY) %>% summarise(WinterRunGenetic_Salvage=min(SampleDate))


## WRML model onramp
onramp <- read.csv(file.path(data_root,"OMR onramp","Alt4onramp.csv")) %>% mutate(WinterRunLAD_Model=as.Date(WinterRunLAD_Model, format="%m/%d/%Y"))


#join data
onramp_full <- onramp %>% left_join(wr_genetic_date) %>% left_join(steelhead_date) %>% left_join(longfin_date) %>% left_join(deltasmelt_date) %>%
   mutate(onramp_date = pmin(WinterRunLAD_Model,WinterRunGenetic_Salvage,Steelhead_Salvage,Longfin_Salvage,DeltaSmelt_Salvage, na.rm = TRUE))

write.csv(onramp_full,file.path(output_root,"Alt4_OnRamp_Example.csv"))
