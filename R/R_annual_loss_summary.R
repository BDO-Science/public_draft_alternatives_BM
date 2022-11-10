library(tidyverse)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)
library(readxl)

##################### Load Salvage Count Data from SacPAS

#Function adjusted from Trinh's code to pull salvage datasets from SacPAS

pull_salvage_salmon <- function(salvageURL = "http://www.cbr.washington.edu/sacramento/data/query_loss_detail.html") {
  startingSession <- session(salvageURL)
  startingForm <- html_form(startingSession)[[1]]
  
  df <- lapply(c(1993:2022), function(x) {
    filledForm <- set_values(startingForm,
                             year = x,
                             species = "1:f")
    
    submittedFormURL <- suppressMessages(submit_form(session = startingSession, 
                                                     form = filledForm, POST = salvageURL)$url)
    
    csvLink <- submittedFormURL
    
    if (length(csvLink) == 0) {
      return(NULL)
    } else {
      csvDownload <- csvLink
    }
    
    df <- csvDownload %>% 
      read_csv() %>% filter(!is.na(nfish)) }) %>%
    bind_rows() 
  df
}

#Run actual function to load data for salmon
salvage_data_salmon <- suppressWarnings(pull_salvage_salmon())

salvage_data_salmon$Date<-as.Date(salvage_data_salmon$'Sample Time')

#add year
salvage_data_salmon$wateryear<-ifelse(month(salvage_data_salmon$Date)>9,year(salvage_data_salmon$Date)+1,year(salvage_data_salmon$Date))

#sum loss by year
sum_salmon<-salvage_data_salmon %>% group_by(wateryear,Species,`LAD Race`) %>% summarise(Loss=sum(`LAD Loss`)) %>%
  mutate(Species=case_when(`LAD Race` == "Winter" ~ "Chinook Salmon - winter-run LAD",
            `LAD Race` == "Spring" ~ "Chinook Salmon - spring-run LAD")) %>% select(-`LAD Race`)

sum_salmon <- sum_salmon[complete.cases(sum_salmon), ]

############
#Acquire steelhead loss data

pull_salvage_sth <- function(salvageURL = "http://www.cbr.washington.edu/sacramento/data/query_loss_detail.html") {
  startingSession <- session(salvageURL)
  startingForm <- html_form(startingSession)[[1]]
  
  df <- lapply(c(1993:2022), function(x) {
    filledForm <- set_values(startingForm,
                             year = x,
                             species = "2:f")
    
    submittedFormURL <- suppressMessages(submit_form(session = startingSession, 
                                                     form = filledForm, POST = salvageURL)$url)
    
    csvLink <- submittedFormURL
    
    if (length(csvLink) == 0) {
      return(NULL)
    } else {
      csvDownload <- csvLink
    }
    
    df <- csvDownload %>% 
      read_csv() %>% filter(!is.na(nfish)) }) %>%
    bind_rows() 
  df
}

salvage_data_steelhead <- suppressWarnings(pull_salvage_sth())
salvage_data_steelhead$Date<-as.Date(salvage_data_steelhead$'Sample Time')

#add year
salvage_data_steelhead$wateryear<-ifelse(month(salvage_data_steelhead$Date)>9,year(salvage_data_steelhead$Date)+1,year(salvage_data_steelhead$Date))

#sum loss by year
sum_steelhead<-salvage_data_steelhead %>% group_by(wateryear,Species) %>% summarise(Loss=sum(Loss))


##########
#Combine species
sum_all<-bind_rows(sum_steelhead,sum_salmon)

annual_loss_plot<-ggplot(data=sum_all, aes(x=wateryear, y=Loss)) +
  geom_bar(stat="identity")+ facet_grid(rows = vars(Species), scales = "free")+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::ylab("Loss at the salvage facilities")+
  ggplot2::xlab("Water year")

annual_loss_plot
