# Identify your working directory for saving outputs of interest
root <- "~/GitHub/public_draft_alternatives_BM"
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")

# The following libraries need to be installed and loaded
# NOTE: You also need to have HEC-DSSVue installed on your computer
# See: https://www.hec.usace.army.mil/software/hec-dssvue/downloads.aspx

library(tidyverse)
library(stringr)
library(lubridate)
library(rJava)

############
# Read dayflow data
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

# Add Water Year information

#Data from https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST
#Copy and pasted into excel
data_wateryear<-read.csv(file.path(data_root, "Dayflow", "DWR_WSIHIST.csv"))

#Add WY type information
data_dayflow<-left_join(data_dayflow,data_wateryear[,c("WY","Sac_WY")])

#############
#Read DSS file

# The following function for is used for turning CalSim time stamps into R dates. 

from_time_stamp <- function(x) {
  day_ref <- as.Date("1899-12-30")
  return(day_ref+x/1440)
}



# Run this workaround if your R session crashes when running .jinit() - below
  # This issue occurs in base R versions 4.2 and later
  # In lieu of this workaround, you can also install a patched R version 
    # E.g., https://cran.r-project.org/bin/windows/base/rpatched.html

replacement <- function(category = "LC_ALL") {
  
  if (identical(category, "LC_MESSAGES"))
    return("")
  
  category <- match(category, .LC.categories)
  if (is.na(category)) 
    stop("invalid 'category' argument")
  .Internal(Sys.getlocale(category))
  
}
base <- asNamespace("base")
environment(replacement) <- base
unlockBinding("Sys.getlocale", base)
assign("Sys.getlocale", replacement, envir = base)
lockBinding("Sys.getlocale", base)


# This code establishes your java connection with HEC-DSSVue

  # Specify your own location for 'HEC-DSSVue'
dss_location <- "C:\\Program Files\\HEC\\HEC-DSSVue\\" 

  # Specify your own location for the 'jar' sub-folder
    # This identifies all possible java executables that meet be needed
jars <- c(list.files("C:\\Program Files\\HEC\\HEC-DSSVue\\jar")) 

jars <- paste0(dss_location, "jar/", jars)

  # Specify your own location for the 'lib' sub-folder
libs <- "-Djava.library.path=C:\\Program Files\\HEC\\HEC-DSSVue\\lib\\"

.jinit(classpath = jars, parameters = libs)

##########################################
# D1641 alternative

# Identify the DSS file you want to access

dss_input <- 'D:\\2022-11-30 - DSS File 2021 LTO ROC\\D1641\\D1641_8.dss'
# Open the DSS file through rJava

dssFile <- .jcall("hec/heclib/dss/HecDss", "Lhec/heclib/dss/HecDss;",   method="open", dss_input)

# From this point on, you can organize the data as you see fit (matrices, arrays, data frames)
  # You can also set up loops to extract and aggregate data from several nodes

#Required Delta Outflow
rmt.java.req.OUT <- dssFile$get("/CALSIM/D407/FLOW-DELIVERY//1MON/2020D09E/") 
req.outflow=data.frame(Date=rmt.java.req.OUT$times %>% from_time_stamp,requiredOUT=rmt.java.req.OUT$values)
#Excess Delta Outflow
rmt.java.exc.OUT <- dssFile$get("/CALSIM/C407/FLOW-CHANNEL//1MON/2020D09E/") 
exc.outflow=data.frame(Date=rmt.java.exc.OUT$times %>% from_time_stamp,excessOUT=rmt.java.exc.OUT$values)

DeltaOutflow_D1641<-left_join(req.outflow,exc.outflow) %>% mutate(DeltaOutflow=requiredOUT+excessOUT) %>% select(Date,DeltaOutflow) %>%
  rename(DeltaOutflow_D1641=DeltaOutflow)


##########################################
# NAA alternative
# Identify the DSS file you want to access

dss_input <- 'D:\\2022-11-30 - DSS File 2021 LTO ROC\\NAA\\Benchmark_2035CT_0117_DV.dss'

# Open the DSS file through rJava

dssFile <- .jcall("hec/heclib/dss/HecDss", "Lhec/heclib/dss/HecDss;",   method="open", dss_input)

# From this point on, you can organize the data as you see fit (matrices, arrays, data frames)
# You can also set up loops to extract and aggregate data from several nodes

#Required Delta Outflow
rmt.java.req.OUT <- dssFile$get("/CALSIM/D407/FLOW-DELIVERY//1MON/2020D09E/") 
req.outflow=data.frame(Date=rmt.java.req.OUT$times %>% from_time_stamp,requiredOUT=rmt.java.req.OUT$values)
#Excess Delta Outflow
rmt.java.exc.OUT <- dssFile$get("/CALSIM/C407/FLOW-CHANNEL//1MON/2020D09E/") 
exc.outflow=data.frame(Date=rmt.java.exc.OUT$times %>% from_time_stamp,excessOUT=rmt.java.exc.OUT$values)

DeltaOutflow_NAA<-left_join(req.outflow,exc.outflow) %>% mutate(DeltaOutflow=requiredOUT+excessOUT) %>% select(Date,DeltaOutflow) %>%
  rename(DeltaOutflow_NAA=DeltaOutflow)


#######
#Combine results
DeltaOutflow<-left_join(DeltaOutflow_D1641,DeltaOutflow_NAA) %>% mutate(Year=year(Date),Month=month(Date)) %>% 
  #Filter June-August per Smith et al. 2021
  filter(Month %in% c(6:8)) %>% 
  #Filter year 1995-2016 for Delta Smelt LCM
  filter(Year %in% c(1993:2016)) %>%
  #summarize by year
  group_by(Year) %>% summarise(DeltaOutflow_D1641=mean(DeltaOutflow_D1641),
                               DeltaOutflow_NAA=mean(DeltaOutflow_NAA))

data_dayflow_sum<- data_dayflow %>%
  #Filter June-August per Smith et al. 2021
  filter(Month %in% c(6:8)) %>% 
  #Filter year 1995-2016 for Delta Smelt LCM
  filter(Year %in% c(1993:2016)) %>%
  #summarize by month, then year
  group_by(Year,Month) %>% summarise(OUT=mean(OUT)) %>%
  group_by(Year) %>% summarise(DeltaOutflow_dayflow=mean(OUT))

DeltaOutflow<-left_join(DeltaOutflow,data_dayflow_sum)

#Produce ratio per Will's instructions
DeltaOutflow_final<-DeltaOutflow
DeltaOutflow_final <- DeltaOutflow_final %>%
  mutate(
    across(c(2:4),
           .fns = ~./DeltaOutflow_dayflow))
