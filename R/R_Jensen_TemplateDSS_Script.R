# Identify your working directory for saving outputs of interest
root <- "~/GitHub/public_draft_alternatives_BM"
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")

# The following libraries need to be installed and loaded
  # NOTE: You also need to have HEC-DSSVue installed on your computer
    # See: https://www.hec.usace.army.mil/software/hec-dssvue/downloads.aspx

require(plyr); require(dplyr); require(stringr); require(lubridate); require(rJava); require(ggplot2); 
require(tidyr)


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


# Identify the DSS file you want to access

dss_input <- 'C:\\Users\\ajensen\\OneDrive - DOI\\Documents\\LTO\\Life Cycle Model Study Plan\\Modeling Input Data\\D1641\\D1641_8.dss'
dss_input <- 'D:\\2022-11-30 - DSS File 2021 LTO ROC\\D1641\\D1641_8.dss'


# Open the DSS file through rJava

dssFile <- .jcall("hec/heclib/dss/HecDss", "Lhec/heclib/dss/HecDss;",   method="open", dss_input)


# Extract data from specific CalSim nodes

  # This pulls CalSim data for node C134, which is a channel flow output, for all months/years
    # You can view the sub-file naming structure (e.g., CALSIM, C134, FLOW-CHANNEL, etc.)
      # by viewing the DSS file in HEC-DSSVue
    # The blank between FLOW-CHANNEL and 1MON tells HEC-DSSVue to pull data for all months and years
rmt.java <- dssFile$get("/CALSIM/C134/FLOW-CHANNEL//1MON/2020D09E/") 

  # If you want to pull data for only 1 decade, you can specify the starting year and month
# dssFile$get("/CALSIM/C134/FLOW-CHANNEL/01JAN1920/1MON/2020D09E/") 

  # Pull out CalSim output values (in this case, flow in cfs)
flows <- rmt.java$values

  # Pull out CalSim dates, and convert to an R date format using the already created function
dates <- rmt.java$times %>% from_time_stamp %>% str()


# From this point on, you can organize the data as you see fit (matrices, arrays, data frames)
  # You can also set up loops to extract and aggregate data from several nodes

#SAC flow at Freeport
rmt.java.SAC <- dssFile$get("/CALSIM/C169/FLOW-CHANNEL//1MON/2020D09E/") 

SACdata=data.frame(Date=rmt.java.SAC$times %>% from_time_stamp,SAC=rmt.java.SAC$values)

#SJR flow at Vernalis
rmt.java.SJR <- dssFile$get("/CALSIM/C639/FLOW-CHANNEL//1MON/2020D09E/") 
SJRdata <- data.frame(Date=rmt.java.SJR$times %>% from_time_stamp, SJR=rmt.java.SJR$values)

#YOLO flow at Vernalis
rmt.java.YOLO <- dssFile$get("/CALSIM/C157/FLOW-CHANNEL//1MON/2020D09E/") 
YOLOdata<-data.frame(YOLO=rmt.java.YOLO$values,
                     Date=rmt.java.YOLO$times %>% from_time_stamp)

#EXPORT from Delta
rmt.java.EXPORT <- dssFile$get("/CALSIM/D409/FLOW-DELIVERY//1MON/2020D09E/") 

EXPORTdata<-data.frame(EXPORT=rmt.java.EXPORT$values,
                     Date=rmt.java.EXPORT$times %>% from_time_stamp )

CalSim_D1641<- left_join(SJRdata,SACdata) %>% left_join(YOLOdata) %>% left_join(EXPORTdata)

##########################################
# NAA alternative
# Identify the DSS file you want to access

dss_input <- 'D:\\2022-11-30 - DSS File 2021 LTO ROC\\NAA\\Benchmark_2035CT_0117_DV.dss'


# Open the DSS file through rJava

dssFile <- .jcall("hec/heclib/dss/HecDss", "Lhec/heclib/dss/HecDss;",   method="open", dss_input)


# Extract data from specific CalSim nodes

# This pulls CalSim data for node C134, which is a channel flow output, for all months/years
# You can view the sub-file naming structure (e.g., CALSIM, C134, FLOW-CHANNEL, etc.)
# by viewing the DSS file in HEC-DSSVue
# The blank between FLOW-CHANNEL and 1MON tells HEC-DSSVue to pull data for all months and years
rmt.java <- dssFile$get("/CALSIM/C134/FLOW-CHANNEL//1MON/2020D09E/") 

# If you want to pull data for only 1 decade, you can specify the starting year and month
# dssFile$get("/CALSIM/C134/FLOW-CHANNEL/01JAN1920/1MON/2020D09E/") 

# Pull out CalSim output values (in this case, flow in cfs)
flows <- rmt.java$values

# Pull out CalSim dates, and convert to an R date format using the already created function
dates <- rmt.java$times %>% from_time_stamp %>% str()


# From this point on, you can organize the data as you see fit (matrices, arrays, data frames)
# You can also set up loops to extract and aggregate data from several nodes

#SAC flow at Freeport
rmt.java.SAC <- dssFile$get("/CALSIM/C169/FLOW-CHANNEL//1MON/2020D09E/") 

SACdata=data.frame(Date=rmt.java.SAC$times %>% from_time_stamp,SAC=rmt.java.SAC$values)

#SJR flow at Vernalis
rmt.java.SJR <- dssFile$get("/CALSIM/C639/FLOW-CHANNEL//1MON/2020D09E/") 
SJRdata <- data.frame(Date=rmt.java.SJR$times %>% from_time_stamp, SJR=rmt.java.SJR$values)

#YOLO flow at Vernalis
rmt.java.YOLO <- dssFile$get("/CALSIM/C157/FLOW-CHANNEL//1MON/2020D09E/") 
YOLOdata<-data.frame(YOLO=rmt.java.YOLO$values,
                     Date=rmt.java.YOLO$times %>% from_time_stamp)

#EXPORT from Delta
rmt.java.EXPORT <- dssFile$get("/CALSIM/D409/FLOW-DELIVERY//1MON/2020D09E/") 

EXPORTdata<-data.frame(EXPORT=rmt.java.EXPORT$values,
                       Date=rmt.java.EXPORT$times %>% from_time_stamp )

CalSim_NAA<- left_join(SJRdata,SACdata) %>% left_join(YOLOdata) %>% left_join(EXPORTdata)


######## CSV output
write.csv(CalSim_D1641,file=file.path(output_root,"CalSim2Example_D1641.csv"),row.names=F)
write.csv(CalSim_NAA,file=file.path(output_root,"CalSim2Example_NAA.csv"),row.names=F)
