#### Script Description Header ####

# File Name: SDG&E JARP Interval Meter Data Cleaning.R
# File Location: "~/Desktop/SDG&E JARP/Wholesale Price Data/Clean Wholesale Price Data"
# Project: San Diego JARP Supplementary Testimony
# Description: Cleans wholeale energy price data for use in SDG&E JARP modeling.

#### User Inputs ####

Data_Year <- 2019

Time_Interval_Minutes <- 15


#### Load Packages ####
library(tidyverse)
library(lubridate)

# Disable Scientific Notation
options(scipen = 999)

# Set Working Directories
setwd("~/Desktop/SDG&E JARP Supplementary Testimony Workpapers/Wholesale Price Data/Clean Wholesale Price Data")
Code_WD <- getwd()

setwd("../")
setwd(file.path("Raw Wholesale Price Data", Data_Year))
Data_WD <- getwd()


#### Load and Clean CAISO RT5M Wholesale Price Data ####
# Data is for SDG&E DLAP, Real-Time 5-Minute Market
# Source: http://oasis.caiso.com/ Interval Locational Marginal Prices (LMP) DLAP_SDGE-APND

CAISO_RT5M_Files <- list.files(pattern = ".csv")

Raw_CAISO_RT5M_Joined <- data.frame(INTERVALSTARTTIME_GMT = character(), VALUE = numeric(), stringsAsFactors = F)

for(CAISO_RT5M_File in CAISO_RT5M_Files){
  
  Raw_CAISO_RT5M_Single <- read.csv(CAISO_RT5M_File) %>%
    filter(XML_DATA_ITEM == "LMP_PRC") %>%
    select(INTERVALSTARTTIME_GMT, VALUE)
  
  Raw_CAISO_RT5M_Joined <- rbind(Raw_CAISO_RT5M_Joined, Raw_CAISO_RT5M_Single)
  
}

rm(Raw_CAISO_RT5M_Single, CAISO_RT5M_File, CAISO_RT5M_Files)

Clean_SDGE_RT5M_LMP <- Raw_CAISO_RT5M_Joined %>%
  mutate(Date_Time = as.POSIXct(gsub("T", " ", substr(INTERVALSTARTTIME_GMT, 1, 16)), tz = "UTC")) %>%
  mutate(Date_Time = with_tz(Date_Time, tz = "America/Los_Angeles")) %>%
  mutate(LMP_RT5M = VALUE/1000) %>% # Convert from $/MWh to $/kWh
  select(Date_Time, LMP_RT5M) %>%
  arrange(Date_Time)

rm(Raw_CAISO_RT5M_Joined)


#### Convert to New Time Resolution ####

if(Time_Interval_Minutes != 5){
  
  Start_Time_Aggregated <- Clean_SDGE_RT5M_LMP$Date_Time[1]
  
  End_Time_Aggregated <- Clean_SDGE_RT5M_LMP$Date_Time[nrow(Clean_SDGE_RT5M_LMP)]
  
  Time_Vector_Aggregated <- data.frame(Date_Time_Aggregated = seq.POSIXt(Start_Time_Aggregated, End_Time_Aggregated, by = paste(Time_Interval_Minutes, "min")))
  
  Clean_SDGE_RT5M_LMP <- Clean_SDGE_RT5M_LMP %>%
    mutate(Date_Time = findInterval(Date_Time, Time_Vector_Aggregated$Date_Time_Aggregated)) %>%
    mutate(Date_Time = Time_Vector_Aggregated$Date_Time_Aggregated[Date_Time]) %>%
    group_by(Date_Time) %>%
    summarize(LMP_RT5M = mean(LMP_RT5M, na.rm = T)) %>%
    ungroup()
  
  rm(Start_Time_Aggregated, End_Time_Aggregated, Time_Vector_Aggregated)
  
}


#### Save Final Dataset ####
setwd(Code_WD)
saveRDS(Clean_SDGE_RT5M_LMP, paste0("Clean_SDGE_RT5M_LMP_",Data_Year,"_",Time_Interval_Minutes, "_min.rds"))
write.csv(Clean_SDGE_RT5M_LMP, paste0("Clean_SDGE_RT5M_LMP_",Data_Year,"_",Time_Interval_Minutes, "_min.csv"), row.names = F)