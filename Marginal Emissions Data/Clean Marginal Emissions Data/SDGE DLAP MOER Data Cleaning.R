#### Script Description Header ####

# File Name: SDGE DLAP MOER Data Cleaning.R
# File Location: "~/Desktop/SDG&E JARP/Marginal Emissions Data/Clean Marginal Emissions Data"
# Project: San Diego JARP Supplementary Testimony
# Description: Cleans marginal emissions data for use in SDG&E JARP modeling.

#### Load Packages
library(tidyverse)
library(lubridate)
library(jsonlite)

# Disable Scientific Notation
options(scipen = 999)

# Set Working Directories
setwd("~/Desktop/SDG&E JARP/Marginal Emissions Data/Clean Marginal Emissions Data")
Code_WD <- getwd()

setwd("../Raw Marginal Emissions Data")
Data_WD <- getwd()


#### Load and Clean WattTime SGIP Historical GHG Data ####
# Data is for SDG&E DLAP
# Source: sgipsignal.com/sgipmoer/?ba=SGIP_CAISO_SDGE

WattTime_Files <- list.files(pattern = ".json")

Raw_WattTime_Joined <- data.frame(point_time = character(), moer = numeric(), version = numeric(), 
                                  freq = numeric(), ba = character(), stringsAsFactors = F)

for(WattTime_File in WattTime_Files){
  
  Raw_WattTime_Single <- read_json(WattTime_File, simplifyVector = TRUE)
  
  Raw_WattTime_Joined <- rbind(Raw_WattTime_Joined, Raw_WattTime_Single)
  
}

rm(Raw_WattTime_Single, WattTime_File, WattTime_Files)

Clean_WattTime_Joined <- Raw_WattTime_Joined %>%
  mutate(Date_Time = as.POSIXct(gsub("T", " ", substr(point_time, 1, 16)), tz = "UTC")) %>%
  mutate(Date_Time = with_tz(Date_Time, tz = "America/Los_Angeles")) %>%
  select(Date_Time, moer) %>%
  group_by(Date_Time) %>%
  summarize(moer = mean(moer)) # Remove duplicates by averaging.

rm(Raw_WattTime_Joined)

# 15 datapoints are missing
Clean_SDGE_SGIP_MOER <- data.frame(Date_Time = seq.POSIXt(from = as.POSIXct("2019-01-01 00:00", tz = "America/Los_Angeles"),
                                                         to = as.POSIXct("2019-12-31 23:55", tz = "America/Los_Angeles"),
                                                         by = "5 min")) %>%
  left_join(Clean_WattTime_Joined, by = "Date_Time") %>%
  mutate(moer = ifelse(is.na(moer), lag(moer, 12*24*7), moer)) # Fill with data from prior week.

rm(Clean_WattTime_Joined)


#### Convert to 15-Minute Data ####

Start_Time_15_min = Clean_SDGE_SGIP_MOER$Date_Time[1]

End_Time_15_min = Clean_SDGE_SGIP_MOER$Date_Time[nrow(Clean_SDGE_SGIP_MOER)]

Time_Vector_15_min <- data.frame(Date_Time_15_min = seq.POSIXt(Start_Time_15_min, End_Time_15_min, by = "15 min"))

Clean_SDGE_SGIP_MOER <- Clean_SDGE_SGIP_MOER %>%
  mutate(Date_Time = findInterval(Date_Time, Time_Vector_15_min$Date_Time_15_min)) %>%
  mutate(Date_Time = Time_Vector_15_min$Date_Time_15_min[Date_Time]) %>%
  group_by(Date_Time) %>%
  summarize(moer = mean(moer)) %>%
  ungroup()

rm(Start_Time_15_min, End_Time_15_min, Time_Vector_15_min)


#### Save Final Dataset ####
setwd(Code_WD)
saveRDS(Clean_SDGE_SGIP_MOER, "Clean_SDGE_SGIP_MOER.rds")