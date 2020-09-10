#### Script Description Header ####

# File Name: CAISO Demand and Renewables Data Cleaning.R
# File Location: "~/Desktop/SDG&E JARP/CAISO Demand and Renewables Data/Clean Net Load Data"
# Project: San Diego JARP Supplementary Testimony
# Description: Cleans CAISO demand and renewable production data, and calculates net load, for use in SDG&E JARP modeling.

#### Load Packages
library(tidyverse)
library(lubridate)

# Disable Scientific Notation
options(scipen = 999)

# Set Working Directories
setwd("~/Desktop/SDG&E JARP/CAISO Demand and Renewables Data/Clean Net Load Data")
Code_WD <- getwd()

setwd("../Raw Demand Data")
CAISO_Demand_Data_WD <- getwd()

setwd("../Raw Renewables Data")
CAISO_Renewables_Data_WD <- getwd()


#### Load and Clean CAISO Demand Data ####

setwd(CAISO_Demand_Data_WD)

CAISO_Demand_Files <- list.files(pattern = ".csv")

Raw_CAISO_Demand_Joined <- data.frame(INTERVALSTARTTIME_GMT = character(), MW = numeric(), stringsAsFactors = F)

for(CAISO_Demand_File in CAISO_Demand_Files){
  
  Raw_CAISO_Demand_Single <- read.csv(CAISO_Demand_File) %>%
    filter(TAC_AREA_NAME == "CA ISO-TAC") %>%
    select(INTERVALSTARTTIME_GMT, MW)
  
  Raw_CAISO_Demand_Joined <- rbind(Raw_CAISO_Demand_Joined, Raw_CAISO_Demand_Single)
  
}

rm(Raw_CAISO_Demand_Single, CAISO_Demand_File, CAISO_Demand_Files)

Clean_CAISO_Demand_Joined <- Raw_CAISO_Demand_Joined %>%
  mutate(Date_Time = as.POSIXct(gsub("T", " ", substr(INTERVALSTARTTIME_GMT, 1, 16)), tz = "UTC")) %>%
  mutate(Date_Time = with_tz(Date_Time, tz = "America/Los_Angeles")) %>%
  mutate(CAISO_Demand_MW = MW) %>%
  select(Date_Time, CAISO_Demand_MW) %>%
  arrange(Date_Time)

rm(Raw_CAISO_Demand_Joined)


#### Load and Clean CAISO Renewables Data ####

setwd(CAISO_Renewables_Data_WD)

CAISO_Renewables_Files <- list.files(pattern = ".csv")

Raw_CAISO_Renewables_Joined <- data.frame(INTERVALSTARTTIME_GMT = character(), MW = numeric(), stringsAsFactors = F)

for(CAISO_Renewables_File in CAISO_Renewables_Files){
  
  Raw_CAISO_Renewables_Single <- read.csv(CAISO_Renewables_File) %>%
    select(INTERVALSTARTTIME_GMT, TRADING_HUB, RENEWABLE_TYPE, MW)
  
  Raw_CAISO_Renewables_Joined <- rbind(Raw_CAISO_Renewables_Joined, Raw_CAISO_Renewables_Single)
  
}

rm(Raw_CAISO_Renewables_Single, CAISO_Renewables_File, CAISO_Renewables_Files)

Clean_CAISO_Renewables_Joined <- Raw_CAISO_Renewables_Joined %>%
  mutate(Date_Time = as.POSIXct(gsub("T", " ", substr(INTERVALSTARTTIME_GMT, 1, 16)), tz = "UTC")) %>%
  mutate(Date_Time = with_tz(Date_Time, tz = "America/Los_Angeles")) %>%
  group_by(Date_Time) %>%
  summarize(CAISO_Renewables_MW = sum(MW)) %>% # Aggregate across trading hubs, renewables types.
  ungroup() %>%
  arrange(Date_Time)

rm(Raw_CAISO_Renewables_Joined)

# 11 datapoints are missing
Complete_CAISO_Renewables_Joined <- data.frame(Date_Time = seq.POSIXt(from = Clean_CAISO_Renewables_Joined$Date_Time[1],
                                                         to = Clean_CAISO_Renewables_Joined$Date_Time[nrow(Clean_CAISO_Renewables_Joined)],
                                                         by = "1 hour")) %>%
  left_join(Clean_CAISO_Renewables_Joined, by = "Date_Time") %>%
  mutate(CAISO_Renewables_MW = ifelse(is.na(CAISO_Renewables_MW), lag(CAISO_Renewables_MW, 24*7), CAISO_Renewables_MW)) # Fill with data from prior week.
  
rm(Clean_CAISO_Renewables_Joined)

#### Calculate Net Demand ####

CAISO_Net_Demand <- Clean_CAISO_Demand_Joined %>%
  left_join(Complete_CAISO_Renewables_Joined, by = "Date_Time") %>%
  mutate(CAISO_Net_Demand_MW = CAISO_Demand_MW - CAISO_Renewables_MW) %>%
  select(Date_Time, CAISO_Net_Demand_MW)

#### Convert to 15-Minute Data ####

Start_Time_15_min = CAISO_Net_Demand$Date_Time[1]

End_Time_15_min =CAISO_Net_Demand$Date_Time[nrow(CAISO_Net_Demand)] + (45*60)

CAISO_Net_Demand_15min <- data.frame(Date_Time_15_min = seq.POSIXt(Start_Time_15_min, End_Time_15_min, by = "15 min"))

rm(Start_Time_15_min, End_Time_15_min)

CAISO_Net_Demand_15min <- CAISO_Net_Demand_15min %>%
  mutate(Date_Time = Date_Time_15_min)

minute(CAISO_Net_Demand_15min$Date_Time) <- 0

CAISO_Net_Demand <- CAISO_Net_Demand_15min %>%
  left_join(CAISO_Net_Demand, by = "Date_Time") %>%
  mutate(Date_Time = Date_Time_15_min) %>%
  select(-Date_Time_15_min)

rm(CAISO_Net_Demand_15min)

#### Save Final Dataset ####
setwd(Code_WD)
saveRDS(CAISO_Net_Demand, "CAISO_Net_Demand.rds")
