#### Script Description Header ####

# File Name: SDG&E JARP Interval Meter Data Cleaning.R
# File Location: "~/Desktop/SDG&E JARP/Interval Meter Data/Clean Interval Meter Data"
# Project: San Diego JARP Supplementary Testimony
# Description: Cleans interval meter data for use in SDG&E JARP modeling.

#### Load Packages
library(tidyverse)
library(lubridate)

# Disable Scientific Notation
options(scipen = 999)

# Set Working Directories
setwd("~/Desktop/SDG&E JARP/Interval Meter Data/Clean Interval Meter Data")
Code_WD <- getwd()

setwd("../Raw Interval Meter Data")
Data_WD <- getwd()

#### Load and Clean EV-TOU-5 Residential Load Profiles ####

rawRES <- read.csv("RES500.csv")

# Customers aren't individually labeled in the raw dataset. Assuming that each time the date resets back to Jan.1, a new customer's data is being provided.
idRES <- rawRES %>%
  mutate(meterID = INTRVL_DATE == "1-Jan-19") %>%
  mutate(meterID = cumsum(meterID))

rm(rawRES)

# There are 57 meters with something other than 365 days of data.
incompleteRES <- idRES %>%
  group_by(meterID) %>%
  summarize(Count = n()) %>%
  filter(Count != 365)

# There are 317 meters in the final complete dataset - 2 of which are EV-TOU-5. Many are switching from DR to TOU-DR-1 midway through the year.
completeRES <- idRES %>%
  filter(!(meterID %in% incompleteRES$meterID)) %>%
  mutate(meterID = INTRVL_DATE == "1-Jan-19") %>%
  mutate(meterID = cumsum(meterID))

rm(idRES, incompleteRES)


# Get summary of customer count by rate
# 208 customers on Schedule DR, 2 customers on schedule EV-TOU-5, and 107 customers on Schedule TOU-DR-1
summaryRES <- completeRES %>%
  group_by(CD_RATE) %>%
  summarize(Customer_Count = n()/365)

rm(summaryRES)


# Convert to long format with standard timestamps
longRES <- completeRES %>%
  gather(key = "Time", value = "Demand_kWh", N1:N24) %>%
  mutate(Time = as.numeric(gsub("N", "", Time)) - 1) %>%
  mutate(Date_Time = paste0(INTRVL_DATE, " ", Time, ":00")) %>%
  mutate(Date_Time = as.POSIXct(strptime(Date_Time, format = "%d-%b-%y %H:%M"), tz = "Etc/GMT+8")) %>% # There doesn't appear to be any Daylight Savings Time offset in this dataset.
  mutate(Date_Time = with_tz(Date_Time, tzone = "America/Los_Angeles")) %>%
  select(meterID, CD_RATE, FL_NET_METER, Date_Time, Demand_kWh) %>%
  arrange(meterID, Date_Time)

rm(completeRES)

#### Convert from Hourly to 15-Minute Data ####

Start_Time_15_min = longRES$Date_Time[1]

End_Time_15_min = longRES$Date_Time[nrow(longRES)] + (45*60)

RES_15min <- data.frame(Date_Time_15_min = seq.POSIXt(Start_Time_15_min, End_Time_15_min, by = "15 min"))

rm(Start_Time_15_min, End_Time_15_min)

RES_15min <- RES_15min %>%
  mutate(Date_Time = Date_Time_15_min)

minute(RES_15min$Date_Time) <- 0

cleanRES <- longRES %>%
  left_join(RES_15min, by = "Date_Time") %>%
  mutate(Date_Time = Date_Time_15_min) %>%
  mutate(Demand_kWh = Demand_kWh * (15/60)) %>% # kWh consumed during a 15-minute interval is 1/4 the amount consumed in an hour.
  select(-Date_Time_15_min)

rm(RES_15min)

rm(longRES)


#### Create Aggregate Residential Profile ####

aggRES <- cleanRES %>%
  group_by(Date_Time) %>%
  summarize(Demand_kWh = sum(Demand_kWh))


#### Save cleaned data to RDS
setwd(Code_WD)
saveRDS(cleanRES, "cleanRES.rds")
saveRDS(aggRES, "aggRES.rds")

rm(cleanRES, aggRES)



#### Load and Clean AL-TOU Commercial/Industrial Load Profiles ####

setwd(Data_WD)

rawLRG <- read.csv("LRG500.csv")

# Customers aren't individually labeled in the raw dataset. Assuming that each time the date resets back to Jan.1, a new customer's data is being provided.
idLRG <- rawLRG %>%
  mutate(meterID = INTRVL_DATE == "1-Jan-19") %>%
  mutate(meterID = cumsum(meterID))

rm(rawLRG)

# 23 of the 461 meters don't have 365 days of data. Remove these, and re-assign meterIDs to the remaining complete meter data.
incompleteLRG <- idLRG %>%
  group_by(meterID) %>%
  summarize(Count = n()) %>%
  filter(Count != 365)

# There are 438 meters in the final complete dataset.
completeLRG <- idLRG %>%
  filter(!(meterID %in% incompleteLRG$meterID)) %>%
  mutate(meterID = INTRVL_DATE == "1-Jan-19") %>%
  mutate(meterID = cumsum(meterID))

rm(idLRG, incompleteLRG)

# Get summary of customer count by rate
# 132 customers on AL-TOU, 306 customers on AL-TOU-CP2
summaryLRG <- completeLRG %>%
  group_by(CD_RATE) %>%
  summarize(Customer_Count = n()/365)

rm(summaryLRG)

# Convert to long format with standard timestamps
cleanLRG <- completeLRG %>%
  gather(key = "Time", value = "Demand_kWh", N1:N96) %>%
  mutate(Time = (as.numeric(gsub("N", "", Time)) - 1)/4) %>%
  mutate(Hour = floor(Time),
         Minute = (Time - floor(Time))*60) %>%  
  mutate(Date_Time = paste0(INTRVL_DATE, " ", Hour, ":", Minute)) %>%
  mutate(Date_Time = as.POSIXct(strptime(Date_Time, format = "%d-%b-%y %H:%M"), tz = "Etc/GMT+8")) %>% # There doesn't appear to be any Daylight Savings Time offset in this dataset.
  mutate(Date_Time = with_tz(Date_Time, tzone = "America/Los_Angeles")) %>%
  select(meterID, CD_RATE, Date_Time, Demand_kWh) %>%
  arrange(meterID, Date_Time)

rm(completeLRG)

#### Create Aggregate Large Commercial/Industrial Profile ####

aggLRG <- cleanLRG %>%
  group_by(Date_Time) %>%
  summarize(Demand_kWh = sum(Demand_kWh))


#### Save cleaned data to RDS
setwd(Code_WD)
saveRDS(cleanLRG, "cleanLRG.rds")
saveRDS(aggLRG, "aggLRG.rds")

rm(cleanLRG, aggLRG)
