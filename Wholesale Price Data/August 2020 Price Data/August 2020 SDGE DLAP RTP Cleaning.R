#### Script Description Header ####

# File Name: August 2020 SDG&E JARP Interval Meter Data Cleaning.R
# File Location: "~/Desktop/SDG&E JARP/Wholesale Price Data/August 2020 Price Data"
# Project: San Diego JARP Supplementary Testimony
# Description: Cleans August 2020 wholeale energy price data for use in SDG&E JARP testimony.

#### Load Packages
library(tidyverse)
library(lubridate)

# Disable Scientific Notation
options(scipen = 999)

# Set Working Directories
setwd("~/Desktop/SDG&E JARP/Wholesale Price Data/August 2020 Price Data")
Code_WD <- getwd()
Data_WD <- getwd()


#### Load and Clean August 2020 CAISO RT5M Wholesale Price Data ####
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

# Plot LMPs on 2020-08-15 (first day of rolling outages)
ggplot(Clean_SDGE_RT5M_LMP %>%
         filter(Date_Time >= as.POSIXct("2020-08-15 00:00", tz = "America/Los_Angeles"),
                Date_Time < as.POSIXct("2020-08-16 00:00", tz = "America/Los_Angeles")),
      aes(x = Date_Time, y = LMP_RT5M)) +
  geom_line() +
  xlab("Date-Time") +
  ylab("Real-Time 5-Minute Locational Marginal Price ($/kWh)") +
  ggtitle("SDG&E LMPs on Saturday, August 15th, 2020 (First Day of Rolling Outages Since 2001)") +
  theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5))

ggsave("SDGE RT5M LMPs 2020-08-15.png", width = 11, height = 8.5, units = "in")