#### Script Description Header ####

# File Name: SDG&E AL-TOU Tariff Creator.R
# File Location: "~/Desktop/SDG&E JARP/Rates/SDG&E AL-TOU"
# Project: San Diego JARP Supplementary Testimony
# Description: Creates SDG&E AL-TOU retail rate for use in SDG&E JARP modeling.

#### User Inputs ####

# This script generates a time-series vector for the user's choice of year.
# This value must be input by the user to match the desired year to be modeled.

Data_Year <- 2019

Data_Timestep_Length = 15 # Timestep length, in minutes


#### Load Packages ####
library(tidyverse)
library(lubridate)

# Disable Scientific Notation
options(scipen = 999)

# Set Working Directories
setwd("~/Desktop/SDG&E JARP/Rates/SDG&E AL-TOU")
Code_WD <- getwd()

Clean_Rate_Data_WD <- file.path(Code_WD, Data_Year,paste0(Data_Timestep_Length, "-Minute Data"))


#### Calculate SDG&E AL-TOU Energy Rate Values ####
# Values are for 2020-04-01 SDG&E AL-TOU, Secondary Voltage, in $USD. (http://regarchive.sdge.com/tm2/pdf/ELEC_ELEC-SCHEDS_AL-TOU.pdf)
# Includes Electric Energy Commodity Cost (http://regarchive.sdge.com/tm2/pdf/ELEC_ELEC-SCHEDS_EECC.pdf)
# Includes Department of Water Resources Bond Charge (http://regarchive.sdge.com/tm2/pdf/ELEC_ELEC-SCHEDS_DWR-BC.pdf)
# Includes 5.78-percent City of SD Franchise Fee (https://www.sdge.com/rates-and-regulations/miscellaneous-tariff-related-information/electric-franchise-fees)
# Note: Power factor charges are not included in this analysis. 

Start_Date_Time <- as.POSIXct(paste0(Data_Year, "-01-01 00:00:00"), tz = "America/Los_Angeles")
End_Date_Time <- as.POSIXct(paste0(Data_Year, "-12-31 23:55:00"), tz = "America/Los_Angeles")

SDGE_AL_TOU <- data.frame(Date_Time = seq.POSIXt(Start_Date_Time, End_Date_Time, by = paste(Data_Timestep_Length, "min")))

Summer_Peak_Energy_Rate <- (0.00671 + 0.11957  + 0.00580) * 1.0758
Summer_Off_Peak_Energy_Rate <-  (0.00671 + 0.10008 + 0.00580) * 1.0758
Summer_Super_Off_Peak_Energy_Rate <- (0.00671 + 0.07487 + 0.00580) * 1.0758

Winter_Peak_Energy_Rate <- (0.00671 + 0.09955 + 0.00580) * 1.0758
Winter_Off_Peak_Energy_Rate <-  (0.00671 + 0.08835  + 0.00580) * 1.0758
Winter_Super_Off_Peak_Energy_Rate <- (0.00671 + 0.07594 + 0.00580) * 1.0758

# Noncoincident_Demand_Rate <- 24.48 * 1.0758
# Summer_Peak_Demand_Rate <- (19.14 + 9.78) * 1.0758
# Winter_Peak_Demand_Rate <- 19.23 * 1.0758


# Holidays for 2019
# Source: https://www.sdge.com/whenmatters
# The Time-of-Use holidays are:
# New Year's Day (January 1)
# President's Day (third Monday in February)
# Memorial Day (last Monday in May)
# Independence Day (July 4)
# Labor Day (first Monday in September)
# Veterans Day (November 11)
# Thanksgiving Day (fourth Thursday in November)
# Christmas Day (December 25).
# When a holiday listed above falls on Sunday, the following Monday is recognized. No change will be made for holidays falling on Saturday.

New_Years_Day <- "2019-01-01"
Presidents_Day <- "2019-02-18"
Memorial_Day <- "2019-05-27"
Independence_Day <- "2019-07-04"
Labor_Day <- "2019-09-02"
Veterans_Day <- "2019-11-11"
Thanksgiving_Day <- "2019-11-28"
Christmas_Day <- "2019-12-25"

Holidays_List <- as.Date(c(New_Years_Day, Presidents_Day, Memorial_Day, Independence_Day, Labor_Day, Veterans_Day, Thanksgiving_Day, Christmas_Day), tz = "America/Los_Angeles")

rm(New_Years_Day, Presidents_Day, Memorial_Day, Independence_Day, Labor_Day, Veterans_Day, Thanksgiving_Day, Christmas_Day)

# When a holiday listed above falls on Sunday, the following Monday is recognized. No change will be made for holidays falling on Saturday.

for(Holiday_Iter in seq_along(Holidays_List)){
  if(weekdays(Holidays_List[Holiday_Iter]) == "Sunday"){
    Holidays_List = c(Holidays_List, Holidays_List[Holiday_Iter] + 1)
  }
}
rm(Holiday_Iter)

SDGE_AL_TOU <- SDGE_AL_TOU %>%
  mutate(Month = month(Date_Time)) %>%
  mutate(Season = ifelse(Month %in% c(6:10), "Summer", "Winter")) %>% # Dates between June 1 and October 31 are considered "Summer"
  mutate(Weekday_Weekend_Holiday = ifelse(weekdays(Date_Time) %in% c("Saturday", "Sunday") |
                                            as.Date(Date_Time, tz = "America/Los_Angeles") %in% Holidays_List, "Weekend/Holiday", "Weekday")) %>% # Monday-Friday = Weekday, Saturday & Sunday = Weekend
  mutate(Hour_Decimal = hour(Date_Time) + minute(Date_Time)/60) # ex. 8:30 am = 8.5

rm(Holidays_List)

# Summer Peak
# Summer Peak = June-October, 4:00 pm to 9:00 pm, all days
SDGE_AL_TOU <- SDGE_AL_TOU %>%
  mutate(Summer_Peak_Binary = ifelse(Season == "Summer" & 
                                       (Hour_Decimal >= (12+4) & Hour_Decimal < (12+9)), 1, 0))

# Summer Super Off-Peak
# Summer Super Off-Peak = 12:00 midnight to 6:00 am on weekdays, or midnight to 2:00 pm on weekends and holidays.

SDGE_AL_TOU <- SDGE_AL_TOU %>%
  mutate(Summer_Super_Off_Peak_Binary = ifelse(Season == "Summer" & 
                                                 ((Weekday_Weekend_Holiday == "Weekday" &
                                                     (Hour_Decimal >= 0 & Hour_Decimal < 6)) |
                                                    (Weekday_Weekend_Holiday == "Weekend/Holiday" &
                                                       (Hour_Decimal >= 0 & Hour_Decimal < (12+2)))),
                                               1, 0))

# Summer Off-Peak
# Summer Off Peak = 6:00 am to 4:00 pm and 9:00 pm to 12:00 midnight on weekdays, plus 2:00 pm to 4:00 pm and 9:00 pm to 12:00 midnight on weekends and holidays.
# In other words, if it's summer and not peak or super-off-peak, it's off-peak.

SDGE_AL_TOU <- SDGE_AL_TOU %>%
  mutate(Summer_Off_Peak_Binary = ifelse(Season == "Summer" &
                                           Summer_Peak_Binary == 0 &
                                           Summer_Super_Off_Peak_Binary == 0,
                                         1, 0))

# Winter Peak
# Winter Peak = November-May, 4:00 pm to 9:00 pm, all days

SDGE_AL_TOU <- SDGE_AL_TOU %>%
  mutate(Winter_Peak_Binary = ifelse(Season == "Winter" &
                                       (Hour_Decimal >= (12+4) & Hour_Decimal < (12+9)), 1, 0))

# Winter Super Off-Peak
# Winter Super Off-Peak = 12:00 midnight to 6:00 am, and 10:00 am-2:00 pm in March and April, on weekdays, plus
# 12:00 midnight-2:00 pm on weekends

SDGE_AL_TOU <- SDGE_AL_TOU %>%
  mutate(Winter_Super_Off_Peak_Binary = ifelse(Season == "Winter" &
                                                 ((Weekday_Weekend_Holiday == "Weekday" &
                                                     ((Hour_Decimal >= 0 & Hour_Decimal < 6) |
                                                        (Month %in% c(3,4) &
                                                           (Hour_Decimal >= 10 & Hour_Decimal < (12+2))))) |
                                                    (Weekday_Weekend_Holiday == "Weekend/Holiday" &
                                                       (Hour_Decimal >= 0 & Hour_Decimal < (12+2)))),
                                               1, 0))

# Winter Off-Peak
# Winter Off Peak = 6:00 am to 4:00 pm (excluding 10:00 am-2:00 pm in March and April) and 9:00 pm-12:00 midnight on weekdays, plus
# 2:00 pm-4:00 pm and 9:00 pm-midnight on weekends and holidays.
# In other words, if it's winter and not peak or super-off-peak, it's off-peak.

SDGE_AL_TOU <- SDGE_AL_TOU %>%
  mutate(Winter_Off_Peak_Binary = ifelse(Season == "Winter" &
                                           Winter_Peak_Binary == 0 &
                                           Winter_Super_Off_Peak_Binary == 0, 1, 0))



# Check that all timesteps are accounted for, and that no timestep has a value of 1 in multiple columns

SDGE_AL_TOU_Cost_Vector_Check <- SDGE_AL_TOU %>%
  mutate(Check_Sum = Summer_Peak_Binary + Summer_Off_Peak_Binary + Summer_Super_Off_Peak_Binary + 
           Winter_Peak_Binary + Winter_Off_Peak_Binary + Winter_Super_Off_Peak_Binary) %>%
  filter(Check_Sum != 1)
# This dataframe should be empty (0 observations).
rm(SDGE_AL_TOU_Cost_Vector_Check)


#### Energy Rates ####

SDGE_AL_TOU <- SDGE_AL_TOU %>%
  mutate(Total_Energy_Rate_AL_TOU = (Summer_Peak_Binary * Summer_Peak_Energy_Rate) + 
           (Summer_Off_Peak_Binary * Summer_Off_Peak_Energy_Rate) +
           (Summer_Super_Off_Peak_Binary * Summer_Super_Off_Peak_Energy_Rate) +
           (Winter_Peak_Binary * Winter_Peak_Energy_Rate) +
           (Winter_Off_Peak_Binary * Winter_Off_Peak_Energy_Rate) +
           (Winter_Super_Off_Peak_Binary * Winter_Super_Off_Peak_Energy_Rate)) %>%
  select(-Month, -Season, -Weekday_Weekend_Holiday, -Hour_Decimal, 
       -Summer_Peak_Binary, -Summer_Off_Peak_Binary, -Summer_Super_Off_Peak_Binary, 
       -Winter_Peak_Binary, -Winter_Off_Peak_Binary, -Winter_Super_Off_Peak_Binary)

rm(Summer_Peak_Energy_Rate, Summer_Off_Peak_Energy_Rate, Summer_Super_Off_Peak_Energy_Rate, Winter_Peak_Energy_Rate, Winter_Off_Peak_Energy_Rate, Winter_Super_Off_Peak_Energy_Rate)


#### Save Final Dataset ####
setwd(Clean_Rate_Data_WD)
saveRDS(SDGE_AL_TOU, "SDGE_AL_TOU.rds")