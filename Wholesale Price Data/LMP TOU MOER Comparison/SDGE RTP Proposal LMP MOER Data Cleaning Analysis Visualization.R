library(tidyverse)
library(lubridate)
library(jsonlite)

options(scipen = 999)

setwd("~/EnerNOC, Inc/Flex - SE/Project Analyst/Policy & Regulatory Info/California/General Rate Cases & Rate Design/SDG&E/2020 RTP Proposal Testimony/Data Cleaning, Analysis, and Visualization")
Code_WD <- getwd()

setwd("../")

Home_WD <- getwd()


#### Load and Clean 2019 CAISO RT5M Wholesale Price Data ####
# Data is for SDG&E DLAP, Real-Time 5-Minute Market
# Source: http://oasis.caiso.com/ Interval Locational Marginal Prices (LMP) DLAP_SDGE-APND

setwd(file.path(Home_WD, "CAISO RT5M Price Data"))

CAISO_RT5M_Files <- list.files(pattern = ".csv")

Raw_CAISO_RT5M_Joined <- data.frame(INTERVALSTARTTIME_GMT = character(), VALUE = numeric(), stringsAsFactors = F)

for(CAISO_RT5M_File in CAISO_RT5M_Files){
  
  Raw_CAISO_RT5M_Single <- read.csv(CAISO_RT5M_File) %>%
    filter(XML_DATA_ITEM == "LMP_PRC") %>%
    select(INTERVALSTARTTIME_GMT, VALUE)
  
  Raw_CAISO_RT5M_Joined <- rbind(Raw_CAISO_RT5M_Joined, Raw_CAISO_RT5M_Single)
  
}

rm(Raw_CAISO_RT5M_Single, CAISO_RT5M_File, CAISO_RT5M_Files)

Clean_CAISO_RT5M_Joined <- Raw_CAISO_RT5M_Joined %>%
  mutate(dttm = as.POSIXct(gsub("T", " ", substr(INTERVALSTARTTIME_GMT, 1, 16)), tz = "UTC")) %>%
  mutate(dttm = with_tz(dttm, tz = "America/Los_Angeles")) %>%
  mutate(Date = as.Date(dttm, tz = "America/Los_Angeles")) %>%
  mutate(LMP_RT5M = VALUE/1000) %>% # Convert from $/MWh to $/kWh
  select(dttm, Date, LMP_RT5M) %>%
  arrange(dttm)

rm(Raw_CAISO_RT5M_Joined)


#### Load and Clean WattTime SGIP Historical GHG Data ####
# Data is for SDG&E DLAP
# Source: sgipsignal.com/sgipmoer/?ba=SGIP_CAISO_SDGE

setwd(file.path(Home_WD, "WattTime SGIP Historical GHG Data"))

WattTime_Files <- list.files(pattern = ".json")

Raw_WattTime_Joined <- data.frame(point_time = character(), moer = numeric(), version = numeric(), 
                                  freq = numeric(), ba = character(), stringsAsFactors = F)

for(WattTime_File in WattTime_Files){
  
  Raw_WattTime_Single <- read_json(WattTime_File, simplifyVector = TRUE)
  
  Raw_WattTime_Joined <- rbind(Raw_WattTime_Joined, Raw_WattTime_Single)
  
}

rm(Raw_WattTime_Single, WattTime_File, WattTime_Files)

Clean_WattTime_Joined <- Raw_WattTime_Joined %>%
  mutate(dttm = as.POSIXct(gsub("T", " ", substr(point_time, 1, 16)), tz = "UTC")) %>%
  mutate(dttm = with_tz(dttm, tz = "America/Los_Angeles")) %>%
  select(dttm, moer) %>%
  group_by(dttm) %>%
  summarize(moer = mean(moer)) # Remove duplicates by averaging.

rm(Raw_WattTime_Joined)

# 15 datapoints are missing
Complete_WattTime_Joined <- data.frame(dttm = seq.POSIXt(from = as.POSIXct("2019-01-01 00:00", tz = "America/Los_Angeles"),
                                                         to = as.POSIXct("2019-12-31 23:55", tz = "America/Los_Angeles"),
                                                         by = "5 min")) %>%
  left_join(Clean_WattTime_Joined, by = "dttm") %>%
  mutate(moer = ifelse(is.na(moer), lag(moer, 12*24*7), moer)) # Fill with data from prior week.

rm(Clean_WattTime_Joined)


#### Join CAISO and WattTime Data ####

Joined_CAISO_WT <- Clean_CAISO_RT5M_Joined %>%
  left_join(Complete_WattTime_Joined, by = "dttm")

rm(Clean_CAISO_RT5M_Joined, Complete_WattTime_Joined)


#### Calculate SDG&E AL-TOU Energy Rate Values ####
# Values are for 2020-02-01 SDG&E AL-TOU, Secondary Voltage, in $USD. (http://regarchive.sdge.com/tm2/pdf/ELEC_ELEC-SCHEDS_AL-TOU.pdf)
# Includes Electric Energy Commodity Cost (http://regarchive.sdge.com/tm2/pdf/ELEC_ELEC-SCHEDS_EECC.pdf)
# Includes Department of Water Resources Bond Charge (http://regarchive.sdge.com/tm2/pdf/ELEC_ELEC-SCHEDS_DWR-BC.pdf)
# Includes 5.78-percent City of SD Franchise Fee (https://www.sdge.com/rates-and-regulations/miscellaneous-tariff-related-information/electric-franchise-fees)
# Note: Power factor charges are not included in this analysis. 

Summer_Peak_Rate <- (0.00523 + 0.13432  + 0.00580) * 1.0758
Summer_Off_Peak_Rate <-  (0.00523 + 0.11233 + 0.00580) * 1.0758
Summer_Super_Off_Peak_Rate <- (0.00523 + 0.08489 + 0.00580) * 1.0758

Winter_Peak_Rate <- (0.00523 + 0.11287 + 0.00580) * 1.0758
Winter_Off_Peak_Rate <-  (0.00523 + 0.10018  + 0.00580) * 1.0758
Winter_Super_Off_Peak_Rate <- (0.00523 + 0.08610 + 0.00580) * 1.0758

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

Joined_CAISO_WT_SDGE <- Joined_CAISO_WT %>%
  mutate(Month = month(dttm)) %>%
  mutate(Season = ifelse(Month %in% c(6:10), "Summer", "Winter")) %>% # Dates between June 1 and October 31 are considered "Summer"
  mutate(Weekday_Weekend_Holiday = ifelse(weekdays(dttm) %in% c("Saturday", "Sunday") |
                                            as.Date(dttm, tz = "America/Los_Angeles") %in% Holidays_List, "Weekend/Holiday", "Weekday")) %>% # Monday-Friday = Weekday, Saturday & Sunday = Weekend
  mutate(Hour_Decimal = hour(dttm) + minute(dttm)/60) # ex. 8:30 am = 8.5

rm(Joined_CAISO_WT, Holidays_List)

# Summer Peak
# Summer Peak = June-October, 4:00 pm to 9:00 pm, all days
Joined_CAISO_WT_SDGE <- Joined_CAISO_WT_SDGE %>%
  mutate(Summer_Peak_Binary = ifelse(Season == "Summer" & 
                                       (Hour_Decimal >= (12+4) & Hour_Decimal < (12+9)), 1, 0))

# Summer Super Off-Peak
# Summer Super Off-Peak = 12:00 midnight to 6:00 am on weekdays, or midnight to 2:00 pm on weekends and holidays.

Joined_CAISO_WT_SDGE <- Joined_CAISO_WT_SDGE %>%
  mutate(Summer_Super_Off_Peak_Binary = ifelse(Season == "Summer" & 
                                                 ((Weekday_Weekend_Holiday == "Weekday" &
                                                     (Hour_Decimal >= 0 & Hour_Decimal < 6)) |
                                                    (Weekday_Weekend_Holiday == "Weekend/Holiday" &
                                                       (Hour_Decimal >= 0 & Hour_Decimal < (12+2)))),
                                               1, 0))

# Summer Off-Peak
# Summer Off Peak = 6:00 am to 4:00 pm and 9:00 pm to 12:00 midnight on weekdays, plus 2:00 pm to 4:00 pm and 9:00 pm to 12:00 midnight on weekends and holidays.
# In other words, if it's summer and not peak or super-off-peak, it's off-peak.

Joined_CAISO_WT_SDGE <- Joined_CAISO_WT_SDGE %>%
  mutate(Summer_Off_Peak_Binary = ifelse(Season == "Summer" &
                                           Summer_Peak_Binary == 0 &
                                           Summer_Super_Off_Peak_Binary == 0,
                                         1, 0))

# Winter Peak
# Winter Peak = November-May, 4:00 pm to 9:00 pm, all days

Joined_CAISO_WT_SDGE <- Joined_CAISO_WT_SDGE %>%
  mutate(Winter_Peak_Binary = ifelse(Season == "Winter" &
                                       (Hour_Decimal >= (12+4) & Hour_Decimal < (12+9)), 1, 0))

# Winter Super Off-Peak
# Winter Super Off-Peak = 12:00 midnight to 6:00 am, and 10:00 am-2:00 pm in March and April, on weekdays, plus
# 12:00 midnight-2:00 pm on weekends

Joined_CAISO_WT_SDGE <- Joined_CAISO_WT_SDGE %>%
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

Joined_CAISO_WT_SDGE <- Joined_CAISO_WT_SDGE %>%
  mutate(Winter_Off_Peak_Binary = ifelse(Season == "Winter" &
                                           Winter_Peak_Binary == 0 &
                                           Winter_Super_Off_Peak_Binary == 0, 1, 0))



# Check that all timesteps are accounted for, and that no timestep has a value of 1 in multiple columns

SDGE_AL_TOU_Cost_Vector_Check <- Joined_CAISO_WT_SDGE %>%
  mutate(Check_Sum = Summer_Peak_Binary + Summer_Off_Peak_Binary + Summer_Super_Off_Peak_Binary + 
           Winter_Peak_Binary + Winter_Off_Peak_Binary + Winter_Super_Off_Peak_Binary) %>%
  filter(Check_Sum != 1)
# This dataframe should be empty (0 observations).
rm(SDGE_AL_TOU_Cost_Vector_Check)


#### Energy Rates ####

Joined_CAISO_WT_SDGE <- Joined_CAISO_WT_SDGE %>%
  mutate(Energy_Rate_ALTOU = (Summer_Peak_Binary * Summer_Peak_Rate) + 
           (Summer_Off_Peak_Binary * Summer_Off_Peak_Rate) +
           (Summer_Super_Off_Peak_Binary * Summer_Super_Off_Peak_Rate) +
           (Winter_Peak_Binary * Winter_Peak_Rate) +
           (Winter_Off_Peak_Binary * Winter_Off_Peak_Rate) +
           (Winter_Super_Off_Peak_Binary * Winter_Super_Off_Peak_Rate)) %>%
  select(-Month, -Season, -Weekday_Weekend_Holiday, -Hour_Decimal, 
       -Summer_Peak_Binary, -Summer_Off_Peak_Binary, -Summer_Super_Off_Peak_Binary, 
       -Winter_Peak_Binary, -Winter_Off_Peak_Binary, -Winter_Super_Off_Peak_Binary)

rm(Summer_Peak_Rate, Summer_Off_Peak_Rate, Summer_Super_Off_Peak_Rate, Winter_Peak_Rate, Winter_Off_Peak_Rate, Winter_Super_Off_Peak_Rate)


#### Save Final Dataset ####
setwd(Home_WD)
saveRDS(Joined_CAISO_WT_SDGE, file.path(Code_WD, "Joined_CAISO_WattTime_SDGE_2019.rds"))
# write.csv(Joined_CAISO_WT_SDGE, "Joined_CAISO_WattTime_SDGE_2019.csv", row.names = F)

# Joined_CAISO_WT_SDGE <- readRDS(file.path(Code_WD, "Joined_CAISO_WattTime_SDGE_2019.rds")) Load saved dataset.


#### Plot LMPs, MOERs, and TOU Retail Rates for Selected Dates ####

Selected_Plot_Date <- as.Date("2019-03-11", tz = "America/Los_Angeles")

Joined_CAISO_WT_SDGE_Long_Filtered <- Joined_CAISO_WT_SDGE %>%
  filter(Date == Selected_Plot_Date) %>%
  gather(key = "Datastream", value = "Value", LMP_RT5M, moer, Energy_Rate_ALTOU)
  
ggplot(Joined_CAISO_WT_SDGE_Long_Filtered, aes(x = dttm, y = Value)) + 
  geom_line(aes(color = Datastream)) +
  facet_grid(Datastream ~ ., scales = "free_y", switch = "y", labeller = as_labeller(c(LMP_RT5M = "LMP ($/kWh)", moer = "Emissions (kg CO2/kWh)", Energy_Rate_ALTOU = "AL-TOU Rate ($/kWh)"))) +
  xlab("Date-Time") +
  ylab(NULL) +
  ggtitle("SDG&E TOU Rate, LMP, and Emissions") +
  theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5)) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  theme(legend.position = "none") +
  scale_x_datetime(limits = c(Joined_CAISO_WT_SDGE_Long_Filtered$dttm[1], 
                              Joined_CAISO_WT_SDGE_Long_Filtered$dttm[nrow(Joined_CAISO_WT_SDGE_Long_Filtered)]),
                   expand = c(0, 0))

ggsave(filename = file.path(Code_WD, paste0("SDG&E LMP Emissions TOU Comparison Plot ", as.character(Selected_Plot_Date), ".png")),
       width = 11, height = 8.5, units = "in")

# # Plot without Marginal Emissions Rates
# 
# Joined_CAISO_WT_SDGE_Long_Filtered <- Joined_CAISO_WT_SDGE %>%
#   select(-moer) %>%
#   filter(Date == Selected_Plot_Date) %>%
#   gather(key = "Datastream", value = "Value", LMP_RT5M, Energy_Rate_ALTOU)
# 
# ggplot(Joined_CAISO_WT_SDGE_Long_Filtered, aes(x = dttm, y = Value)) + 
#   geom_line(aes(color = Datastream)) +
#   facet_grid(Datastream ~ ., scales = "free_y", switch = "y", labeller = as_labeller(c(LMP_RT5M = "LMP ($/kWh)", Energy_Rate_ALTOU = "AL-TOU Rate ($/kWh)"))) +
#   xlab("Date-Time") +
#   ylab(NULL) +
#   ggtitle("SDG&E TOU Rate and LMP") +
#   theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5)) +
#   theme(strip.background = element_blank(), strip.placement = "outside") +
#   theme(legend.position = "none") +
#   scale_x_datetime(limits = c(Joined_CAISO_WT_SDGE_Long_Filtered$dttm[1], 
#                               Joined_CAISO_WT_SDGE_Long_Filtered$dttm[nrow(Joined_CAISO_WT_SDGE_Long_Filtered)]),
#                    expand = c(0, 0))
# 
# ggsave(filename = file.path(Code_WD, paste0("SDG&E LMP TOU Comparison Plot ", as.character(Selected_Plot_Date), ".png")),
#        width = 11, height = 8.5, units = "in")

# Plot SDG&E DLAP RT5M LMPs

LMP_Duration_Curve <- Joined_CAISO_WT_SDGE %>%
  arrange(-LMP_RT5M) %>%
  mutate(LMP_Rank = row_number())

ggplot(LMP_Duration_Curve, aes(x = LMP_Rank, y = LMP_RT5M)) + 
  geom_point() +
  xlab("Ranked 5-Minute Intervals") +
  ylab("LMP ($/kWh)") +
  ggtitle("LMP Duration Curve") +
  theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5))

ggsave(filename = file.path(Code_WD, "LMP Duration Curve Plot.png"),
       width = 11, height = 8.5, units = "in")

ggplot(LMP_Duration_Curve %>% filter(LMP_Rank <= 4000), aes(x = LMP_Rank, y = LMP_RT5M)) + 
  geom_point() +
  xlab("Ranked 5-Minute Intervals") +
  ylab("LMP ($/kWh)") +
  ggtitle("LMP Duration Curve (Top Hours)") +
  theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5))

ggsave(filename = file.path(Code_WD, "LMP Duration Curve Plot Top Hours.png"),
       width = 11, height = 8.5, units = "in")