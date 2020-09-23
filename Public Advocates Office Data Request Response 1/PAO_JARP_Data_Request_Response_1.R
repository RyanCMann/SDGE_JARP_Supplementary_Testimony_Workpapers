#### Script Description Header ####

# File Name: PAO_JARP_Data_Request_Response_1.R
# File Location: "~/Desktop/SDG&E JARP Supplementary Testimony Workpapers/Public Advocates Office Data Request Response 1"
# Project: San Diego JARP Supplementary Testimony
# Description: Analysis to answer California Public Advocates Office JARP Data Request #1 questions.

#### Load Packages ####
library(tidyverse)
library(lubridate)
library(data.table)

# Disable Scientific Notation
options(scipen = 999)

# Set Working Directories
setwd("~/Desktop/SDG&E JARP Supplementary Testimony Workpapers/Public Advocates Office Data Request Response 1")
Code_WD <- getwd()

setwd("../")
Main_WD <- getwd()

setwd(file.path(Main_WD, "Interval Meter Data", "Clean Interval Meter Data"))
Meter_WD <- getwd()

setwd(file.path(Main_WD, "Rates"))
Rates_WD <- getwd()

setwd(file.path(Main_WD, "Wholesale Price Data", "Clean Wholesale Price Data"))
Wholesale_Price_WD <- getwd()

# The following questions relate to JARP’s supplemental testimony, served on August 31, 2020: 
 
# 2.	In Figure 1 on p. 7, JARP provides a graph showing day-of 5-minutes RTPs for the SDG&E Default Load Aggregation Point (DLAP) on August 15, 2020, the day that rolling blackouts occurred. 
#   a.	Please provide a frequency distribution indicating the total number of 5-minute intervals that this SDG&E DLAP price exceeded $0.20, $0.40, $0.60, $0.80, and $1.00 per kWh
#       during the years 2018 and 2019. 
#   b.	For each count in Part a above, please indicate how many of each of those 5-minute intervals fall in adjacent 15-minute intervals,
#       given that the actual billing will be done on 15-minute intervals that each aggregate three 5-minute intervals. 
#   c.	Please confirm that the total number of 5-minute intervals in a year is 105,120 (= 8,760 hours per year x 60/5 intervals per hour).


# Frequency Distribution - 2018 5-minute SDG&E LMPs
Clean_SDGE_RT5M_LMP_2018_5_min <- readRDS(file.path(Wholesale_Price_WD, "Clean_SDGE_RT5M_LMP_2018_5_min.rds"))
nrow(Clean_SDGE_RT5M_LMP_2018_5_min) # 105,120

SDGE_LMP_Frequency_Distribution_2018_5_min <- Clean_SDGE_RT5M_LMP_2018_5_min %>%
  mutate(Greater_Than_20_Cents = LMP_RT5M > 0.20,
         Greater_Than_40_Cents = LMP_RT5M > 0.40,
         Greater_Than_60_Cents = LMP_RT5M > 0.60,
         Greater_Than_80_Cents = LMP_RT5M > 0.80,
         Greater_Than_1_Dollar = LMP_RT5M > 1.00) %>%
  summarize(Greater_Than_20_Cents_Count = sum(Greater_Than_20_Cents),
            Greater_Than_40_Cents_Count = sum(Greater_Than_40_Cents),
            Greater_Than_60_Cents_Count = sum(Greater_Than_60_Cents),
            Greater_Than_80_Cents_Count = sum(Greater_Than_80_Cents),
            Greater_Than_1_Dollar_Count = sum(Greater_Than_1_Dollar))

write.csv(SDGE_LMP_Frequency_Distribution_2018_5_min, file.path(Code_WD, "SDGE_LMP_Frequency_Distribution_2018_5_min.csv"), row.names = F)
rm(Clean_SDGE_RT5M_LMP_2018_5_min, SDGE_LMP_Frequency_Distribution_2018_5_min)

# Frequency Distribution - 2019 5-minute SDG&E LMPs
Clean_SDGE_RT5M_LMP_2019_5_min <- readRDS(file.path(Wholesale_Price_WD, "Clean_SDGE_RT5M_LMP_2019_5_min.rds"))
nrow(Clean_SDGE_RT5M_LMP_2019_5_min) # 105,120

SDGE_LMP_Frequency_Distribution_2019_5_min <- Clean_SDGE_RT5M_LMP_2019_5_min %>%
  mutate(Greater_Than_20_Cents = LMP_RT5M > 0.20,
         Greater_Than_40_Cents = LMP_RT5M > 0.40,
         Greater_Than_60_Cents = LMP_RT5M > 0.60,
         Greater_Than_80_Cents = LMP_RT5M > 0.80,
         Greater_Than_1_Dollar = LMP_RT5M > 1.00) %>%
  summarize(Greater_Than_20_Cents_Count = sum(Greater_Than_20_Cents),
            Greater_Than_40_Cents_Count = sum(Greater_Than_40_Cents),
            Greater_Than_60_Cents_Count = sum(Greater_Than_60_Cents),
            Greater_Than_80_Cents_Count = sum(Greater_Than_80_Cents),
            Greater_Than_1_Dollar_Count = sum(Greater_Than_1_Dollar))

write.csv(SDGE_LMP_Frequency_Distribution_2019_5_min, file.path(Code_WD, "SDGE_LMP_Frequency_Distribution_2019_5_min.csv"), row.names = F)
rm(Clean_SDGE_RT5M_LMP_2019_5_min, SDGE_LMP_Frequency_Distribution_2019_5_min)

# Frequency Distribution - 2018 15-minute SDG&E LMPs
Clean_SDGE_RT5M_LMP_2018_15_min <- readRDS(file.path(Wholesale_Price_WD, "Clean_SDGE_RT5M_LMP_2018_15_min.rds"))
nrow(Clean_SDGE_RT5M_LMP_2018_15_min) # 35,040

SDGE_LMP_Frequency_Distribution_2018_15_min <- Clean_SDGE_RT5M_LMP_2018_15_min %>%
  mutate(Greater_Than_20_Cents = LMP_RT5M > 0.20,
         Greater_Than_40_Cents = LMP_RT5M > 0.40,
         Greater_Than_60_Cents = LMP_RT5M > 0.60,
         Greater_Than_80_Cents = LMP_RT5M > 0.80,
         Greater_Than_1_Dollar = LMP_RT5M > 1.00) %>%
  summarize(Greater_Than_20_Cents_Count = sum(Greater_Than_20_Cents),
            Greater_Than_40_Cents_Count = sum(Greater_Than_40_Cents),
            Greater_Than_60_Cents_Count = sum(Greater_Than_60_Cents),
            Greater_Than_80_Cents_Count = sum(Greater_Than_80_Cents),
            Greater_Than_1_Dollar_Count = sum(Greater_Than_1_Dollar))

write.csv(SDGE_LMP_Frequency_Distribution_2018_15_min, file.path(Code_WD, "SDGE_LMP_Frequency_Distribution_2018_15_min.csv"), row.names = F)
rm(Clean_SDGE_RT5M_LMP_2018_15_min, SDGE_LMP_Frequency_Distribution_2018_15_min)

# Frequency Distribution - 2019 15-minute SDG&E LMPs
Clean_SDGE_RT5M_LMP_2019_15_min <- readRDS(file.path(Wholesale_Price_WD, "Clean_SDGE_RT5M_LMP_2019_15_min.rds"))
nrow(Clean_SDGE_RT5M_LMP_2019_15_min) # 35,040

SDGE_LMP_Frequency_Distribution_2019_15_min <- Clean_SDGE_RT5M_LMP_2019_15_min %>%
  mutate(Greater_Than_20_Cents = LMP_RT5M > 0.20,
         Greater_Than_40_Cents = LMP_RT5M > 0.40,
         Greater_Than_60_Cents = LMP_RT5M > 0.60,
         Greater_Than_80_Cents = LMP_RT5M > 0.80,
         Greater_Than_1_Dollar = LMP_RT5M > 1.00) %>%
  summarize(Greater_Than_20_Cents_Count = sum(Greater_Than_20_Cents),
            Greater_Than_40_Cents_Count = sum(Greater_Than_40_Cents),
            Greater_Than_60_Cents_Count = sum(Greater_Than_60_Cents),
            Greater_Than_80_Cents_Count = sum(Greater_Than_80_Cents),
            Greater_Than_1_Dollar_Count = sum(Greater_Than_1_Dollar))

write.csv(SDGE_LMP_Frequency_Distribution_2019_15_min, file.path(Code_WD, "SDGE_LMP_Frequency_Distribution_2019_15_min.csv"), row.names = F)
rm(Clean_SDGE_RT5M_LMP_2019_15_min, SDGE_LMP_Frequency_Distribution_2019_15_min)


# 3.	Please clarify the following concerning the $7.26 annual cost shift from structural benefiters to residential non-participating customer, listed on p. 11 of your testimony: 
#   a. The $7.26 annual cost shift in your testimony assumes that more than 10% of residential customers signed up for the proposed RTP-DR rate 
#       (given that this 10% represents only structural benefiters), and
#   b. The $7.26 annual cost shift could not happen if the 3% cap on residential participation proposed on p. 17 were applied.
#       What percentage of participants are expected to be structural benefiters? 

SDGE_EV_TOU_5 <- readRDS(file.path(Rates_WD, "SDG&E EV-TOU-5", "2019", "15-Minute Data", "SDGE_EV_TOU_5.rds"))
SDGE_EV_TOU_5_RTP <- readRDS(file.path(Rates_WD, "SDG&E EV-TOU-5-RTP", "2019", "15-Minute Data", "SDGE_EV_TOU_5_RTP.rds"))
cleanRES <- readRDS(file.path(Meter_WD, "cleanRES.rds"))

# Calculate Total Energy Revenue for all residential customers on EV-TOU-5 and EV-TOU-5-RTP
revenueReductionRES <- cleanRES %>%
  left_join(SDGE_EV_TOU_5, by = "Date_Time") %>%
  left_join(SDGE_EV_TOU_5_RTP, by = "Date_Time") %>%
  mutate(Total_Energy_Revenue_EV_TOU_5 = Demand_kWh * Total_Energy_Rate_EV_TOU_5,
         Total_Energy_Revenue_EV_TOU_5_RTP = Demand_kWh * Total_Energy_Rate_EV_TOU_5_RTP) %>%
  group_by(meterID) %>%
  summarize(Total_Energy_Revenue_EV_TOU_5 = sum(Total_Energy_Revenue_EV_TOU_5),
            Total_Energy_Revenue_EV_TOU_5_RTP = sum(Total_Energy_Revenue_EV_TOU_5_RTP)) %>%
  ungroup()

# Calculate Rate Switch Savings: difference between total annual energy revenue on EV-TOU-5 and EV-TOU-5-RTP
revenueReductionRES <- revenueReductionRES %>%
  mutate(rateSwitchSavings = Total_Energy_Revenue_EV_TOU_5 - Total_Energy_Revenue_EV_TOU_5_RTP) %>%
  mutate(positiveRateSwitchSavingsFlag = rateSwitchSavings >= 0) %>%
  mutate(rateSwitchSavingsRank = rank(-rateSwitchSavings, ties.method = "first")) %>%
  mutate(top3Percent = rateSwitchSavingsRank <= round(nrow(revenueReductionRES) * (3/100))) %>%
  mutate(revenueReduction = rateSwitchSavings * top3Percent) %>%
  arrange(rateSwitchSavingsRank)

# Calculate impact on non-participants if only 3% of customers switched to RTP.
totalRevenueReductionRES <- sum(revenueReductionRES$revenueReduction)
perCustomerRevenueReductionRES <- totalRevenueReductionRES/round(nrow(revenueReductionRES) * (3/100))
percentRevenueReductionRES <- totalRevenueReductionRES/sum(revenueReductionRES$Total_Energy_Revenue_EV_TOU_5)
# Having 3% of residential EV-TOU-5 customers switch to EV-TOU-5-RTP with no change in usage would reduce SDG&E revenue collection by 0.3091198%.
# The average structural benefiter would save $115.29 per year.

perCustomerRESbillIncreaseRateClass <- totalRevenueReductionRES/length(unique(cleanRES$meterID)) # Divide by number of resi customers. All customers in rate class see increase (not just non-RTP).
percentRESbillIncreaseRateClass <- totalRevenueReductionRES/sum(revenueReductionRES$Total_Energy_Revenue_EV_TOU_5)
# This would require an 0.3091198% increase in non-RTP customer bills to recover the lost revenue,
# or $3.64 per non-RTP EV-TOU-5 customer per year.
# These customers are (slightly) more expensive to serve, so this is actually undoing a previous cost shift.

rm(totalRevenueReductionRES, perCustomerRevenueReductionRES, percentRevenueReductionRES, perCustomerRESbillIncreaseRateClass, percentRESbillIncreaseRateClass)


# Calculate total residential customers, top 3% structural benefiters, top 10% structural benefiters, and all structural benefiters
customerCountRES <- nrow(revenueReductionRES) # 317 customers

top3PercentRES <- round(nrow(revenueReductionRES) * (3/100)) # 10 customers

top10PercentRES <- round(nrow(revenueReductionRES) * (10/100)) # 32 customers
  
structuralBenefitersRES <- nrow(revenueReductionRES %>% filter(positiveRateSwitchSavingsFlag)) # 252 customers

structuralBenefitersPercentageRES <- structuralBenefitersRES/customerCountRES # 79% of all customers are structural benefiters.

averageBillImpactRES <- revenueReductionRES %>%
  group_by(positiveRateSwitchSavingsFlag) %>%
  summarize(averageBillImpact = mean(rateSwitchSavings)) %>%
  ungroup()
# If all structural benefiters were allowed to rate-switch (and did not modify their load profile), they would save $23.87/year on average.
# If all structural losers were forced to rate switch (and did not modify their load profile), they would pay an extra $92.56/year on average.

# Create Duration Curves for LMPs
ggplot(revenueReductionRES,
      aes(x = rateSwitchSavingsRank, y = rateSwitchSavings, color = as.factor(positiveRateSwitchSavingsFlag))) +
  geom_point() +
  geom_vline(xintercept = top3PercentRES) +
  geom_vline(xintercept = top10PercentRES) +
  geom_vline(xintercept = structuralBenefitersRES) +
  xlab("Rate Switch Savings Ranking") +
  ylab("Rate Switch Savings ($/year)") +
  ggtitle("Residential Rate Switch Savings - SDG&E EV-TOU-5-RTP") +
  theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("red", "forestgreen")) +
  theme(legend.position = "none")

ggsave(file.path(Code_WD, "Residential Rate Switch Savings - SDG&E EV-TOU-5-RTP.png"), width = 11, height = 8.5, units = "in")

rm(cleanRES, revenueReductionRES, top3PercentRES, top10PercentRES, structuralBenefitersRES, structuralBenefitersPercentageRES, averageBillImpactRES)


# 4.	In Table 4 on p. 14, bill savings are shown in the first two columns. For example, an average residential participant shifting 25% of his or her load would save $28.17 per year.
#     Please indicate the average bill size for each of the customers shown in the table and what percentage bill reduction each of these bill savings represent.
    
# Residential Bill Savings Percentage Calculation
aggRES <- readRDS(file.path(Meter_WD, "aggRES.rds"))

aveRES <- aggRES %>%
  mutate(Demand_kWh = Demand_kWh / customerCountRES)
rm(aggRES, customerCountRES)

totalAnnualVolumetricRES <- aveRES %>%
  left_join(SDGE_EV_TOU_5_RTP, by = "Date_Time") %>%
  mutate(Total_Energy_Revenue_EV_TOU_5_RTP = Demand_kWh * Total_Energy_Rate_EV_TOU_5_RTP) %>%
  summarize(Total_Energy_Revenue_EV_TOU_5_RTP = sum(Total_Energy_Revenue_EV_TOU_5_RTP)) # $1,176.58/year
rm(aveRES, SDGE_EV_TOU_5, SDGE_EV_TOU_5_RTP)

perCustomerAvoidedWholesaleCostShifted25RES <- 10.22
perCustomerSavingsCPP_EV_TOU_5_RTP_shifted25 <- 17.95
percentBillReductionshifted25RES <- (perCustomerAvoidedWholesaleCostShifted25RES + perCustomerSavingsCPP_EV_TOU_5_RTP_shifted25)/totalAnnualVolumetricRES$Total_Energy_Revenue_EV_TOU_5_RTP # 2.4%
rm(perCustomerAvoidedWholesaleCostShifted25RES, perCustomerSavingsCPP_EV_TOU_5_RTP_shifted25, percentBillReductionshifted25RES)

perCustomerAvoidedWholesaleCostShifted50RES <- 20.44
perCustomerSavingsCPP_EV_TOU_5_RTP_shifted50 <- 35.90
percentBillReductionshifted50RES <- (perCustomerAvoidedWholesaleCostShifted50RES + perCustomerSavingsCPP_EV_TOU_5_RTP_shifted50)/totalAnnualVolumetricRES$Total_Energy_Revenue_EV_TOU_5_RTP # 4.8%
rm(perCustomerAvoidedWholesaleCostShifted50RES, perCustomerSavingsCPP_EV_TOU_5_RTP_shifted50, percentBillReductionshifted50RES)
rm(totalAnnualVolumetricRES)

# Non-Residential Bill Savings Percentage Calculation
cleanLRG <- readRDS(file.path(Meter_WD, "cleanLRG.rds"))
customerCountLRG <- length(unique(cleanLRG$meterID)) # 438 non-residential customers
rm(cleanLRG)

aggLRG <- readRDS(file.path(Meter_WD, "aggLRG.rds"))

aveLRG <- aggLRG %>%
  mutate(Demand_kWh = Demand_kWh / customerCountLRG)
rm(aggLRG, customerCountLRG)

SDGE_AL_TOU_RTP <- readRDS(file.path(Rates_WD, "SDG&E AL-TOU-RTP", "2019", "15-Minute Data", "SDGE_AL_TOU_RTP.rds"))

totalAnnualVolumetricLRG <- aveLRG %>%
  left_join(SDGE_AL_TOU_RTP, by = "Date_Time") %>%
  mutate(Total_Energy_Revenue_AL_TOU_RTP = Demand_kWh * Total_Energy_Rate_AL_TOU_RTP) %>%
  summarize(Total_Energy_Revenue_AL_TOU_RTP = sum(Total_Energy_Revenue_AL_TOU_RTP)) # $47,981.82/year
rm(aveLRG, SDGE_AL_TOU_RTP)

perCustomerAvoidedWholesaleCostShifted25LRG <- 880.35
perCustomerSavingsCPP_AL_TOU_RTP_shifted25 <- 1185.52
percentBillReductionshifted25LRG <- (perCustomerAvoidedWholesaleCostShifted25LRG + perCustomerSavingsCPP_AL_TOU_RTP_shifted25)/totalAnnualVolumetricLRG$Total_Energy_Revenue_AL_TOU_RTP # 4.3%
rm(perCustomerAvoidedWholesaleCostShifted25LRG, perCustomerSavingsCPP_AL_TOU_RTP_shifted25, percentBillReductionshifted25LRG)

perCustomerAvoidedWholesaleCostShifted50LRG <- 1760.69
perCustomerSavingsCPP_AL_TOU_RTP_shifted50 <- 2371.04
percentBillReductionshifted50LRG <- (perCustomerAvoidedWholesaleCostShifted50LRG + perCustomerSavingsCPP_AL_TOU_RTP_shifted50)/totalAnnualVolumetricLRG$Total_Energy_Revenue_AL_TOU_RTP # 8.6%
rm(perCustomerAvoidedWholesaleCostShifted50LRG, perCustomerSavingsCPP_AL_TOU_RTP_shifted50, percentBillReductionshifted50LRG)
rm(totalAnnualVolumetricLRG)
