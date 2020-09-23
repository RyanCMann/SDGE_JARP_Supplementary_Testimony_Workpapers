#### Script Description Header ####

# File Name: SDGE_JARP_Data_Request_Response_2.R
# File Location: "~/Desktop/SDG&E JARP/Data Request Response 2"
# Project: San Diego JARP Supplementary Testimony
# Description: Analysis to answer SDG&E JARP Data Request #2 questions.

#### Load Packages ####
library(tidyverse)
library(lubridate)
library(data.table)

# Disable Scientific Notation
options(scipen = 999)

# Set Working Directories
setwd("~/Desktop/SDG&E JARP Supplementary Testimony Workpapers/Data Request Response 2")
Code_WD <- getwd()

setwd("../")
Main_WD <- getwd()

setwd(file.path(Main_WD, "Interval Meter Data", "Clean Interval Meter Data"))
Meter_WD <- getwd()

setwd(file.path(Main_WD, "Marginal Emissions Data", "Clean Marginal Emissions Data"))
Emissions_WD <- getwd()

setwd(file.path(Main_WD, "Rates"))
Rates_WD <- getwd()

setwd(file.path(Main_WD, "Wholesale Price Data", "Clean Wholesale Price Data"))
Wholesale_Price_WD <- getwd()

setwd(file.path(Main_WD, "CAISO Demand and Renewables Data", "Clean Net Load Data"))
Net_Load_WD <- getwd()


# Residential Customer Class
#
# 1.	Using EV-TOU 5 as a basis and 2019 CAISO wholesale pricing data, create a real-time pricing rate that is revenue neutral across the residential class.
#     By revenue neutral, we mean that SDG&E would receive the same revenue on an annual basis from all residential customers assuming no change in the quantity or timing of usage.
#     The rate should have the following characteristics.
#   a.	The UDC component should be identical to EV-TOU 5
#   b.	The RTP rate should have the same CARE and non-CARE fixed charges
#   c.	The commodity rate should include a component to cover SDG&E’s costs of procuring and delivering electricity from renewable sources used to comply with SDG&E’s RPS obligation.
#       SDG&E should allocate a set percentage of RPS energy to each interval according to the percentage of RPS-eligible renewable energy in SDG&E’s portfolio during the calendar year.
#       The portion of the total kWh consumed by a customer in each billing interval covered by the RPS allocation will be charged according the cost of RPS energy.
#       Electricity consumed above the RPS allocation will be billed at the real-time price described in bullet “f” below.
#       The RPS allocation of electricity should be shaped to ensure that most customers will consume more than the RPS allocation in each billing interval.
#       The price and/or shape of the RPS allocation could be fixed across the year or could vary by time to better match the timing and cost of the renewable generation in SDG&E’s portfolio.
#   d.	The commodity rate should include a component to cover any above-market energy costs from non-renewable sources that would appear in SDG&E’s PCIA charges.
#   e.	The commodity rate should include the generation capacity cost associated with each TOU period in EV-TOU 5.
#   f.	The summer on-peak generation capacity costs should be disaggregated into the cost to serve the top 50 hours, cost to serve hours 51 – 150, and cost to serve all other hours.
#       The cost to serve the top 50 hours and hours 51 – 150 should be designed as two different dynamic rate components (similar to OG&E’s Variable Peak Pricing tariff) called on a day-ahead basis.
#   g.	Remaining generation energy costs will be recovered from a real-time pricing component using CAISO’s fifteen-minute market prices CA SDG&E’s Default Load Aggregation Point,
#       grossed up for losses in the distribution system.

# See Rates/SDG&E EV-TOU-5-RTP. This rate does not account for RPS or above-market costs directly. It recovers capacity costs using the C-CPP adder from Schedule VGI.

SDGE_EV_TOU_5 <- readRDS(file.path(Rates_WD, "SDG&E EV-TOU-5", "2019", "15-Minute Data", "SDGE_EV_TOU_5.rds"))

SDGE_EV_TOU_5_RTP <- readRDS(file.path(Rates_WD, "SDG&E EV-TOU-5-RTP", "2019", "15-Minute Data", "SDGE_EV_TOU_5_RTP.rds"))

# Check for revenue neutrality - load up aggregated load profile, calculate total annual costs with new and old rate, confirm that they're equal.
aggRES <- readRDS(file.path(Meter_WD, "aggRES.rds"))

neutralityCheckRES <- aggRES %>%
  left_join(SDGE_EV_TOU_5, by = "Date_Time") %>%
  left_join(SDGE_EV_TOU_5_RTP, by = "Date_Time") %>%
  mutate(Total_Energy_Revenue_EV_TOU_5 = Demand_kWh * Total_Energy_Rate_EV_TOU_5,
         Total_Energy_Revenue_EV_TOU_5_RTP = Demand_kWh * Total_Energy_Rate_EV_TOU_5_RTP) %>%
  summarize(Total_Energy_Revenue_EV_TOU_5 = sum(Total_Energy_Revenue_EV_TOU_5),
            Total_Energy_Revenue_EV_TOU_5_RTP = sum(Total_Energy_Revenue_EV_TOU_5_RTP))

# neutralityCheckRES$Total_Energy_Revenue_EV_TOU_5 and neutralityCheckRES$Total_Energy_Revenue_EV_TOU_5_RTP are effectively identical.
# This confirms that the EV-TOU-5-RTP rate is revenue-neutral with the EV-TOU-5 rate for the pool of residential customers used in this analysis.

rm(neutralityCheckRES)

# 2.	Calculate the reduction in revenues SDG&E would collect assuming the top 10% of residential customers who would benefit the most from the rate designed in response to Question 1
# opt-in to the rate with no change in their electricity usage.
# What is the amount by which each residential non-RTP customer’s bill would need to increase on an annual basis to cover the revenue shortfall?

# Load residential meter data
cleanRES <- readRDS(file.path(Meter_WD, "cleanRES.rds"))

revenueReductionRES <- cleanRES %>%
  left_join(SDGE_EV_TOU_5, by = "Date_Time") %>%
  left_join(SDGE_EV_TOU_5_RTP, by = "Date_Time") %>%
  mutate(Total_Energy_Revenue_EV_TOU_5 = Demand_kWh * Total_Energy_Rate_EV_TOU_5,
         Total_Energy_Revenue_EV_TOU_5_RTP = Demand_kWh * Total_Energy_Rate_EV_TOU_5_RTP) %>%
  group_by(meterID) %>%
  summarize(Total_Energy_Revenue_EV_TOU_5 = sum(Total_Energy_Revenue_EV_TOU_5),
            Total_Energy_Revenue_EV_TOU_5_RTP = sum(Total_Energy_Revenue_EV_TOU_5_RTP)) %>%
  ungroup()

revenueReductionRES <- revenueReductionRES %>%
  mutate(rateSwitchSavings = Total_Energy_Revenue_EV_TOU_5 - Total_Energy_Revenue_EV_TOU_5_RTP) %>%
  mutate(rateSwitchSavingsRank = rank(-rateSwitchSavings, ties.method = "first")) %>%
  mutate(top10Percent = rateSwitchSavingsRank <= round(nrow(revenueReductionRES) * (10/100))) %>%
  mutate(revenueReduction = rateSwitchSavings * top10Percent)

totalRevenueReductionRES <- sum(revenueReductionRES$revenueReduction)
perCustomerRevenueReductionRES <- totalRevenueReductionRES/round(nrow(revenueReductionRES) * (10/100))
percentRevenueReductionRES <- totalRevenueReductionRES/sum(revenueReductionRES$Total_Energy_Revenue_EV_TOU_5)
# Having 10% of residential EV-TOU-5 customers switch to EV-TOU-5-RTP with no change in usage would reduce SDG&E revenue collection by 0.6173753%.
# The average structural benefiter would save $71.96 per year.

perCustomerRESbillIncreaseRateClass <- totalRevenueReductionRES/length(unique(cleanRES$meterID)) # Divide by number of resi customers. All customers in rate class see increase (not just non-RTP).
percentRESbillIncreaseRateClass <- totalRevenueReductionRES/sum(revenueReductionRES$Total_Energy_Revenue_EV_TOU_5)
# This would require an 0.6173753% increase in non-RTP customer bills to recover the lost revenue,
# or $7.26 per non-RTP EV-TOU-5 customer per year.
# These customers are (slightly) more expensive to serve, so this is actually undoing a previous cost shift.

rm(revenueReductionRES, totalRevenueReductionRES, perCustomerRevenueReductionRES, percentRevenueReductionRES, perCustomerRESbillIncreaseRateClass, percentRESbillIncreaseRateClass)

# 3.	For 2019, identify the 600 fifteen-minute intervals with the highest DLAP-level fifteen-minute prices.

# Note: Modified the methodology here: customer will also be load-shifting during the 150 hours with C-CPP adders, which are assumed to occur during the hours with CAISO highest net load.
# There is surprising little overlap between the top 150 hours with highest net load, and the top 600 15-minute intervals with highest real-time LMPs.
# There are only 41 intervals (6.8333% of the total) with both Top 600 LMPs as well as nonzero C-CPP adder values.
# There are 559 intervals with LMPs in the top 600 but no C-CPP adder, and 559 intervals with nonzero C-CPP adder vallues but with LMPs not in the top 600.

Clean_SDGE_RT5M_LMP <- readRDS(file.path(Wholesale_Price_WD, "Clean_SDGE_RT5M_LMP_2019_15_min.rds"))

highest600LMPs2019 <- Clean_SDGE_RT5M_LMP %>%
  top_n(n = 600, wt = LMP_RT5M)

CAISO_Net_Demand <- readRDS(file.path(Net_Load_WD, "CAISO_Net_Demand.rds"))

highest600NetLoad2019 <- CAISO_Net_Demand %>%
  top_n(n = 600, wt = CAISO_Net_Demand_MW)

loadReductionIntervals <- highest600LMPs2019 %>%
  full_join(highest600NetLoad2019, by = "Date_Time") %>%
  mutate(loadReductionIntervalFlag = 1) %>%
  select(Date_Time, loadReductionIntervalFlag)

# Month Distribution of Top 600 LMPs
ggplot(highest600LMPs2019 %>%
        mutate(Month = lubridate::month(Date_Time)),
      aes(Month)) +
  geom_histogram(binwidth = 1) +
  xlab("Month of Year") +
  scale_x_continuous(breaks=seq(1,12)) +
  ylab("Intervals in Top 600 LMPs") +
  ggtitle("Intervals in Top 600 LMPs by Month of Year") +
  theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5))

ggsave("Intervals in Top 600 LMPs by Month of Year.png", width = 11, height = 8.5, units = "in")

highest600LMPs2019MonthSummary <- highest600LMPs2019 %>%
  mutate(Month = lubridate::month(Date_Time)) %>%
  group_by(Month) %>%
  summarize(Top_600_LMP_Interval_Count = n()) %>%
  ungroup() %>%
  mutate(Percent_of_Total = Top_600_LMP_Interval_Count/sum(Top_600_LMP_Interval_Count))

write.csv(highest600LMPs2019MonthSummary, "Highest 600 LMPs 2019 Month Summary.csv", row.names = F)


# Hour-of-Day Distribution of Top 600 LMPs
ggplot(highest600LMPs2019 %>%
        mutate(Hour = lubridate::hour(Date_Time)),
      aes(Hour)) +
  geom_histogram(binwidth = 1) +
  xlab("Hour of Day") +
  ylab("Intervals in Top 600 LMPs") +
  ggtitle("Intervals in Top 600 LMPs by Hour of Day") +
  theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5))

ggsave("Intervals in Top 600 LMPs by Hour of Day.png", width = 11, height = 8.5, units = "in")

highest600LMPs2019HoursSummary <- highest600LMPs2019 %>%
  mutate(Hour = lubridate::hour(Date_Time)) %>%
  group_by(Hour) %>%
  summarize(Top_600_LMP_Interval_Count = n()) %>%
  ungroup() %>%
  mutate(Percent_of_Total = Top_600_LMP_Interval_Count/sum(Top_600_LMP_Interval_Count))

write.csv(highest600LMPs2019HoursSummary, "Highest 600 LMPs 2019 Hours Summary.csv", row.names = F)

rm(highest600LMPs2019, highest600NetLoad2019, highest600LMPs2019MonthSummary, highest600LMPs2019HoursSummary)


# 4.	For the following questions (5 through 12), assume uptake of RTP by 10% of the residential customer base in each baseline territory.
# Assume that the customers adopting RTP have, as a group, average load profiles and total consumption for their respective baseline territories.

# Will use aggregate residential profile multiplied by 10% for for questions 5-12.
 # This is equivalent to if exactly 10% of customers (by load, not by count) switched to RTP, and if their aggregate profile was identical to that of the broader group.


# 5.	For each of the 600 intervals identified in #3, estimate the total reduction in peak kWh consumed assuming actual load is curtailed by: (i) 25% and (ii) 50%.
# Additionally, estimate the reduction in SDG&E peak kW for 2019 resulting from these assumed load shifts.

# Note: load is reduced during both the top 600 LMP intervals as well as the 600 intervals with C-CPP adders.

aggParticipantRES <- aggRES %>%
  mutate(Demand_kWh = Demand_kWh * (10/100))

shifted25RES <- aggParticipantRES %>%
  left_join(loadReductionIntervals, by = "Date_Time") %>%
  mutate(loadReductionIntervalFlag = !is.na(loadReductionIntervalFlag)) %>%
  mutate(Demand_Shift_kWh = -Demand_kWh * loadReductionIntervalFlag * (25/100)) # 10% of the customers reduce demand by 25%

peakReductionRES25 <- -sum(shifted25RES$loadReductionIntervalFlag * shifted25RES$Demand_Shift_kWh)/nrow(loadReductionIntervals) # Divide by 1159 hours of curtailment.
perCustomerPeakReductionRES25 <- (peakReductionRES25 * 4)/(0.1 * length(unique(cleanRES$meterID))) # Multiply by 4 to convert from 15-minute kWh to average kW. Divide by number of RTP customers (10% of total)
peakReductionPercentRES25 <- peakReductionRES25/(sum(shifted25RES$loadReductionIntervalFlag * shifted25RES$Demand_kWh * (100/10))/nrow(loadReductionIntervals))
# Demand during these 1159 high-priced intervals is reduced by 2.5% on average, if 10% of customers reduce demand by 25%.
# The average reduction by an RTP customer is 0.207 kW.

shifted50RES <- aggParticipantRES %>%
  left_join(loadReductionIntervals, by = "Date_Time") %>%
  mutate(loadReductionIntervalFlag = !is.na(loadReductionIntervalFlag)) %>%
  mutate(Demand_Shift_kWh = -Demand_kWh * loadReductionIntervalFlag * (50/100)) # 10% of the customers reduce demand by 50%

peakReductionRES50 <- -sum(shifted50RES$loadReductionIntervalFlag * shifted50RES$Demand_Shift_kWh)/nrow(loadReductionIntervals) # Divide by 1159 hours of curtailment.
perCustomerPeakReductionRES50 <- (peakReductionRES50 * 4)/(0.1 * length(unique(cleanRES$meterID))) # Multiply by 4 to convert from 15-minute kWh to average kW. Divide by number of RTP customers (10% of total)
peakReductionPercentRES50 <- peakReductionRES50/(sum(shifted50RES$loadReductionIntervalFlag * shifted50RES$Demand_kWh * (100/10))/nrow(loadReductionIntervals))
# Demand during these 1159 high-priced intervals is reduced by 5% on average, if 10% of customers reduce demand by 50%.
# The average reduction by an RTP customer is 0.41444 kW.

rm(peakReductionRES25, peakReductionPercentRES25, peakReductionRES50, peakReductionPercentRES50)

# 6.	For each of the curtailment scenarios in #4, assume 100% of the curtailed load is shifted to:
# the lowest-priced intervals on the same day or no more than 12 hours subsequent to the high-priced intervals.

# Methodology: the number of intervals of curtailment are assumed to be equal to the number of intervals of load increase for every day.
# For example, if a given day has 2 intervals (30 minutes) with high LMPs or C-CPP adders, it's assumed that load is shifted to the 2 intervals of that day with lowest LMPs.

shifted25RES <- shifted25RES %>%
  left_join(Clean_SDGE_RT5M_LMP, by = "Date_Time") %>%
  mutate(Date = as.Date(Date_Time, tz = "America/Los_Angeles")) %>%
  group_by(Date) %>%
  mutate(lmpRanking = rank(LMP_RT5M, ties.method = "first")) %>%
  mutate(dailyLoadReductionIntervalCount = sum(loadReductionIntervalFlag)) %>%
  mutate(dailyLowLMPFlag = lmpRanking <= dailyLoadReductionIntervalCount) %>%
  mutate(Demand_Increase_kWh = ifelse(dailyLoadReductionIntervalCount > 0, -sum(Demand_Shift_kWh)/dailyLoadReductionIntervalCount * dailyLowLMPFlag, 0)) %>%
  mutate(Demand_Shift_kWh = Demand_Shift_kWh + Demand_Increase_kWh) %>%
  ungroup()

shifted50RES <- shifted50RES %>%
  left_join(Clean_SDGE_RT5M_LMP, by = "Date_Time") %>%
  mutate(Date = as.Date(Date_Time, tz = "America/Los_Angeles")) %>%
  group_by(Date) %>%
  mutate(lmpRanking = rank(LMP_RT5M, ties.method = "first")) %>%
  mutate(dailyLoadReductionIntervalCount = sum(loadReductionIntervalFlag)) %>%
  mutate(dailyLowLMPFlag = lmpRanking <= dailyLoadReductionIntervalCount) %>%
  mutate(Demand_Increase_kWh = ifelse(dailyLoadReductionIntervalCount > 0, -sum(Demand_Shift_kWh)/dailyLoadReductionIntervalCount * dailyLowLMPFlag, 0)) %>%
  mutate(Demand_Shift_kWh = Demand_Shift_kWh + Demand_Increase_kWh) %>%
  ungroup()

# 7.	For the load curtailed in #5, estimate SDG&E’s avoided costs. In the avoided cost calculation, include the differences between the high- and low-priced wholesale energy purchased
#     as well as the avoided Resource Adequacy benefits that accrue in two years resulting from RTP-induced reductions in peak load as reflected in future load forecasts.
#     The future avoided RA value may be discounted to the current year using SDG&E’s standard discount rate.

# If load is reduced by 25% during the top LMP and C-CPP hours (and shifted to other hours), SDG&E's avoided wholesale cost is equal to $10.22 per residential customer per year, or 4.82% of current costs.
avoidedWholesaleCostShifted25RES <- -sum(shifted25RES$Demand_Shift_kWh * shifted25RES$LMP_RT5M)
perCustomerAvoidedWholesaleCostShifted25RES <- avoidedWholesaleCostShifted25RES/(0.1 * length(unique(cleanRES$meterID)))
avoidedWholesaleCostPercentShifted25RES <- avoidedWholesaleCostShifted25RES/sum(shifted25RES$Demand_kWh * shifted25RES$LMP_RT5M)

# Assuming a peak load reduction of 0.207 kW per customer (from Question #5) and a planning reserve margin of 15%,
# avoided per-customer generation capacity cost is equal to $12.87/year using a low-end cost of $4.50/kW-month (85th percentile 2018-2022 capacity price from 2018 CPUC Resource Adequacy Report).
# Avoided per-customer generation capacity cost is equal to $33.46/year using a high-end cost of $11.70/kW-month (from 2019 SDG&E testimony).
avoidedGenCapCostLowShifted25RES <- perCustomerPeakReductionRES25 * 1.15 * 4.50 * 12
avoidedGenCapCostHighShifted25RES <- perCustomerPeakReductionRES25 * 1.15 * 11.70 * 12

# If load is reduced by 50% during the top LMP and C-CPP intervals (and shifted to other intervals), SDG&E's avoided cost is equal to $20.44 per residential customer per year, or 9.65% of current costs.
avoidedWholesaleCostShifted50RES <- -sum(shifted50RES$Demand_Shift_kWh * shifted50RES$LMP_RT5M)
perCustomerAvoidedWholesaleCostShifted50RES <- avoidedWholesaleCostShifted50RES/(0.1 * length(unique(cleanRES$meterID)))
avoidedWholesaleCostPercentShifted50RES <- avoidedWholesaleCostShifted50RES/sum(shifted50RES$Demand_kWh * shifted50RES$LMP_RT5M)

# Assuming a peak load reduction of 0.414 kW per customer (from Question #5) and a planning reserve margin of 15%,
# avoided per-customer generation capacity cost is equal to $25.74/year using a low-end cost of $4.50/kW-month (85th percentile 2018-2022 capacity price from 2018 CPUC Resource Adequacy Report).
# Avoided per-customer generation capacity cost is equal to $66.92/year using a high-end cost of $11.70/kW-month (from 2019 SDG&E testimony).
avoidedGenCapCostLowShifted50RES <- perCustomerPeakReductionRES50 * 1.15 * 4.50 * 12
avoidedGenCapCostHighShifted50RES <- perCustomerPeakReductionRES50 * 1.15 * 11.70 * 12

rm(perCustomerPeakReductionRES25, perCustomerPeakReductionRES50)
rm(avoidedWholesaleCostShifted25RES, perCustomerAvoidedWholesaleCostShifted25RES, avoidedWholesaleCostPercentShifted25RES)
rm(avoidedWholesaleCostShifted50RES, perCustomerAvoidedWholesaleCostShifted50RES, avoidedWholesaleCostPercentShifted50RES)


# 8.	Estimate the avoided greenhouse gases that result from the RTP participants’ load shifting.

Clean_SDGE_SGIP_MOER <- readRDS(file.path(Emissions_WD, "Clean_SDGE_SGIP_MOER.rds"))

shifted25RES <- shifted25RES %>%
  left_join(Clean_SDGE_SGIP_MOER, by = "Date_Time")

shifted50RES <- shifted50RES %>%
  left_join(Clean_SDGE_SGIP_MOER, by = "Date_Time")

avoidedGHGShifted25RES <- -sum(shifted25RES$Demand_Shift_kWh * shifted25RES$moer)
perCustomerAvoidedGHGShifted25RES <- avoidedGHGShifted25RES/(0.1 * length(unique(cleanRES$meterID)))
avoidedGHGPercentShifted25RES <- -sum(shifted25RES$Demand_Shift_kWh * shifted25RES$moer)/sum(shifted25RES$Demand_kWh * shifted25RES$moer)
# If load is reduced by 25% during the top LMP and C-CPP intervals (and shifted to other intervals),
# SDG&E's avoided GHG emissions are equal to 19.4 kg per residential customer per year, or 1.44% of current emissions.

avoidedGHGShifted50RES <- -sum(shifted50RES$Demand_Shift_kWh * shifted50RES$moer)
perCustomerAvoidedGHGShifted50RES <- avoidedGHGShifted50RES/(0.1 * length(unique(cleanRES$meterID)))
avoidedGHGPercentShifted50RES <- avoidedGHGShifted50RES/sum(shifted50RES$Demand_kWh * shifted50RES$moer)
# If load is reduced by 50% during the top LMP and C-CPP intervals (and shifted to other intervals),
# SDG&E's avoided GHG emissions are equal to 38.8 kg per residential customer per year, or 2.88% of current emissions.

rm(avoidedGHGShifted25RES, perCustomerAvoidedGHGShifted25RES, avoidedGHGPercentShifted25RES, avoidedGHGShifted50RES, perCustomerAvoidedGHGShifted50RES, avoidedGHGPercentShifted50RES)

# 9.	Estimate the reduced revenue from the RTP participants compared to the revenues SDG&E would have received if the customers had remained on TOU-DR-1
# and had not modified the timing of their electricity consumption.

# Modified analysis - used EV-TOU-5 as basis for comparison rather than TOU-DR-1.

shifted25RES <- shifted25RES %>%
  left_join(SDGE_EV_TOU_5, by = "Date_Time") %>%
  left_join(SDGE_EV_TOU_5_RTP, by = "Date_Time")

shifted50RES <- shifted50RES %>%
  left_join(SDGE_EV_TOU_5, by = "Date_Time") %>%
  left_join(SDGE_EV_TOU_5_RTP, by = "Date_Time")

totalAnnualRevenue_EV_TOU_5 <- sum(shifted25RES$Demand_kWh * shifted25RES$Total_Energy_Rate_EV_TOU_5)/(0.1 * length(unique(cleanRES$meterID))) # $1,176.575 per residential customer per year

totalAnnualRevenue_EV_TOU_5_RTP <- sum(shifted25RES$Demand_kWh * shifted25RES$Total_Energy_Rate_EV_TOU_5_RTP)/(0.1 * length(unique(cleanRES$meterID))) # $1,176.575 (same as above, because rate is revenue-neutral)

# Total revenue collected per residential RTP customer per year with 25% load-shifting is $1,139.91 (3.12% lower than revenue collected without 25% load-shifting).
totalAnnualRevenue_EV_TOU_5_RTP_shifted25 <- sum((shifted25RES$Demand_kWh + shifted25RES$Demand_Shift_kWh) * shifted25RES$Total_Energy_Rate_EV_TOU_5_RTP)/(0.1 * length(unique(cleanRES$meterID)))
revenueReductionPercentage_EV_TOU_5_RTP_shifted25 <- (totalAnnualRevenue_EV_TOU_5_RTP - totalAnnualRevenue_EV_TOU_5_RTP_shifted25)/totalAnnualRevenue_EV_TOU_5_RTP

# Total revenue collected per residential RTP customer per year with 50% load-shifting is $1,103.25 (6.23% lower than revenue collected without 50% load-shifting).
totalAnnualRevenue_EV_TOU_5_RTP_shifted50 <- sum((shifted50RES$Demand_kWh + shifted50RES$Demand_Shift_kWh) * shifted50RES$Total_Energy_Rate_EV_TOU_5_RTP)/(0.1 * length(unique(cleanRES$meterID)))
revenueReductionPercentage_EV_TOU_5_RTP_shifted50 <- (totalAnnualRevenue_EV_TOU_5_RTP - totalAnnualRevenue_EV_TOU_5_RTP_shifted50)/totalAnnualRevenue_EV_TOU_5_RTP

# Total C-CPP Savings with 25% load-shifting
CPPSavings_shifted25RES <- shifted25RES %>%
  left_join(CAISO_Net_Demand, by = "Date_Time") %>%
  top_n(n = 600, wt = CAISO_Net_Demand_MW)
perCustomerSavingsCPP_EV_TOU_5_RTP_shifted25 <- -sum(CPPSavings_shifted25RES$Demand_Shift_kWh) * 0.47718/(0.1 * length(unique(cleanRES$meterID)))
# The average customer performing 25% load shifting saves $17.95 per year.

# Total C-CPP Savings with 50% load-shifting
CPPSavings_shifted50RES <- shifted50RES %>%
  left_join(CAISO_Net_Demand, by = "Date_Time") %>%
  top_n(n = 600, wt = CAISO_Net_Demand_MW)
perCustomerSavingsCPP_EV_TOU_5_RTP_shifted50 <- -sum(CPPSavings_shifted50RES$Demand_Shift_kWh) * 0.47718/(0.1 * length(unique(cleanRES$meterID)))
# The average customer performing 50% load shifting saves $35.90 per year.


# Total undercollection per residential RTP customer per year with 25% load-shifting is $5.08 for the low avoided generation capacity cost scenario.
perCustomerUndercollection_LowShifted25RES <- perCustomerSavingsCPP_EV_TOU_5_RTP_shifted25 - avoidedGenCapCostLowShifted25RES

# Total overcollection per residential RTP customer per year with 25% load-shifting is $15.51 for the high avoided generation capacity cost scenario.
perCustomerOvercollection_HighShifted25RES <- avoidedGenCapCostHighShifted25RES - perCustomerSavingsCPP_EV_TOU_5_RTP_shifted25

# Total undercollection per residential RTP customer per year with 50% load-shifting is $10.16 for the low avoided generation capacity cost scenario.
perCustomerUndercollection_LowShifted50RES <- perCustomerSavingsCPP_EV_TOU_5_RTP_shifted50 - avoidedGenCapCostLowShifted50RES

# Total overcollection per residential RTP customer per year with 50% load-shifting is $31.02 for the high avoided generation capacity cost scenario.
perCustomerOvercollection_HighShifted50RES <- avoidedGenCapCostHighShifted50RES - perCustomerSavingsCPP_EV_TOU_5_RTP_shifted50

rm(avoidedGenCapCostLowShifted25RES, avoidedGenCapCostHighShifted25RES, avoidedGenCapCostLowShifted50RES, avoidedGenCapCostHighShifted50RES)
rm(CPPSavings_shifted25RES, CPPSavings_shifted50RES, perCustomerSavingsCPP_EV_TOU_5_RTP_shifted25, perCustomerSavingsCPP_EV_TOU_5_RTP_shifted50)
rm(perCustomerUndercollection_LowShifted25RES, perCustomerOvercollection_HighShifted25RES, perCustomerUndercollection_LowShifted50RES, perCustomerOvercollection_HighShifted50RES)

rm(totalAnnualRevenue_EV_TOU_5, totalAnnualRevenue_EV_TOU_5_RTP)
rm(totalAnnualRevenue_EV_TOU_5_RTP_shifted25, revenueReductionPercentage_EV_TOU_5_RTP_shifted25, totalAnnualRevenue_EV_TOU_5_RTP_shifted50, revenueReductionPercentage_EV_TOU_5_RTP_shifted50)

rm(shifted25RES, shifted50RES)


# Omitting Questions 10 through 15, because prior rounds of analysis found that "reverse-load-shifting" returned lower economic and GHG benefits.

rm(cleanRES, aggRES, aggParticipantRES, SDGE_EV_TOU_5, SDGE_EV_TOU_5_RTP)

# Non-Residential Customer Class
# 16.	Using AL-TOU as a basis and 2019 CAISO wholesale pricing data, create a real-time pricing rate that is revenue neutral across the general service class.
#     By revenue neutral, we mean that SDG&E would receive the same revenue on an annual basis from all general customers assuming no change in the quantity or timing of usage.
#     The rate should have the following characteristics.
# a.	The basic service fee, demand charge, and energy charge components of the UDC should be identical to AL-TOU.
# b.	The commodity rate should include a component to cover SDG&E’s costs of procuring and delivering electricity from renewable sources used to comply with SDG&E’s RPS obligation.
#     SDG&E should allocate a set percentage of RPS energy to each interval according to the percentage of RPS-eligible renewable energy in SDG&E’s portfolio during the calendar year.
#     The portion of the total kWh consumed by a customer in each billing interval covered by the RPS allocation will be charged according the cost of RPS energy.
#     Electricity consumed above the RPS allocation will be billed at the real-time price described in bullet “f” below.
#     The RPS allocation of electricity should be shaped to ensure that most customers will consume more than the RPS allocation in each billing interval.
#     The price and/or shape of the RPS allocation could be fixed across the year or could vary by time to better match the timing and cost of the renewable generation in SDG&E’s portfolio.
# c.	The commodity rate should include a component to cover any above-market energy costs from non-renewable sources that would appear in SDG&E’s PCIA charges.
# d.	The commodity rate should include the generation capacity cost associated with each TOU period in AL-TOU.
# e.	The summer on-peak generation capacity costs should be disaggregated into the cost to serve the top 50 hours, cost to serve hours 51 – 150, and cost to serve all other hours.
#     The cost to serve the top 50 hours and hours 51 – 150 should be designed as two different dynamic rate components called on a day-ahead basis.
# f.	Remaining generation energy costs will be recovered from a real-time pricing component using CAISO’s fifteen-minute market prices CA SDG&E’s Default Load Aggregation Point,
#     grossed up for losses in the distribution system.

# See Rates/SDG&E AL-TOU-RTP. This rate does not account for RPS or above-market costs directly. It also does not modify the approach taken to recovering generation capacity costs.

SDGE_AL_TOU <- readRDS(file.path(Rates_WD, "SDG&E AL-TOU", "2019", "15-Minute Data", "SDGE_AL_TOU.rds"))

SDGE_AL_TOU_RTP <- readRDS(file.path(Rates_WD, "SDG&E AL-TOU-RTP", "2019", "15-Minute Data", "SDGE_AL_TOU_RTP.rds"))

# Check for revenue neutrality - load up aggregated load profile, calculate total annual costs with new and old rate, confirm that they're equal.
aggLRG <- readRDS(file.path(Meter_WD, "aggLRG.rds"))

neutralityCheckLRG <- aggLRG %>%
  left_join(SDGE_AL_TOU, by = "Date_Time") %>%
  left_join(SDGE_AL_TOU_RTP, by = "Date_Time") %>%
  mutate(Total_Energy_Revenue_SDGE_AL_TOU = Demand_kWh * Total_Energy_Rate_AL_TOU,
         Total_Energy_Revenue_SDGE_AL_TOU_RTP = Demand_kWh * Total_Energy_Rate_AL_TOU_RTP) %>%
  summarize(Total_Energy_Revenue_SDGE_AL_TOU = sum(Total_Energy_Revenue_SDGE_AL_TOU),
            Total_Energy_Revenue_SDGE_AL_TOU_RTP = sum(Total_Energy_Revenue_SDGE_AL_TOU_RTP))

# neutralityCheckLRG$Total_Energy_Revenue_SDGE_AL_TOU and neutralityCheckLRG$Total_Energy_Revenue_SDGE_AL_TOU_RTP are effectively identical.
# This confirms that the AL-TOU-RTP rate is revenue-neutral with the AL-TOU rate for the pool of residential customers used in this analysis.

rm(neutralityCheckLRG)

# 17.	Calculate the reduction in revenues SDG&E would collect assuming the top 10% of general service customers who would benefit the most from the rate designed in response to Question 16
# opt-in to the rate with no change in their electricity usage.
# What is the amount by which general service non-RTP customers’ bills would need to increase on an annual basis to cover the revenue shortfall?

# Load large commercial/industrial meter data
cleanLRG <- readRDS(file.path(Meter_WD, "cleanLRG.rds"))

revenueReductionLRG <- cleanLRG %>%
  left_join(SDGE_AL_TOU, by = "Date_Time") %>%
  left_join(SDGE_AL_TOU_RTP, by = "Date_Time") %>%
  mutate(Total_Energy_Revenue_AL_TOU = Demand_kWh * Total_Energy_Rate_AL_TOU,
         Total_Energy_Revenue_AL_TOU_RTP = Demand_kWh * Total_Energy_Rate_AL_TOU_RTP) %>%
  group_by(meterID) %>%
  summarize(Total_Energy_Revenue_AL_TOU = sum(Total_Energy_Revenue_AL_TOU),
            Total_Energy_Revenue_AL_TOU_RTP = sum(Total_Energy_Revenue_AL_TOU_RTP)) %>%
  ungroup()

revenueReductionLRG <- revenueReductionLRG %>%
  mutate(rateSwitchSavings = Total_Energy_Revenue_AL_TOU - Total_Energy_Revenue_AL_TOU_RTP) %>%
  mutate(rateSwitchSavingsRank = rank(-rateSwitchSavings, ties.method = "first")) %>%
  mutate(top10Percent = rateSwitchSavingsRank <= round(nrow(revenueReductionLRG) * (10/100))) %>%
  mutate(revenueReduction = rateSwitchSavings * top10Percent)

totalRevenueReductionLRG <- sum(revenueReductionLRG$revenueReduction)
perCustomerRevenueReductionLRG <- totalRevenueReductionLRG/round(nrow(revenueReductionLRG) * (10/100))
percentRevenueReductionLRG <- totalRevenueReductionLRG/sum(revenueReductionLRG$Total_Energy_Revenue_AL_TOU)
# Having 10% of AL-TOU customers switch to AL-TOU-RTP with no change in usage would reduce revenue collection by 0.4162704%.
# The average structural benefiter would save $1988.26 per year.

perCustomerLRGbillIncreaseRateClass <- totalRevenueReductionLRG/length(unique(cleanLRG$meterID)) # Divide by number of C&I customers. All customers in rate class see increase (not just non-RTP).
percentLRGbillIncreaseRateClass <- totalRevenueReductionLRG/sum(revenueReductionLRG$Total_Energy_Revenue_AL_TOU)
# This would require an 0.4162704% increase in rates across the rate-class to recover the lost revenue, or $199.73 per AL-TOU customer per year.
# This is actually undoing a previous cost shift.

rm(revenueReductionLRG, totalRevenueReductionLRG, perCustomerRevenueReductionLRG, percentRevenueReductionLRG, perCustomerLRGbillIncreaseRateClass, percentLRGbillIncreaseRateClass)


# 18.	For the following questions (19 through 28), assume uptake of RTP by 10% of the general service (AL-TOU) customer base.
#     Assume that the customers adopting RTP have, as a group, average load profiles and total consumption for this customer class.

# Will use aggregated large commercial/industrial profile multiplied by 10% for questions 19-28.
# This is equivalent to if exactly 10% of customers (by load, not by count) switched to RTP, and if their aggregate profile was identical to that of the broader group.


# 19.	For each of the 600 intervals identified in #18, estimate the total reduction in peak kWh consumed assuming actual load is curtailed by:
#       (i) 25% and
#       (ii) 50%.
#     Additionally, estimate the reduction in SDG&E peak kW for 2019 resulting from these assumed load shifts.

# Note: load is reduced during both the top 600 LMP intervals as well as the 600 intervals with C-CPP adders.

aggParticipantLRG <- aggLRG %>%
  mutate(Demand_kWh = Demand_kWh * (10/100))

shifted25LRG <- aggParticipantLRG %>%
  left_join(loadReductionIntervals, by = "Date_Time") %>%
  mutate(loadReductionIntervalFlag = !is.na(loadReductionIntervalFlag)) %>%
  mutate(Demand_Shift_kWh = -Demand_kWh * loadReductionIntervalFlag * (25/100)) # 10% of the customers reduce demand by 25%

peakReductionLRG25 <- -sum(shifted25LRG$loadReductionIntervalFlag * shifted25LRG$Demand_Shift_kWh)/nrow(loadReductionIntervals) # Divide by 1159 hours of curtailment.
perCustomerPeakReductionLRG25 <- (peakReductionLRG25 * 4)/(0.1 * length(unique(cleanLRG$meterID))) # Multiply by 4 to convert from 15-minute kWh to average kW. Divide by number of RTP customers (10% of total)
peakReductionPercentLRG25 <- peakReductionLRG25/(sum(shifted25LRG$loadReductionIntervalFlag * shifted25LRG$Demand_kWh * (100/10))/nrow(loadReductionIntervals))
# Demand during these 1159 high-priced intervals is reduced by 2.5% on average, if 10% of customers reduce demand by 25%.
# The average reduction by an RTP customer is 15.01 kW.

shifted50LRG <- aggParticipantLRG %>%
  left_join(loadReductionIntervals, by = "Date_Time") %>%
  mutate(loadReductionIntervalFlag = !is.na(loadReductionIntervalFlag)) %>%
  mutate(Demand_Shift_kWh = -Demand_kWh * loadReductionIntervalFlag * (50/100)) # 10% of the customers reduce demand by 50%

peakReductionLRG50 <- -sum(shifted50LRG$loadReductionIntervalFlag * shifted50LRG$Demand_Shift_kWh)/nrow(loadReductionIntervals) # Divide by 1159 hours of curtailment.
perCustomerPeakReductionLRG50 <- (peakReductionLRG50 * 4)/(0.1 * length(unique(cleanLRG$meterID))) # Multiply by 4 to convert from 15-minute kWh to average kW. Divide by number of RTP customers (10% of total)
peakReductionPercentLRG50 <- peakReductionLRG50/(sum(shifted50LRG$loadReductionIntervalFlag * shifted50LRG$Demand_kWh * (100/10))/nrow(loadReductionIntervals))
# Demand during these 1159 high-priced intervals is reduced by 5% on average, if 10% of customers reduce demand by 50%.
# The average reduction by an RTP customer is 30.03 kW.

rm(peakReductionLRG25, peakReductionPercentLRG25, peakReductionLRG50, peakReductionPercentLRG50)


# 20.	For each of the curtailment scenarios in #19, assume 100% of the curtailed load is shifted to:
#     the lowest-priced intervals on the same day or no more than 12 hours subsequent to the high-priced intervals.

# Methodology: the number of intervals of curtailment are assumed to be equal to the number of intervals of load increase for every day.
# For example, if a given day has 2 intervals (30 minutes) with LMPs in the annual top 600, it's assumed that load is shifted to the 2 intervals of that day with lowest LMPs.

shifted25LRG <- shifted25LRG %>%
  left_join(Clean_SDGE_RT5M_LMP, by = "Date_Time") %>%
  mutate(Date = as.Date(Date_Time, tz = "America/Los_Angeles")) %>%
  group_by(Date) %>%
  mutate(lmpRanking = rank(LMP_RT5M, ties.method = "first")) %>%
  mutate(dailyLoadReductionIntervalCount = sum(loadReductionIntervalFlag)) %>%
  mutate(dailyLowLMPFlag = lmpRanking <= dailyLoadReductionIntervalCount) %>%
  mutate(Demand_Increase_kWh = ifelse(dailyLoadReductionIntervalCount > 0, -sum(Demand_Shift_kWh)/dailyLoadReductionIntervalCount * dailyLowLMPFlag, 0)) %>%
  mutate(Demand_Shift_kWh = Demand_Shift_kWh + Demand_Increase_kWh) %>%
  ungroup()

shifted50LRG <- shifted50LRG %>%
  left_join(Clean_SDGE_RT5M_LMP, by = "Date_Time") %>%
  mutate(Date = as.Date(Date_Time, tz = "America/Los_Angeles")) %>%
  group_by(Date) %>%
  mutate(lmpRanking = rank(LMP_RT5M, ties.method = "first")) %>%
  mutate(dailyLoadReductionIntervalCount = sum(loadReductionIntervalFlag)) %>%
  mutate(dailyLowLMPFlag = lmpRanking <= dailyLoadReductionIntervalCount) %>%
  mutate(Demand_Increase_kWh = ifelse(dailyLoadReductionIntervalCount > 0, -sum(Demand_Shift_kWh)/dailyLoadReductionIntervalCount * dailyLowLMPFlag, 0)) %>%
  mutate(Demand_Shift_kWh = Demand_Shift_kWh + Demand_Increase_kWh) %>%
  ungroup()


# 21.	For the load curtailed in #19, estimate SDG&E’s avoided costs. In the avoided cost calculation, include the differences between the high- and low-priced wholesale energy purchased
# as well as the avoided Resource Adequacy benefits that accrue in two years resulting from RTP-induced reductions in peak load as reflected in future load forecasts.
# The future avoided RA value may be discounted to the current year using SDG&E’s standard discount rate.

# If load is reduced by 25% during the top LMP and C-CPP hours (and shifted to other hours), SDG&E's avoided wholesale cost is equal to $880.35 per AL-TOU-RTP customer per year, or 5.05% of current wholesale costs.
avoidedWholesaleCostShifted25LRG <- -sum(shifted25LRG$Demand_Shift_kWh * shifted25LRG$LMP_RT5M)
perCustomerAvoidedWholesaleCostShifted25LRG <- avoidedWholesaleCostShifted25LRG/(0.1 * length(unique(cleanLRG$meterID)))
avoidedWholesaleCostPercentShifted25LRG <- avoidedWholesaleCostShifted25LRG/sum(shifted25LRG$Demand_kWh * shifted25LRG$LMP_RT5M)

# Assuming a peak load reduction of 15.01 kW per customer (from Question #19) and a planning reserve margin of 15%,
# avoided per-customer generation capacity cost is equal to $932.12/year using a low-end cost of $4.50/kW-month (85th percentile 2018-2022 capacity price from 2018 CPUC Resource Adequacy Report).
# Avoided per-customer generation capacity cost is equal to $2,423.96/year using a high-end cost of $11.70/kW-month (from 2019 SDG&E testimony).
avoidedGenCapCostLowShifted25LRG <- perCustomerPeakReductionLRG25 * 1.15 * 4.50 * 12
avoidedGenCapCostHighShifted25LRG <- perCustomerPeakReductionLRG25 * 1.15 * 11.70 * 12


# If load is reduced by 50% during the top LMP and C-CPP intervals (and shifted to other intervals),
# SDG&E's avoided wholesale cost is equal to $1,760.69 per AL-TOU-RTP customer per year, or 10.11% of current wholesale costs.
avoidedWholesaleCostShifted50LRG <- -sum(shifted50LRG$Demand_Shift_kWh * shifted50LRG$LMP_RT5M)
perCustomerAvoidedWholesaleCostShifted50LRG <- avoidedWholesaleCostShifted50LRG/(0.1 * length(unique(cleanLRG$meterID)))
avoidedWholesaleCostPercentShifted50LRG <- avoidedWholesaleCostShifted50LRG/sum(shifted50LRG$Demand_kWh * shifted50LRG$LMP_RT5M)

# Assuming a peak load reduction of 30.03 kW per customer (from Question #19) and a planning reserve margin of 15%,
# avoided per-customer generation capacity cost is equal to $1,864.59/year using a low-end cost of $4.50/kW-month (85th percentile 2018-2022 capacity price from 2018 CPUC Resource Adequacy Report).
# Avoided per-customer generation capacity cost is equal to $4,847.93/year using a high-end cost of $11.70/kW-month (from 2019 SDG&E testimony).
avoidedGenCapCostLowShifted50LRG <- perCustomerPeakReductionLRG50 * 1.15 * 4.50 * 12
avoidedGenCapCostHighShifted50LRG <- perCustomerPeakReductionLRG50 * 1.15 * 11.70 * 12

rm(perCustomerPeakReductionLRG25, perCustomerPeakReductionLRG50)
rm(avoidedWholesaleCostShifted25LRG, perCustomerAvoidedWholesaleCostShifted25LRG, avoidedWholesaleCostPercentShifted25LRG)
rm(avoidedWholesaleCostShifted50LRG, perCustomerAvoidedWholesaleCostShifted50LRG, avoidedWholesaleCostPercentShifted50LRG)


# 22.	Estimate the avoided greenhouse gases that result from the RTP participants’ load shifting.

shifted25LRG <- shifted25LRG %>%
  left_join(Clean_SDGE_SGIP_MOER, by = "Date_Time")

shifted50LRG <- shifted50LRG %>%
  left_join(Clean_SDGE_SGIP_MOER, by = "Date_Time")

avoidedGHGShifted25LRG <- -sum(shifted25LRG$Demand_Shift_kWh * shifted25LRG$moer)
perCustomerAvoidedGHGShifted25LRG <- avoidedGHGShifted25LRG/(0.1 * length(unique(cleanLRG$meterID)))
avoidedGHGPercentShifted25LRG <- -sum(shifted25LRG$Demand_Shift_kWh * shifted25LRG$moer)/sum(shifted25LRG$Demand_kWh * shifted25LRG$moer)
# If load is reduced by 25% during the top LMP and C-CPP intervals (and shifted to other intervals),
# SDG&E's avoided GHG emissions are equal to 1,489.24 kg per AL-TOU-RTP customer per year, or 1.31% of current emissions.

avoidedGHGShifted50LRG <- -sum(shifted50LRG$Demand_Shift_kWh * shifted50LRG$moer)
perCustomerAvoidedGHGShifted50LRG <- avoidedGHGShifted50LRG/(0.1 * length(unique(cleanLRG$meterID)))
avoidedGHGPercentShifted50LRG <- avoidedGHGShifted50LRG/sum(shifted50LRG$Demand_kWh * shifted50LRG$moer)
# If load is reduced by 50% during the top LMP and C-CPP intervals (and shifted to other intervals),
# SDG&E's avoided GHG emissions are equal to 2,978.48 kg per AL-TOU-RTP customer per year, or 2.63% of current emissions.

rm(avoidedGHGShifted25LRG, perCustomerAvoidedGHGShifted25LRG, avoidedGHGPercentShifted25LRG, avoidedGHGShifted50LRG, perCustomerAvoidedGHGShifted50LRG, avoidedGHGPercentShifted50LRG)


# 23.	Estimate the reduced revenue from the RTP participants compared to the revenues SDG&E would have received if the customers had remained on AL-TOU
# and had not modified the timing of their electricity consumption.

shifted25LRG <- shifted25LRG %>%
  left_join(SDGE_AL_TOU, by = "Date_Time") %>%
  left_join(SDGE_AL_TOU_RTP, by = "Date_Time")

shifted50LRG <- shifted50LRG %>%
  left_join(SDGE_AL_TOU, by = "Date_Time") %>%
  left_join(SDGE_AL_TOU_RTP, by = "Date_Time")

totalAnnualRevenue_AL_TOU <- sum(shifted25LRG$Demand_kWh * shifted25LRG$Total_Energy_Rate_AL_TOU)/(0.1 * length(unique(cleanLRG$meterID))) # 47,981.82 per C&I customer per year.

totalAnnualRevenue_AL_TOU_RTP <- sum(shifted25LRG$Demand_kWh * shifted25LRG$Total_Energy_Rate_AL_TOU_RTP)/(0.1 * length(unique(cleanLRG$meterID))) # 47,981.82 (same as above, because rate is revenue-neutral)

# Total revenue collected per AL-TOU-RTP customer per year with 25% load-shifting is $46,117.02 (3.89% lower than revenue collected without 25% load-shifting).
totalAnnualRevenue_AL_TOU_RTP_shifted25 <- sum((shifted25LRG$Demand_kWh + shifted25LRG$Demand_Shift_kWh) * shifted25LRG$Total_Energy_Rate_AL_TOU_RTP)/(0.1 * length(unique(cleanLRG$meterID)))
revenueReductionPercentage_AL_TOU_RTP_shifted25 <- (totalAnnualRevenue_AL_TOU_RTP - totalAnnualRevenue_AL_TOU_RTP_shifted25)/totalAnnualRevenue_AL_TOU_RTP

# Total revenue collected per AL-TOU-RTP customer per year with 50% load-shifting is $44,252.22 (7.77% lower than revenue collected without 50% load-shifting).
totalAnnualRevenue_AL_TOU_RTP_shifted50 <- sum((shifted50LRG$Demand_kWh + shifted50LRG$Demand_Shift_kWh) * shifted50LRG$Total_Energy_Rate_AL_TOU_RTP)/(0.1 * length(unique(cleanLRG$meterID)))
revenueReductionPercentage_AL_TOU_RTP_shifted50 <- (totalAnnualRevenue_AL_TOU_RTP - totalAnnualRevenue_AL_TOU_RTP_shifted50)/totalAnnualRevenue_AL_TOU_RTP

# Total C-CPP Savings with 25% load shifting
CPPSavings_shifted25LRG <- shifted25LRG %>%
  left_join(CAISO_Net_Demand, by = "Date_Time") %>%
  top_n(n = 600, wt = CAISO_Net_Demand_MW)
perCustomerSavingsCPP_AL_TOU_RTP_shifted25 <- -sum(CPPSavings_shifted25LRG$Demand_Shift_kWh) * 0.47718/(0.1 * length(unique(cleanLRG$meterID)))
# The average customer performing 25% load shifting saves $1,185.52 per year.

# Total C-CPP Savings with 50% load shifting
CPPSavings_shifted50LRG <- shifted50LRG %>%
  left_join(CAISO_Net_Demand, by = "Date_Time") %>%
  top_n(n = 600, wt = CAISO_Net_Demand_MW)
perCustomerSavingsCPP_AL_TOU_RTP_shifted50 <- -sum(CPPSavings_shifted50LRG$Demand_Shift_kWh) * 0.47718/(0.1 * length(unique(cleanLRG$meterID)))
# The average customer performing 50% load shifting saves $2,371.04 per year.

# Total undercollection per general service RTP customer per year with 25% load-shifting is $253.23 for the low avoided generation capacity cost scenario.
perCustomerUndercollection_LowShifted25LRG <- perCustomerSavingsCPP_AL_TOU_RTP_shifted25 - avoidedGenCapCostLowShifted25LRG

# Total overcollection per general service RTP customer per year with 25% load-shifting is $1,238.44 for the high avoided generation capacity cost scenario.
perCustomerOvercollection_HighShifted25LRG <- avoidedGenCapCostHighShifted25LRG - perCustomerSavingsCPP_AL_TOU_RTP_shifted25

# Total undercollection per general service RTP customer per year with 50% load-shifting is $506.45 for the low avoided generation capacity cost scenario.
perCustomerUndercollection_LowShifted50LRG <- perCustomerSavingsCPP_AL_TOU_RTP_shifted50 - avoidedGenCapCostLowShifted50LRG

# Total overcollection per general service RTP customer per year with 50% load-shifting is $2,476.89 for the high avoided generation capacity cost scenario.
perCustomerOvercollection_HighShifted50LRG <- avoidedGenCapCostHighShifted50LRG - perCustomerSavingsCPP_AL_TOU_RTP_shifted50

rm(avoidedGenCapCostLowShifted25LRG, avoidedGenCapCostHighShifted25LRG, avoidedGenCapCostLowShifted50LRG, avoidedGenCapCostHighShifted50LRG)
rm(CPPSavings_shifted25LRG, CPPSavings_shifted50LRG, perCustomerSavingsCPP_AL_TOU_RTP_shifted25, perCustomerSavingsCPP_AL_TOU_RTP_shifted50)
rm(perCustomerUndercollection_LowShifted25LRG, perCustomerOvercollection_HighShifted25LRG, perCustomerUndercollection_LowShifted50LRG, perCustomerOvercollection_HighShifted50LRG)

rm(totalAnnualRevenue_AL_TOU, totalAnnualRevenue_AL_TOU_RTP)
rm(totalAnnualRevenue_AL_TOU_RTP_shifted25, revenueReductionPercentage_AL_TOU_RTP_shifted25, totalAnnualRevenue_AL_TOU_RTP_shifted50, revenueReductionPercentage_AL_TOU_RTP_shifted50)

rm(shifted25LRG, shifted50LRG)

# Omitting Questions 24 through 28, because prior rounds of analysis found that "reverse-load-shifting" returned lower economic and GHG benefits.

rm(cleanLRG, aggLRG, aggParticipantLRG, SDGE_AL_TOU, SDGE_AL_TOU_RTP)
