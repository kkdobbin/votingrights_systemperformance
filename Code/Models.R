# Models (This code should match Paper_analysisandtables.Rmd)

#LOAD LIBRARIES
library(tidyverse)
library(vtable)
library(sjPlot)

#LOAD AND READY DATA
Data <- read.csv(here::here("Data_processed/Systemperformancedata.csv"))
Data <- Data[,-1]
Data$enfranchisement_final <- as.factor(Data$enfranchisement_final)
Data$Final_inst_update <- as.factor(Data$Final_inst_update)

#correct data types, levels and reference factors
Data[,c(1:11,16, 19:44, 46:61)] <- lapply(Data[,c(1:11,16, 19:44, 46:61)], factor)
levels(Data$WQ_combined_count) <- c(levels(Data$WQ_combined_count), "4")

Data$FAILING <- ifelse(Data$FINAL_SAFER_STATUS == "Failing", "Failing", "Not failing")
Data$FAILING <- as.factor(Data$FAILING)

Data$Source <- as.factor(Data$Source)
Data$public <- as.factor(Data$public)

#re-order factor levels for enfranchisement to be full, limited, none
Data$enfranchisement_final <- factor(Data$enfranchisement_final, levels = c("Full", "Limited", "None"))

Data$FAILING <- relevel(Data$FAILING, ref = "Not failing")
Data$enfranchisement_final <- relevel(Data$enfranchisement_final, ref = "Full")

#Make self produced water the reference (compared to purchased)
Data$Purchased <- relevel(Data$Purchased, ref = "Self-produced")

Data$HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL <- relevel(Data$HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL, ref = "NONE")
Data$TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL <- relevel(Data$TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL, ref = "NONE")
Data$PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL <- relevel(Data$PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL, ref = "NONE")
Data$CONSTITUENTS_OF_EMERGING_CONCERN_RISK_LEVEL_BI <- relevel(Data$CONSTITUENTS_OF_EMERGING_CONCERN_RISK_LEVEL_BI, ref = "NONE")
Data$NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI <- relevel(Data$NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI, ref = "NONE")
Data$ABESENCE_OF_INTERTIES_RISK_LEVEL <- relevel(Data$ABESENCE_OF_INTERTIES_RISK_LEVEL, ref = "NONE")
Data$BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL <- relevel(Data$BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL, ref = "NONE")
Data$SOURCE_CAPACITY_VIOLATION_RISK_LEVEL <- relevel(Data$SOURCE_CAPACITY_VIOLATION_RISK_LEVEL, ref = "NONE")
Data$PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI <- relevel(Data$PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI, ref = "NONE")
Data$EXTREME_WATER_BILL_RISK_LEVEL_BI <- relevel(Data$EXTREME_WATER_BILL_RISK_LEVEL_BI, ref = "NONE")
Data$FUNDING_any_failingoratriskonly <- relevel(Data$FUNDING_any_failingoratriskonly, ref = "No")
Data$OPERATOR_CERTIFICATION_VIOLATIONS_RISK_LEVEL <- relevel(Data$OPERATOR_CERTIFICATION_VIOLATIONS_RISK_LEVEL, ref = "NONE")
Data$MONITORING_AND_REPORTING_VIOLATIONS_RISK_LEVEL <- relevel(Data$MONITORING_AND_REPORTING_VIOLATIONS_RISK_LEVEL, ref = "NONE")
Data$DAYS_CASH_ON_HAND_RISK_LEVEL <- relevel(Data$DAYS_CASH_ON_HAND_RISK_LEVEL, ref = "NONE")
Data$OPERATING_RATIO_RISK_LEVEL <- relevel(Data$OPERATING_RATIO_RISK_LEVEL, ref = "NONE")

#add in demographic data
Demographics <- read.csv(here::here("Data_processed/Systemdemographicdata.csv"))
Demographics <- Demographics[,-c(1,3)]
Demographics$PWSID <- as.factor(Demographics$PWSID)

Data <- left_join(Data, Demographics, by = "PWSID")

#MODELS
Failing <- glm(FAILING ~ enfranchisement_final + LN_POP + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Failing)

#Water quality
WQ1 <- glm(WQ_combined_any ~ enfranchisement_final + LN_POP + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(WQ1)

##WQ sub components
ecoli <- glm(HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL ~ enfranchisement_final + LN_POP + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(ecoli)

TT <- glm(TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL ~ enfranchisement_final + LN_POP + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(TT)

MCL <- glm(PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL ~ enfranchisement_final + LN_POP + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MCL)

#Affordability 
Affordability1 <- glm(Affordability_combined_any ~ enfranchisement_final + LN_POP + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Affordability1)

## Affordability sub components
MHI <- glm(PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI ~ enfranchisement_final + LN_POP + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MHI)

extreme <- glm(EXTREME_WATER_BILL_RISK_LEVEL_BI ~ enfranchisement_final + LN_POP + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(extreme)

covid <- glm(didnotapplycovid ~ enfranchisement_final + LN_POP + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(covid)

# TMF
TMF1 <- glm(TMF_combined_any ~ enfranchisement_final + LN_POP + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(TMF1)

## TMF sub components
Opcert_violations <- glm(OPERATOR_CERTIFICATION_VIOLATIONS_RISK_LEVEL ~ enfranchisement_final + LN_POP + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Opcert_violations)

mrviolations <- glm(MONITORING_AND_REPORTING_VIOLATIONS_RISK_LEVEL ~ enfranchisement_final + LN_POP + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(mrviolations)

cash <- glm(DAYS_CASH_ON_HAND_RISK_LEVEL ~ enfranchisement_final + LN_POP + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(cash)

