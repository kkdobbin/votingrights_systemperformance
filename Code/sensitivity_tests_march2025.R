#Sensitivity tests March 2025
#MUST RUN models script before this one

#subbing enfranchisement for public private 
Data$public <- relevel(Data$public, ref = "Yes")

#Water quality
summary(WQ1)

WQ1P <- glm(WQ_combined_any ~  public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(WQ1P)
VIF(WQ1P)

WQ1P2 <- glm(WQ_combined_any ~ enfranchisement_final +  public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(WQ1P2)
VIF(WQ1P2)

##WQ sub components

summary(ecoli)
VIF(ecoli)

ecoliP <- glm(HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(ecoliP)

ecoliP2 <- glm(HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL ~ enfranchisement_final +  public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(ecoliP2)
VIF(ecoliP2)

summary(TT)

TTP <- glm(TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(TTP)

TTP2 <- glm(TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL ~ enfranchisement_final + public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(TTP2)


summary(MCL)

MCLP <- glm(PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MCLP)

MCLP2 <- glm(PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL ~ enfranchisement_final + public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MCLP2)

#Affordability 
summary(Affordability1)

Affordability1P <- glm(Affordability_combined_any ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Affordability1P)

Affordability1P2 <- glm(Affordability_combined_any ~ enfranchisement_final + public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Affordability1P2)

## Affordability sub components

summary(MHI)

MHIP <- glm(PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MHIP)

MHIP2 <- glm(PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI ~ enfranchisement_final + public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MHIP2)

summary(extreme)

extremeP <- glm(EXTREME_WATER_BILL_RISK_LEVEL_BI ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(extremeP)

extremeP2 <- glm(EXTREME_WATER_BILL_RISK_LEVEL_BI ~ enfranchisement_final + public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(extremeP2)

summary(covid)

covidP <- glm(didnotapplycovid ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(covidP)

covidP2 <- glm(didnotapplycovid ~ enfranchisement_final + public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(covidP2)

# Accessibility

summary(Accessibility1)

Accessibility1P <- glm(Accessibility_combined_any ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Accessibility1P)

Accessibility1P2 <- glm(Accessibility_combined_any ~ enfranchisement_final + public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Accessibility1P2)

## Accessibility sub components

summary(Single_source)

Single_sourceP <- glm(NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Single_sourceP)

Single_sourceP2 <- glm(NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI ~ enfranchisement_final + public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Single_sourceP2)


summary(Sourcecapacity_violations)

Sourcecapacity_violationsP <- glm(SOURCE_CAPACITY_VIOLATION_RISK_LEVEL ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Sourcecapacity_violationsP)

Sourcecapacity_violationsP2 <- glm(SOURCE_CAPACITY_VIOLATION_RISK_LEVEL ~ enfranchisement_final + public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Sourcecapacity_violationsP2)

summary(Service_dis)

Service_disP <- glm(Service_disruptions ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Service_disP)

Service_disP2 <- glm(Service_disruptions ~ enfranchisement_final + public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Service_disP2)

library(regclass)
VIF(Service_disP2)
VIF(Service_dis)

#subbing enfranchisement for special purpose v general purpose (very imperfect since we can't know how many services they actually do in practice but special districts specialized, general purpose not and private systems specialized)

Data$specialized <- ifelse(Data$Final_inst_update == "City" | 
                          Data$Final_inst_update == "County Waterworks District" |
                          Data$Final_inst_update == "Maintenance District", "No", "Yes")

Data <- Data %>% mutate(specialized = case_when(PWSID == "CA2800526" ~ "No", .default = specialized), 
                        specialized = case_when(PWSID == "CA2810013" ~ "No", .default = specialized)) #NAPA RIDs are subsidary districts managed by county (along with Glenn county) everywhere else they are independent special districts

Data$specialized <- as.factor(Data$specialized)
summary(Data$specialized)


#Water quality
summary(WQ1)
VIF(WQ1)

WQ1S <- glm(WQ_combined_any ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(WQ1S)

WQ1S2 <- glm(WQ_combined_any ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(WQ1S2)
VIF(WQ1S2)

##WQ sub components

summary(ecoli)

ecoliS <- glm(HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(ecoliS)

ecoliS2 <- glm(HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(ecoliS2)

summary(TT)

TTS <- glm(TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(TTS)

TTS2 <- glm(TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(TTS2)

summary(MCL)

MCLS <- glm(PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MCLS)

MCLS2 <- glm(PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MCLS2)

#Affordability 
summary(Affordability1)

Affordability1S <- glm(Affordability_combined_any ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Affordability1S)

Affordability1S2 <- glm(Affordability_combined_any ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Affordability1S2)

## Affordability sub components

summary(MHI)

MHIS <- glm(PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MHIS)

MHIS2 <- glm(PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MHIS2)

summary(extreme)

extremeS <- glm(EXTREME_WATER_BILL_RISK_LEVEL_BI ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(extremeS)

extremeS2 <- glm(EXTREME_WATER_BILL_RISK_LEVEL_BI ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(extremeS2)

summary(covid)

covidS <- glm(didnotapplycovid ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(covidS)

covidS2 <- glm(didnotapplycovid ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(covidS2)

# Accessibility

summary(Accessibility1)

Accessibility1S <- glm(Accessibility_combined_any ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Accessibility1S)

Accessibility1S2 <- glm(Accessibility_combined_any ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Accessibility1S2)

## Accessibility sub components

summary(Single_source)

Single_sourceS <- glm(NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Single_sourceS)

Single_sourceS2 <- glm(NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Single_sourceS2)

summary(Sourcecapacity_violations)

Sourcecapacity_violationsS <- glm(SOURCE_CAPACITY_VIOLATION_RISK_LEVEL ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Sourcecapacity_violationsS)

Sourcecapacity_violationsS2 <- glm(SOURCE_CAPACITY_VIOLATION_RISK_LEVEL ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Sourcecapacity_violationsS2)

summary(Service_dis)

Service_disS <- glm(Service_disruptions ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Service_disS)

Service_disS2 <- glm(Service_disruptions ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Service_disS2)

#For profit not for profit

Data$profit <- ifelse(Data$Final_inst_update == "Investor Owned Utility" | 
                        Data$Final_inst_update == "Mobile Home Park", "Yes", "No")

Data$profit <- as.factor(Data$profit)
summary(Data$profit)
Data$profit <- relevel(Data$profit, ref = "No")

#Water quality
summary(WQ1)
VIF(WQ1)

WQ1I <- glm(WQ_combined_any ~ profit + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(WQ1I)
VIF(WQ1I)

WQ1I2 <- glm(WQ_combined_any ~ enfranchisement_final + profit + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(WQ1I2)
VIF(WQ1I2)

##WQ sub components

summary(ecoli)

ecoliI <- glm(HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL ~ profit + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(ecoliI)

summary(TT)

TTI <- glm(TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL ~ profit + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(TTI)

summary(MCL)

MCLI <- glm(PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL ~ profit + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MCLI)

#Affordability 
summary(Affordability1)

Affordability1I <- glm(Affordability_combined_any ~ profit + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Affordability1I)

Affordability1I2 <- glm(Affordability_combined_any ~ enfranchisement_final + profit + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Affordability1I2)
VIF(Affordability1I2)

## Affordability sub components

summary(MHI)

MHII <- glm(PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI ~ profit + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MHII)

summary(extreme)

extremeI <- glm(EXTREME_WATER_BILL_RISK_LEVEL_BI ~ profit + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(extremeI)

summary(covid)

covidI <- glm(didnotapplycovid ~ profit + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(covidI)

# Accessibility

summary(Accessibility1)

Accessibility1I <- glm(Accessibility_combined_any ~ profit + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Accessibility1I)

## Accessibility sub components

summary(Single_source)

Single_sourceI <- glm(NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI ~ profit + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Single_sourceI)

summary(Sourcecapacity_violations)

Sourcecapacity_violationsI <- glm(SOURCE_CAPACITY_VIOLATION_RISK_LEVEL ~ profit + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Sourcecapacity_violationsI)

summary(Service_dis)

Service_disI <- glm(Service_disruptions ~ profit + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Service_disI)


#SPECIALIZED INCLUDING MHP
#subbing enfranchisement for special purpose v general purpose (very imperfect since we can't know how many services they actually do in practice but special districts specialized, general purpose not and private systems specialized)

Data$specialized2 <- ifelse(Data$Final_inst_update == "City" | 
                             Data$Final_inst_update == "County Waterworks District" |
                             Data$Final_inst_update == "Maintenance District" |
                             Data$Final_inst_update == "Mobile Home Park", "No", "Yes")

Data <- Data %>% mutate(specialized2 = case_when(PWSID == "CA2800526" ~ "No", .default = specialized2), 
                        specialized2 = case_when(PWSID == "CA2810013" ~ "No", .default = specialized2)) #NAPA RIDs are subsidary districts managed by county (along with Glenn county) everywhere else they are independent special districts

Data$specialized2 <- as.factor(Data$specialized2)
summary(Data$specialized2)


#Water quality
summary(WQ1)
VIF(WQ1)

WQ1S <- glm(WQ_combined_any ~ specialized2 + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(WQ1S)

WQ1S2 <- glm(WQ_combined_any ~ enfranchisement_final + specialized2 + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(WQ1S2)
VIF(WQ1S2)

##WQ sub components

summary(ecoli)

ecoliS <- glm(HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL ~ specialized2 + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(ecoliS)

ecoliS2 <- glm(HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL ~ enfranchisement_final + specialized2 + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(ecoliS2)

summary(TT)

TTS <- glm(TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(TTS)

TTS2 <- glm(TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(TTS2)

summary(MCL)

MCLS <- glm(PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL ~ specialized2 + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MCLS)

MCLS2 <- glm(PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL ~ enfranchisement_final + specialized2 + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MCLS2)

#Affordability 
summary(Affordability1)

Affordability1S <- glm(Affordability_combined_any ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Affordability1S)

Affordability1S2 <- glm(Affordability_combined_any ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Affordability1S2)

## Affordability sub components

summary(MHI)

MHIS <- glm(PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI ~ specialized2 + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MHIS)

MHIS2 <- glm(PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI ~ enfranchisement_final + specialized2 + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MHIS2)

summary(extreme)

extremeS <- glm(EXTREME_WATER_BILL_RISK_LEVEL_BI ~ specialized2 + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(extremeS)

extremeS2 <- glm(EXTREME_WATER_BILL_RISK_LEVEL_BI ~ enfranchisement_final + specialized2 + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(extremeS2)

summary(covid)

covidS <- glm(didnotapplycovid ~ specialized2 + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(covidS)

covidS2 <- glm(didnotapplycovid ~ enfranchisement_final + specialized2 + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(covidS2)

# Accessibility

summary(Accessibility1)

Accessibility1S <- glm(Accessibility_combined_any ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Accessibility1S)

Accessibility1S2 <- glm(Accessibility_combined_any ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Accessibility1S2)

## Accessibility sub components

summary(Single_source)

Single_sourceS <- glm(NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI ~ specialized2 + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Single_sourceS)

Single_sourceS2 <- glm(NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI ~ enfranchisement_final + specialized2 + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Single_sourceS2)

summary(Sourcecapacity_violations)

Sourcecapacity_violationsS <- glm(SOURCE_CAPACITY_VIOLATION_RISK_LEVEL ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Sourcecapacity_violationsS)

Sourcecapacity_violationsS2 <- glm(SOURCE_CAPACITY_VIOLATION_RISK_LEVEL ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Sourcecapacity_violationsS2)

summary(Service_dis)

Service_disS <- glm(Service_disruptions ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Service_disS)

Service_disS2 <- glm(Service_disruptions ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Service_disS2)