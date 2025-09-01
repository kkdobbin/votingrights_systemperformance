#Sensitivity tests conducted March 2025
## MUST RUN models script before this one

#################subbing enfranchisement for public private 
Data$public <- relevel(Data$public, ref = "Yes")

#Water quality
library(car)
library(regclass)
summary(WQ1)

WQ1P <- glm(WQ_combined_any ~  public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(WQ1P)
VIF(WQ1P)

WQ1P2 <- glm(WQ_combined_any ~ enfranchisement_final +  public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(WQ1P2)
VIF(WQ1P2) #VIF way too high

##WQ sub components

summary(ecoli)
VIF(ecoli)

ecoliP <- glm(HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(ecoliP)
VIF(ecoliP)

summary(TT)

TTP <- glm(TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(TTP)
VIF(TTP)

summary(MCL)

MCLP <- glm(PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MCLP)
VIF(MCLP)

#Affordability 
summary(Affordability1)

Affordability1P <- glm(Affordability_combined_any ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Affordability1P)
VIF(Affordability1P)

## Affordability sub components

summary(MHI)

MHIP <- glm(PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MHIP)

summary(extreme)

extremeP <- glm(EXTREME_WATER_BILL_RISK_LEVEL_BI ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(extremeP)

summary(covid)

covidP <- glm(didnotapplycovid ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(covidP)


# Accessibility

summary(Accessibility1)

Accessibility1P <- glm(Accessibility_combined_any ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Accessibility1P)

## Accessibility sub components

summary(Single_source)

Single_sourceP <- glm(NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Single_sourceP)

summary(Sourcecapacity_violations)

Sourcecapacity_violationsP <- glm(SOURCE_CAPACITY_VIOLATION_RISK_LEVEL ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Sourcecapacity_violationsP)

summary(Service_dis)

Service_disP <- glm(Service_disruptions ~ public + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Service_disP)
VIF(Service_disP)




##########################################################For profit versus not for profit instead of enfranchisement

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
summary(WQ1I2) #profit overlaps with enfranchisement too much

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


####################################Subbing specialized v no specialized for enfranchisement (not specialized includes MHP)
#subbing enfranchisement for special purpose v general purpose (very imperfect since we can't know how many services they actually do in practice but special districts specialized, general purpose not and private systems specialized)

Data$specialized <- ifelse(Data$Final_inst_update == "City" | 
                             Data$Final_inst_update == "County Waterworks District" |
                             Data$Final_inst_update == "Maintenance District" |
                             Data$Final_inst_update == "Mobile Home Park", "No", "Yes")

Data <- Data %>% mutate(specialized = case_when(PWSID == "CA2800526" ~ "No", .default = specialized), 
                        specialized = case_when(PWSID == "CA2810013" ~ "No", .default = specialized)) #NAPA Resort Improvement Districts are subsidary districts managed by county (along with Glenn county) everywhere else they are independent special districts

Data$specialized <- as.factor(Data$specialized)
summary(Data$specialized)
Data$specialized <- relevel(Data$specialized, ref = "No")

#Water quality
summary(WQ1)
VIF(WQ1)

WQ1S <- glm(WQ_combined_any ~ specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(WQ1S)

WQ1S2 <- glm(WQ_combined_any ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(WQ1S2)
VIF(WQ1S2) #VIF okay use both for sensitivity analysis

##WQ sub components

summary(ecoli)

ecoliS2 <- glm(HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(ecoliS2)

summary(TT)

TTS2 <- glm(TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(TTS2)

summary(MCL)

MCLS2 <- glm(PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MCLS2)

#Affordability 
summary(Affordability1)

Affordability1S2 <- glm(Affordability_combined_any ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Affordability1S2)

## Affordability sub components

summary(MHI)

MHIS2 <- glm(PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MHIS2)

summary(extreme)

extremeS2 <- glm(EXTREME_WATER_BILL_RISK_LEVEL_BI ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(extremeS2)

summary(covid)

covidS2 <- glm(didnotapplycovid ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(covidS2)

# Accessibility

summary(Accessibility1)

Accessibility1S2 <- glm(Accessibility_combined_any ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Accessibility1S2)

## Accessibility sub components

summary(Single_source)

Single_sourceS2 <- glm(NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Single_sourceS2)

summary(Sourcecapacity_violations)

Sourcecapacity_violationsS2 <- glm(SOURCE_CAPACITY_VIOLATION_RISK_LEVEL ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Sourcecapacity_violationsS2)

summary(Service_dis)

Service_disS2 <- glm(Service_disruptions ~ enfranchisement_final + specialized + POPULATION + Source + Purchased + MHI + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Service_disS2)

