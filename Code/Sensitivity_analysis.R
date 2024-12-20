#Sensitivity analysis - breaking out enfranchisement types more


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

#create alternative to enfranchisement variable
Data$enfranchisement_alt <- NA
Data$enfranchisement_alt <- ifelse(Data$enfranchisement_final == "Limited", "Limited", Data$enfranchisement_alt)
Data$enfranchisement_alt <- ifelse(Data$enfranchisement_final == "Full", "Full - other", Data$enfranchisement_alt)
Data$enfranchisement_alt <- ifelse(Data$enfranchisement_final == "Full" & Data$Final_inst_update == "City", "Full - city", Data$enfranchisement_alt)
Data$enfranchisement_alt <- ifelse(Data$enfranchisement_final == "None", "None - IOU", Data$enfranchisement_alt)
Data$enfranchisement_alt <-  ifelse(Data$enfranchisement_final == "None" & Data$Final_inst_update == "Mobile Home Park", "None - MHP", Data$enfranchisement_alt)

Data$enfranchisement_alt <- as.factor(Data$enfranchisement_alt)

#MODELS
Failing_alt <- glm(FAILING ~ enfranchisement_alt + LN_POP + Source + Purchased + MHI_KD + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Failing_alt)

#Water quality
WQ1_alt <- glm(WQ_combined_any ~ enfranchisement_alt + LN_POP + Source + Purchased + MHI_KD + Percent.renter + Percent.white, data = Data, family= binomial)
summary(WQ1_alt)

##WQ sub components
ecoli_alt <- glm(HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL ~ enfranchisement_alt + LN_POP + Source + Purchased + MHI_KD + Percent.renter + Percent.white, data = Data, family= binomial)
summary(ecoli_alt)

TT_alt <- glm(TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL ~ enfranchisement_alt + LN_POP + Source + Purchased + MHI_KD + Percent.renter + Percent.white, data = Data, family= binomial)
summary(TT_alt)

MCL_alt <- glm(PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL ~ enfranchisement_alt + LN_POP + Source + Purchased + MHI_KD + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MCL_alt)

#Affordability 
Affordability1_alt <- glm(Affordability_combined_any ~ enfranchisement_alt + LN_POP + Source + Purchased + MHI_KD + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Affordability1_alt)

## Affordability sub components
MHI_alt <- glm(PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI ~ enfranchisement_alt + LN_POP + Source + Purchased + MHI_KD + Percent.renter + Percent.white, data = Data, family= binomial)
summary(MHI_alt)

extreme_alt <- glm(EXTREME_WATER_BILL_RISK_LEVEL_BI ~ enfranchisement_alt + LN_POP + Source + Purchased + MHI_KD + Percent.renter + Percent.white, data = Data, family= binomial)
summary(extreme_alt)

covid_alt <- glm(didnotapplycovid ~ enfranchisement_alt + LN_POP + Source + Purchased + MHI_KD + Percent.renter + Percent.white, data = Data, family= binomial)
summary(covid_alt)

# TMF
TMF1_alt <- glm(TMF_combined_any ~ enfranchisement_alt + LN_POP + Source + Purchased + MHI_KD + Percent.renter + Percent.white, data = Data, family= binomial)
summary(TMF1_alt)

## TMF sub components
Opcert_violations_alt <- glm(OPERATOR_CERTIFICATION_VIOLATIONS_RISK_LEVEL ~ enfranchisement_alt + LN_POP + Source + Purchased + MHI_KD + Percent.renter + Percent.white, data = Data, family= binomial)
summary(Opcert_violations_alt)

mrviolations_alt <- glm(MONITORING_AND_REPORTING_VIOLATIONS_RISK_LEVEL ~ enfranchisement_alt + LN_POP + Source + Purchased + MHI_KD + Percent.renter + Percent.white, data = Data, family= binomial)
summary(mrviolations_alt)

cash_alt <- glm(DAYS_CASH_ON_HAND_RISK_LEVEL ~ enfranchisement_alt + LN_POP + Source + Purchased + MHI_KD + Percent.renter + Percent.white, data = Data, family= binomial)
summary(cash_alt)

# Figures for sensitivity analysis

#Predicted probabilities figures for GLMs. 
library(marginaleffects)

#pull out probabilities in confidence intervals
Effect_WQ_alt <- predictions(WQ1_alt,
                         newdata = datagrid(enfranchisement_alt = c("Full - city", "Full - other", "Limited", "None - MHP", "None - IOU")))
Effect_Affordability_alt <- predictions(Affordability1_alt,
                                    newdata = datagrid(enfranchisement_alt = c("Full - city", "Full - other", "Limited", "None - MHP", "None - IOU")))
Effect_TMF_alt <- predictions(TMF1_alt,
                          newdata = datagrid(enfranchisement_alt = c("Full - city", "Full - other", "Limited", "None - MHP", "None - IOU")))

#add key and combine data sets
Effect_WQ_alt$Key <- "Water quality risk"
Effect_Affordability_alt$Key <- "Affordability risk"
Effect_TMF_alt$Key <- "TMF capacity risk"
Probabilities_alt <- bind_rows(Effect_WQ_alt, Effect_TMF_alt, Effect_Affordability_alt)
Probabilities_alt$Key <- factor(Probabilities_alt$Key, levels = c("Water quality risk", "TMF capacity risk", "Affordability risk"))


#make figure faceting with key
plotCombined_alt <- Probabilities_alt %>%
  ggplot(aes(x = enfranchisement_alt, y = estimate*100)) +
  geom_errorbar(aes(ymin = conf.low*100, ymax = conf.high*100, color = enfranchisement_alt), width = 0.2, show.legend = FALSE) +
  geom_point(aes(color = enfranchisement_alt), show.legend = FALSE) +
  xlab(" ") +
  ylab(" ") +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~Key, scales = "free", nrow = 1, ncol = 3) +
  cowplot::theme_half_open(); plotCombined_alt

ggsave("Fig2_alt.jpeg", plotCombined_alt, path = "Figures/", height = 6, width = 7.487, units = "in", dpi = 720)

#water quality subindicators figure
Effect_WQ_ecoli_alt <- predictions(ecoli_alt,
                               newdata = datagrid(enfranchisement_alt = c("Full - city", "Full - other", "Limited", "None - MHP", "None - IOU")))
Effect_WQ_TT_alt <- predictions(TT_alt,
                            newdata = datagrid(enfranchisement_alt = c("Full - city", "Full - other", "Limited", "None - MHP", "None - IOU")))
Effect_WQ_MCL_alt <- predictions(MCL_alt,
                             newdata = datagrid(enfranchisement_alt = c("Full - city", "Full - other", "Limited", "None - MHP", "None - IOU")))


Effect_WQ_ecoli_alt$Key <- "E. coli"
Effect_WQ_TT_alt$Key <- "Treatment Technique violations"
Effect_WQ_MCL_alt$Key <- "MCL exceedences"

Probabilities_WQ_alt <- bind_rows(Effect_WQ_ecoli_alt, Effect_WQ_MCL_alt, Effect_WQ_TT_alt)
Probabilities_WQ_alt$Category <- "Water quality"

plotCombined_WQ_alt <- Probabilities_WQ_alt %>%
  ggplot(aes(x = enfranchisement_alt, y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "grey") +
  geom_point() +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~Key, scales = "free", nrow = 2, ncol = 2) +
  cowplot::theme_half_open(); plotCombined_WQ_alt

#affordability subindicators figure
Effect_AF_MHI_alt <- predictions(MHI_alt,
                             newdata = datagrid(enfranchisement_alt = c("Full - city", "Full - other", "Limited", "None - MHP", "None - IOU")))
Effect_AF_extreme_alt <- predictions(extreme_alt,
                                 newdata = datagrid(enfranchisement_alt = c("Full - city", "Full - other", "Limited", "None - MHP", "None - IOU")))
Effect_AF_covid_alt <- predictions(covid_alt,
                               newdata = datagrid(enfranchisement_alt = c("Full - city", "Full - other", "Limited", "None - MHP", "None - IOU")))


Effect_AF_MHI_alt$Key <- "Unaffordable bill"
Effect_AF_extreme_alt$Key <- "Extreme bill"
Effect_AF_covid_alt$Key <- "No arrearage relief"
Probabilities_AF_alt <- bind_rows(Effect_AF_MHI_alt, Effect_AF_extreme_alt, Effect_AF_covid_alt)
Probabilities_AF_alt$Category <- "Affordability"

plotCombined_AF_alt <- Probabilities_AF_alt %>%
  ggplot(aes(x = enfranchisement_alt, y = estimate*100)) +
  geom_errorbar(aes(ymin = conf.low*100, ymax = conf.high*100, color = enfranchisement_alt), width = 0.2, show.legend = FALSE) +
  geom_point(aes(color = enfranchisement_alt), show.legend = FALSE) +
  xlab(" ") +
  ylab(" ") +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~Key, scale = "free", nrow = 1, ncol = 3) +
  cowplot::theme_half_open() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)); plotCombined_AF_alt

ggsave("FigSIAF_alt.jpeg", plotCombined_AF_alt, path = "Figures/", height = 6, width = 9, units = "in", dpi = 720)

#TMF subindicators figure
Effect_TMF_opcertviolations_alt <- predictions(Opcert_violations_alt,
                                           newdata = datagrid(enfranchisement_alt = c("Full - city", "Full - other", "Limited", "None - MHP", "None - IOU")))
Effect_TMF_mrviolations_alt <- predictions(mrviolations_alt,
                                       newdata = datagrid(enfranchisement_alt = c("Full - city", "Full - other", "Limited", "None - MHP", "None - IOU")))
Effect_TMF_cash_alt <- predictions(cash_alt,
                               newdata = datagrid(enfranchisement_alt = c("Full - city", "Full - other", "Limited", "None - MHP", "None - IOU")))

Effect_TMF_opcertviolations_alt$Key <- "Operator certification violations"
Effect_TMF_mrviolations_alt$Key <- "M&R violations"
Effect_TMF_cash_alt$Key <- "<30 days cash on hand"
Probabilities_TMF_alt <- bind_rows(Effect_TMF_opcertviolations_alt, Effect_TMF_mrviolations_alt, Effect_TMF_cash_alt)
Probabilities_TMF_alt$Category <- "TMF capacity"

plotCombined_TMF_alt <- Probabilities_TMF_alt %>%
  ggplot(aes(x = enfranchisement_alt, y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "grey") +
  geom_point() +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~Key, scales = "free", nrow = 2, ncol = 2) +
  cowplot::theme_half_open(); plotCombined_TMF_alt

#Make one massive combined plot with all twelve
library(RColorBrewer)

#Probabilities_MASSIVE <- bind_rows(Effect_WQ_ecoli, Effect_WQ_CEC, Effect_WQ_MCL, Effect_WQ_TT, Effect_AF_MHI, Effect_AF_extreme, Effect_AF_covid, Effect_AF_funding, Effect_AC_sources, Effect_AC_interties, Effect_AC_bottled, Effect_AC_sourcecapacity, Effect_TMF_opcertviolations, Effect_TMF_mrviolations, Effect_TMF_cash, Effect_TMF_operating)
Probabilities_MASSIVE_alt <- bind_rows(Probabilities_WQ_alt, Probabilities_AF_alt, Probabilities_TMF_alt)
Probabilities_MASSIVE_alt$Key <- as.factor(Probabilities_MASSIVE_alt$Key)
Probabilities_MASSIVE_alt$Key <- factor(Probabilities_MASSIVE_alt$Key, levels=c("E. coli", "MCL exceedences", "Treatment Technique violations", "Bill ≥ 150% of statewide average", "Bill ≥ 1.5% of MHI", "Did not apply for arrearage relief", "Operator certification violations", "M&R violations", "<30 days cash on hand"))

plotCombined_MASSIVE_alt <- Probabilities_MASSIVE_alt %>%
  ggplot(aes(x = enfranchisement_alt, y = estimate*100)) +
  geom_errorbar(aes(ymin = conf.low*100, ymax = conf.high*100, color = enfranchisement_alt), width = 0.2, show.legend = FALSE) +
  geom_point(aes(color = enfranchisement_alt), show.legend = FALSE) +
  xlab(" ") +
  ylab(" ") +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~Category+Key, scales = "free", nrow = 4, ncol = 3) +
  cowplot::theme_half_open(); plotCombined_MASSIVE_alt

ggsave("Fig3_alt.jpeg", plotCombined_MASSIVE_alt, path = "Figures/", height = 6, width = 9, units = "in", dpi = 720)


