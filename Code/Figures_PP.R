#Predicted probabilities figures for GLMs. NOTE THIS SCRIPT REQUIRES FIRST RUNNING THE Models.R script
library(marginaleffects)

#pull out probabilities in confidence intervals
Effect_WQ <- predictions(WQ1,
                         newdata = datagrid(enfranchisement_final = c("Full", "Limited", "None")))
Effect_Affordability <- predictions(Affordability1,
                                    newdata = datagrid(enfranchisement_final = c("Full", "Limited", "None")))
Effect_TMF <- predictions(TMF1,
                          newdata = datagrid(enfranchisement_final = c("Full", "Limited", "None")))

#add key and combine data sets
Effect_WQ$Key <- "Water quality risk"
Effect_Affordability$Key <- "Affordability risk"
Effect_TMF$Key <- "TMF capacity risk"
Probabilities <- bind_rows(Effect_WQ, Effect_TMF, Effect_Affordability)
Probabilities$Key <- factor(Probabilities$Key, levels = c("Water quality risk", "TMF capacity risk", "Affordability risk"))


#make figure faceting with key
plotCombined <- Probabilities %>%
  ggplot(aes(x = enfranchisement_final, y = estimate*100)) +
  geom_errorbar(aes(ymin = conf.low*100, ymax = conf.high*100, color = enfranchisement_final), width = 0.2, show.legend = FALSE) +
  geom_point(aes(color = enfranchisement_final), show.legend = FALSE) +
  xlab(" ") +
  ylab(" ") +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~Key, scales = "free", nrow = 1, ncol = 3) +
  cowplot::theme_half_open(); plotCombined

ggsave("Fig2.jpeg", plotCombined, path = "Figures/", height = 6, width = 7.487, units = "in", dpi = 720)

#water quality subindicators figure
Effect_WQ_ecoli <- predictions(ecoli,
                               newdata = datagrid(enfranchisement_final = c("Full", "Limited", "None")))
Effect_WQ_TT <- predictions(TT,
                            newdata = datagrid(enfranchisement_final = c("Full", "Limited", "None")))
Effect_WQ_MCL <- predictions(MCL,
                             newdata = datagrid(enfranchisement_final = c("Full", "Limited", "None")))


Effect_WQ_ecoli$Key <- "E. coli"
Effect_WQ_TT$Key <- "Treatment Technique violations"
Effect_WQ_MCL$Key <- "MCL exceedences"

Probabilities_WQ <- bind_rows(Effect_WQ_ecoli, Effect_WQ_MCL, Effect_WQ_TT)
Probabilities_WQ$Category <- "Water quality"

plotCombined_WQ <- Probabilities_WQ %>%
  ggplot(aes(x = enfranchisement_final, y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "grey") +
  geom_point() +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~Key, scales = "free", nrow = 2, ncol = 2) +
  cowplot::theme_half_open(); plotCombined_WQ

#affordability subindicators figure
Effect_AF_MHI <- predictions(MHI,
                             newdata = datagrid(enfranchisement_final = c("Full", "Limited", "None")))
Effect_AF_extreme <- predictions(extreme,
                                 newdata = datagrid(enfranchisement_final = c("Full", "Limited", "None")))
Effect_AF_covid <- predictions(covid,
                               newdata = datagrid(enfranchisement_final = c("Full", "Limited", "None")))


Effect_AF_MHI$Key <- "Bill ≥ 1.5% of MHI"
Effect_AF_extreme$Key <- "Bill ≥ 150% of statewide average"
Effect_AF_covid$Key <- "Did not apply for arrearage relief"
Probabilities_AF <- bind_rows(Effect_AF_MHI, Effect_AF_extreme, Effect_AF_covid)
Probabilities_AF$Category <- "Affordability"

plotCombined_AF <- Probabilities_AF %>%
  ggplot(aes(x = enfranchisement_final, y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "grey") +
  geom_point() +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~Key, scale = "free", nrow = 2, ncol = 2) +
  cowplot::theme_half_open(); plotCombined_AF

#TMF subindicators figure
Effect_TMF_opcertviolations <- predictions(Opcert_violations,
                                           newdata = datagrid(enfranchisement_final = c("Full", "Limited", "None")))
Effect_TMF_mrviolations <- predictions(mrviolations,
                                       newdata = datagrid(enfranchisement_final = c("Full", "Limited", "None")))
Effect_TMF_cash <- predictions(cash,
                               newdata = datagrid(enfranchisement_final = c("Full", "Limited", "None")))

Effect_TMF_opcertviolations$Key <- "Operator certification violations"
Effect_TMF_mrviolations$Key <- "M&R violations"
Effect_TMF_cash$Key <- "<30 days cash on hand"
Probabilities_TMF <- bind_rows(Effect_TMF_opcertviolations, Effect_TMF_mrviolations, Effect_TMF_cash)
Probabilities_TMF$Category <- "TMF capacity"

plotCombined_TMF <- Probabilities_TMF %>%
  ggplot(aes(x = enfranchisement_final, y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "grey") +
  geom_point() +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~Key, scales = "free", nrow = 2, ncol = 2) +
  cowplot::theme_half_open(); plotCombined_TMF

#Make one massive combined plot with all twelve
library(RColorBrewer)

#Probabilities_MASSIVE <- bind_rows(Effect_WQ_ecoli, Effect_WQ_CEC, Effect_WQ_MCL, Effect_WQ_TT, Effect_AF_MHI, Effect_AF_extreme, Effect_AF_covid, Effect_AF_funding, Effect_AC_sources, Effect_AC_interties, Effect_AC_bottled, Effect_AC_sourcecapacity, Effect_TMF_opcertviolations, Effect_TMF_mrviolations, Effect_TMF_cash, Effect_TMF_operating)
Probabilities_MASSIVE <- bind_rows(Probabilities_WQ, Probabilities_AF, Probabilities_TMF)
Probabilities_MASSIVE$Key <- as.factor(Probabilities_MASSIVE$Key)
Probabilities_MASSIVE$Key <- factor(Probabilities_MASSIVE$Key, levels=c("E. coli", "MCL exceedences", "Treatment Technique violations", "Bill ≥ 150% of statewide average", "Bill ≥ 1.5% of MHI", "Did not apply for arrearage relief", "Operator certification violations", "M&R violations", "<30 days cash on hand"))

plotCombined_MASSIVE <- Probabilities_MASSIVE %>%
  ggplot(aes(x = enfranchisement_final, y = estimate*100)) +
  geom_errorbar(aes(ymin = conf.low*100, ymax = conf.high*100, color = enfranchisement_final), width = 0.2, show.legend = FALSE) +
  geom_point(aes(color = enfranchisement_final), show.legend = FALSE) +
  xlab(" ") +
  ylab(" ") +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~Category+Key, scales = "free", nrow = 4, ncol = 3) +
  cowplot::theme_half_open(); plotCombined_MASSIVE

ggsave("Fig3.jpeg", plotCombined_MASSIVE, path = "Figures/", height = 6, width = 9, units = "in", dpi = 720)

#Make bar graph for enfranchisement variables

Bargraph <- ggplot(Data, aes(enfranchisement_final)) + 
  geom_bar(aes(fill = enfranchisement_final), show.legend = FALSE) + 
  scale_fill_brewer(palette = "Dark2") +
  xlab(" ") +
  ylab(" "); Bargraph