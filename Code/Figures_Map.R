#Map systems by enfranchisement type.
##NOTE THIS SCRIPT REQUIRES FIRST RUNNING THE Data_compilation_SES.R script

#Load libraries
library(tidyverse)
library(sf)
library(ggmap)
library(mapproj)
library(maps)
library(gridExtra)

#load data 
Data_geo$Centroid <- NA
Data_geo$Centroid <- st_centroid(Data_geo$geometry) #Get centroids to service area boundaries
Data_geo <- Data_geo %>% drop_na(enfranchisement_final) #get rid of NAs. Mostly cuts out all the non-CWS but also some CWS in the analysis without service area boundary data

#Basemaps
caCountiesTmp <- tigris::counties(state = 06) %>%
  st_as_sf()

#export county data to fix county variable
Countydata <- Data_geo %>% select(SABL_PWSID, COUNTY) %>% st_drop_geometry()
write.csv(Countydata, file = here::here("Data_processed/Countydata.csv"))
Counties <- read.csv("Data_processed/Countydata_fixed.csv")
Data_geo <- left_join(Data_geo, Counties, by = "SABL_PWSID")
Data_geo$COUNTY_FIXED <- as.factor(Data_geo$COUNTY_FIXED)

caCountiesTmp_Mendocino <- caCountiesTmp %>% filter(NAME == "Mendocino")
Mendocino <- Data_geo %>% filter(Data_geo$COUNTY_FIXED == "MENDOCINO")

#maps - statewide
Enfranchisement_Type <- ggplot() +
  geom_sf(data = caCountiesTmp, fill = "white") +
  geom_sf(data = caCountiesTmp_Mendocino, linewidth = .75, fill = "white") +
  geom_sf(mapping = aes(colour = enfranchisement_final, geometry = Centroid), size = .35, data = Data_geo, inherit.aes = FALSE) +
  theme_void() +
  theme(legend.position = c(-0.0025, 0.2), 
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
        legend.direction = "vertical",
        legend.margin=margin(5,5,5,5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        panel.background = element_blank()
  ) +
  labs(color = "Enfranchisement type") +
  scale_color_brewer(palette = "Dark2") +
  theme(text = element_text(family = "Helvetica")); Enfranchisement_Type

ggsave('Figures/Fig1a.png', width = 7.08661, height = 8, dpi = 720, bg='#ffffff')


#maps - make smaller zoomed in on one county

Demo <- ggplot() +
  geom_sf(data = caCountiesTmp_Mendocino, fill = "white") +
  geom_sf(mapping = aes(colour = enfranchisement_final, geometry = geometry), data = Mendocino, 
          inherit.aes = FALSE) +
  theme_void() +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2") +
  theme(panel.background=element_blank()); Demo

ggsave('Figures/Fig1b.png', width = 4, height = 4, dpi = 720, bg='#ffffff')


#I think Mendocino is best, Combine into panel figure
Combined <- grid.arrange(Enfranchisement_Type, Demo, nrow=1)
