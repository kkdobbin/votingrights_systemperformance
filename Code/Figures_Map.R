#Map systems by enfranchisement type. NOTE THIS SCRIPT REQUIRES FIRST RUNNING THE Data_compillation_SES.R script

#Load libraries
library(tidyverse)
library(sf)
library(ggmap)
library(mapproj)
library(maps)

#load data 
Data_geo$Centroid <- NA
Data_geo$Centroid <- st_centroid(Data_geo$geometry) #Get centroids to service area boundaries
Data_geo <- Data_geo %>% drop_na(enfranchisement_final) #get rid of NAs. Mostly cuts out all the non-CWS but also some CWS in the analysis without service area boundary data

#Basemap
caCountiesTmp <- tigris::counties(state = 06) %>%
  st_as_sf()

#maps
Enfranchisement_Type <- ggplot() +
  geom_sf(data = caCountiesTmp) +
  geom_sf(mapping = aes(colour = enfranchisement_final, geometry = Centroid), data = Data_geo, inherit.aes = FALSE) +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) +
  labs(color = "Enfranchisement type") +
  theme(panel.background=element_blank())

ggsave('Figures/Map.png', width = 4, height = 6, dpi = 300, bg='#ffffff')

Tulare <- Data_geo %>% filter(COUNTY == "TULARE")
