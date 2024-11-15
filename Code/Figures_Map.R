#Map systems by enfranchisement type. NOTE THIS SCRIPT REQUIRES FIRST RUNNING THE Data_compillation_SES.R script

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
  scale_color_brewer(palette = "Dark2") +
  theme(panel.background=element_blank())

ggsave('Figures/Map.png', width = 4, height = 6, dpi = 300, bg='#ffffff')

#make smaller zoomed in demo map 1
caCountiesTmp_Tulare <- caCountiesTmp %>% filter(NAME == "Tulare")

Tulare <- Data_geo %>% filter(COUNTY == "TULARE")
ExeterandTooleville <- Tulare %>% filter(SABL_PWSID == "CA5410003" | SABL_PWSID == "CA5400567")
ExeterandTooleville$Label <- c("City of Exeter", "Tooleville Mutual Water Company")

Demo1 <- ggplot() +
  geom_sf(mapping = aes(colour = enfranchisement_final, geometry = geometry), data = ExeterandTooleville, 
          inherit.aes = FALSE) +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) +
  labs(color = "Enfranchisement type") +
  scale_color_brewer(palette = "Dark2") +
  geom_sf_text(data = ExeterandTooleville, aes(label = Label))
  theme(panel.background=element_blank()); Demo1

ggsave('Figures/Demo_map1.png', width = 6, height = 3, dpi = 300, bg='#ffffff')

#make smaller zoomed in demo map 2
Kern <- Data_geo %>% filter(COUNTY == "KERN")
Kerndemo <- Kern %>% filter(SABL_PWSID == "CA1500588" | SABL_PWSID == "CA1510001")
Kerndemo$Label <- c("Arvin Community Services District", "Son Shine Properties")

Demo2 <- ggplot() +
  geom_sf(mapping = aes(colour = enfranchisement_final, geometry = geometry), data = Kerndemo, 
          inherit.aes = FALSE) +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) +
  labs(color = "Enfranchisement type") +
  scale_color_brewer(palette = "Dark2") +
  geom_sf_text(data = Kerndemo, aes(label = Label))
theme(panel.background=element_blank()); Demo2

ggsave('Figures/Demo_map2.png', width = 4, height = 3, dpi = 300, bg='#ffffff')


#make demo map of Tulare county
Demo3 <- ggplot() +
  geom_sf(data = caCountiesTmp_Tulare) +
  geom_sf(mapping = aes(colour = enfranchisement_final, geometry = geometry), data = Tulare, 
          inherit.aes = FALSE) +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) +
  labs(color = "Enfranchisement type") +
  scale_color_brewer(palette = "Dark2") +
theme(panel.background=element_blank()); Demo3

ggsave('Figures/Demo_map1.png', width = 6, height = 3, dpi = 300, bg='#ffffff')


Combined <- grid.arrange(Enfranchisement_Type, Demo1, nrow=1)
