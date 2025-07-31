#Map systems by enfranchisement type.
## NOTE THIS SCRIPT REQUIRES FIRST RUNNING THE Data_compilation_SES.R script

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
  geom_sf(data = caCountiesTmp, fill = "white") +
  geom_sf(data = caCountiesTmp_Mendocino, linewidth = .75, fill = "white") +
  geom_sf(mapping = aes(colour = enfranchisement_final, geometry = Centroid), size = .35, data = Data_geo, inherit.aes = FALSE) +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.direction = "vertical",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) +
  labs(color = "Enfranchisement type") +
  scale_color_brewer(palette = "Dark2") +
  theme(panel.background=element_blank()); Enfranchisement_Type

ggsave('Figures/Map.png', width = 5, height = 8, dpi = 300, bg='#ffffff')

#export county data to fix county variable
Countydata <- Data_geo %>% select(SABL_PWSID, COUNTY) %>% st_drop_geometry()
write.csv(Countydata, file = here::here("Data_processed/Countydata.csv"))
Counties <- read.csv("Data_processed/Countydata_fixed.csv")
Data_geo <- left_join(Data_geo, Counties, by = "SABL_PWSID")
Data_geo$COUNTY_FIXED <- as.factor(Data_geo$COUNTY_FIXED)

#make smaller zoomed in demo map 1
caCountiesTmp_Tulare <- caCountiesTmp %>% filter(NAME == "Tulare")

Tulare <- Data_geo %>% filter(Data_geo$COUNTY_FIXED == "TULARE")
ExeterandTooleville <- Data_geo %>% filter(SABL_PWSID == "CA5410003" | SABL_PWSID == "CA5400567")
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

ggsave('Figures/Demo_map2.png', width = 6, height = 3, dpi = 300, bg='#ffffff')

#Make a county map for a more simple smaller/county

caCountiesTmp_Mendocino <- caCountiesTmp %>% filter(NAME == "Mendocino")
Mendocino <- Data_geo %>% filter(Data_geo$COUNTY_FIXED == "MENDOCINO")

Demo4 <- ggplot() +
  geom_sf(data = caCountiesTmp_Mendocino, fill = "white") +
  geom_sf(mapping = aes(colour = enfranchisement_final, geometry = geometry), data = Mendocino, 
          inherit.aes = FALSE) +
  theme_void() +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2") +
  theme(panel.background=element_blank()); Demo4

ggsave('Figures/Demo_map3.png', width = 5, height = 5, dpi = 300, bg='#ffffff')

#Make a county map for a more simple smaller/county

caCountiesTmp_Yolo <- caCountiesTmp %>% filter(NAME == "Yolo")
Yolo <- Data_geo %>% filter(Data_geo$COUNTY_FIXED == "YOLO")

Demo5 <- ggplot() +
  geom_sf(data = caCountiesTmp_Yolo) +
  geom_sf(mapping = aes(colour = enfranchisement_final, geometry = geometry), data = Yolo, 
          inherit.aes = FALSE) +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) +
  labs(color = "Enfranchisement type") +
  scale_color_brewer(palette = "Dark2") +
  theme(panel.background=element_blank()); Demo5

ggsave('Figures/Demo_map4.png', width = 6, height = 3, dpi = 300, bg='#ffffff')

#I think Mendocino is best, Combine into panel figure
Combined <- grid.arrange(Enfranchisement_Type, Demo4, nrow=1)
