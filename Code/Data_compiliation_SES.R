#Data compilation for demographics

#LOAD LIBRARIES
library(tidyverse)
library(ggmap)
library(censusapi)
library(tidycensus)
library(sf)
library(sp)

Demographics_blockg <- get_acs(geography = "block group", variables = c("B03002_001E", "B03002_003E", "B03002_004E", "B03002_005E", "B03002_006E", "B03002_007E", "B03002_012E", "B19013_001E", "B25003_001E", "B25003_002E", "B25003_003E", "B11001_001E"), state = "CA", year =2022, output = "wide", survey = "acs5", geometry = TRUE)

#rename variables
Demographics_blockg <- Demographics_blockg %>% dplyr::rename(Race.estimate.total = B03002_001E, Race.white.alone = B03002_003E, Race.black.alone = B03002_004E, Race.native.alone = B03002_005E, Race.asian.alone = B03002_006E, Race.PI.alone = B03002_007E, Race.hispanicorlatino = B03002_012E, Median.hh.income = B19013_001E, Tenure.estimate.total = B25003_001E, Tenure.owner = B25003_002E, Tenure.renter = B25003_003E, Households.total = B11001_001E)

#Make median household income into a count by multiplying median income by total households (see https://crd230.github.io/lab3.html)
Demographics_blockg$Income.aggregatecount <- Demographics_blockg$Median.hh.income*Demographics_blockg$Households.total

#load in boundary polygons for CWS
PWS_boundary <- st_read("Data_raw/California_Drinking_Water_System_Area_Boundaries/") #New version of SABL downloaded November 15, 2024
str(PWS_boundary)
#plot(st_geometry(PWS_boundary)) #takes a while but works!

PWS_boundary$SABL_PWSID <- as.factor(PWS_boundary$SABL_PWSID)
PWS_boundary$WATER_SYST <- as.factor(PWS_boundary$WATER_SYST)
PWS_boundary <- PWS_boundary %>% filter(BOUNDARY_T != "Jurisdictional") #remove jurisdictional boundaries to get rid of duplicates

Data_small <- read.csv(here::here("Data_processed/Systemperformancedata.csv"))
Data_small <- Data_small[,c(2,11)]
  
Data_geo <- left_join(PWS_boundary, Data_small, by = c("SABL_PWSID" = "PWSID"))
summary(as.factor(Data_geo$enfranchisement_final)) #Most are in the boundary dataset (2344 of 2405)
Data_geo <- Data_geo[,c(2,4,5,15,19,33:36)]
Data_geo <- Data_geo %>% distinct(SABL_PWSID, .keep_all = TRUE) #Get rid of one duplicate boundary in the dataset

#areal interpolation (https://crd230.github.io/lab3.html)
#make data set of just boundaries to play with
Boundaries <- Data_geo %>% dplyr::select(SABL_PWSID, geometry)
Boundaries <- Boundaries %>% unique() #all unique

library(areal)
Boundaries <- st_as_sf(Boundaries) #make sf object

#make to be the same crs
Boundaries <- st_transform(Boundaries, crs = 3857)
st_crs(Boundaries) 
Boundaries <- st_make_valid(Boundaries)
Demographics_blockg <- st_transform(Demographics_blockg, crs = 3857)
st_crs(Demographics_blockg) 

#now try areal interpolation
Interpolation <- aw_interpolate(Boundaries, tid = SABL_PWSID, source = Demographics_blockg, sid = GEOID, weight = "total", output = "tibble", extensive = c("Race.estimate.total", "Race.white.alone", "Race.black.alone", "Race.native.alone", "Race.asian.alone", "Race.PI.alone", "Race.hispanicorlatino", "Tenure.estimate.total", "Tenure.owner", "Tenure.renter", "Households.total", "Income.aggregatecount"))

#now add into cases, first remove geometry columns
Data_dem <- left_join(Data_small, Interpolation, by = c("PWSID" = "SABL_PWSID"))

#make into percents/bock group MHI
Data_dem$MHI_KD <- ((Data_dem$Income.aggregatecount)/(Data_dem$Households.total))
summary(Data_dem$MHI)

Data_dem$Percent.hispanicorlatino <- ((Data_dem$Race.hispanicorlatino)/(Data_dem$Race.estimate.total))*100
summary(Data_dem$Percent.hispanicorlatino)

Data_dem$Percent.white <- ((Data_dem$Race.white.alone)/(Data_dem$Race.estimate.total))*100
summary(Data_dem$Percent.white)

Data_dem$Percent.black <- ((Data_dem$Race.black.alone)/(Data_dem$Race.estimate.total))*100
summary(Data_dem$Percent.black)

Data_dem$Percent.asian <- ((Data_dem$Race.asian.alone)/(Data_dem$Race.estimate.total))*100
summary(Data_dem$Percent.asian)

Data_dem$Percent.renter<- ((Data_dem$Tenure.renter)/(Data_dem$Tenure.estimate.total))*100
summary(Data_dem$Percent.renter)

#Add in county
SDWIS <- read_csv("Data_raw/CA_SDWIS_allcws_Nov152024.csv")
SDWIS <- SDWIS[,c(1,6)]
SDWIS <- SDWIS %>% dplyr::rename("COUNTY" = "Principal County Served")
SDWIS <- SDWIS %>% dplyr::rename("SABL_PWSID" = "Water System No.")
Data_geo <- left_join(Data_geo, SDWIS)

#save data_geo to make map later
write.csv(Data_geo, file = here::here("Data_processed/Data_geo.csv"))

#reduce to only needed variables
Demographics <- Data_dem[,c(1,2,15:20)]

#write csv
write.csv(Demographics, file = here::here("Data_processed/Systemdemographicdata.csv"))
