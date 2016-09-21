library(readr)
library(readxl)
library(maptools)
library(RColorBrewer)
library(MASS)
library(dplyr)
library(rgdal)
library(plyr)
library(maps)
library(mapproj)
library(data.table)
library(reshape)


crime2016 <- read_csv('DataSets/Crime_Incidents__2016.csv')
crime2015 <- read_csv('DataSets/Crime_Incidents__2015.csv')
crime2014 <- read_csv('DataSets/Crime_Incidents__2014.csv')
crime2013 <- read_csv('DataSets/Crime_Incidents__2013.csv')
crime2012 <- read_csv('DataSets/Crime_Incidents__2012.csv')
crime2011 <- read_csv('DataSets/Crime_Incidents__2011.csv')
streetCams <- read_csv('DataSets/CCTV__Street_Cameras.csv')
policeStations <- read_csv('DataSets/Police_Stations.csv')
wardDemo <- read_csv('DataSets/wardDemographics.csv')
wardWellBeing <- read_csv('DataSets/wardWellBeing.csv')
metroBus <- read_csv('DataSets/Metro_Bus_Stops.csv')
metroStations <- read_csv('DataSets/Metro_Stations_Regional.csv')

names(wardDemo)[names(wardDemo)=='WARD2012'] <- 'WARD'
names(wardWellBeing)[names(wardWellBeing) == 'WARD2012'] <- 'WARD'
full.demo <- merge(wardWellBeing,wardDemo,by='WARD')

# create training data frame combining demographic data and crime data
crime.train <- rbind(crime2011,crime2012) %>% 
  rbind(crime2013) %>% 
  rbind(crime2014) %>% 
  rbind(crime2015)

#Assuming that burglaries are more likely to happen in Wards where the average HH income is over 115000?
#What wards are these? 
# Establish which wards are wealthier
subset(wardWellBeing,AvgFamilyIncAdj_2010_14 > 115000 )
#Ward 2, 3 4, 6 

# Ward 2: Chinatown, Downtown, Dupont Circle
# Ward 3: Glover Park, Fox Hall, Georgetown
# Ward 4: Brightwood, Fort Totten, Petworth
# Ward 6: Capital Hill, Navy Yard, Southwest Waterfront

#Demographic data 
wardWell_subset<- data.frame(wardWellBeing$WARD, wardWellBeing$PctPoorPersons_2010_14,
                             wardWellBeing$PctPoorChildren_2010_14, wardWellBeing$PctPoorElderly_2010_14,
                             wardWellBeing$PctUnemployed_2010_14, wardWellBeing$Pct16andOverEmployed_2010_14,wardWellBeing$AvgFamilyIncAdj_2010_14)
wardWell_subset<-rename(wardWell_subset, c(wardWellBeing.WARD = "WARD"))
wardDemo_subset<-data.frame(wardDemo$WARD,wardDemo$PctPopUnder18Years_2010, wardDemo$PctPop65andOverYears_2010, 
                            wardDemo$PctPop65andOverYears_2010, wardDemo$PctBlackNonHispBridge_2010,
                            wardDemo$PctWhiteNonHispBridge_2010,wardDemo$PctHisp_2010, wardDemo$PctAsianPINonHispBridge_2010,
                            wardDemo$Pct_births_teen_2010)

#Join Data
wardDemo_comb <- join(wardWell_subset,wardDemo_subset, by = 'WARD')
#Join demographic data with  crime data
ward_crime <- join(crime.train , wardDemo_comb, by = 'WARD' , type = "left")

# Shapefile creation and projection
dsn <-"Ward__2012"
shapefile <- "Ward__2012"
dc<- readOGR(dsn,layer = shapefile)
summary(dc)
plot(dc, add = T)

crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
wgs<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
sp <- spTransform(dc, crswgs84)

# get color palette for heat map
palette.function = colorRampPalette(rev(brewer.pal(11,'Spectral')))
heat.colors = palette.function(32)

# Group by ward and summarize to get count of each type of crime
ward.group <- crime.train %>%
  group_by(WARD , OFFENSE) %>%
  summarise(count = n())

ward_demoe.group <- crime.train %>%
  group_by(WARD) %>%
  summarise(count = n())

# Linear regression doesn't work in this type of data
lm.model <- lm(Totcrime~PctPoorPersons_2010_14+PctWhiteNonHispBridge_2010+
  PctUnemployed_2010_14,data=)#PctBlackNonHispBridge_2010+PctWhiteNonHispBridge_2010,data=)











# Crime Locations 2016
names(crime2016)[1]<-paste("X")
crime2016Loc <- as.data.frame(subset(crime2016, select = c('X','Y')))

# Plot Crime KDE
est = kde2d(crime2016Loc[,'X'], crime2016Loc[,'Y'], h=0.1, n=c(100,100))  # h=0.1
image(est, col = heat.colors, useRaster=TRUE, asp=1)

# Plot Street Cam locations
colnames(streetCams)[colnames(streetCams) == 'LONGITUDE'] <- 'Long'
colnames(streetCams)[colnames(streetCams) == 'LATITUDE'] <- 'Lat'
streetCams[, c('Long', 'Lat')] <- sapply(streetCams[, c('Long', 'Lat')], as.factor)

streetCamsLocation <- as.data.frame(subset(streetCams, select = c(('Long'), ('Lat')))) %>%
  filter('Lat' > 0) # NEED TO REMOVE ZEROS?????
plot(streetCamsLocation)

filtered = streetCams[streetCams$LATITUDE != 0,]