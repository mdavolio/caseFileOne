library(readr)
library(plyr)
library(dplyr)
library(readxl)
library(maptools)
library(rgdal)
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

#Comebine all crime data using rbind function.
crime<-rbind(crime2015, crime2014)
crime<-rbind(crime,crime2013)
crime<-rbind(crime,crime2012)
crime<-rbind(crime,crime2011)


#dcBorder <- readShapeSpatial('DataSets/district_of_columbia_administrative.shp')

#dsn <-"/Users/Kerry/Documents/UVA-DSI/Fall/SYS6018-Data Mining/Ward__2012/"
#shapefile <- "Ward__2012"
#dc<- readOGR(dsn,layer = shapefile)
#summary(dc)
#plot(dc)

#Hypotheis: Are the wealthier areas in DC more suceptible to less violent crimes such as burglaries?
#Burglary is more likely to occur in areas where the average household income is above $
#How many burglaries occur in those districts?

# How frequent a particular occurs? 
count(crime, 'OFFENSE')

###THIS DOESNT WORK
# group crime by ward- to see how frequent specific crime occurs in ward
by_ward<- group_by(crime2016, OFFENSE, WARD) %>% summarise(count = n())
#summarise(by_ward,  n = aggregate(OFFENSE, count)
library(dplyr)
df1<- crime2016 %>%
  group_by(WARD)


#Assuming that burglaries are more likely to happen in Wards where the average HH income is over 115000?
#What wards are these? 

subset(wardWellBeing,AvgFamilyIncAdj_2010_14 > 115000)
#Ward 2, 3 4, 6 

# Ward 2: Chinatown, Downtown, Dupont Circle
# Ward 3: Glover Park, Fox Hall, Georgetown
# Ward 4: Brightwood, Fort Totten, Petworth
# Ward 6: Capital Hill, Navy Yard, Southwest Waterfront

#Demographic data 
wardWell_subset<- data.frame(wardWellBeing$WARD2012, wardWellBeing$PctPoorPersons_2010_14,wardWellBeing$PctPoorChildren_2010_14, wardWellBeing$PctPoorElderly_2010_14, wardWellBeing$PctUnemployed_2010_14, wardWellBeing$Pct16andOverEmployed_2010_14,wardWellBeing$AvgFamilyIncAdj_2010_14)
wardWell_subset<-rename(wardWell_subset, c(wardWellBeing.WARD2012 = "WARD"))

wardDemo_subset<-data.frame(wardDemo$WARD2012,wardDemo$PctPopUnder18Years_2010, wardDemo$PctPop65andOverYears_2010, wardDemo$PctPop65andOverYears_2010, wardDemo$PctBlackNonHispBridge_2010,wardDemo$PctWhiteNonHispBridge_2010,wardDemo$PctHisp_2010, wardDemo$PctAsianPINonHispBridge_2010, wardDemo$Pct_births_teen_2010)
wardDemo_subset<-rename(wardDemo_subset, c(wardDemo.WARD2012 = "WARD"))
#Join Data
ward2010<- join(wardWell_subset,wardDemo_subset, by = 'WARD')
#Join demographic data with  crime data
ward_crime <- join(crime,ward2010, by = 'WARD' , type = "left")


#Crime Data - Turn Burglary into binary variable 
colnames(ward_crime)
keep <- c('Y', 'OFFENSE', 'METHOD', 'WARD', 'wardWellBeing.PctPoorPersons_2010_14', 'wardWellBeing.PctUnemployed_2010_14','wardWellBeing.AvgFamilyIncAdj_2010_14') # NEED TO PUT X VARIABLE BACK IN
new_ward_crime <- ward_crime[keep]
ward_crime$Burglary<- ifelse(ward_crime$OFFENSE == 'BURGLARY' & (ward_crime$WARD == 2 |ward_crime$WARD == 3 | ward_crime$WARD == 4 | ward_crime$WARD == 6), 1, 0) 
new_ward_crime$Burglary<- ifelse(new_ward_crime$OFFENSE == 'BURGLARY' & (new_ward_crime$WARD == 2 |new_ward_crime$WARD == 3 | new_ward_crime$WARD == 4 | new_ward_crime$WARD == 6), 1, 0) 


#crimeLog_Reg<- glm(ward_crime$Burglary ~ ward_crime$wardWellBeing.PctPoorPersons_2010_14 + ward_crime$wardWellBeing.PctPoorChildren_2010_14 + ward_crime$wardWellBeing.PctPoorElderly_2010_14 + ward_crime$wardWellBeing.AvgFamilyIncAdj_2010_14 + ward_crime$wardWellBeing.PctUnemployed_2010_14 + ward_crime$wardDemo.PctWhiteNonHispBridge_2010 + ward_crime$wardDemo.PctPop65andOverYears_2010.1, data = ward_crime, family = "binomial")
crimeLog_Reg<- glm(Burglary ~ wardWellBeing.PctPoorPersons_2010_14 + wardWellBeing.PctUnemployed_2010_14 + wardWellBeing.AvgFamilyIncAdj_2010_14, data = new_ward_crime, family = "binomial")
#crimeLog_Reg<- glm(ward_crime$Burglary ~ ward_crime$wardWellBeing.PctPoorPersons_2010_14 + ward_crime$wardWellBeing.AvgFamilyIncAdj_2010_14 + ward_crime$wardDemo.Pct_births_teen_2010, data = ward_crime, family = "binomial")

summary(crimeLog_Reg)

crime2016 <- read_csv('DataSets/Crime_Incidents__2016.csv')
#Add demographic data

ward_crime_2016 <- data.frame(join(crime2016,ward2010, by = 'WARD' , type = "left"))

colnames(ward_crime_2016) 
#Make sure column names are the same
ward_crime_2016<-rename(ward_crime_2016, c(X.X = "X"))

#THIS ISNT WORKING...Gives error - both test and training dataframes are differnt sizes.

ward_crime_2016$Burglary <- predict(crimeLog_Reg, newdata = ward_crime_2016, type="response")

# pred<-as.data.frame(predict(crimeLog_Reg, newdata = ward_crime_2016, type="response"))

