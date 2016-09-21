library(readr)
library(dplyr)
library(readxl)

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

# create full data frame combining demographic data and crime data
crime.full <- rbind(crime2011,crime2012) %>% 
  rbind(crime2013) %>% 
  rbind(crime2014) %>% 
  rbind(crime2015) %>% 
  rbind(crime2016) 
# merge crime data and demographic data with respect to ward 
demo.crime.df <- merge(crime.full,full.demo,by = 'WARD')

# group by WARD and get some summary statistics 
by_ward <- group_by(demo.crime.df,WARD)
by_ward_offense <- group_by(demo.crime.df,WARD,OFFENSE)
new <- summarize(by_ward,Totcrime = n()) # total of incidence in five years in each ward
new2 <- summarize(by_ward_offense,Tottype = n()) # number of each type of crime occurred in each ward



# Linear regression doesn't work in this type of data
# lm.model <- lm(Totcrime~PctPoorPersons_2010_14+PctWhiteNonHispBridge_2010+
# PctUnemployed_2010_14,data=b)#PctBlackNonHispBridge_2010+PctWhiteNonHispBridge_2010,data=)

#TotPop_2010+PctForeignBorn_2010_14
#+PctFamiliesOwnChildrenFH_2010_14+PctPoorPersons_2010_14
#+PctUnemployed_2010_14+PctBlackNonHispBridge_2010+AvgFamilyIncAdj_2010_14

#summary(lm.model)



# try to fit a lda model using population data 
library(MASS)
train.indices = sample(1:nrow(demo.crime.df), as.integer(nrow(demo.crime.df) * 0.75)) 
lda.fit = lda(OFFENSE~PctPoorPersons_2010_14+PctWhiteNonHispBridge_2010
              +PctUnemployed_2010_14+factor(SHIFT),data = demo.crime.df)
predictions = predict(lda.fit, newdata = demo.crime.df[-train.indices, ])
num.correct = sum(predictions$class == demo.crime.df[-train.indices,]$OFFENSE)
accuracy = num.correct / nrow(demo.crime.df[-train.indices, ])
accuracy
# an accuracy of 39.3%