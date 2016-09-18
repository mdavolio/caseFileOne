library(readr)
library(readxl)
library(maptools)
library(RColorBrewer)
library(MASS)
library(dplyr)

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

# get color palette for heat map
palette.function = colorRampPalette(rev(brewer.pal(11,'Spectral')))
heat.colors = palette.function(32)

# Crime Locations
names(crime2016)[1]<-paste("X")
crime2016Loc <- as.data.frame(subset(crime2016, select = c('X','Y')))

# Plot Crime KDE
est = kde2d(crime2016Loc[,'X'], crime2016Loc[,'Y'], h=0.1, n=c(100,100))  # h=0.1
image(est, col = heat.colors, useRaster=TRUE, asp=1)
