# THOR PE from DSCOE Inferno during Introduction to R Training at the Center for Army Analysis
# 17 Jan 2017
# LTC Melanie Vinton


# Get and Clean the Data
thornam<-read.csv("C:/path/to/file/thor_vietnam_1965.csv")
nam65 <- subset(thornam, TGTLATDD_DDD_WGS84>7, na.rm=TRUE)
nam65 <- subset(nam65, TGTLATDD_DDD_WGS84<24, na.rm=TRUE)
nam65 <- subset(nam65, TGTLONDDD_DDD_WGS84>99, na.rm=TRUE)
nam65 <- subset(nam65, TGTLONDDD_DDD_WGS84<110, na.rm=TRUE)

nam65$AIRFORCEGROUP<-nam65$AIRFORCESQDN<-nam65$CALLSIGN<- NULL
nam65$RELEASEALTITUDE <-nam65$RELEASEFLTSPEED<-nam65$RESULTSBDA <-nam65$TGTID<- NULL
nam65$WEAPONTYPECLASS<-nam65$WEAPONSLOADEDWEIGHT<-nam65$SOURCEID<-nam65$SOURCERECORD<-NULL
nam65$ID<-nam65$ADDITIONALINFO<-nam65$TGTORIGCOORDS<-nam65$TGTORIGCOORDSFORMAT<- NULL
nam65$NUMWEAPONSDELIVERED<-nam65$WEAPONTYPEWEIGHT<-nam65$TGTWEATHER<-NULL
nam65$NUMWEAPONSJETTISONED<-nam65$NUMWEAPONSRETURNED<-NULL
nam65$AIRCRAFT_ORIGINAL<-nam65$AIRCRAFT_ROOT<- nam65$UNIT<- NULL
nam65$GEOZONE<-nam65$FLTHOURS<-nam65$MFUNC<-nam65$TIMEOFFTARGET<-NULL

nam65 <- droplevels(nam65)


# Daily Missions over 90 Days
library(plyr)
missions <- count(nam65, vars='MSNDATE')
head(missions,2)
tail(missions,2)

library(lubridate)
nam65$MSNDATE <- ymd(nam65$MSNDATE)
missions <- count(nam65,vars='MSNDATE')
head(missions, 2)
tail(missions,2)

library(ggplot2)
library(scales)
datebreaks <- seq(as.Date("1965-10-01"), as.Date("1965-12-31"), by="week")

ggplot(missions,aes(x = MSNDATE, y = freq))+
  geom_area(color='blue', fill='lightblue',alpha=.3)+
  geom_point(color='blue')+
  scale_x_date(breaks=datebreaks,labels=date_format("%b %d, %Y")) +
  theme(axis.text.x=element_text(angle=30)) +
  annotate("text", x=as.Date('1965-12-25'), y=20, label="Christmas Day") +
  ggtitle("Number of Missions Per Day") +
  xlab("Date") +
  ylab('# of Missions')


# Distribution of Strike Mission Times
summary(nam65$TIMEONTARGET)

nam65$TIMEONTARGET[nam65$TIMEONTARGET > 2400] <- median(nam65$TIMEONTARGET, na.rm=TRUE)
summary(nam65$TIMEONTARGET)

levels(nam65$MFUNC_DESC)

strike <- subset(nam65, MFUNC_DESC == "STRIKE")
hist(strike$TIMEONTARGET, breaks = 24)

strike$TIMEONTARGET[strike$TIMEONTARGET == 0] <- 0010

strike$hour <- cut(strike$TIMEONTARGET, c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,
                                          1600,1700,1800,1900,2000,2100,2200,2300,2400), 
                   labels=c('0100','0200','0300','0400','0500','0600','0700','0800','0900',1000,1100,1200,1300,1400,1500,
                            1600,1700,1800,1900,2000,2100,2200,2300,2400))


ggplot(strike,aes(x=hour)) +
  geom_bar() +
  xlab("Time on Target") + ylab("Number of Missions") + 
  ggtitle("Breakdown of the Time of Day of Strike Missions\n(By Hour)")+
  scale_y_continuous(breaks=seq(0,1500,250))+
  theme(panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=30)) 


# Proportion of Kinetic Missions by Service
kin <- round(prop.table(xtabs(~ MILSERVICE + MFUNC_DESC_CLASS , data=nam65),2)*100,2)
kin


# Map Close Air Support Mission in December
library(stringr)
nam65$month <- str_sub(nam65$MSNDATE,start=6, end=7)

nam65dec <- subset(nam65, month==12 & MFUNC_DESC=="CLOSE AIR SUPPORT")

library(leaflet)
cas12<-leaflet() %>%
  setView(lng = 107, lat = 17, zoom = 5) %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  addCircleMarkers(data=nam65dec, lng=~TGTLONDDD_DDD_WGS84, lat=~TGTLATDD_DDD_WGS84, 
                   radius = 2,
                   popup=~nam65dec$MILSERVICE)
cas12












