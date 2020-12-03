#####################################################################
#File name: PM25-Explore.R
#Author: Jonas LaPier
#Date: November 2020
#Purpose: Exploratory analysis of PM2.5 air quality data 
#####################################################################

# Steve says that daily data is sufficient and there isn't a need to 
# look at higher time resolutions.

# Attach Dependencies
library(extrafont)
library(ggplot2)
library(lmodel2)
library(MASS)
library(rgdal)
library(jsonlite)
setwd("~/Documents/School 20-21/Environmental Modeling/Wildfire-Smoke")

#### Daily Mean PM2.5 2020 Western States ####
# Load and Combine Data sets
samples = read.csv("Data.nosync/CA_2020_ad_viz_plotval_data.csv")
samples = rbind(samples, read.csv("Data.nosync/ID_2020_ad_viz_plotval_data.csv"))
samples = rbind(samples, read.csv("Data.nosync/OR_2020_ad_viz_plotval_data.csv"))
samples = rbind(samples, read.csv("Data.nosync/NV_2020_ad_viz_plotval_data.csv"))
samples = rbind(samples, read.csv("Data.nosync/WA_2020_ad_viz_plotval_data.csv"))

# Load Map of US States
mapUSm<- readOGR(dsn="US48", layer="US_48states")  

# Fortify polygons
states.df = fortify(mapUSm[c(1,8,11,22,24),]) # State Outlines

# Single out unique locations
locations = samples[!duplicated(samples$Site.ID),]

# Plot the sampling locations
ggplot() + 
  geom_point(data=locations, aes(x=SITE_LONGITUDE, y=SITE_LATITUDE),
             color="red", alpha=1, pch=16,size=2) +
  geom_point(data=samples, aes(x=SITE_LONGITUDE, y=SITE_LATITUDE),
             color="black", alpha=1, pch=21,size=2) +
  #scale_color_gradientn(limits = c(0,10), colors=terrain.colors(10), na.value="salmon") +
  geom_polygon(data=states.df, aes(long,lat,group=group), fill=NA, size=.5) +
  geom_path(data=states.df, aes(long,lat,group=group), color="black") +
  labs(title="PM 2.5 Sampling Locations in the Western States", x="Longitude",
       y="Latitude", color='ug/l') +
  coord_fixed() + theme_minimal()

# Plot Trends in Time
samples$Date = as.POSIXct(samples$Date, format="%m/%d/%Y")
ggplot() +
  geom_point(data=samples, aes(x=Date, y=Daily.Mean.PM2.5.Concentration, color=STATE),
             alpha=0.5) +
  labs(title="Daily 2020 PM 2.5 Counts (All Sites)", x="Date",
       y="Daily Mean PM 2.5 Concentration (ug/m^3)") +
  scale_x_continuous(breaks= seq(min(samples$Date),max(samples$Date),by=2.628e+6), 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                "Sep","Oct","Nov")) +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Look at one Site Near Home
spokane.samples = samples[samples$CBSA_NAME=="Spokane-Spokane Valley, WA",]

# Plot the 4 Spokane Monitoring Sites together
ggplot() +
  geom_line(data=spokane.samples[spokane.samples$Site.ID==530630021,],
            aes(x=Date, y=Daily.Mean.PM2.5.Concentration),color="blue",size=1) +
  geom_line(data=spokane.samples[spokane.samples$Site.ID==530630047,],
            aes(x=Date, y=Daily.Mean.PM2.5.Concentration),color="red",size=1) +
  geom_line(data=spokane.samples[spokane.samples$Site.ID==530650002,],
            aes(x=Date, y=Daily.Mean.PM2.5.Concentration),color="purple",size=1) +
  geom_line(data=spokane.samples[spokane.samples$Site.ID==530650005,],
            aes(x=Date, y=Daily.Mean.PM2.5.Concentration),color="orange",size=1) +
  labs(title="Daily 2020 PM 2.5 Counts (Spokane Sites)", x="Date",
       y="Daily Mean PM 2.5 Concentration (ug/m^3)") +
  scale_x_continuous(breaks= seq(min(samples$Date),max(samples$Date),by=2.628e+6), 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                "Sep","Oct","Nov")) +
  theme(text=element_text(size=16,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))


#### Daily PM2.5 1999-2020 California ####

# Load and Combine Data sets
setwd("~/Documents/School 20-21/Environmental Modeling/Wildfire-Smoke")
samples = read.csv("Data.nosync/CA_1999_ad_viz_plotval_data.csv")
for (yr in 2000:2020) {
  samples = rbind(samples, read.csv(paste0("Data.nosync/CA_",yr,"_ad_viz_plotval_data.csv")))
}

# Load Map of US States
mapUSm<- readOGR(dsn="US48", layer="US_48states")  

# Fortify polygons
CA.df = fortify(mapUSm[24,]) # State Outlines


# Single out unique locations
locations = samples[!duplicated(samples$Site.ID),]

# Plot the sampling locations
ggplot() + 
  geom_point(data=locations, aes(x=SITE_LONGITUDE, y=SITE_LATITUDE),
             color="red", alpha=1, pch=16,size=2) +
  geom_point(data=samples, aes(x=SITE_LONGITUDE, y=SITE_LATITUDE),
             color="black", alpha=1, pch=21,size=2) +
  #scale_color_gradientn(limits = c(0,10), colors=terrain.colors(10), na.value="salmon") +
  geom_polygon(data=CA.df, aes(long,lat,group=group), fill=NA, size=.5) +
  geom_path(data=CA.df, aes(long,lat,group=group), color="black") +
  labs(title="PM 2.5 Sampling Locations in California", x="Longitude",
       y="Latitude", color='ug/l') +
  coord_fixed() + theme_minimal()

# Aggregate into Statewide Daily Values
CA.mean = tapply(samples$Daily.Mean.PM2.5.Concentration, samples$Date, mean)
CA.median = tapply(samples$Daily.Mean.PM2.5.Concentration, samples$Date, median)
CA.95 = tapply(samples$Daily.Mean.PM2.5.Concentration, samples$Date, quantile, p=0.95)
# Daily Statewide Block Estimates
CA.daily = data.frame("date"=as.POSIXct(names(CA.mean)), "mean"=CA.mean,
                      "median"=CA.median,"percentile95"=CA.95)

# Plot Trends in Time
colors = c("Observations"="black","Daily Mean"="red","Daily 95th Percentile"="red")
samples$Date = as.POSIXct(samples$Date, format="%m/%d/%Y")
ggplot() +
  geom_point(data=samples, aes(x=Date, y=Daily.Mean.PM2.5.Concentration,color="Observations"),
                               alpha=0.5,size=0.1) +
  geom_line(data=CA.daily, aes(x=date, y=mean,color="Daily Mean"),
             alpha=1,size=0.1) +
  labs(title="Daily 2020 PM 2.5 Counts (All Sites)", x="Year",
       y="Daily Mean PM 2.5 Concentration (ug/m^3)") +
  scale_x_continuous(breaks= seq(min(samples$Date),max(samples$Date),by=3.154e+7), 
                     labels = c(1999:2020)) +
  scale_color_manual(values=colors, name="Legend") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

#### Analysis of California Data - STL ####

# Aggregate into Statewide Daily Values
CA.mean = tapply(samples$Daily.Mean.PM2.5.Concentration, samples$Date, mean)
CA.median = tapply(samples$Daily.Mean.PM2.5.Concentration, samples$Date, median)
CA.95 = tapply(samples$Daily.Mean.PM2.5.Concentration, samples$Date, quantile, p=0.95)

# Convert to Time Series
CA.mean.ts = ts(CA.mean, frequency = 365.25)
CA.median.ts = ts(CA.median, frequency = 365.25)
CA.95.ts = ts(CA.95, frequency = 365.25)

# Seasonal and Trend Decomposition
CA.mean.stl = stl(CA.mean.ts, s.window=365.25)
CA.median.stl = stl(CA.median.ts, s.window=365.25)
CA.95.stl = stl(CA.95.ts, s.window=365.25)

# All Trend Lines Together
trends = data.frame("Date"=as.Date(names(CA.mean)),
                    "Mean"=as.numeric(CA.mean.stl$time.series[,2]),
                    "Median"=as.numeric(CA.median.stl$time.series[,2]),
                    "95th Percentile"=as.numeric(CA.95.stl$time.series[,2]))

# All Seasonal Components Together
seasons = data.frame("Date"=as.Date(names(CA.mean)),
                    "Mean"=as.numeric(CA.mean.stl$time.series[,1]),
                    "Median"=as.numeric(CA.median.stl$time.series[,1]),
                    "95th Percentile"=as.numeric(CA.95.stl$time.series[,1]))

# All Remainders Together
remainders = data.frame("Date"=as.Date(names(CA.mean)),
                     "Mean"=as.numeric(CA.mean.stl$time.series[,3]),
                     "Median"=as.numeric(CA.median.stl$time.series[,3]),
                     "95th Percentile"=as.numeric(CA.95.stl$time.series[,3]))

colors = c("Mean"="red","Median"="orange","95th Percentile"="Purple")

# Plot the Decompositions
# Trends
ggplot(data=trends) +
  geom_line(aes(x=Date,y=Mean, color="Mean"), size=1.3) +
  geom_line(aes(x=Date,y=Median, color="Median"), size=1.3) +
  geom_line(aes(x=Date,y=X95th.Percentile, color="95th Percentile"), size=1.3) +
  scale_color_manual(values=colors) +
  xlab("Year") + ylab("Daily Mean PM 2.5 Concentration (ug/m^3)") +
  ggtitle("Trend Decomposition for CA Aggregate PM 2.5") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Seasons
ggplot(data=seasons) +
  geom_line(aes(x=Date,y=X95th.Percentile, color="95th Percentile"), size=.4) +
  geom_line(aes(x=Date,y=Mean, color="Mean"), size=.4) +
  geom_line(aes(x=Date,y=Median, color="Median"), size=.4) +
  scale_color_manual(values=colors) +
  xlab("Year") + ylab("Daily Mean PM 2.5 Concentration (ug/m^3)") +
  ggtitle("Seasonal Decomposition for CA Aggregate PM 2.5") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Plot the Trend for one Season
ggplot(data=seasons[seasons$Date>as.Date("2018-12-31")&seasons$Date<as.Date("2020-01-01"),]) +
  geom_line(aes(x=Date,y=X95th.Percentile, color="95th Percentile"), size=.4) +
  geom_line(aes(x=Date,y=Mean, color="Mean"), size=.4) +
  geom_line(aes(x=Date,y=Median, color="Median"), size=.4) +
  scale_color_manual(values=colors) +
  xlab("Month") + ylab("Daily Mean PM 2.5 Concentration (ug/m^3)") +
  ggtitle("Seasonal Decomposition for CA Aggregate PM 2.5 - One Season") +
  scale_x_continuous(breaks= seq(as.Date("2019-01-01"),as.Date("2019-12-31"),by="month"), 
                     labels =  c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                 "Sep","Oct","Nov","Dec")) +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Remainders
p1 = ggplot(data=remainders) +
  geom_point(aes(x=Date,y=X95th.Percentile), color="purple", size=.4, alpha=0.7) +
  ggtitle("95th Percentile                                                          Remainders") +
  ylab("") + ylim(c(-50,150)) +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))
p2 = ggplot(data=remainders) +
  geom_point(aes(x=Date,y=Mean), color="red", size=.4, alpha=0.7) +
  ylab("Daily Mean PM 2.5 Concentration (ug/m^3)") +
  ggtitle("Mean") + ylim(c(-50,150)) +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))
p3 = ggplot(data=remainders) + ylim(c(-50,150)) +
  geom_point(aes(x=Date,y=Median), color="orange", size=.4, alpha=0.7) +
  ggtitle("Median") +
  xlab("Year") + ylab("") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))
multiplot(p1,p2,p3)


#### Analysis of California Data - AR ####

# Auto Correlation Plots
acf(CA.mean, lag.max = 1460)
acf(CA.median, lag.max = 1460)
acf(CA.95, lag.max = 1460)

#### Raw Data PM2.5 ####

# API Call Notes
# sample call
# https://aqs.epa.gov/data/api/sampleData/byState?email=test@aqs.api&key=test&param=45201&bdate=19950515&edate=19950515&state=37

# State Codes
# CA: 6
# ID: 16
# NV: 32
# OR: 41
# WA: 53

# Parameter Codes
#"code": "88101",
#"value_represented": "PM2.5 - Local Conditions"

# Call for WA state, 2020 data
# https://aqs.epa.gov/data/api/sampleData/byState?email=camerajonas@gmail.com&
# key=silverswift17&param=88101&bdate=20200101&edate=20201114&state=53

# Read in JSON File
data = fromJSON("Data.nosync/WA-2020-raw.json", flatten=TRUE)
raw.data = data[[2]]

raw.data$date = as.POSIXct(raw.data$date_gmt)
plot(raw.data$date,raw.data$sample_measurement)


#### Spokane Data ####

# Load and Combine Data sets
WA.samples = read.csv("Data.nosync/WA_2007_ad_viz_plotval_data.csv")
for (yr in 2007:2020) {
  WA.samples = rbind(WA.samples, read.csv(paste0("Data.nosync/WA_",yr,"_ad_viz_plotval_data.csv")))
}
WA.samples$Date = as.POSIXct(WA.samples$Date, format="%m/%d/%Y")

# Look at one Site Near Home
spokane.samples = WA.samples[WA.samples$CBSA_NAME=="Spokane-Spokane Valley, WA",]
spokane.samples$Site.ID = as.factor(spokane.samples$Site.ID)

# Plot the 4 Spokane Monitoring Sites together
ggplot() +
  geom_point(data=spokane.samples,
            aes(x=Date, y=Daily.Mean.PM2.5.Concentration, color=Site.ID),size=0.8) +
  labs(title="Daily PM 2.5 Counts - Spokane Sites", x="Date",
       y="Daily Mean PM 2.5 Concentration (ug/m^3)") +
  scale_x_continuous(breaks= seq(min(WA.samples$Date),max(WA.samples$Date),by="year"), 
                     labels = c(2007:2020)) +
  theme(text=element_text(size=16,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Plot the 2020 Data
ggplot() +
  geom_point(data=spokane.samples,
             aes(x=Date, y=Daily.Mean.PM2.5.Concentration, color=Site.ID),size=0.8) +
  labs(title="2020 Daily PM 2.5 Counts - Spokane Sites", x="Date",
       y="Daily Mean PM 2.5 Concentration (ug/m^3)") + 
  scale_x_continuous(breaks= seq(as.POSIXct("2020-01-01"),max(WA.samples$Date),by="month"), 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                "Sep","Oct","Nov"),
                     limits=c(as.POSIXct("2020-01-01"),max(WA.samples$Date))) +
  theme(text=element_text(size=16,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Combine the Daily Data
spokane.daily = tapply(spokane.samples$Daily.Mean.PM2.5.Concentration,
                       spokane.samples$Date, mean)
spokane.daily = data.frame("Date"=as.POSIXct(names(spokane.daily)),
                           "Daily.Mean.PM2.5.Concentration"=spokane.daily)

# Check for missing values
sum(is.na(spokane.daily))

# Apply Savitsky Golay Filter
library(pracma)
spokane.daily$savgol = savgol(spokane.daily$Daily.Mean.PM2.5.Concentration,fl=7,forder=2,dorder=0)

# Apply Pspline fit
library(pspline)
pspline.fit = sm.spline(spokane.daily$Date, spokane.daily$Daily.Mean.PM2.5.Concentration,
                                  spar=1E14)
spokane.daily$pspline = pspline.fit$ysmth


# Plot Daily Means with smoothed fits
colors = c("savgol"="red","pspline"="green")
ggplot() +
  geom_point(data=spokane.daily,
             aes(x=Date, y=Daily.Mean.PM2.5.Concentration),size=5,shape=16) +
  geom_line(data=spokane.daily,
            aes(x=Date, y=savgol, color="savgol"),size=0.8) +
  geom_line(data=spokane.daily,
            aes(x=Date, y=pspline, color="pspline"),size=0.8,linetype="dashed") +
  labs(title="2020 Daily PM 2.5 Counts - Spokane Sites", x="Date",
       y="Daily 2020 Mean PM 2.5 Concentration (ug/m^3)") + 
  scale_x_continuous(breaks= seq(as.POSIXct("2020-01-01"),max(WA.samples$Date),by="month"), 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                "Sep","Oct","Nov"),
                     limits=c(as.POSIXct("2020-01-01"),max(WA.samples$Date))) +
  scale_color_manual(values=colors, name="Smoother") +
  theme(text=element_text(size=16,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Isolate Smoke Event
spokane.daily$high.smoke = spokane.daily$Daily.Mean.PM2.5.Concentration >
  quantile(spokane.daily$Daily.Mean.PM2.5.Concentration,p=0.98)

# Plot the Smoke Event
# Plot Daily Means with smoothed fits
colors = c("savgol"="red","pspline"="green")
ggplot() +
  geom_point(data=spokane.daily,
             aes(x=Date, y=Daily.Mean.PM2.5.Concentration, color=high.smoke),size=5,shape=16) +
  labs(title="2020 Daily PM 2.5 Counts - Spokane Sites", x="Date",
       y="Daily 2020 Mean PM 2.5 Concentration (ug/m^3)") + 
  scale_x_continuous(breaks= seq(as.POSIXct("2020-01-01"),max(WA.samples$Date),by="month"), 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                "Sep","Oct","Nov"),
                     limits=c(as.POSIXct("2020-01-01"),max(WA.samples$Date))) +
  #scale_color_manual(values=colors, name="Smoother") +
  theme(text=element_text(size=16,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Get Date Range for Smoke Event
range(spokane.daily[spokane.daily$high.smoke&spokane.daily$Date>as.POSIXct("2020-01-01"),]$Date)

# Smoke event for Spokane WA
# "2020-09-11 PDT" "2020-09-19 PDT"


#### Portland Data ####
# Load and Combine Data sets
OR.samples = read.csv("Data.nosync/OR_2007_ad_viz_plotval_data.csv")
for (yr in 2007:2020) {
  OR.samples = rbind(OR.samples, read.csv(paste0("Data.nosync/OR_",yr,"_ad_viz_plotval_data.csv")))
}
OR.samples$Date = as.POSIXct(OR.samples$Date, format="%m/%d/%Y")

# Look at one Site Near Home
portland.samples = OR.samples[OR.samples$CBSA_NAME=="Portland-Vancouver-Hillsboro, OR-WA",]
portland.samples$Site.ID = as.factor(portland.samples$Site.ID)

# Plot the 4 portland Monitoring Sites together
ggplot() +
  geom_point(data=portland.samples,
             aes(x=Date, y=Daily.Mean.PM2.5.Concentration, color=Site.ID),size=0.8) +
  labs(title="Daily PM 2.5 Counts - Portland Sites", x="Date",
       y="Daily Mean PM 2.5 Concentration (ug/m^3)") +
  scale_x_continuous(breaks= seq(min(OR.samples$Date),max(OR.samples$Date),by="year"), 
                     labels = c(2007:2020)) +
  theme(text=element_text(size=16,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Plot the 2020 Data
ggplot() +
  geom_point(data=portland.samples,
             aes(x=Date, y=Daily.Mean.PM2.5.Concentration, color=Site.ID),size=0.8) +
  labs(title="2020 Daily PM 2.5 Counts - Portland Sites", x="Date",
       y="Daily Mean PM 2.5 Concentration (ug/m^3)") + 
  scale_x_continuous(breaks= seq(as.POSIXct("2020-01-01"),max(OR.samples$Date),by="month"), 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                "Sep","Oct","Nov"),
                     limits=c(as.POSIXct("2020-01-01"),max(OR.samples$Date))) +
  theme(text=element_text(size=16,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Combine the Daily Data
portland.daily = tapply(portland.samples$Daily.Mean.PM2.5.Concentration,
                       portland.samples$Date, mean)
portland.daily = data.frame("Date"=as.POSIXct(names(portland.daily)),
                           "Daily.Mean.PM2.5.Concentration"=portland.daily)

# Check for missing values
sum(is.na(portland.daily))

# Apply Savitsky Golay Filter
library(pracma)
portland.daily$savgol = savgol(portland.daily$Daily.Mean.PM2.5.Concentration,fl=7,forder=2,dorder=0)

# Apply Pspline fit
library(pspline)
pspline.fit = sm.spline(portland.daily$Date, portland.daily$Daily.Mean.PM2.5.Concentration,
                        spar=1E14)
portland.daily$pspline = pspline.fit$ysmth


# Plot Daily Means with smoothed fits
colors = c("savgol"="red","pspline"="green")
ggplot() +
  geom_point(data=portland.daily,
             aes(x=Date, y=Daily.Mean.PM2.5.Concentration),size=5,shape=16) +
  geom_line(data=portland.daily,
            aes(x=Date, y=savgol, color="savgol"),size=0.8) +
  geom_line(data=portland.daily,
            aes(x=Date, y=pspline, color="pspline"),size=0.8,linetype="dashed") +
  labs(title="2020 Daily PM 2.5 Counts - Portland Sites", x="Date",
       y="Daily Mean PM 2.5 Concentration (ug/m^3)") + 
  scale_x_continuous(breaks= seq(as.POSIXct("2020-01-01"),max(OR.samples$Date),by="month"), 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                "Sep","Oct","Nov"),
                     limits=c(as.POSIXct("2020-01-01"),max(OR.samples$Date))) +
  scale_color_manual(values=colors, name="Smoother") +
  theme(text=element_text(size=16,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Isolate Smoke Event
portland.daily$high.smoke = portland.daily$Daily.Mean.PM2.5.Concentration >
  quantile(portland.daily$Daily.Mean.PM2.5.Concentration,p=0.98)

# Plot the Smoke Event
# Plot Daily Means with smoothed fits
colors = c("savgol"="red","pspline"="green")
ggplot() +
  geom_point(data=portland.daily,
             aes(x=Date, y=Daily.Mean.PM2.5.Concentration, color=high.smoke),size=5,shape=16) +
  labs(title="2020 Daily PM 2.5 Counts - Portland Sites", x="Date",
       y="Daily Mean PM 2.5 Concentration (ug/m^3)") + 
  scale_x_continuous(breaks= seq(as.POSIXct("2020-01-01"),max(OR.samples$Date),by="month"), 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                "Sep","Oct","Nov"),
                     limits=c(as.POSIXct("2020-01-01"),max(OR.samples$Date))) +
  #scale_color_manual(values=colors, name="Smoother") +
  theme(text=element_text(size=16,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Get Date Range for Smoke Event
range(portland.daily[portland.daily$high.smoke&portland.daily$Date>as.POSIXct("2020-01-01"),]$Date)

# Smoke event for Portland Oregon
# "2020-09-07 PDT" "2020-09-18 PDT"
#### San Francisco Data ####
# Load and Combine Data sets
CA.samples = read.csv("Data.nosync/CA_2007_ad_viz_plotval_data.csv")
for (yr in 2007:2020) {
  CA.samples = rbind(CA.samples, read.csv(paste0("Data.nosync/CA_",yr,"_ad_viz_plotval_data.csv")))
}
CA.samples$Date = as.POSIXct(CA.samples$Date, format="%m/%d/%Y")

# Look at one Site Near Home
sanfrancisco.samples = CA.samples[CA.samples$CBSA_NAME=="San Francisco-Oakland-Hayward, CA",]
sanfrancisco.samples$Site.ID = as.factor(sanfrancisco.samples$Site.ID)

# Plot the 4 San Francisco Monitoring Sites together
ggplot() +
  geom_point(data=sanfrancisco.samples,
             aes(x=Date, y=Daily.Mean.PM2.5.Concentration, color=Site.ID),size=0.8) +
  labs(title="Daily PM 2.5 Counts - San Francisco Sites", x="Date",
       y="Daily Mean PM 2.5 Concentration (ug/m^3)") +
  scale_x_continuous(breaks= seq(min(CA.samples$Date),max(CA.samples$Date),by="year"), 
                     labels = c(2007:2020)) +
  theme(text=element_text(size=16,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Plot the 2020 Data
ggplot() +
  geom_point(data=sanfrancisco.samples,
             aes(x=Date, y=Daily.Mean.PM2.5.Concentration, color=Site.ID),size=0.8) +
  labs(title="2020 Daily PM 2.5 Counts - San Francisco Sites", x="Date",
       y="Daily Mean PM 2.5 Concentration (ug/m^3)") + 
  scale_x_continuous(breaks= seq(as.POSIXct("2020-01-01"),max(CA.samples$Date),by="month"), 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                "Sep","Oct","Nov"),
                     limits=c(as.POSIXct("2020-01-01"),max(CA.samples$Date))) +
  theme(text=element_text(size=16,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Combine the Daily Data
sanfrancisco.daily = tapply(sanfrancisco.samples$Daily.Mean.PM2.5.Concentration,
                        sanfrancisco.samples$Date, mean)
sanfrancisco.daily = data.frame("Date"=as.POSIXct(names(sanfrancisco.daily)),
                            "Daily.Mean.PM2.5.Concentration"=sanfrancisco.daily)

# Check for missing values
sum(is.na(sanfrancisco.daily))

# Apply Savitsky Golay Filter
library(pracma)
sanfrancisco.daily$savgol = savgol(sanfrancisco.daily$Daily.Mean.PM2.5.Concentration,fl=7,forder=2,dorder=0)

# Apply Pspline fit
library(pspline)
pspline.fit = sm.spline(sanfrancisco.daily$Date, sanfrancisco.daily$Daily.Mean.PM2.5.Concentration,
                        spar=1E14)
sanfrancisco.daily$pspline = pspline.fit$ysmth


# Plot Daily Means with smoothed fits
colors = c("savgol"="red","pspline"="green")
ggplot() +
  geom_point(data=sanfrancisco.daily,
             aes(x=Date, y=Daily.Mean.PM2.5.Concentration),size=5,shape=16) +
  geom_line(data=sanfrancisco.daily,
            aes(x=Date, y=savgol, color="savgol"),size=0.8) +
  geom_line(data=sanfrancisco.daily,
            aes(x=Date, y=pspline, color="pspline"),size=0.8,linetype="dashed") +
  labs(title="2020 Daily PM 2.5 Counts - San Francisco Sites", x="Date",
       y="Daily Mean PM 2.5 Concentration (ug/m^3)") + 
  scale_x_continuous(breaks= seq(as.POSIXct("2020-01-01"),max(CA.samples$Date),by="month"), 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                "Sep","Oct","Nov"),
                     limits=c(as.POSIXct("2020-01-01"),max(CA.samples$Date))) +
  scale_color_manual(values=colors, name="Smoother") +
  theme(text=element_text(size=16,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Isolate Smoke Event
sanfrancisco.daily$high.smoke = sanfrancisco.daily$Daily.Mean.PM2.5.Concentration >
  quantile(sanfrancisco.daily$Daily.Mean.PM2.5.Concentration,p=0.98)

# Plot the Smoke Event
# Plot Daily Means with smoothed fits
colors = c("savgol"="red","pspline"="green")
ggplot() +
  geom_point(data=sanfrancisco.daily,
             aes(x=Date, y=Daily.Mean.PM2.5.Concentration, color=high.smoke),size=5,shape=16) +
  labs(title="2020 Daily PM 2.5 Counts - San Francisco Sites", x="Date",
       y="Daily Mean PM 2.5 Concentration (ug/m^3)") + 
  scale_x_continuous(breaks= seq(as.POSIXct("2020-01-01"),max(CA.samples$Date),by="month"), 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                "Sep","Oct","Nov"),
                     limits=c(as.POSIXct("2020-01-01"),max(CA.samples$Date))) +
  #scale_color_manual(values=colors, name="Smoother") +
  theme(text=element_text(size=16,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Get Date Range for Smoke Event
range(sanfrancisco.daily[sanfrancisco.daily$high.smoke&sanfrancisco.daily$Date>as.POSIXct("2020-01-01"),]$Date)

# Smoke event for San Francisco CA
# "2020-08-19 PDT" "2020-10-07 PDT"



#### County Health Data ####
# Read in csv Files
disease = read.csv(file=paste0("Disease_data/Prevalence_2007.csv"))
disease$year = 2007
for (yr in 2008:2017) {
  file = read.csv(file=paste0("Disease_data/Prevalence_",yr,".csv"))
  yr.appended = cbind(file,rep(yr,nrow(file)))
  names(yr.appended) = c(names(file),"year")
  disease = rbind(disease, yr.appended)
}
for (i in 3:24) {
  disease[,i] = as.numeric(disease[,i])
}

# Pull out Trend For Spokane County
spokane.disease = disease[disease$County=="Spokane ",]
spokane.disease = spokane.disease[order(spokane.disease$year),]

# Plot Respiratory and Cardiovascular Illnesses
ggplot(data=spokane.disease) +
  geom_line(aes(x=year,y=Asthma,color="Asthma"), size=2) +
  geom_line(aes(x=year,y=COPD,color="COPD"), size=2) +
  geom_line(aes(x=year,y=Cancer,color="Cancer"), size=2) +
  geom_line(aes(x=year,y=Heart.Failure,color="Heart Failure"), size=2) +
  geom_line(aes(x=year,y=Hypertension,color="Hypertension"), size=2) +
  geom_line(aes(x=year,y=Ischemic.Heart.Disease,color="Ischemic Heart Disease"), size=2) +
  scale_x_continuous(breaks= c(2007:2017), 
                     labels = c(2007:2017)) +
  ylab("Prevalence (%)") + xlab("Year") + 
  ggtitle("Spokane Medicare Disease Prevalence Trends") +
  labs(color="Disease") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Pull out Trend For Spokane County
spokane.disease = disease[disease$County=="Spokane ",]

# Plot Respiratory and Cardiovascular Illnesses
ggplot(data=spokane.disease) +
  geom_line(aes(x=year,y=Asthma,color="Asthma"), size=2) +
  geom_line(aes(x=year,y=COPD,color="COPD"), size=2) +
  geom_line(aes(x=year,y=Cancer,color="Cancer"), size=2) +
  geom_line(aes(x=year,y=Heart.Failure,color="Heart Failure"), size=2) +
  geom_line(aes(x=year,y=Hypertension,color="Hypertension"), size=2) +
  geom_line(aes(x=year,y=Ischemic.Heart.Disease,color="Ischemic Heart Disease"), size=2) +
  scale_x_continuous(breaks= c(2007:2017), 
                     labels = c(2007:2017)) +
  ylab("Prevalence (%)") + xlab("Year") + 
  ggtitle("Spokane Medicare Disease Prevalence Trends") +
  labs(color="Disease") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Pull out Trend For Multnomah County
portland.disease = disease[disease$County=="Multnomah ",]

# Plot Respiratory and Cardiovascular Illnesses
ggplot(data=portland.disease) +
  geom_line(aes(x=year,y=Asthma,color="Asthma"), size=2) +
  geom_line(aes(x=year,y=COPD,color="COPD"), size=2) +
  geom_line(aes(x=year,y=Cancer,color="Cancer"), size=2) +
  geom_line(aes(x=year,y=Heart.Failure,color="Heart Failure"), size=2) +
  geom_line(aes(x=year,y=Hypertension,color="Hypertension"), size=2) +
  geom_line(aes(x=year,y=Ischemic.Heart.Disease,color="Ischemic Heart Disease"), size=2) +
  scale_x_continuous(breaks= c(2007:2017), 
                     labels = c(2007:2017)) +
  ylab("Prevalence (%)") + xlab("Year") + 
  ggtitle("Portland Medicare Disease Prevalence Trends") +
  labs(color="Disease") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Pull out Trend For Multnomah County
sanfrancisco.disease = disease[disease$County=="San Francisco ",]

# Plot Respiratory and Cardiovascular Illnesses
ggplot(data=sanfrancisco.disease) +
  geom_line(aes(x=year,y=Asthma,color="Asthma"), size=2) +
  geom_line(aes(x=year,y=COPD,color="COPD"), size=2) +
  geom_line(aes(x=year,y=Cancer,color="Cancer"), size=2) +
  geom_line(aes(x=year,y=Heart.Failure,color="Heart Failure"), size=2) +
  geom_line(aes(x=year,y=Hypertension,color="Hypertension"), size=2) +
  geom_line(aes(x=year,y=Ischemic.Heart.Disease,color="Ischemic Heart Disease"), size=2) +
  scale_x_continuous(breaks= c(2007:2017), 
                     labels = c(2007:2017)) +
  ylab("Prevalence (%)") + xlab("Year") + 
  ggtitle("San Francisco Medicare Disease Prevalence Trends") +
  labs(color="Disease") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

#### Annual County Level Regression ####

## Load WA,CA,OR
# Load WA
samples = read.csv("Data.nosync/WA_2007_ad_viz_plotval_data.csv")
for (yr in 2007:2020) {
  samples = rbind(samples, read.csv(paste0("Data.nosync/WA_",yr,"_ad_viz_plotval_data.csv")))
}
# Load OR
samples = read.csv("Data.nosync/OR_2007_ad_viz_plotval_data.csv")
for (yr in 2007:2020) {
  samples = rbind(samples, read.csv(paste0("Data.nosync/OR_",yr,"_ad_viz_plotval_data.csv")))
}
# Load CA
samples = read.csv("Data.nosync/CA_2007_ad_viz_plotval_data.csv")
for (yr in 2007:2020) {
  samples = rbind(samples, read.csv(paste0("Data.nosync/CA_",yr,"_ad_viz_plotval_data.csv")))
}
samples$Date = as.POSIXct(samples$Date, format="%m/%d/%Y")

# Load in Health Data
disease = read.csv(file=paste0("Disease_data/Prevalence_2007.csv"))
disease$year = 2007
for (yr in 2008:2017) {
  file = read.csv(file=paste0("Disease_data/Prevalence_",yr,".csv"))
  yr.appended = cbind(file,rep(yr,nrow(file)))
  names(yr.appended) = c(names(file),"year")
  disease = rbind(disease, yr.appended)
}
for (i in 3:24) {
  disease[,i] = as.numeric(disease[,i])
}

# Remove Spaces from Disease County Column
disease$County = sapply(disease$County,FUN=function(x) gsub(" ", "",x))

# Create Year Column
samples$Year = format(samples$Date,'%Y')

# Aggregate Annual PM2.5 
annual.county.mean = tapply(samples$Daily.Mean.PM2.5.Concentration,
                            list(samples$COUNTY, samples$Year), mean, na.rm=TRUE)
annual.county.median = tapply(samples$Daily.Mean.PM2.5.Concentration,
                            list(samples$COUNTY, samples$Year), median, na.rm=TRUE)
annual.county.95 = tapply(samples$Daily.Mean.PM2.5.Concentration,
                            list(samples$COUNTY, samples$Year), quantile, p=.95)

# Collapse the Matrices
# Mean
annual.county.df = data.frame("Mean"= as.vector(annual.county.mean))

# Median
annual.county.df$median = as.vector(annual.county.median)

# 95th Quantile
annual.county.df$quantile95 = as.vector(annual.county.95)

# Year and County Columns
annual.county.df$County = rep(rownames(annual.county.mean), times=ncol(annual.county.mean))
annual.county.df$Year = rep(colnames(annual.county.mean), each=nrow(annual.county.mean))
names(annual.county.df) = c("Mean","Median","Quantile95","County","Year")

# Merge the PM data with the disease data based on county
annual.county.PM.disease = merge(annual.county.df, disease, by='County')
annual.county.PM.disease = annual.county.PM.disease[annual.county.PM.disease$Year==annual.county.PM.disease$year,]
annual.county.PM.disease = annual.county.PM.disease[
  annual.county.PM.disease$State=="Washington "|
  annual.county.PM.disease$State=="Oregon "|
  annual.county.PM.disease$State=="California ",]

# Take a Peek
# Plot Respiratory and Cardiovascular Illnesses
ggplot(data=annual.county.PM.disease) +
  geom_point(aes(x=year+.5,y=Asthma,color="Asthma"), size=2) +
  geom_point(aes(x=year+.3,y=COPD,color="COPD"), size=2) +
  geom_point(aes(x=year+.4,y=Cancer,color="Cancer"), size=2) +
  geom_point(aes(x=year+.2,y=Heart.Failure,color="Heart Failure"), size=2) +
  geom_point(aes(x=year,y=Hypertension,color="Hypertension"), size=2) +
  geom_point(aes(x=year+.1,y=Ischemic.Heart.Disease,color="Ischemic Heart Disease"), size=2) +
  scale_x_continuous(breaks= c(2007:2017), 
                     labels = c(2007:2017)) +
  ylab("Prevalence (%)") + xlab("Year") + 
  ggtitle("WA, OR, CA County Medicare Disease Prevalence Trends") +
  labs(color="Disease") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Plot Disease Against PM
# Mean
ggplot(data=annual.county.PM.disease) +
  geom_point(aes(x=Mean,y=Asthma,color="Asthma"), size=2) +
  geom_smooth(aes(x=Mean,y=Asthma,color="Asthma"), size=2, method="loess") +
  geom_point(aes(x=Mean,y=COPD,color="COPD"), size=2) +
  geom_smooth(aes(x=Mean,y=COPD,color="COPD"), size=2, method="loess") +
  geom_point(aes(x=Mean,y=Cancer,color="Cancer"), size=2) +
  geom_smooth(aes(x=Mean,y=Cancer,color="Cancer"), size=2, method="loess") +
  geom_point(aes(x=Mean,y=Heart.Failure,color="Heart Failure"), size=2) +
  geom_smooth(aes(x=Mean,y=Heart.Failure,color="Heart Failure"), size=2, method="loess") +
  geom_point(aes(x=Mean,y=Hypertension,color="Hypertension"), size=2) +
  geom_smooth(aes(x=Mean,y=Hypertension,color="Hypertension"), size=2, method="loess") +
  geom_point(aes(x=Mean,y=Ischemic.Heart.Disease,color="Ischemic Heart Disease"), size=2) +
  geom_smooth(aes(x=Mean,y=Ischemic.Heart.Disease,color="Ischemic Heart Disease"), size=2, method="loess") +
  ylab("Prevalence (%)") + xlab("Mean Annual PM 2.5") + 
  ggtitle("WA, OR, CA County Medicare Disease Prevalence Trends (Mean)") +
  labs(color="Disease") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Median
ggplot(data=annual.county.PM.disease) +
  geom_point(aes(x=Median,y=Asthma,color="Asthma"), size=2) +
  geom_smooth(aes(x=Median,y=Asthma,color="Asthma"), size=2, method="loess") +
  geom_point(aes(x=Median,y=COPD,color="COPD"), size=2) +
  geom_smooth(aes(x=Median,y=COPD,color="COPD"), size=2, method="loess") +
  geom_point(aes(x=Median,y=Cancer,color="Cancer"), size=2) +
  geom_smooth(aes(x=Median,y=Cancer,color="Cancer"), size=2, method="loess") +
  geom_point(aes(x=Median,y=Heart.Failure,color="Heart Failure"), size=2) +
  geom_smooth(aes(x=Median,y=Heart.Failure,color="Heart Failure"), size=2, method="loess") +
  geom_point(aes(x=Median,y=Hypertension,color="Hypertension"), size=2) +
  geom_smooth(aes(x=Median,y=Hypertension,color="Hypertension"), size=2, method="loess") +
  geom_point(aes(x=Median,y=Ischemic.Heart.Disease,color="Ischemic Heart Disease"), size=2) +
  geom_smooth(aes(x=Median,y=Ischemic.Heart.Disease,color="Ischemic Heart Disease"), size=2, method="loess") +
  ylab("Prevalence (%)") + xlab("Median Annual PM 2.5") + 
  ggtitle("WA, OR, CA County Medicare Disease Prevalence Trends (Median)") +
  labs(color="Disease") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Quantile 95
ggplot(data=annual.county.PM.disease) +
  geom_point(aes(x=Quantile95,y=Asthma,color="Asthma"), size=2) +
  geom_smooth(aes(x=Quantile95,y=Asthma,color="Asthma"), size=2, method="loess") +
  geom_point(aes(x=Quantile95,y=COPD,color="COPD"), size=2) +
  geom_smooth(aes(x=Quantile95,y=COPD,color="COPD"), size=2, method="loess") +
  geom_point(aes(x=Quantile95,y=Cancer,color="Cancer"), size=2) +
  geom_smooth(aes(x=Quantile95,y=Cancer,color="Cancer"), size=2, method="loess") +
  geom_point(aes(x=Quantile95,y=Heart.Failure,color="Heart Failure"), size=2) +
  geom_smooth(aes(x=Quantile95,y=Heart.Failure,color="Heart Failure"), size=2, method="loess") +
  geom_point(aes(x=Quantile95,y=Hypertension,color="Hypertension"), size=2) +
  geom_smooth(aes(x=Quantile95,y=Hypertension,color="Hypertension"), size=2, method="loess") +
  geom_point(aes(x=Quantile95,y=Ischemic.Heart.Disease,color="Ischemic Heart Disease"), size=2) +
  geom_smooth(aes(x=Quantile95,y=Ischemic.Heart.Disease,color="Ischemic Heart Disease"), size=2, method="loess") +
  ylab("Prevalence (%)") + xlab("95th Percentile Annual PM 2.5") + 
  ggtitle("WA, OR, CA County Medicare Disease Prevalence Trends (95th Percentile)") +
  labs(color="Disease") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

#### Bootstrap Analysis ####

# Run a Robust Regression on all the Trends
diseases = c("Asthma","COPD","Cancer","Heart.Failure",
             "Hypertension","Ischemic.Heart.Disease")
aggregation.types = c("Mean", "Median", "Quantile95")

output = data.frame("disease"=rep(diseases,3))

# Mean

# Define Objects
slopes = data.frame("OLS"=numeric(100),"MA"=numeric(100),"SMA"=numeric(100))

# Cycle through diseases
for (w in 1:length(aggregation.types)) {
  # Remove Outliers 
  if (w == 1) {
    inlier.max =3*IQR(annual.county.PM.disease$Mean,na.rm=TRUE)+median(annual.county.PM.disease$Mean,na.rm=TRUE)
    inliers = annual.county.PM.disease$Mean < inlier.max
    data = annual.county.PM.disease[inliers,] 
  } else if (w ==2) {
    inlier.max =3*IQR(annual.county.PM.disease$Median,na.rm=TRUE)+median(annual.county.PM.disease$Median,na.rm=TRUE)
    inliers = annual.county.PM.disease$Median < inlier.max
    data = annual.county.PM.disease[inliers,] 
  } else if (w ==3) {
    inlier.max =3*IQR(annual.county.PM.disease$Quantile95,na.rm=TRUE)+median(annual.county.PM.disease$Quantile95,na.rm=TRUE)
    inliers = annual.county.PM.disease$Quantile95 < inlier.max
    data = annual.county.PM.disease[inliers,] 
  }
  for (i in 1:length(diseases)) {
    # bootstrap 100 times
    for (j in 1:100) {
      disease = diseases[i]
      fm = as.formula(paste(disease,"~", aggregation.types[w]))
      lmodel.inliers = suppressMessages(lmodel2(fm,
                                                data=data[sample(1:nrow(data), nrow(data), replace = TRUE),]),
                                        classes = "message")
      slopes$OLS[j] = lmodel.inliers$regression.results["Slope"][1,]
      slopes$MA[j] = lmodel.inliers$regression.results["Slope"][2,]
      slopes$SMA[j] = lmodel.inliers$regression.results["Slope"][3,]
    }
    # Calculate Index
    idx = i+(w-1)*6
    
    # Save out Slope Estimates and confidence intervals
    output$OLS.slope[idx] = mean(slopes$OLS)
    output$OLS.slope.low[idx] = quantile(slopes$OLS, p=0.025)
    output$OLS.slope.high[idx] = quantile(slopes$OLS, p=0.975)
    
    output$MA.slope[idx] = mean(slopes$MA)
    output$MA.slope.low[idx] = quantile(slopes$MA, p=0.025)
    output$MA.slope.high[idx] = quantile(slopes$MA, p=0.975)
    
    output$SMA.slope[idx] = mean(slopes$SMA)
    output$SMA.slope.low[idx] = quantile(slopes$SMA, p=0.025)
    output$SMA.slope.high[idx] = quantile(slopes$SMA, p=0.975)
    
    output$aggregation.type[idx] = aggregation.types[w]
  }
  print(aggregation.types[w])
}

# Make the Barplot (SMA)
ggplot(data=output, aes(x=disease, y=SMA.slope, fill=aggregation.type)) +
  geom_bar(stat="identity", width=0.8, position=position_dodge()) +
  geom_errorbar(aes(ymin=SMA.slope.low, ymax=SMA.slope.high), width=.2,
                position=position_dodge(.8)) +
  labs(fill="Aggregation Method") +
  xlab("Disease") + ylab("Slope") + ggtitle("Bootstrapped Slope Estimates (SMA)") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
      panel.border = element_blank(),
      panel.grid.major = element_line(color = "lightgrey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"))

# Make the Barplot (MA)
ggplot(data=output, aes(x=disease, y=MA.slope, fill=aggregation.type)) +
  geom_bar(stat="identity", width=0.8, position=position_dodge()) +
  geom_errorbar(aes(ymin=MA.slope.low, ymax=MA.slope.high), width=.2,
                position=position_dodge(.8)) +
  labs(fill="Aggregation Method") +
  xlab("Disease") + ylab("Slope") + ggtitle("Bootstrapped Slope Estimates (MA)") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Make the Barplot (OLS)
ggplot(data=output, aes(x=disease, y=OLS.slope, fill=aggregation.type)) +
  geom_bar(stat="identity", width=0.8, position=position_dodge()) +
  geom_errorbar(aes(ymin=OLS.slope.low, ymax=OLS.slope.high), width=.2,
                position=position_dodge(.8)) +
  labs(fill="Aggregation Method") +
  xlab("Disease") + ylab("Slope") + ggtitle("Bootstrapped Slope Estimates (OLS)") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

#### Scaled Bootsrap Analysis ####
# Run a Robust Regression on all the Trends
diseases = c("Asthma","COPD","Cancer","Heart.Failure",
             "Hypertension","Ischemic.Heart.Disease")
aggregation.types = c("Mean", "Median", "Quantile95")

output = data.frame("disease"=rep(diseases,3))

# Mean

# Define Objects
slopes = data.frame("OLS"=numeric(100),"MA"=numeric(100),"SMA"=numeric(100))

# Cycle through diseases
for (w in 1:length(aggregation.types)) {
  # Remove Outliers 
  if (w == 1) {
    inlier.max =3*IQR(annual.county.PM.disease$Mean,na.rm=TRUE)+median(annual.county.PM.disease$Mean,na.rm=TRUE)
    inliers = annual.county.PM.disease$Mean < inlier.max
    data = annual.county.PM.disease[inliers,] 
    data$Mean = data$Mean/max(data$Mean, na.rm=TRUE)
  } else if (w ==2) {
    inlier.max =3*IQR(annual.county.PM.disease$Median,na.rm=TRUE)+median(annual.county.PM.disease$Median,na.rm=TRUE)
    inliers = annual.county.PM.disease$Median < inlier.max
    data = annual.county.PM.disease[inliers,] 
    data$Median = data$Median/max(data$Median, na.rm=TRUE)
  } else if (w ==3) {
    inlier.max =3*IQR(annual.county.PM.disease$Quantile95,na.rm=TRUE)+median(annual.county.PM.disease$Quantile95,na.rm=TRUE)
    inliers = annual.county.PM.disease$Quantile95 < inlier.max
    data = annual.county.PM.disease[inliers,] 
    data$Quantile95 = data$Quantile95/max(data$Quantile95, na.rm=TRUE)
  }
  for (i in 1:length(diseases)) {
    # bootstrap 100 times
    for (j in 1:100) {
      disease = diseases[i]
      fm = as.formula(paste(disease,"~", aggregation.types[w]))
      lmodel.inliers = suppressMessages(lmodel2(fm,
                       data=data[sample(1:nrow(data), nrow(data), replace = TRUE),]),
                       classes = "message")
      slopes$OLS[j] = lmodel.inliers$regression.results["Slope"][1,]
      slopes$MA[j] = lmodel.inliers$regression.results["Slope"][2,]
      slopes$SMA[j] = lmodel.inliers$regression.results["Slope"][3,]
    }
    # Calculate Index
    idx = i+(w-1)*6
    
    # Save out Slope Estimates and confidence intervals
    output$OLS.slope[idx] = mean(slopes$OLS)
    output$OLS.slope.low[idx] = quantile(slopes$OLS, p=0.025)
    output$OLS.slope.high[idx] = quantile(slopes$OLS, p=0.975)
    
    output$MA.slope[idx] = mean(slopes$MA)
    output$MA.slope.low[idx] = quantile(slopes$MA, p=0.025)
    output$MA.slope.high[idx] = quantile(slopes$MA, p=0.975)
    
    output$SMA.slope[idx] = mean(slopes$SMA)
    output$SMA.slope.low[idx] = quantile(slopes$SMA, p=0.025)
    output$SMA.slope.high[idx] = quantile(slopes$SMA, p=0.975)
    
    output$aggregation.type[idx] = aggregation.types[w]
  }
  print(aggregation.types[w])
}

# Make the Barplot (SMA)
ggplot(data=output, aes(x=disease, y=SMA.slope, fill=aggregation.type)) +
  geom_bar(stat="identity", width=0.8, position=position_dodge()) +
  geom_errorbar(aes(ymin=SMA.slope.low, ymax=SMA.slope.high), width=.2,
                position=position_dodge(.8)) +
  labs(fill="Aggregation Method") +
  xlab("Disease") + ylab("Slope") + ggtitle("Scaled Bootstrapped Slope Estimates (SMA)") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Make the Barplot (MA)
ggplot(data=output, aes(x=disease, y=MA.slope, fill=aggregation.type)) +
  geom_bar(stat="identity", width=0.8, position=position_dodge()) +
  geom_errorbar(aes(ymin=MA.slope.low, ymax=MA.slope.high), width=.2,
                position=position_dodge(.8)) +
  labs(fill="Aggregation Method") +
  xlab("Disease") + ylab("Slope") + ggtitle("Scaled Bootstrapped Slope Estimates (MA)") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Make the Barplot (OLS)
ggplot(data=output, aes(x=disease, y=OLS.slope, fill=aggregation.type)) +
  geom_bar(stat="identity", width=0.8, position=position_dodge()) +
  geom_errorbar(aes(ymin=OLS.slope.low, ymax=OLS.slope.high), width=.2,
                position=position_dodge(.8)) +
  labs(fill="Aggregation Method") +
  xlab("Disease") + ylab("Slope") + ggtitle("Scaled Bootstrapped Slope Estimates (OLS)") +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))
#### Mulitplot ####
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
