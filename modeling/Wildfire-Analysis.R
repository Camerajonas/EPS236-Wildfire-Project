#####################################################################
#File name: Wildfire-Analysis.R
#Author: Blake Bullwinkel
#Date: November 2020
#Purpose: Exploratory analysis of wildfire data in CA 
#####################################################################

# Load dependencies
library(sp)
library(rgdal)
library(ggplot2)
library(ggalt)
library(lubridate)
library(extrafont)
library(data.table)
library(zoo)

#### All California Fires 2013-2020 ####
# Read in the California data
cali_data = read.csv('ca-wildfires.csv')

# Enforce latitude and longitude limits to remove incorrect data
cali_data = cali_data[(cali_data$incident_longitude > -125 & cali_data$incident_longitude < -115 & cali_data$incident_latitude > 32 & cali_data$incident_latitude < 42),]

# Remove specific incorrect value
cali_data = cali_data[!(cali_data$incident_id=='ac568737-dae6-4050-b1f4-62ab1f9ec930'),]

# Convert relevant date fields to POSIXct objects
cali_data$incident_dateonly_created = ymd(cali_data$incident_dateonly_created)
cali_data$incident_dateonly_extinguished = ymd(cali_data$incident_dateonly_extinguished)
cali_data = cali_data[(cali_data$incident_dateonly_created > '2010-01-01'),]

# Load Map of US States
mapUSm <- readOGR(dsn="US48", layer="US_48states")  

options(scipen=999) # turn off scientific notation

# Fortify polygons
states.df = fortify(mapUSm[c(24),]) # State Outlines

# Plot the California wildfires
tiff("Presentation Plots/cali_fire_map.tiff", units='in', width=10, height=6, res=300)
cali_fire_map <- ggplot() + 
  geom_point(data=cali_data, aes(x=incident_longitude, y=incident_latitude, size=incident_acres_burned), color="red", alpha=0.2, pch=16) +
  geom_polygon(data=states.df, aes(long,lat,group=group), fill=NA, size=.5) +
  geom_path(data=states.df, aes(long,lat,group=group), color="black") +
  labs(title="Map of California Wildfires",
       y="Latitude", 
       x="Longitude",
       size="Area Burned (acres)",
       fill="Area Burned (acres)",
       subtitle="2013-2020") +
  coord_fixed() + theme_minimal()

plot(cali_fire_map)
dev.off()

#### Monthly California Fires Analysis 2013-2020 ####
# Create a data.table object
cali_dt = setDT(cali_data)

# Calculate the total number of wildires in CA each month
cali_monthly_fires = cali_dt[, .N, by=.(year(incident_dateonly_created), month(incident_dateonly_created))]
cali_monthly_fires$date = as.yearmon(paste(cali_monthly_fires$year, cali_monthly_fires$month), "%Y %m")

# Calculate the total acres burned by wildfires in CA each month
cali_monthly_acres = cali_data[, sum(incident_acres_burned), by=.(year(incident_dateonly_created), month(incident_dateonly_created))]
cali_monthly_acres$date = as.yearmon(paste(cali_monthly_acres$year, cali_monthly_acres$month), "%Y %m")

# Create a histogram showing the number of wildires in California per month from 2013-2020
tiff("Presentation Plots/cali_monthly_fires_plot.tiff", units='in', width=10, height=6, res=300)
cali_monthly_fires_plot <- ggplot(cali_monthly_fires, aes(x=date, y=N)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  #geom_smooth(method="loess", se=F) + 
  labs(subtitle="2013-2020", 
       y="Number of Fires", 
       x="Month", 
       title="Number of CA Wildfires per Month", 
       caption = "Source: Cal Fire (fire.ca.gov)") +
  theme(text=element_text(size=14), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

plot(cali_monthly_fires_plot)
dev.off()

# Create a histogram showing the total acres burned by wildires in California per month from 2013-2020
tiff("Presentation Plots/cali_monthly_acres_plot.tiff", units='in', width=10, height=6, res=300)
cali_monthly_acres_plot <- ggplot(cali_monthly_acres, aes(x=date, y=V1)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  #geom_smooth(method="loess", se=F) + 
  labs(subtitle="2013-2020", 
       y="Area Burned (acres)", 
       x="Month", 
       title="Area Burned by CA Wildfires per Month", 
       caption = "Source: Cal Fire (fire.ca.gov)") +
  theme(text=element_text(size=14), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

plot(cali_monthly_acres_plot)
dev.off()

#### San Francisco Smoke Event 2020-08-19 to 2020-10-07 ####
# Wildfire in Marin County, CA:
#   incident date created: 2020-08-18
#   incident date extinguished: 2020-10-02
#   incident acres burned: 4929
sanfran_fire = cali_data[(cali_data$incident_dateonly_created >= '2020-08-01') & (cali_data$incident_dateonly_created <= '2020-10-07') & (cali_data$incident_county == 'Marin'),]

# Filter wildfire dataframe for San Francisco Meropolitan Area
sanfran_msa_data = cali_data[(cali_data$incident_county == 'San Francisco' |
                                cali_data$incident_county == 'Alameda' |
                                cali_data$incident_county == 'Marin' |
                                cali_data$incident_county == 'Contra Costa' |
                                cali_data$incident_county == 'San Mateo'),]

# Filter individual outliers
sanfran_msa_data = sanfran_msa_data[(sanfran_msa_data$incident_latitude < 39 
                                     & sanfran_msa_data$incident_longitude < -121.5
                                     & sanfran_msa_data$incident_latitude > 37.1),]

# Plot the size of wildfires in the San Francisco MSA from 2013-2020
tiff("Presentation Plots/sanfran_msa_plot.tiff", units='in', width=10, height=6, res=300)
sanfran_msa_plot <- ggplot(sanfran_msa_data, aes(x=incident_dateonly_created, y=incident_acres_burned)) + 
  geom_point(aes(col=incident_county)) + 
  labs(subtitle="2013-2020", 
       y="Area Burned (acres)", 
       x="Date",
       color="County",
       title="Area Burned by Wildfires in the San Francisco MSA", 
       caption = "Source: Cal Fire (fire.ca.gov)") +
  theme(text=element_text(size=14), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

plot(sanfran_msa_plot)
dev.off()

# Load Map of US States
mapMSA <- readOGR(dsn="cb_2018_us_cbsa_5m", layer="cb_2018_us_cbsa_5m")  

# Fortify polygons
sanfran.msa.df = fortify(mapMSA[c(193),]) # County Outlines

# Plot the San Francisco MSA fires
tiff("Presentation Plots/sanfran_fire_map.tiff", units='in', width=10, height=6, res=300)
sanfran_fire_map <- ggplot() + 
  geom_point(data=sanfran_msa_data, aes(x=incident_longitude, y=incident_latitude, size=incident_acres_burned), color="red", alpha=0.2, pch=16) +
  geom_polygon(data=sanfran.msa.df, aes(long,lat,group=group), fill=NA, size=.5) +
  geom_path(data=sanfran.msa.df, aes(long,lat,group=group), color="black") +
  labs(title="Map of San Francisco MSA Wildfires",
       y="Latitude", 
       x="Longitude",
       size="Area Burned (acres)", 
       fill="Area Burned (acres)", 
       subtitle="2013-2020") +
  coord_fixed() + theme_minimal()

plot(sanfran_fire_map)
dev.off()
