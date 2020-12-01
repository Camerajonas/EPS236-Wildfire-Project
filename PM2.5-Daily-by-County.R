#####################################################################
#File name: PM25-Daily-by-County.R
#Author: Jonas LaPier
#Date: December 2020
#Purpose: Load Pm2.5 data for the counties of interest
#####################################################################

setwd("~/Documents/School 20-21/Environmental Modeling/Wildfire-Smoke")

# Load Spokane County Data
WA.samples = read.csv("Data.nosync/WA_2007_ad_viz_plotval_data.csv")
for (yr in 2007:2020) {
  WA.samples = rbind(WA.samples, read.csv(paste0("Data.nosync/WA_",yr,"_ad_viz_plotval_data.csv")))
}
WA.samples$Date = as.POSIXct(WA.samples$Date, format="%m/%d/%Y")
spokane.samples = WA.samples[WA.samples$COUNTY=="Spokane",]

# Load Multnomah County
OR.samples = read.csv("Data.nosync/OR_2007_ad_viz_plotval_data.csv")
for (yr in 2007:2020) {
  OR.samples = rbind(OR.samples, read.csv(paste0("Data.nosync/OR_",yr,"_ad_viz_plotval_data.csv")))
}
OR.samples$Date = as.POSIXct(OR.samples$Date, format="%m/%d/%Y")
portland.samples = OR.samples[OR.samples$COUNTY=="Multnomah",]

# Load San Francisco Samples
CA.samples = read.csv("Data.nosync/CA_2007_ad_viz_plotval_data.csv")
for (yr in 2007:2020) {
  CA.samples = rbind(CA.samples, read.csv(paste0("Data.nosync/CA_",yr,"_ad_viz_plotval_data.csv")))
}
CA.samples$Date = as.POSIXct(CA.samples$Date, format="%m/%d/%Y")
sanfrancisco.samples = CA.samples[CA.samples$CBSA_NAME=="San Francisco",]
