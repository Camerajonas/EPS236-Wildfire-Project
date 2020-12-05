#####################################################################
#File name: PM25-Health-Analysis.R
#Author: Jonas LaPier
#Date: November 2020
#Purpose: Exploratory analysis of PM2.5 air quality data with health data
#####################################################################

# Attach Dependencies
library(extrafont)
library(ggplot2)
library(lmodel2)
library(MASS)
library(rgdal)
library(jsonlite)
setwd("~/Documents/School 20-21/Environmental Modeling/Wildfire-Smoke")

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
pt.sz=2.5
a = 0.5
ggplot(data=annual.county.PM.disease[annual.county.PM.disease$Mean<30,]) + #outliers removed
  geom_smooth(aes(x=Mean,y=Asthma,color="Asthma"), size=2, method="lm") +
  geom_smooth(aes(x=Mean,y=COPD,color="COPD"), size=2, method="lm") +
  geom_smooth(aes(x=Mean,y=Heart.Failure,color="Heart Failure"), size=2, method="lm") +
  geom_smooth(aes(x=Mean,y=Cancer,color="Cancer"), size=2, method="lm") +
  geom_smooth(aes(x=Mean,y=Hypertension,color="Hypertension"), size=2, method="lm") +
  geom_smooth(aes(x=Mean,y=Ischemic.Heart.Disease,color="Ischemic Heart Disease"), size=2, method="lm") +
  geom_point(aes(x=Mean,y=Asthma,color="Asthma"), size=pt.sz, alpha=a) +
  geom_point(aes(x=Mean,y=COPD,color="COPD"), size=pt.sz, alpha=a) +
  geom_point(aes(x=Mean,y=Cancer,color="Cancer"), size=pt.sz, alpha=a) +
  geom_point(aes(x=Mean,y=Heart.Failure,color="Heart Failure"), size=pt.sz, alpha=a) +
  geom_point(aes(x=Mean,y=Hypertension,color="Hypertension"), size=pt.sz, alpha=a) +
  geom_point(aes(x=Mean,y=Ischemic.Heart.Disease,color="Ischemic Heart Disease"), size=pt.sz, alpha=a) +
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
ggplot(data=output[output$aggregation.type=="Mean",], aes(x=disease, y=MA.slope)) +
  geom_bar(stat="identity", width=0.8, position=position_dodge(), fill="salmon") +
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
#### Wildfire Effect on Annual Mean PM2.5 ####

# Load and Combine Data sets
# Spokane
WA.samples = read.csv("Data.nosync/WA_2007_ad_viz_plotval_data.csv")
for (yr in 2007:2020) {
  WA.samples = rbind(WA.samples, read.csv(paste0("Data.nosync/WA_",yr,"_ad_viz_plotval_data.csv")))
}
WA.samples$Date = as.POSIXct(WA.samples$Date, format="%m/%d/%Y")
spokane.samples = WA.samples[WA.samples$CBSA_NAME=="Spokane-Spokane Valley, WA",]
spokane.samples$Site.ID = as.factor(spokane.samples$Site.ID)
spokane.daily = tapply(spokane.samples$Daily.Mean.PM2.5.Concentration,
                       spokane.samples$Date, mean) # Combine the Daily Data
spokane.daily = data.frame("Date"=as.POSIXct(names(spokane.daily)),
                           "Daily.Mean.PM2.5.Concentration"=spokane.daily)

# Portland
OR.samples = read.csv("Data.nosync/OR_2007_ad_viz_plotval_data.csv")
for (yr in 2007:2020) {
  OR.samples = rbind(OR.samples, read.csv(paste0("Data.nosync/OR_",yr,"_ad_viz_plotval_data.csv")))
}
OR.samples$Date = as.POSIXct(OR.samples$Date, format="%m/%d/%Y")
portland.samples = OR.samples[OR.samples$CBSA_NAME=="Portland-Vancouver-Hillsboro, OR-WA",]
portland.samples$Site.ID = as.factor(portland.samples$Site.ID)
portland.daily = tapply(portland.samples$Daily.Mean.PM2.5.Concentration,
                        portland.samples$Date, mean) # Combine the Daily Data
portland.daily = data.frame("Date"=as.POSIXct(names(portland.daily)),
                            "Daily.Mean.PM2.5.Concentration"=portland.daily)

# San Francisco
CA.samples = read.csv("Data.nosync/CA_2007_ad_viz_plotval_data.csv")
for (yr in 2007:2020) {
  CA.samples = rbind(CA.samples, read.csv(paste0("Data.nosync/CA_",yr,"_ad_viz_plotval_data.csv")))
}
CA.samples$Date = as.POSIXct(CA.samples$Date, format="%m/%d/%Y")
sanfrancisco.samples = CA.samples[CA.samples$CBSA_NAME=="San Francisco-Oakland-Hayward, CA",]
sanfrancisco.samples$Site.ID = as.factor(sanfrancisco.samples$Site.ID)
sanfrancisco.daily = tapply(sanfrancisco.samples$Daily.Mean.PM2.5.Concentration,
                            sanfrancisco.samples$Date, mean) # Combine the Daily Data
sanfrancisco.daily = data.frame("Date"=as.POSIXct(names(sanfrancisco.daily)),
                                "Daily.Mean.PM2.5.Concentration"=sanfrancisco.daily)

# Isolate Smoke Events
spokane.daily$high.smoke = spokane.daily$Daily.Mean.PM2.5.Concentration >
  quantile(spokane.daily$Daily.Mean.PM2.5.Concentration,p=0.98)
portland.daily$high.smoke = portland.daily$Daily.Mean.PM2.5.Concentration >
  quantile(portland.daily$Daily.Mean.PM2.5.Concentration,p=0.97)
sanfrancisco.daily$high.smoke = sanfrancisco.daily$Daily.Mean.PM2.5.Concentration >
  quantile(sanfrancisco.daily$Daily.Mean.PM2.5.Concentration,p=0.97)

# Plot Smoke Events Together
# Plot the Smoke Event
# Plot Daily Means with smoothed fits
colors = c("TRUE"="red","FALSE"="black")
p1 = ggplot() +
  geom_point(data=spokane.daily,
             aes(x=Date, y=Daily.Mean.PM2.5.Concentration, color=high.smoke),size=3,shape=16) +
  labs(title=paste0("Spokane", strrep(" ",55) ,"2020 Daily PM 2.5 Counts"), y="", color="Smoke Event") + 
  scale_x_continuous(breaks= seq(as.POSIXct("2020-01-01"),max(WA.samples$Date),by="month"), 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                "Sep","Oct","Nov"),
                     limits=c(as.POSIXct("2020-01-01"),max(WA.samples$Date))) +
  scale_color_manual(values=colors, name="Smoke Event") +
  theme(text=element_text(size=16,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))
p2 = ggplot() +
  geom_point(data=portland.daily,
             aes(x=Date, y=Daily.Mean.PM2.5.Concentration, color=high.smoke),size=3,shape=16) +
  labs(title="Portland",
       y="Mean PM 2.5 Concentration (ug/m^3)", color="Smoke Event") + 
  scale_x_continuous(breaks= seq(as.POSIXct("2020-01-01"),max(WA.samples$Date),by="month"), 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                "Sep","Oct","Nov"),
                     limits=c(as.POSIXct("2020-01-01"),max(WA.samples$Date))) +
  scale_color_manual(values=colors, name="Smoke Event") +
  theme(text=element_text(size=16,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))
p3 = ggplot() +
  geom_point(data=sanfrancisco.daily,
             aes(x=Date, y=Daily.Mean.PM2.5.Concentration, color=high.smoke),size=3,shape=16) +
  labs(title="San Francisco", x="Date",y="", color="Smoke Event") + 
  scale_x_continuous(breaks= seq(as.POSIXct("2020-01-01"),max(WA.samples$Date),by="month"), 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                "Sep","Oct","Nov"),
                     limits=c(as.POSIXct("2020-01-01"),max(WA.samples$Date))) +
  scale_color_manual(values=colors, name="Smoke Event") +
  theme(text=element_text(size=16,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

multiplot(p1,p2,p3)

# Check the Mean Annual Value w/ and w/out the smoke
cat(paste0(
  "Spokane Without Smoke: ", mean(spokane.daily$Daily.Mean.PM2.5.Concentration[!spokane.daily$high.smoke]), "\n",
  "Spokane With Smoke: ", mean(spokane.daily$Daily.Mean.PM2.5.Concentration), "\n",
  "Spokane Difference: ", mean(spokane.daily$Daily.Mean.PM2.5.Concentration)-
    mean(spokane.daily$Daily.Mean.PM2.5.Concentration[!spokane.daily$high.smoke]), "\n","\n",
  "Portland Without Smoke: ", mean(portland.daily$Daily.Mean.PM2.5.Concentration[!portland.daily$high.smoke]), "\n",
  "Portland With Smoke: ", mean(portland.daily$Daily.Mean.PM2.5.Concentration), "\n",
  "Portland Difference: ", mean(portland.daily$Daily.Mean.PM2.5.Concentration)-
    mean(portland.daily$Daily.Mean.PM2.5.Concentration[!portland.daily$high.smoke]), "\n","\n",
  "San Francisco Without Smoke: ", mean(sanfrancisco.daily$Daily.Mean.PM2.5.Concentration[!sanfrancisco.daily$high.smoke]), "\n",
  "San Francisco With Smoke: ", mean(sanfrancisco.daily$Daily.Mean.PM2.5.Concentration), "\n",
  "San Francisco Difference: ", mean(sanfrancisco.daily$Daily.Mean.PM2.5.Concentration)-
    mean(sanfrancisco.daily$Daily.Mean.PM2.5.Concentration[!sanfrancisco.daily$high.smoke]), "\n","\n"
))

# Translate the Differences into Health Outcome Correlations
# Need to Run Regular bootstrap analysis to get outcome matrix
PM.effect = data.frame("Mean.MA.slope"=rep(output$MA.slope[1:6],3),
                       "low.MA.slope"=rep(output$MA.slope.low[1:6],3),
                       "high.MA.slope"=rep(output$MA.slope.high[1:6],3),
                       "disease"=rep(output$disease[1:6],3),
                       "effect"=numeric(18),
                       "effect.low"=numeric(18),
                       "effect.high"=numeric(18))
PM.effect$effect[1:6] = PM.effect$Mean.MA.slope[1:6]*(mean(spokane.daily$Daily.Mean.PM2.5.Concentration)-
                                                        mean(spokane.daily$Daily.Mean.PM2.5.Concentration[!spokane.daily$high.smoke]))
PM.effect$effect.low[1:6] = PM.effect$low.MA.slope[1:6]*(mean(spokane.daily$Daily.Mean.PM2.5.Concentration)-
                                                           mean(spokane.daily$Daily.Mean.PM2.5.Concentration[!spokane.daily$high.smoke]))
PM.effect$effect.high[1:6] = PM.effect$high.MA.slope[1:6]*(mean(spokane.daily$Daily.Mean.PM2.5.Concentration)-
                                                             mean(spokane.daily$Daily.Mean.PM2.5.Concentration[!spokane.daily$high.smoke]))
PM.effect$city[1:6] = rep("Spokane",6)
PM.effect$effect[7:12] = PM.effect$Mean.MA.slope[7:12]*(mean(portland.daily$Daily.Mean.PM2.5.Concentration)-
                                                          mean(portland.daily$Daily.Mean.PM2.5.Concentration[!portland.daily$high.smoke]))
PM.effect$effect.low[7:12] = PM.effect$low.MA.slope[7:12]*(mean(portland.daily$Daily.Mean.PM2.5.Concentration)-
                                                             mean(portland.daily$Daily.Mean.PM2.5.Concentration[!portland.daily$high.smoke]))
PM.effect$effect.high[7:12] = PM.effect$high.MA.slope[7:12]*(mean(portland.daily$Daily.Mean.PM2.5.Concentration)-
                                                               mean(portland.daily$Daily.Mean.PM2.5.Concentration[!portland.daily$high.smoke]))
PM.effect$city[7:12] = rep("Portland",6)
PM.effect$effect[13:18] = PM.effect$Mean.MA.slope[13:18]*(mean(sanfrancisco.daily$Daily.Mean.PM2.5.Concentration)-
                                                            mean(sanfrancisco.daily$Daily.Mean.PM2.5.Concentration[!sanfrancisco.daily$high.smoke]))
PM.effect$effect.low[13:18] = PM.effect$low.MA.slope[13:18]*(mean(sanfrancisco.daily$Daily.Mean.PM2.5.Concentration)-
                                                               mean(sanfrancisco.daily$Daily.Mean.PM2.5.Concentration[!sanfrancisco.daily$high.smoke]))
PM.effect$effect.high[13:18] = PM.effect$high.MA.slope[13:18]*(mean(sanfrancisco.daily$Daily.Mean.PM2.5.Concentration)-
                                                                 mean(sanfrancisco.daily$Daily.Mean.PM2.5.Concentration[!sanfrancisco.daily$high.smoke]))
PM.effect$city[13:18] = rep("San Francisco",6)

# Plot the Effects
# Make the Barplot (MA)
ggplot(data=PM.effect, aes(x=disease, y=effect, fill=city)) +
  geom_bar(stat="identity", width=0.8, position=position_dodge()) +
  geom_errorbar(aes(ymin=effect.low, ymax=effect.high), width=.2,
                position=position_dodge(.8)) +
  labs(fill="City") +
  xlab("Disease") + ylab("Change in Prevalence") + 
  ggtitle("Estimated Impact of 2020 Wildfire Smoke based on MA Regression Slopes") +
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
