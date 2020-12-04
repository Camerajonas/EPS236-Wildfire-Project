#####################################################################
#File name: COVID-Analysis.R
#Author: Blake Bullwinkel
#Date: November 2020
#Purpose: Exploratory analysis of COVID-19 and PM data in CA, WA, OR
#####################################################################

# Load dependencies
library(lubridate)
library(ggplot2)

#### California COVID/PM Analysis ####
# Read in the COVID-19 data
covid_data = read.csv('covid-counties.csv')

# Filter state for California
ca_covid_data = covid_data[(covid_data$state=='California'),]

# Convert the dates to POSIXct objects
ca_covid_data$date = ymd(ca_covid_data$date)

# Calculate the daily new COVID cases in California by county
ca_covid_data = transform(ca_covid_data, new_cases = ave(cases, county, FUN=function(x) x - c(0, head(x, -1))))

# Filter county for San Francisco
sanfran_covid_data = ca_covid_data[(ca_covid_data$county=='San Francisco'),]

# Plot the daily new cases in San Francisco
sanfran_covid_plot = ggplot(sanfran_covid_data, aes(x=date, y=new_cases)) +
  geom_bar(stat='identity', fill='steelblue') +
  labs(x='Date', 
       y='New Cases', 
       title='Daily COVID-19 Cases in San Francisco',
       caption='Source: New York Times (github.com/nytimes/covid-19-data)') +
  theme(text=element_text(size=14), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

plot(sanfran_covid_plot)

# San Francisco STL decomposition
sanfran_covid_ts = ts(sanfran_covid_data$new_cases, frequency = 7)
sanfran_covid_stl = stl(sanfran_covid_ts, s.window = 7)
sanfran_covid_decomposition = data.frame("Date"=as.Date(sanfran_covid_data$date),
                                         "All Cases"=as.numeric(sanfran_covid_data$new_cases),
                                         "Seasonal"=as.numeric(sanfran_covid_stl$time.series[,1]),
                                         "Trend"=as.numeric(sanfran_covid_stl$time.series[,2]),
                                         "Remainder"=as.numeric(sanfran_covid_stl$time.series[,3]))
sanfran_remainders = data.frame("Date"=as.Date(sanfran_covid_data$date),
                                "Daily Cases"=as.numeric(sanfran_covid_stl$time.series[,3]))

# Plot the San Francisco STL decomposition
tiff("Presentation Plots/sanfran_covid_decomposition.tiff", units='in', width=10, height=6, res=300)
ggplot(data=sanfran_covid_decomposition) +
  geom_line(aes(x=Date,y=All.Cases, color='All Cases'), size=1.3, alpha=0.5) +
  geom_line(aes(x=Date,y=Trend, color='Trend'), size=1.3, alpha=0.7) +
  geom_line(aes(x=Date,y=Remainder, color='Remainder'), size=1.3) +
  labs(x='Date',
       y='COVID-19 Cases',
       title='Trend Decomposition of COVID-19 Cases in San Francisco',
       color='',
       caption = 'Source: New York Times (github.com/nytimes/covid-19-data)') +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))
dev.off()

# Read in the daily PM2.5 data
ca_pm = read.csv('CA_2020_ad_viz_plotval_data.csv')

# Convert date fields to POSIXct objects
ca_pm$Date = mdy(ca_pm$Date)
colnames(ca_pm)[18] = 'county'

# Get the San Francisco PM2.5 data
sanfran_pm = ca_pm[(ca_pm$county=='San Francisco'),]

# Merge the San Francisco remainders on the PM2.5 data
sanfran_daily_covid_pm = merge(sanfran_remainders, sanfran_pm, by='Date')

# Create a scatterplot of Daily PM vs COVID remainder in San Francisco
tiff("Presentation Plots/sanfran_daily_covid_pm.tiff", units='in', width=10, height=6, res=300)
ggplot(data=sanfran_daily_covid_pm) +
  geom_point(aes(x=Daily.Mean.PM2.5.Concentration,y=Daily.Cases), size=1.3, color='steelblue') +
  labs(x='Daily Mean PM2.5 Concentration',
       y='Daily COVID-19 Cases',
       title='Daily PM Concentration vs COVID-19 Cases in San Francisco') +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))
dev.off()


#### Washington COVID/PM Analysis ####

# Filter state for Washington
wa_covid_data = covid_data[(covid_data$state=='Washington'),]

# Convert the dates to POSIXct objects
wa_covid_data$date = ymd(wa_covid_data$date)

# Calculate the daily new COVID cases in Washington by county
wa_covid_data = transform(wa_covid_data, new_cases = ave(cases, county, FUN=function(x) x - c(0, head(x, -1))))

# Filter county for Spokane
spokane_covid_data = wa_covid_data[(wa_covid_data$county=='Spokane'),]

# Plot the daily new cases in Spokane
spokane_covid_plot = ggplot(spokane_covid_data, aes(x=date, y=new_cases)) +
  geom_bar(stat='identity', fill='steelblue') +
  labs(x='Date', 
       y='New Cases', 
       title='Daily COVID-19 Cases in Spokane, WA',
       caption='Source: New York Times (github.com/nytimes/covid-19-data)') +
  theme(text=element_text(size=14), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

plot(spokane_covid_plot)

# Spokane STL decomposition
spokane_covid_ts = ts(spokane_covid_data$new_cases, frequency = 7)
spokane_covid_stl = stl(spokane_covid_ts, s.window = 7)
spokane_covid_decomposition = data.frame("Date"=as.Date(spokane_covid_data$date),
                                         "All Cases"=as.numeric(spokane_covid_data$new_cases),
                                         "Seasonal"=as.numeric(spokane_covid_stl$time.series[,1]),
                                         "Trend"=as.numeric(spokane_covid_stl$time.series[,2]),
                                         "Remainder"=as.numeric(spokane_covid_stl$time.series[,3]))
spokane_remainders = data.frame("Date"=as.Date(spokane_covid_data$date),
                                "Daily Cases"=as.numeric(spokane_covid_stl$time.series[,3]))

# Plot the Spokane STL decomposition
tiff("Presentation Plots/spokane_covid_decomposition.tiff", units='in', width=10, height=6, res=300)
ggplot(data=spokane_covid_decomposition) +
  geom_line(aes(x=Date,y=All.Cases, color='All Cases'), size=1.3, alpha=0.5) +
  geom_line(aes(x=Date,y=Trend, color='Trend'), size=1.3, alpha=0.7) +
  geom_line(aes(x=Date,y=Remainder, color='Remainder'), size=1.3) +
  labs(x='Date',
       y='COVID-19 Cases',
       title='Trend Decomposition of COVID-19 Cases in Spokane, WA',
       color='',
       caption = 'Source: New York Times (github.com/nytimes/covid-19-data)') +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))
dev.off()

# Read in the daily PM2.5 data
wa_pm = read.csv('WA_2020_ad_viz_plotval_data.csv')

# Convert date fields to POSIXct objects
wa_pm$Date = mdy(wa_pm$Date)
colnames(wa_pm)[18] = 'county'

# Get the Spokane PM2.5 data
spokane_pm = wa_pm[(wa_pm$county=='Spokane'),]

# Merge the Spokane remainders on the PM2.5 data
spokane_daily_covid_pm = merge(spokane_remainders, spokane_pm, by='Date')

# Create a scatterplot of Daily PM vs COVID remainder in Spokane
tiff("Presentation Plots/spokane_daily_covid_pm.tiff", units='in', width=10, height=6, res=300)
ggplot(data=spokane_daily_covid_pm) +
  geom_point(aes(x=Daily.Mean.PM2.5.Concentration,y=Daily.Cases), size=1.3, color='steelblue') +
  labs(x='Daily Mean PM2.5 Concentration',
       y='Daily COVID-19 Cases',
       title='Daily PM Concentration vs COVID-19 Cases in Spokane, WA') +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))
dev.off()

#### Oregon COVID/PM Analysis ####

# Filter state for Oregon
or_covid_data = covid_data[(covid_data$state=='Oregon'),]

# Convert the dates to POSIXct objects
or_covid_data$date = ymd(or_covid_data$date)

# Calculate the daily new COVID cases in Oregon by county
or_covid_data = transform(or_covid_data, new_cases = ave(cases, county, FUN=function(x) x - c(0, head(x, -1))))

# Filter county for Multnomah
multnomah_covid_data = or_covid_data[(or_covid_data$county=='Multnomah'),]

# Plot the daily new cases in Multnomah
multnomah_covid_plot = ggplot(multnomah_covid_data, aes(x=date, y=new_cases)) +
  geom_bar(stat='identity', fill='steelblue') +
  labs(x='Date', 
       y='New Cases', 
       title='Daily COVID-19 Cases in Multnomah, OR',
       caption='Source: New York Times (github.com/nytimes/covid-19-data)') +
  theme(text=element_text(size=14), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

plot(multnomah_covid_plot)

# Multnomah STL decomposition
multnomah_covid_ts = ts(multnomah_covid_data$new_cases, frequency = 7)
multnomah_covid_stl = stl(multnomah_covid_ts, s.window = 7)
multnomah_covid_decomposition = data.frame("Date"=as.Date(multnomah_covid_data$date),
                                         "All Cases"=as.numeric(multnomah_covid_data$new_cases),
                                         "Seasonal"=as.numeric(multnomah_covid_stl$time.series[,1]),
                                         "Trend"=as.numeric(multnomah_covid_stl$time.series[,2]),
                                         "Remainder"=as.numeric(multnomah_covid_stl$time.series[,3]))
multnomah_remainders = data.frame("Date"=as.Date(multnomah_covid_data$date),
                                "Daily Cases"=as.numeric(multnomah_covid_stl$time.series[,3]))

# Plot the Multnomah STL decomposition
tiff("Presentation Plots/multnomah_covid_decomposition.tiff", units='in', width=10, height=6, res=300)
ggplot(data=multnomah_covid_decomposition) +
  geom_line(aes(x=Date,y=All.Cases, color='All Cases'), size=1.3, alpha=0.5) +
  geom_line(aes(x=Date,y=Trend, color='Trend'), size=1.3, alpha=0.7) +
  geom_line(aes(x=Date,y=Remainder, color='Remainder'), size=1.3) +
  labs(x='Date',
       y='COVID-19 Cases',
       title='Trend Decomposition of COVID-19 Cases in Multnomah, OR',
       color='',
       caption = 'Source: New York Times (github.com/nytimes/covid-19-data)') +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))
dev.off()

# Read in the daily PM2.5 data
or_pm = read.csv('OR_2020_ad_viz_plotval_data.csv')

# Convert date fields to POSIXct objects
or_pm$Date = mdy(or_pm$Date)
colnames(or_pm)[18] = 'county'

# Get the Multnomah PM2.5 data
multnomah_pm = or_pm[(or_pm$county=='Multnomah'),]

# Merge the Multnomah remainders on the PM2.5 data
multnomah_daily_covid_pm = merge(multnomah_remainders, multnomah_pm, by='Date')

# Create a scatterplot of Daily PM vs COVID remainder in Multnomah
tiff("Presentation Plots/multnomah_daily_covid_pm.tiff", units='in', width=10, height=6, res=300)
ggplot(data=multnomah_daily_covid_pm) +
  geom_point(aes(x=Daily.Mean.PM2.5.Concentration,y=Daily.Cases), size=1.3, color='steelblue') +
  labs(x='Daily Mean PM2.5 Concentration',
       y='Daily COVID-19 Cases',
       title='Daily PM Concentration vs COVID-19 Cases in Multnomah, OR') +
  theme(text=element_text(size=14,  family="Arial Narrow"), 
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))
dev.off()
