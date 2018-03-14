##### R code for Time series analysis on Power forecasting in Telangana #####
##### Author: GLIM PGPBABIH Capstone Group 3                            #####
#############################################################################
# Load the required librry packages.
library('readxl')
require(graphics)
library('forecast')
library('timeSeries')
library('TTR')
library ("tseries")
library("ggplot2")

# Set the working directory to path where excel data is stored.
setwd("C:/GLIM/CP/Capstone Project")

# Read the data for the year 2014. 
# Telangana was formed in June 2014.So, data is available from that time only.
Jun14<- data.frame(read_excel("Consolidated 2014.xlsx",  sheet = 'Jun 14'))
Jul14<- data.frame(read_excel("Consolidated 2014.xlsx",  sheet = 'Jul 14'))
Aug14<- data.frame(read_excel("Consolidated 2014.xlsx",  sheet = 'Aug 14'))
Sep14<- data.frame(read_excel("Consolidated 2014.xlsx",  sheet = 'Sep 14'))
Oct14<- data.frame(read_excel("Consolidated 2014.xlsx",  sheet = 'Oct 14'))
Nov14<- data.frame(read_excel("Consolidated 2014.xlsx",  sheet = 'Nov 14'))
Dec14<- data.frame(read_excel("Consolidated 2014.xlsx",  sheet = 'Dec 14'))

# Read the data for the year 2015. 
Jan15<- data.frame(read_excel("Consolidated 2015.xlsx",  sheet = 'Jan 15'))
Feb15<- data.frame(read_excel("Consolidated 2015.xlsx",  sheet = 'Feb 15'))
Mar15<- data.frame(read_excel("Consolidated 2015.xlsx",  sheet = 'Mar 15'))
Apr15<- data.frame(read_excel("Consolidated 2015.xlsx",  sheet = 'Apr 15'))
May15<- data.frame(read_excel("Consolidated 2015.xlsx",  sheet = 'May 15'))
Jun15<- data.frame(read_excel("Consolidated 2015.xlsx",  sheet = 'Jun 15'))
Jul15<- data.frame(read_excel("Consolidated 2015.xlsx",  sheet = 'Jul 15'))
Aug15<- data.frame(read_excel("Consolidated 2015.xlsx",  sheet = 'Aug 15'))
Sep15<- data.frame(read_excel("Consolidated 2015.xlsx",  sheet = 'Sep 15'))
Oct15<- data.frame(read_excel("Consolidated 2015.xlsx",  sheet = 'Oct 15'))
Nov15<- data.frame(read_excel("Consolidated 2015.xlsx",  sheet = 'Nov 15'))
Dec15<- data.frame(read_excel("Consolidated 2015.xlsx",  sheet = 'Dec 15'))

# Read the data for the year 2016. 
Jan16<- data.frame(read_excel("Consolidated 2016.xlsx",  sheet = 'Jan 16'))
Feb16<- data.frame(read_excel("Consolidated 2016.xlsx",  sheet = 'Feb 16'))
Mar16<- data.frame(read_excel("Consolidated 2016.xlsx",  sheet = 'Mar 16'))
Apr16<- data.frame(read_excel("Consolidated 2016.xlsx",  sheet = 'Apr 16'))
May16<- data.frame(read_excel("Consolidated 2016.xlsx",  sheet = 'May 16'))
Jun16<- data.frame(read_excel("Consolidated 2016.xlsx",  sheet = 'Jun 16'))
Jul16<- data.frame(read_excel("Consolidated 2016.xlsx",  sheet = 'Jul 16'))
Aug16<- data.frame(read_excel("Consolidated 2016.xlsx",  sheet = 'Aug 16'))
Sep16<- data.frame(read_excel("Consolidated 2016.xlsx",  sheet = 'Sep 16'))
Oct16<- data.frame(read_excel("Consolidated 2016.xlsx",  sheet = 'Oct 16'))
Nov16<- data.frame(read_excel("Consolidated 2016.xlsx",  sheet = 'Nov 16'))
Dec16<- data.frame(read_excel("Consolidated 2016.xlsx",  sheet = 'Dec 16'))

# Read the data for the year 2017. Data available till June 2017 only. 
Jan17<- data.frame(read_excel("Consolidated 2017.xlsx",  sheet = 'Jan 17'))
Feb17<- data.frame(read_excel("Consolidated 2017.xlsx",  sheet = 'Feb 17'))
Mar17<- data.frame(read_excel("Consolidated 2017.xlsx",  sheet = 'Mar 17'))
Apr17<- data.frame(read_excel("Consolidated 2017.xlsx",  sheet = 'Apr 17'))
May17<- data.frame(read_excel("Consolidated 2017.xlsx",  sheet = 'May 17'))
#Jun17<- data.frame(read_excel("Consolidated 2017.xlsx",  sheet = 'Jun 17'))
#Jul17<- data.frame(read_excel("Consolidated 2017.xlsx",  sheet = 'Jul 17'))
#Aug17<- data.frame(read_excel("Consolidated 2017.xlsx",  sheet = 'Aug 17'))
#Sep17<- data.frame(read_excel("Consolidated 2017.xlsx",  sheet = 'Sep 17'))
#Oct17<- data.frame(read_excel("Consolidated 2017.xlsx",  sheet = 'Oct 17'))
#Nov17<- data.frame(read_excel("Consolidated 2017.xlsx",  sheet = 'Nov 17'))
#Dec17<- data.frame(read_excel("Consolidated 2017.xlsx",  sheet = 'Dec 17'))

#############################################################################
# Consolidated data for Telangana State for the period June 2014 - May 2017.
Telangana_Power <- rbind(Jun14[c(2),],Jul14[c(2),],Aug14[c(2),],Sep14[c(2),],
                         Oct14[c(2),],Nov14[c(2),],Dec14[c(2),],
                         Jan15[c(2),],Feb15[c(2),],Mar15[c(2),],Apr15[c(2),],  
                         May15[c(2),],Jun15[c(2),],Jul15[c(2),],Aug15[c(2),],
                         Sep15[c(2),],Oct15[c(2),],Nov15[c(2),],Dec15[c(2),],
                         Jan16[c(2),],Feb16[c(2),],Mar16[c(2),],Apr16[c(2),],
                         May16[c(2),],Jun16[c(2),],Jul16[c(2),],Aug16[c(2),],
                         Sep16[c(2),],Oct16[c(2),],Nov16[c(2),],Dec16[c(2),],
                         Jan17[c(2),],Feb17[c(2),],Mar17[c(2),],Apr17[c(2),],
                         May17[c(2),])

# Create Time series object for power requirement in Telangana
TS_Power_Req <- ts(Telangana_Power$Requirement..MU., start = c(2014, 6), frequency = 12)

# Graph for Power requirement trend in Telangana since June 2014
plot(TS_Power_Req)

# Visualize the seasonality  and trends in Power requirement for Telangana
#TS_Power_STL <- stl(TS_Power_Req,s.window = 12)
#plot(TS_Power_STL)

#########################
TS_Power_STL <- decompose(TS_Power_Req) 
TS_Power_STL$seasonal
plot(TS_Power_STL, col = "steelblue")

#TS_Power_STL$seasonal

TS_Power_Seasonally_adjusted <- TS_Power_Req - TS_Power_STL$seasonal
plot(TS_Power_Seasonally_adjusted)

adf.test(TS_Power_Req, alternative = "stationary")

#TS_Power_Seasonally_adjusted_Forcast <- HoltWinters(TS_Power_Seasonally_adjusted,
#                                                    gamma = FALSE,
#                                                    l.start = 4399.550,
#                                                    b.start = 55.645)
#TS_Power_Seasonally_adjusted_Forcast
#TS_Power_Seasonally_adjusted_Forcast$SSE

#plot(TS_Power_Seasonally_adjusted_Forcast)

TS_Power_Req_Forecast <- HoltWinters(TS_Power_Req)
TS_Power_Req_Forecast
TS_Power_Req_Forecast$SSE
plot(TS_Power_Req_Forecast)

# Forecasting the Power demand using Holt-Winter methodology
#TS_Power_Forecast <- HoltWinters(TS_Power_Req, alpha = 0.9, 
#                                 beta =  0.4, gamma = 0.1,
#                                 seasonal = "mult") 
# Visualize the forecasting results
#plot(TS_Power_Forecast)

# Predicting the demand for next 6 months based on Holt-Winter methodology.
TS_Power_Demand_Future <- forecast(TS_Power_Req_Forecast, 
                                   h = 6, 
                                   level = c(85,0.95))
# Future demand table
TS_Power_Demand_Future
# visualizing future demand
#plot(TS_Power_Demand_Future)


plot(TS_Power_Demand_Future,ylab="Power Demand in Telangana (MU)",
     plot.conf=FALSE, type="o", fcol="white", xlab="Year")
lines(fitted(TS_Power_Demand_Future), col="red", lty=2)
lines(TS_Power_Demand_Future$mean, type="o", col="red")
legend("topleft",lty=1, pch=0.5, col=1:3, 
       c("TS Power Requirement Original Data","Holt Winters' Methodology Additive"))


acf(TS_Power_Demand_Future$residuals, lag.max=20, na.action = na.contiguous)

Box.test(TS_Power_Demand_Future$residuals, lag=20, type="Ljung-Box")


plot.ts(TS_Power_Demand_Future$residuals)            # make a time plot
#plotForecastErrors(TS_Power_Demand_Future$residuals) # make a histogram





accuracy(TS_Power_Demand_Future)

#ARIMA
auto.arima(TS_Power_Req,D=1,stepwise=FALSE,approximation=FALSE)
TS_Power_Demand_Arima <- arima(TS_Power_Req, order = c(1,1,0), seasonal = c(0,1,0))
TS_Power_Demand_Arima

TS_Power_Demand_Future_Arima <- forecast(TS_Power_Demand_Arima, h=6, level=c(85,95))
TS_Power_Demand_Future_Arima

accuracy(TS_Power_Demand_Future_Arima)

plot(TS_Power_Demand_Future_Arima)
#########################

######################################################################################
## Below code is for Power forecasting in South India
## Data is used for the period June 2014 till May 2017 for comparison

Power_Demand_South_India <- rbind(Jun14[c(8),],Jul14[c(8),],Aug14[c(8),],Sep14[c(8),],
                            Oct14[c(8),],Nov14[c(8),],Dec14[c(8),],
                            Jan15[c(8),],Feb15[c(8),],Mar15[c(8),],Apr15[c(8),],
                            May15[c(8),],Jun15[c(8),],Jul15[c(8),],Aug15[c(8),],
                            Sep15[c(8),],Oct15[c(8),],Nov15[c(8),],Dec15[c(8),],
                            Jan16[c(8),],Feb16[c(8),],Mar16[c(8),],Apr16[c(8),],
                            May16[c(8),],Jun16[c(8),],Jul16[c(8),],Aug16[c(8),],
                            Sep16[c(8),],Oct16[c(8),],Nov16[c(8),],Dec16[c(8),],
                            Jan17[c(8),],Feb17[c(8),],Mar17[c(8),],Apr17[c(8),],
                            May17[c(8),])

# Create Time series object for power requirement in South India
Power_Req_South_India <- ts(Power_Demand_South_India$Requirement..MU., start = c(2014, 6), frequency = 12)

# Graph for Power requirement trend in South India since June 2014
plot(Power_Req_South_India)

# Visualize the seasonality  and trends in Power requirement for Telangana
SI_Power_STL <- stl(Power_Req_South_India,s.window = 12)
plot(SI_Power_STL)

# Forecasting the Power demand using Holt-Winter methodology
SI_Power_Forecast <- HoltWinters(Power_Req_South_India, alpha = 0.9, 
                                 beta =  0.4, gamma = 0.1,
                                 seasonal = "mult") 
# Visualize the forecasting results
plot(SI_Power_Forecast)

# Predicting the demand for next 6 months based on Holt-Winter methodology.
SI_Power_Demand_Future <- forecast.HoltWinters(SI_Power_Forecast, 
                                               h = 6, 
                                               level = c(85,0.95))
# Future demand table
SI_Power_Demand_Future
# visualizing future demand
plot(SI_Power_Demand_Future)

#########
## Below code is for Power forecasting in All India
## Data is used for the period June 2014 till May 2017 for comparison

Power_Demand_All_India <- rbind(Jun14[c(9),],Jul14[c(9),],Aug14[c(9),],Sep14[c(9),],
                                  Oct14[c(9),],Nov14[c(9),],Dec14[c(9),],
                                  Jan15[c(9),],Feb15[c(9),],Mar15[c(9),],Apr15[c(9),],
                                  May15[c(9),],Jun15[c(9),],Jul15[c(9),],Aug15[c(9),],
                                  Sep15[c(9),],Oct15[c(9),],Nov15[c(9),],Dec15[c(9),],
                                  Jan16[c(9),],Feb16[c(9),],Mar16[c(9),],Apr16[c(9),],
                                  May16[c(9),],Jun16[c(9),],Jul16[c(9),],Aug16[c(9),],
                                  Sep16[c(9),],Oct16[c(9),],Nov16[c(9),],Dec16[c(9),],
                                  Jan17[c(9),],Feb17[c(9),],Mar17[c(9),],Apr17[c(9),],
                                  May17[c(9),])

# Create Time series object for power requirement in South India
Power_Req_All_India <- ts(Power_Demand_All_India$Requirement..MU., start = c(2014, 6), frequency = 12)

# Graph for Power requirement trend in South India since June 2014
plot(Power_Req_All_India)

# Visualize the seasonality  and trends in Power requirement for Telangana
AI_Power_STL <- stl(Power_Req_All_India,s.window = 12)
plot(AI_Power_STL)

# Forecasting the Power demand using Holt-Winter methodology
AI_Power_Forecast <- HoltWinters(Power_Req_All_India, alpha = 0.9, 
                                 beta =  0.4, gamma = 0.1,
                                 seasonal = "mult") 
# Visualize the forecasting results
plot(AI_Power_Forecast)

# Predicting the demand for next 6 months based on Holt-Winter methodology.
AI_Power_Demand_Future <- forecast.HoltWinters(AI_Power_Forecast, 
                                               h = 6, 
                                               level = c(85,0.95))
# Future demand table
AI_Power_Demand_Future
# visualizing future demand
plot(AI_Power_Demand_Future)
