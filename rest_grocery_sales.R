require('forecast')
library('ggplot2')
library('autoplotly')
library('plotly')

data <- read.csv("monthly.csv", header=T)


#### Restaurant and Drinking Places Seasonally Adjusted ########
arima_fit_rest_sa = auto.arima(data[,6],seasonal=TRUE,
                               max.p = 5,
                       max.q = 12,
                       max.P = 5,
                       max.Q = 5,
                       max.order = 12,
                       max.d = 5,
                       max.D = 2,
                       allowdrift = T,allowmean = T)

# Forecast for the next 10 time units
arima_forecast_rest_sa = forecast(arima_fit_rest_sa, h = 236)
write.table(arima_forecast_rest_sa, file="forecast_rest_sa.csv", sep=",")

# Plot forecasts
date <-data[,1]
plot(arima_forecast_rest_sa, 
         xlab="Date",
         ylab="Food Services and Drinking Places SA",
         title="Food Services and Drinking Places SA")

autoplot(arima_forecast_rest_sa, 
         xlab="Date",
         ylab="Food Services and Drinking Places SA",
         title="Food Services and Drinking Places SA")

autoplotly(arima_forecast_rest_sa,
     xlab="Date",
     ylab="Food Services and Drinking Places SA",
     color = 'Species',
     label = "TRUE") +
  ggplot2::ggtitle("Food Services and Drinking Places SA") +
  ggplot2::labs(y = "Million USD$", x = "Obs = Jan '92")


checkresiduals(arima_fit_rest_sa)

#autoplot(arima_fit_rest_sa)

#################

#### Restaurant and Drinking Places NOT Seasonally Adjusted ########
arima_fit_rest_nsa = auto.arima(data[,7],seasonal=FALSE,   max.p = 5,
                               max.q = 12,
                               max.P = 5,
                               max.Q = 5,
                               max.order = 12,
                               max.d = 5,
                               max.D = 2,
                               allowdrift = T,allowmean = T)

# Forecast for the next 10 time units
arima_forecast_rest_nsa = forecast(arima_fit_rest_nsa, h = 236)
write.table(arima_forecast_rest_nsa, file="forecast_rest_nsa.csv", sep=",")

# Plot forecasts
plot(arima_forecast_rest_nsa, 
     xlab="Date",
     ylab="Food Services and Drinking Places NSA",
     title="Food Services and Drinking Places NSA")

autoplot(arima_forecast_rest_nsa, 
         xlab="Date",
         ylab="Food Services and Drinking Places NSA",
         title="Food Services and Drinking Places NSA")

autoplotly(arima_forecast_rest_nsa, 
           xlab="Date",
           ylab="Food Services and Drinking Places NSA",
           color = 'Species',
           label = "TRUE") +
  ggplot2::ggtitle("Food Services and Drinking Places NSA") +
  ggplot2::labs(y = "Million USD$", x = "Obs = Jan '92")

checkresiduals(arima_fit_rest_nsa)

#autoplot(arima_fit_rest_nsa)
###-------------------------------------------------------------------------------#############

##########Grocery Stores Sales Seasonally Adjusted SA##########################

arima_fit_g_sa = auto.arima(data[,4],seasonal=TRUE,   max.p = 5,
                       max.q = 12,
                       max.P = 5,
                       max.Q = 5,
                       max.order = 12,
                       max.d = 5,
                       max.D = 2,
                       allowdrift = T,allowmean = T)

# Forecast for the next 10 time units
arima_forecast_g_sa = forecast(arima_fit_g_sa, h = 236)
write.table(arima_forecast_g_sa, file="forecast_grocery_sa.csv", sep=",")

# Plot forecasts
plot(arima_forecast_g_sa, 
         xlab="Date",
         ylab="Grocery Stores Sales SA")

autoplot(arima_forecast_g_sa, 
           xlab="Date",
           ylab="Grocery Stores Sales SA")

autoplotly(arima_forecast_g_sa, 
         xlab="Date",
         ylab="Grocery Stores Sales SA",
         color = 'Species',
         label = "TRUE") +
  ggplot2::ggtitle("Grocery Stores Sales SA") +
  ggplot2::labs(y = "Million USD$", x = "Obs = Jan '92")


checkresiduals(arima_fit_g_sa)

#autoplot(arima_fit_g_sa)

###############

###############Grocery Stores Sales NOT Seasonally Adjusted NSA##########################

arima_fit_g_nsa = auto.arima(data[,5], seasonal=FALSE,   max.p = 5,
                            max.q = 12,
                            max.P = 5,
                            max.Q = 5,
                            max.order = 12,
                            max.d = 5,
                            max.D = 2,
                            allowdrift = F,allowmean = F)

# Forecast for the next 10 time units
arima_forecast_g_nsa = forecast(arima_fit_g_nsa, h = 236)
write.table(arima_forecast_g_nsa, file="forecast_grocery_nsa.csv", sep=",")

# Plot forecasts
plot(arima_forecast_g_nsa, 
     xlab="Date",
     ylab="Grocery Stores Sales NSA")

autoplot(arima_forecast_g_nsa, 
         xlab="Date",
         ylab="Grocery Stores Sales NSA")

autoplotly(arima_forecast_g_nsa, 
           xlab="Date",
           ylab="Grocery Stores Sales NSA",
           color = 'Species',
           label = "TRUE") +
  ggplot2::ggtitle("Grocery Stores Sales NSA") +
  ggplot2::labs(y = "Million USD$", x = "Obs = Jan '92")


checkresiduals(arima_fit_g_nsa)

#autoplot(arima_fit_g_nsa)

#########################-------------------------------------------------------######################

##################Restaurant Employment SA#####################

arima_fit_restemp_sa = auto.arima(data[,8],seasonal=TRUE,   max.p = 5,
                       max.q = 12,
                       max.P = 5,
                       max.Q = 5,
                       max.order = 12,
                       max.d = 5,
                       max.D = 2,
                       allowdrift = T,allowmean = T)

# Forecast for the next 10 time units
arima_forecast_restemp_sa = forecast(arima_fit_restemp_sa, h = 236)
write.table(arima_forecast_restemp_sa, file="forecast_empl_rest_sa.csv", sep=",")

# Plot forecasts
plot(arima_forecast_restemp_sa, 
         xlab="Date",
         ylab="Restaurant Employment SA")

autoplot(arima_forecast_restemp_sa, 
           xlab="Date",
           ylab="Restaurant Employment SA")


autoplotly(arima_forecast_restemp_sa, 
     xlab="Date",
     ylab="Restaurant Employment",
     color = 'Species',
     label = "TRUE") +
  ggplot2::ggtitle("Restaurant Employment SA") +
  ggplot2::labs(y = "Million USD$", x = "Obs = Jan '92")

checkresiduals(arima_fit_restemp_sa)

#autoplot(arima_fit_restemp_sa)

#######################################

##################Restaurant Employment NOT Seasonally Adjusted NSA#####################

arima_fit_restemp_nsa = auto.arima(data[,9],seasonal=FALSE,   max.p = 5,
                                  max.q = 12,
                                  max.P = 5,
                                  max.Q = 5,
                                  max.order = 12,
                                  max.d = 5,
                                  max.D = 2,
                                  allowdrift = F,allowmean = F)

# Forecast for the next 10 time units
arima_forecast_restemp_nsa = forecast(arima_fit_restemp_nsa, h = 236)
write.table(arima_forecast_restemp_nsa, file="forecast_empl_rest_nsa.csv", sep=",")

# Plot forecasts
plot(arima_forecast_restemp_nsa, 
     xlab="Date",
     ylab="Restaurant Employment NSA")

autoplot(arima_forecast_restemp_nsa, 
         xlab="Date",
         ylab="Restaurant Employment NSA")


autoplotly(arima_forecast_restemp_nsa, 
           xlab="Date",
           ylab="Restaurant Employment NSA",
           color = 'Species',
           label = "TRUE") +
  ggplot2::ggtitle("Restaurant Employment NSA") +
  ggplot2::labs(y = "Million USD$", x = "Obs = Jan '92")

checkresiduals(arima_fit_restemp_nsa)

#autoplot(arima_fit_restemp_nsa)
