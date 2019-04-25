attach(data)

data$test = (- Ap_1 + Ap_2 + Ap_3 + Ap_4 + An_1 - An_2 - An_3 - An_4)/4
test









#################
# Linear Models #
#################



# load libraries
library(readxl)
library(shiny)
library(ggplot2)
library(GGally)
library(stargazer)
library(dplyr)
library(tidyr)
library(reshape2)
library(pracma)
library(scales)

# upload data
data <- read_excel("GitHub/Master-Thesis/Datasets/RS975_not_sa.xlsx")



#################
# Linear Models #
#################

# GDP YoY
model1 <- lm(GDP_year ~ E_I_sa, data = data)
model2 <- lm(GDP_year ~ E_I_sa + Var_I_sa, data = data)
model3 <- lm(GDP_year ~ E_I_sa + Var_I_sa + Z_I_sa, data = data)
model4 <- lm(GDP_year ~ E_I_sa + Var_I_sa + Z_I_sa, data = data)




stargazer(model1, model2, model3, model4, align = TRUE,
          intercept.bottom = FALSE,
          single.row = FALSE, 
          df = FALSE,
          covariate.labels = c("Constant","3 months lag of YoY GDP", "BSI", "Variance"),
          dep.var.caption  = "Linear Regression",
          dep.var.labels   = "Year on Year GDP (in \\%)")


# plot different predictions
data$predicted_model1 <- predict(model1, data)
data$predicted_model2 <- predict(model2, data)
data$predicted_model3 <- predict(model3, data)
data$predicted_model4 <- predict(model4, data)

# plot the different models
data$Obs = 1:nrow(data)
data$GDP_year_plot <- with(data, interp1(Obs, GDP_year, Obs, "linear"))
data$predicted_model2_plot <- with(data, interp1(Obs, predicted_model2, Obs, "linear"))
data$predicted_model3_plot <- with(data, interp1(Obs, predicted_model3, Obs, "linear"))
data$date <- as.Date(data$period)

tmp <- data[c("date", "GDP_year_plot", "predicted_model1", "predicted_model2_plot", 
                "predicted_model3_plot", "predicted_model4")]

meltdf <- melt(tmp,id="date")
meltdf$date <- as.Date(meltdf$date)
meltdf$GDP_year <- data$GDP_year
ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + 
  geom_line(na.rm=FALSE) + 
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  geom_point(aes(y = GDP_year)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")


# GDP YEAR  with difference previous month
model1 <- lm(GDP_year ~ E_I + E_I_diff, data = data)
model2 <- lm(GDP_year ~ E_I + Var_I, data = data)
model3 <- lm(GDP_year ~ E_I + E_I_diff + Var_I, data = data)
model4 <- lm(GDP_year ~ E_I + E_I_diff + Var_I + GDP_year_lag1, data = data)

stargazer(model1, model2, model3, model4, align = TRUE)

# plot different predictions
data$predicted_model1 <- predict(model1, data)
data$predicted_model2 <- predict(model2, data)
data$predicted_model3 <- predict(model3, data)
data$predicted_model4 <- predict(model4, data)

# plot the different models
data$Obs = 1:nrow(data)
data$GDP_year_plot <- with(data, interp1(Obs, GDP_year, Obs, "linear"))
data$predicted_model4 <- with(data, interp1(Obs, predicted_model4, Obs, "linear"))

tmp <- data[c("date", "GDP_year_plot", "predicted_model1", "predicted_model2", 
              "predicted_model3", "predicted_model4")]

meltdf <- melt(tmp,id="date")
meltdf$GDP_year <- data$GDP_year
ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + 
  geom_line(na.rm=FALSE) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  geom_point(aes(y = GDP_year))

autoplot(model4, colour = 'blue')

ggnostic(model3)

################################
# Linear Autoregressive Models #
################################

















######################### ALL INDICATORS

# GDP YEAR  with difference previous month
model1 <- lm(GDP_year ~ E_I + Var_I + Z_I + Var_Z_I, data = data)
model2 <- lm(GDP_year ~ E_I + Var_I + Z_I, data = data)
model3 <- lm(GDP_year ~ E_I + Z_I + Var_Z_I, data = data)

stargazer(model1, model2, model3, align = TRUE)
#, model2, model3, model4

# plot different predictions
data$predicted_model1 <- predict(model1, data)
data$predicted_model2 <- predict(model2, data)
data$predicted_model3 <- predict(model3, data)

# plot the different models
data$Obs = 1:nrow(data)

data$date <- as.Date(data$period)
tmp <- data[c("date", "GDP_year_plot", "predicted_model1", "predicted_model2", "predicted_model3")]


meltdf <- melt(tmp,id="date")
meltdf$GDP_year <- data$GDP_year
ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + 
  geom_line(na.rm=FALSE) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  geom_point(aes(y = GDP_year))


ggnostic(model1)


library(forecast)

accuracy(model1)
accuracy(model2)

fore <- forecast(model1, data)

plot(fore)











# autocorrelation of the residuals
acf(model2$residuals)


plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(model1$residuals)
fore <- forecast(model1, data)
plotForecastErrors(fore$residuals)


