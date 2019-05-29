
# Podgorica54
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
library(ggfortify)
library(forecast)
library(GGally)
library(lattice)
library(survival)
library(Formula)
library(Hmisc)
library(forecast)


# upload data
#data <- read_excel("Master-Thesis/Datasets/data_sa.xlsx")

# create a lag variable for the indicator
# data$E_sa_lag1 <- Lag(data$E_sa, 1)

# fill NA's by linear method for YoY GDP, this will help to plot the data
data$Obs <- as.numeric(data$Obs)
data$GDP_year_plot <- with(data, interp1(Obs, GDP_year, Obs, "linear"))




#################
# Linear Models #
#################

# GDP YoY
model1 <- lm(GDP_year ~ E_sa, data = data)
model2 <- lm(GDP_year ~ E_sa + Var_sa, data = data)
model3 <- lm(GDP_year ~ E_sa + Var_sa + Z_sa + Var_Z_sa, data = data)
model4 <- lm(GDP_year ~ E_sa + Var_sa + Z2_sa + Var_Z2_sa, data = data)
model5 <- lm(GDP_year ~ E_sa + Var_sa + Z3_sa + Var_Z3_sa, data = data)


modelfull <- lm(GDP_year ~ E_sa + Var_sa + Z_sa + Var_Z_sa + Z2_sa + Var_Z2_sa  + Z3_sa + Var_Z3_sa, data = data)

modelempty <- lm(GDP_year ~ 1, data=data)

AIC(model1, model2, model3, model4, model5)$AIC

BIC(model1, model2, model3, model4, model5)$BIC

# create summary table to copare the 5 different models
stargazer(model1, model2, model3, model4, model5, align = TRUE,
          intercept.bottom = FALSE,
          single.row = FALSE, 
          df = FALSE,
          covariate.labels = c("Constant","BSI", "Var(BSI)", "EIR1", "Var(EIR1)", "EIR2", "Var(EIR2)", "EIR3", "Var(EIR3)"),
          dep.var.caption  = "Linear Regression",
          dep.var.labels   = "Year on Year GDP (in \\%)")

summary(modelfull)

autoplot(model3, colour = 'blue')

ggnostic(model3)



###########################
### model selection       #
###########################

# start with full model and take out non-signficant variables


# step procedure
step(modelfull, direction ="both")

step(modelfull, direction ="forward")

step(modelfull, direction ="back")

stepMod <- step(modelempty, scope = list(lower = modelempty, upper = modelfull), direction = "both", trace = 0, steps = 1000)  # perform step-wise algorithm
shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept 

shortlistedVars


########################################
### variable relative importance       #
########################################

library(Boruta)
library(mlbench)
# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(GDP_year ~ E_sa + Z_sa + Z2_sa + Z3_sa + Var_sa + Var_Z_sa + Var_Z2_sa + Var_Z3_sa, data=na.omit(data), doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance

# plotZHistory(boruta_output)


library(earth)
marsModel <- earth(GDP_year ~ E_sa + Z_sa + Z2_sa + Z3_sa + Var_sa + Var_Z_sa + Var_Z2_sa + Var_Z3_sa, data=na.omit(data)) # build model
ev <- evimp (marsModel) # estimate variable importance

ev

plot(ev)



library(party)
cf1 <- cforest(GDP_year ~ E_sa + Z_sa + Z2_sa + Z3_sa + Var_sa + Var_Z_sa + Var_Z2_sa + Var_Z3_sa, data=na.omit(data), control=cforest_unbiased(mtry=2,ntree=50)) # fit the random forest
tb <- sort(varimp(cf1)) # get variable importance, based on mean decrease in accuracy

stargazer(tb)


tb2 <- sort(varimp(cf1, conditional=TRUE))  # conditional=True, adjusts for correlations between predictors
stargazer(tb2)

library(relaimpo)
lmMod <- lm(GDP_year ~ E_sa + Z_sa + Z2_sa + Z3_sa + Var_sa + Var_Z_sa + Var_Z2_sa + Var_Z3_sa, data=na.omit(data))  # fit lm() model
relImportance <- calc.relimp(lmMod, type = "lmg", rela = TRUE)  # calculate relative importance scaled to 100
sort(relImportance$lmg, decreasing=TRUE)  # relative importance



######################### ALL INDICATORS

# plot different predictions
data$predicted_model1 <- predict(model1, data)
data$predicted_model2 <- predict(model2, data)
data$predicted_model3 <- predict(model3, data)
data$predicted_model4 <- predict(model4, data)
data$predicted_model5 <- predict(model5, data)

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


data$date <- as.Date(data$period)
tmp <- data[c("date", "GDP_year_plot", "Respondents", "Var", "Var_Z")]


meltdf <- melt(tmp,id="date")
meltdf$GDP_year <- data$GDP_year
ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + 
  geom_line(na.rm=FALSE) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  geom_point(aes(y = GDP_year))



scaleFactor <- max(data$Respondents) / max(data$Var)

ggplot(data, aes(x=period)) +
  geom_line(aes(y=Respondents), col="blue") +
  geom_line(aes(y=Var * scaleFactor), col="red") +
  scale_y_continuous(name="Respondents", sec.axis=sec_axis(~./scaleFactor, name="Variance"))


scaleFactor <- max(data$Respondents) / max(data$Var_Z)

ggplot(data, aes(x=period)) +
  geom_line(aes(y=Respondents), col="blue") +
  geom_line(aes(y=Var_Z * scaleFactor), col="red") +
  scale_y_continuous(name="Respondents", sec.axis=sec_axis(~./scaleFactor, name="Volatility"))


ggnostic(model1)




accuracy(model1)
accuracy(model2)
accuracy(model3)
accuracy(model4)
accuracy(model5)







# autocorrelation of the residuals
acf(model3$residuals)


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

##############################################

dm.test(model1$residuals, model3$residuals)


dm.test(model2$residuals, model4$residuals)

##############################################
##############################################
# out of sample 

# select data before 2000 (obs 145)
subset <- data[1:264,]

# fitting out of sample
modelsubset1 <- lm(GDP_year ~ E_sa, data = subset)
modelsubset2 <- lm(GDP_year ~ E_sa + Var_sa, data = subset)
modelsubset3 <- lm(GDP_year ~ E_sa + Var_sa + Z_sa + Var_Z_sa, data = subset)


stargazer(model1, model2, model3, modelsubset1, modelsubset2, modelsubset3, align = TRUE,
          intercept.bottom = FALSE,
          single.row = FALSE, 
          df = FALSE,
          covariate.labels = c("Constant","BSI", "Var(BSI)", "EIR1", "Var(EIR1)", "EIR2", "Var(EIR2)", "EIR3", "Var(EIR3)"),
          dep.var.caption  = "Linear Regression",
          dep.var.labels   = "Year on Year GDP (in \\%)")

fore1 <- forecast(modelsubset1, data)
fore2 <- forecast(modelsubset2, data)
fore3 <- forecast(modelsubset3, data)
data$fore1 <- fore1$mean 
data$fore2 <- fore2$mean 
data$fore3 <- fore3$mean 

data$date <- as.Date(data$period)
tmp <- data[c("date", "GDP_year_plot", "fore1", "fore2", "fore3")]
meltdf <- melt(tmp,id="date")
meltdf$GDP_year <- data$GDP_year
levels(meltdf$variable) <- c("period", "YoY GDP", "Predictions 1", "Predictions 2", "Predictions 3")
ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + 
  geom_line(na.rm=FALSE) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  geom_point(aes(y = GDP_year)) + theme_bw() +   theme(axis.text.x = element_text(angle=45, hjust = 1))

