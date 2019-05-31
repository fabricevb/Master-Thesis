

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

# First 5 models
model1 <- lm(GDP_year ~ E_sa, data = data)
model2 <- lm(GDP_year ~ E_sa + Var_sa, data = data)
model3 <- lm(GDP_year ~ E_sa + Var_sa + Z_sa + Var_Z_sa, data = data)
model4 <- lm(GDP_year ~ E_sa + Var_sa + Z2_sa + Var_Z2_sa, data = data)
model5 <- lm(GDP_year ~ E_sa + Var_sa + Z3_sa + Var_Z3_sa, data = data)


modelfull <- lm(GDP_year ~ E_sa + Var_sa + Z_sa + Var_Z_sa + Z2_sa + Var_Z2_sa  + Z3_sa + Var_Z3_sa, data = data)

modelempty <- lm(GDP_year ~ 1, data=data)

# create summary table to compare the 5 different models
stargazer(model1, model2, model3, model4, model5, align = TRUE,
          intercept.bottom = FALSE,
          single.row = FALSE, 
          df = FALSE,
          covariate.labels = c("Constant","BSI", "Var(BSI)", "EIR1", "Var(EIR1)", "EIR2", "Var(EIR2)", "EIR3", "Var(EIR3)"),
          dep.var.caption  = "Linear Regression",
          dep.var.labels   = "Year on Year GDP")
AIC(model1, model2, model3, model4, model5)$AIC
BIC(model1, model2, model3, model4, model5)$BIC


autoplot(model3, colour = 'blue')

ggnostic(model3)


#######################
### MODEL EVALUATION  #
#######################

round(accuracy(model1), digits = 3)
round(accuracy(model2), digits = 3)
round(accuracy(model3), digits = 3)
round(accuracy(model4), digits = 3)
round(accuracy(model5), digits = 3)

### Diebold-Mariano Test
dm.test(residuals(model1), residuals(model2))
dm.test(residuals(model1), residuals(model3))
dm.test(residuals(model2), residuals(model3))


###########################
### model selection       #
###########################

# step procedure
stepMod <- step(modelempty, scope = list(lower = modelempty, upper = modelfull), direction = "forward", trace = 0, steps = 1000)  # perform step-wise algorithm
shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept 
shortlistedVars

step(modelfull, direction ="backward")

step(modelfull, direction ="both")



############################
### PLOT MODEL PREDICTIONS #
############################

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
levels(meltdf$variable) <- c("YoY GDP", "Model 1", "Model 2", "Model 3")
ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + 
  geom_line(na.rm=FALSE) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ylab("YoY GDP") + 
  theme_bw() + theme(axis.text.x = element_text(angle=45, hjust = 1))



##############################################
# out of sample ###
##############################################

# select data before 2000 (obs 145)
subset <- data[1:145,]

# fitting out of sample
modelsubset1 <- lm(GDP_year ~ E_sa, data = subset)
modelsubset2 <- lm(GDP_year ~ E_sa + Var_sa, data = subset)
modelsubset3 <- lm(GDP_year ~ E_sa + Var_sa + Z_sa + Var_Z_sa, data = subset)


stargazer(modelsubset1, modelsubset2, modelsubset3, align = TRUE,
          intercept.bottom = FALSE,
          single.row = FALSE, 
          df = FALSE,
          covariate.labels = c("Constant","BSI", "Var(BSI)", "EIR1", "Var(EIR1)"),
          dep.var.caption  = "Linear Regression",
          dep.var.labels   = "Year on Year GDP")
AIC(modelsubset1, modelsubset2, modelsubset3)$AIC
BIC(modelsubset1, modelsubset2, modelsubset3)$BIC

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
levels(meltdf$variable) <- c("YoY GDP", "Model 1", "Model 2", "Model 3")
ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + 
  geom_line(na.rm=FALSE) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ylab("YoY GDP") + 
  geom_vline(aes(xintercept=as.numeric(date[145])), colour="black", size=0.5) +
  theme_bw() + theme(axis.text.x = element_text(angle=45, hjust = 1))



# select data before 2012 (obs 288)
subset2 <- data[1:288,]

# fitting out of sample
modelsubset21 <- lm(GDP_year ~ E_sa, data = subset2)
modelsubset22 <- lm(GDP_year ~ E_sa + Var_sa, data = subset2)
modelsubset23 <- lm(GDP_year ~ E_sa + Var_sa + Z_sa + Var_Z_sa, data = subset2)

stargazer(modelsubset21, modelsubset22, modelsubset23, align = TRUE,
          intercept.bottom = FALSE,
          single.row = FALSE, 
          df = FALSE,
          covariate.labels = c("Constant","BSI", "Var(BSI)", "EIR1", "Var(EIR1)"),
          dep.var.caption  = "Linear Regression",
          dep.var.labels   = "Year on Year GDP")
AIC(modelsubset21, modelsubset22, modelsubset23)$AIC
BIC(modelsubset21, modelsubset22, modelsubset23)$BIC


fore21 <- forecast(modelsubset21, data)
fore22 <- forecast(modelsubset22, data)
fore23 <- forecast(modelsubset23, data)
data$fore21 <- fore21$mean 
data$fore22 <- fore22$mean 
data$fore23 <- fore23$mean 

data$date <- as.Date(data$period)
tmp <- data[c("date", "GDP_year_plot", "fore21", "fore22", "fore23")]
meltdf <- melt(tmp,id="date")
meltdf$GDP_year <- data$GDP_year
levels(meltdf$variable) <- c("YoY GDP", "Model 1", "Model 2", "Model 3")
ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + 
  geom_line(na.rm=FALSE) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ylab("YoY GDP") + 
  geom_vline(aes(xintercept=as.numeric(date[289])), colour="black", size=0.5) +
  theme_bw() + theme(axis.text.x = element_text(angle=45, hjust = 1))

