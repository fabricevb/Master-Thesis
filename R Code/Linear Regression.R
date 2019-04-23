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
data <- read_excel("GitHub/Master-Thesis/Datasets/RS975_sa.xlsx")



#################
# Linear Models #
#################

# GDP YoY
model1 <- lm(GDP_year ~ E_I, data = data)
model2 <- lm(GDP_year ~ GDP_year_lag1 + E_I, data = data)
model3 <- lm(GDP_year ~ GDP_year_lag1 + E_I + Var_I, data = data)
model4 <- lm(GDP_year ~ E_I + Var_I, data = data)

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

