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

# GDP
model1 <- lm(GDP_year ~ E_2, data = data)
model2 <- lm(GDP_year ~ GDP_year_lag1 + E_2, data = data)
model3 <- lm(GDP_year ~ GDP_year_lag1 + E_2 + Var_2, data = data)
model4 <- lm(GDP_year ~ E_2 + Var_2, data = data)

stargazer(model1, model2, model3, model4, align = TRUE)


# plot different predictions
data$predicted_model1 <- predict(model1, data)
data$predicted_model2 <- predict(model2, data)
data$predicted_model3 <- predict(model3, data)
data$predicted_model4 <- predict(model4, data)

ggplot(data, aes(x=period, y=predicted_model1)) +
    geom_line(colour = "blue") +
   geom_line(data=data[!is.na(data$GDP_year),], colour = "grey")
#  geom_line(data=data[!is.na(data$predicted_model1),], colour = "red")
#  geom_line(data=data[!is.na(data$predicted_model3),], colour = "blue") +
#  geom_line(data=data[!is.na(data$predicted_model4),], colour = "green")

  #  geom_line(data=data[data$predicted_model2)] +
#  geom_line(data=data$predicted_model3) +
#  geom_line(data=data$predicted_model4)


data$GDP_year_plot <- with(data, interp1(Obs, GDP_year, Obs, "linear"))
data$predicted_model2_plot <- with(data, interp1(Obs, predicted_model2, Obs, "linear"))
data$predicted_model3_plot <- with(data, interp1(Obs, predicted_model3, Obs, "linear"))



tmp <- data[c("date", "GDP_year_plot", "predicted_model1", "predicted_model2_plot", 
                "predicted_model3_plot", "predicted_model4")]


meltdf <- melt(tmp,id="date")
ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + 
  geom_line(na.rm=FALSE) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=45, hjust = 1))






# GDP YEAR  
model1 <- lm(GDP_year ~ E_2, data = data)
model2 <- lm(GDP_year ~ GDP_year_lag1 + E_2, data = data)
model3 <- lm(GDP_year ~ GDP_year_lag1 + E_2 + Var_2, data = data)
model4 <- lm(GDP_year ~ E_2 + Var_2, data = data)

stargazer(model1, model2, model3, model4, align = TRUE)

model1 <- glm(GDP_year ~ E_2 + E_2_diff, data = data)
model2 <- glm(GDP_year ~ GDP_year_lag1 + E_2 + E_2_diff, data = data)
model3 <- glm(GDP_year ~ GDP_year_lag1 + E_2 + E_2_diff + Var_2, data = data)
model4 <- glm(GDP_year ~ E_2 + E_2_diff + Var_2, data = data)

stargazer(model1, model2, model3, model4, align = TRUE)




model <- glm(GDP_year ~ E_2 + E_2_lag1 + E_2_lag2, data = data)
model1 <- glm(GDP_year ~ GDP_year_lag1 + E_2 + E_2_lag1, data = data)
model2 <- glm(GDP_year ~ GDP_year_lag1 + E_2 + E_2_lag1 + E_2_lag2, data = data)
model3 <- glm(GDP_year ~ GDP_year_lag1 + E_2 + E_2_lag1 + E_2_lag2, data = data)
model4 <- glm(GDP_year ~ GDP_year_lag1 + E_2 + E_2_lag1 + E_2_lag2 + Var_2 + Var_2_lag1 + Var_2_lag2, data = data)

stargazer(model1, model2, model4, align = TRUE)

# autoplot(model4, which = 1:6, ncol = 2, label.size = 3, colour = "steelblue") + theme_bw()

summary(model)



################################
# Linear Autoregressive Models #
################################

