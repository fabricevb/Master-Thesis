#################
# Linear Models #
#################



# load libraries
library(readxl)
library(shiny)
library(ggplot2)
library(GGally)

# upload data
data <- read_excel("GitHub/Master-Thesis/Datasets/RS975_sa.xlsx")



#################
# Linear Models #
#################

# GDP
model1 <- lm(GDP ~ E_2, data = data)
model2 <- lm(GDP ~ GDP_lag1 + E_2, data = data)
model3 <- lm(GDP ~ GDP_lag1 + E_2 + Var_2, data = data)
model4 <- lm(GDP ~ E_2 + Var_2, data = data)

stargazer(model1, model2, model3, model4, align = TRUE)


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

