




#########################################
# Create lags for simplicity in dataset #
#########################################

attach(data)

library(Hmisc)
library(xtable)
library(stargazer)
library(xlsx)


data <- read_excel("GitHub/Master-Thesis/Datasets/RS975_sa.xlsx")

# add date formated column
data$date <- as.Date(period)

# add lags for E(x), Var(x), E(z) and Var(z)
data$E_1_lag1 <- Lag(E_1, -1)
data$E_1_lag2 <- Lag(E_1, -2)
data$E_1_lag3 <- Lag(E_1, -3)
data$E_1_lag4 <- Lag(E_1, -4)

data$E_2_lag1 <- Lag(E_2, -1)
data$E_2_lag2 <- Lag(E_2, -2)
data$E_2_lag3 <- Lag(E_2, -3)
data$E_2_lag4 <- Lag(E_2, -4)

data$E_3_lag1 <- Lag(E_3, -1)
data$E_3_lag2 <- Lag(E_3, -2)
data$E_3_lag3 <- Lag(E_3, -3)
data$E_3_lag4 <- Lag(E_3, -4)

data$E_4_lag1 <- Lag(E_4, -1)
data$E_4_lag2 <- Lag(E_4, -2)
data$E_4_lag3 <- Lag(E_4, -3)
data$E_4_lag4 <- Lag(E_4, -4)

data$Var_1_lag1 <- Lag(Var_1, -1)
data$Var_1_lag2 <- Lag(Var_1, -2)
data$Var_1_lag3 <- Lag(Var_1, -3)
data$Var_1_lag4 <- Lag(Var_1, -4)

data$Var_2_lag1 <- Lag(Var_2, -1)
data$Var_2_lag2 <- Lag(Var_2, -2)
data$Var_2_lag3 <- Lag(Var_2, -3)
data$Var_2_lag4 <- Lag(Var_2, -4)

data$Var_3_lag1 <- Lag(Var_3, -1)
data$Var_3_lag2 <- Lag(Var_3, -2)
data$Var_3_lag3 <- Lag(Var_3, -3)
data$Var_3_lag4 <- Lag(Var_3, -4)

data$Var_4_lag1 <- Lag(Var_4, -1)
data$Var_4_lag2 <- Lag(Var_4, -2)
data$Var_4_lag3 <- Lag(Var_4, -3)
data$Var_4_lag4 <- Lag(Var_4, -4)


# add lags for GDP and YoY GDP
data$GDP_year_lag12 <- Lag(GDP_year, -1)
data$GDP_year_lag13 <- Lag(GDP_year, -2)
data$GDP_year_lag1 <- Lag(GDP_year, -3)
data$GDP_year_lag2 <- Lag(GDP_year, -6)
data$GDP_year_lag3 <- Lag(GDP_year, -9)
data$GDP_year_lag4 <- Lag(GDP_year, -12)

data$GDP_lag1 <- Lag(GDP, -3)
data$GDP_lag2 <- Lag(GDP, -6)
data$GDP_lag3 <- Lag(GDP, -9)
data$GDP_lag4 <- Lag(GDP, -12)


### add difference with previous month

# Indicator 
data$E_1_diff <- data$E_1 - data$E_1_lag1
data$E_2_diff <- data$E_2 - data$E_2_lag1
data$E_3_diff <- data$E_3 - data$E_3_lag1
data$E_4_diff <- data$E_4 - data$E_4_lag1

# Variance
data$Var_1_diff <- data$Var_1 - data$Var_1_lag1
data$Var_2_diff <- data$Var_2 - data$Var_2_lag1
data$Var_3_diff <- data$Var_3 - data$Var_3_lag1
data$Var_4_diff <- data$Var_4 - data$Var_4_lag1



######### SAVE DATASET IN EXCEL
write.xlsx(data, "GitHub/Master-Thesis/Datasets/RS975_sa.xlsx") 






