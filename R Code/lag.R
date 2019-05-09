
# upload packages
library(Hmisc)
library(xtable)
library(stargazer)
library(xlsx)
library(readxl)
library(pracma)


#########################################
# Create lags for simplicity in dataset #
#########################################

# upload data from the "datasets" folder
data <- read_excel("GitHub/Master-Thesis/Datasets/RS975_not_sa.xlsx")

# add date formated column
data$date <- as.Date(period)

# add an observation column 
data$Obs = 1:nrow(data)

# create a plotable variable for GDP with a linearized filling of the NA's
data$GDP_year_plot <- with(data, interp1(Obs, GDP_year, Obs, "linear"))
data$GDP_plot <- with(data, interp1(Obs, GDP, Obs, "linear"))


###################
### Create lags ###
###################

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

######################################
### add difference with previous month
######################################

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


######################################
### Combine to arrive to the Indicator
######################################

data$E_I <- (data$E_1 + data$E_2 + data$E_3 + data$E_4)/4 
data$Var_I <- (data$Var_1 + data$Var_2 + data$Var_3 + data$Var_4)/4
data$Z_I <- (data$Z_1 + data$Z_2 + data$Z_3 + data$Z_4)/4
data$Var_Z_I <- (data$Var_Z_1 + data$Var_Z_2 + data$Var_Z_3 + data$Var_Z_4)/4

data$Ap_I <- (data$Ap_1 + data$Ap_2 + data$Ap_3 + data$Ap_4)/4
data$A0_I <- (data$A0_1 + data$A0_2 + data$A0_3 + data$A0_4)/4
data$An_I <- (data$An_1 + data$An_2 + data$An_3 + data$An_4)/4

data$Z_pp_I <- (data$Z_pp_1 + data$Z_pp_2 + data$Z_pp_3 + data$Z_pp_4)/4

data$E_I_sa_lag1 <- Lag(E_I_sa, 1)
data$E_I_sa_diff <- data$E_I_sa - data$E_I_sa_lag1

###############

###############
  
# lags

data$E_I_lag1 <- Lag(E_I, -1)
data$E_I_lag2 <- Lag(E_I, -2)
data$E_I_lag3 <- Lag(E_I, -3)
data$E_I_lag4 <- Lag(E_I, -4)
data$E_I_lag1 <- Lag(E_I, -1)
data$Var_I_lag1 <- Lag(Var_I, -2)
data$Z_I_lag1 <- Lag(Z_I, -3)
data$Var_Z_I_lag1 <- Lag(Var_Z_I, -4)

# difference
  data$E_I_diff <- data$E_I - data$E_I_lag1
  
  


###############################
######### SAVE DATASET IN EXCEL
###############################

write.xlsx(data, "GitHub/Master-Thesis/Datasets/RS975_not_sa.xlsx") 






