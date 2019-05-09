# upload packages
library(Hmisc)
library(xtable)
library(stargazer)
library(xlsx)
library(readxl)
library(pracma)

library(ggplot2)
library(tidyverse)
library(GGally)
library(lattice)
library(survival)
library(Formula)
library(corrplot)

#########################################

# upload data from the "datasets" folder
data <- read_excel("GitHub/Master-Thesis/Datasets/RS975_not_sa.xlsx")

# create time serie dataset
  #data_ts = ts(data, start=c(1988,1), frequency=12)

# add date formated column
data$date <- as.Date(period)

# add an observation column 
data$Obs = 1:nrow(data)

# create a plotable variable for GDP with a linearized filling of the NA's
data$GDP_year_plot <- with(data, interp1(Obs, GDP_year, Obs, "linear"))
data$GDP_plot <- with(data, interp1(Obs, GDP, Obs, "linear"))


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


###################
### Create lags ###
###################

# GDP
data$GDP_year_lag1 <- Lag(GDP_year, -3)
data$GDP_lag1 <- Lag(GDP, -3)

# lags
data$E_I_lag1 <- Lag(E_I, -1)
data$Var_I_lag1 <- Lag(Var_I, -2)
data$Z_I_lag1 <- Lag(Z_I, -3)
data$Var_Z_I_lag1 <- Lag(Var_Z_I, -4)

# difference
data$E_I_diff <- data$E_I - data$E_I_lag1


####################
### CORRELATIONS ###
####################


#### GDP corr with the different indicators
stargazer(cor(na.omit(data[c("GDP", "GDP_year", "E_I", "E_1", "E_2","E_3","E_4")])), title="Correlation Matrix")

ggpairs(data[c("GDP", "GDP_year", "E_I", "E_1", "E_2","E_3","E_4")], colour = "cyl", upper = list(continuous = wrap("cor", size = 8)))  + theme_bw()










