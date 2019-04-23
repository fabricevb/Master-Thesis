
# install.packages("devtools")

# Install rjdemetra and rjdqa
# library(devtools)
# devtools::install_github("jdemetraéé/rjdemetra", args = "--no-multiarch")
# devtools::install_github("AQLT/rjdqa", args = "--no-multiarch")


Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-11.0.2/")

#import libraries
library(rJava)
library(RJDemetra)
library(tidyverse)
library(readxl)
library(xlsx)
 # library(rjdqa)
library(ggplot2)
library(ggfortify)
library(zoo)
library(xts)


# Import all datasets
MI018 <-read_excel("C:/Users/Fabrice/Documents/RS_975.xlsx",
                 sheet="MI018")

MI027 <-read_excel("C:/Users/Fabrice/Documents/RS_975.xlsx",
                   sheet="MI027")

MI032 <-read_excel("C:/Users/Fabrice/Documents/RS_975.xlsx",
                   sheet="MI032")

MI033 <-read_excel("C:/Users/Fabrice/Documents/RS_975.xlsx",
                   sheet="MI033")


# question 18 has to be interpreted the other way
MI018$Solde_UW <- - MI018$Solde_UW

# save period in a vector
period <- MI018$period


# create time series 
MI018_ts = ts(MI018, start=c(1988,1), frequency=12)
MI027_ts = ts(MI027, start=c(1988,1), frequency=12)
MI032_ts = ts(MI032, start=c(1988,1), frequency=12)
MI033_ts = ts(MI033, start=c(1988,1), frequency=12)

data_ts


  #################################
  # apply seasonal correction X13 #
  #################################

# for E(X)
MI018_E <- MI018_ts[, "Solde_UW"]
MI027_E <- MI027_ts[, "Solde_UW"]
MI032_E <- MI032_ts[, "Solde_UW"]
MI033_E <- MI033_ts[, "Solde_UW"]


MI018_E_model <- x13_def(MI018_E) # X-13ARIMA method
MI027_E_model <- x13_def(MI027_E) # X-13ARIMA method
MI032_E_model <- x13_def(MI032_E) # X-13ARIMA method
MI033_E_model <- x13_def(MI033_E) # X-13ARIMA method

MI018_E_model_ts <- tramoseats_def(MI018_E) # X-13ARIMA method
MI027_E_model_ts <- tramoseats_def(MI027_E) # X-13ARIMA method
MI032_E_model_ts <- tramoseats_def(MI032_E) # X-13ARIMA method
MI033_E_model_ts <- tramoseats_def(MI033_E) # X-13ARIMA method

# for Var(X)
MI018_Var <- MI018_ts[, "var_UW"]
MI027_Var <- MI027_ts[, "var_UW"]
MI032_Var <- MI032_ts[, "var_UW"]
MI033_Var <- MI033_ts[, "var_UW"]

MI018_Var_model <- x13_def(MI018_Var) # X-13ARIMA method
MI027_Var_model <- x13_def(MI027_Var) # X-13ARIMA method
MI032_Var_model <- x13_def(MI032_Var) # X-13ARIMA method
MI033_Var_model <- x13_def(MI033_Var) # X-13ARIMA method

# for E(Z)
MI018_EZ <- MI018_ts[, "Solde_Z_UW"]
MI027_EZ <- MI027_ts[, "Solde_Z_UW"]
MI032_EZ <- MI032_ts[, "Solde_Z_UW"]
MI033_EZ <- MI033_ts[, "Solde_Z_UW"]

MI018_EZ_model <- x13_def(MI018_EZ) # X-13ARIMA method
MI027_EZ_model <- x13_def(MI027_EZ) # X-13ARIMA method
MI032_EZ_model <- x13_def(MI032_EZ) # X-13ARIMA method
MI033_EZ_model <- x13_def(MI033_EZ) # X-13ARIMA method

# for Var(Z)
MI018_VarZ <- MI018_ts[, "Var_Z_UW"]
MI027_VarZ <- MI027_ts[, "Var_Z_UW"]
MI032_VarZ <- MI032_ts[, "Var_Z_UW"]
MI033_VarZ <- MI033_ts[, "Var_Z_UW"]

MI018_VarZ_model <- x13_def(MI018_VarZ) # X-13ARIMA method
MI027_VarZ_model <- x13_def(MI027_VarZ) # X-13ARIMA method
MI032_VarZ_model <- x13_def(MI032_VarZ) # X-13ARIMA method
MI033_VarZ_model <- x13_def(MI033_VarZ) # X-13ARIMA method


# for Ap_p A0_p An_p

# positive answers
MI018_Ap_p <- MI018_ts[, "Ap_p"]
MI027_Ap_p <- MI027_ts[, "Ap_p"]
MI032_Ap_p <- MI032_ts[, "Ap_p"]
MI033_Ap_p <- MI033_ts[, "Ap_p"]

MI018_Ap_model <- x13_def(MI018_Ap_p) # X-13ARIMA method
MI027_Ap_model <- x13_def(MI027_Ap_p) # X-13ARIMA method
MI032_Ap_model <- x13_def(MI032_Ap_p) # X-13ARIMA method
MI033_Ap_model <- x13_def(MI033_Ap_p) # X-13ARIMA method

# neutral answers
MI018_A0_p <- MI018_ts[, "A0_p"]
MI027_A0_p <- MI027_ts[, "A0_p"]
MI032_A0_p <- MI032_ts[, "A0_p"]
MI033_A0_p <- MI033_ts[, "A0_p"]

MI018_A0_model <- x13_def(MI018_A0_p) # X-13ARIMA method
MI027_A0_model <- x13_def(MI027_A0_p) # X-13ARIMA method
MI032_A0_model <- x13_def(MI032_A0_p) # X-13ARIMA method
MI033_A0_model <- x13_def(MI033_A0_p) # X-13ARIMA method

# negative answers
MI018_An_p <- MI018_ts[, "An_p"]
MI027_An_p <- MI027_ts[, "An_p"]
MI032_An_p <- MI032_ts[, "An_p"]
MI033_An_p <- MI033_ts[, "An_p"]

MI018_An_model <- x13_def(MI018_An_p) # X-13ARIMA method
MI027_An_model <- x13_def(MI027_An_p) # X-13ARIMA method
MI032_An_model <- x13_def(MI032_An_p) # X-13ARIMA method
MI033_An_model <- x13_def(MI033_An_p) # X-13ARIMA method




#############
# correction so that proportions add up to 1
#############



# for Z_pp_p Z_p0_p Z_pn_p Z_0p_p Z_00_p Z_0n_p Z_np_p Z_n0_p Z_nn_p tvar_UW


################################
# plot seasonal corrected data #
################################

par(mfrow=c(2,2))

# Basic plot with the original series, the trend and the SA series
plot(MI018_E_model, type_chart = "sa-trend")
plot(MI027_E_model, type_chart = "sa-trend")
plot(MI032_E_model, type_chart = "sa-trend")
plot(MI033_E_model, type_chart = "sa-trend")

plot(MI018_E_model_ts, type_chart = "sa-trend")
plot(MI027_E_model_ts, type_chart = "sa-trend")
plot(MI032_E_model_ts, type_chart = "sa-trend")
plot(MI033_E_model_ts, type_chart = "sa-trend")

# look at different results (plots)
# S-I ratio
plot(MI018_E_model$decomposition)
plot(MI027_E_model$decomposition)
plot(MI032_E_model$decomposition)
plot(MI033_E_model$decomposition)


# Basic plot with the original series, the trend and the SA series
plot(MI018_Var_model, type_chart = "sa-trend")
plot(MI027_Var_model, type_chart = "sa-trend")
plot(MI032_Var_model, type_chart = "sa-trend")
plot(MI033_Var_model, type_chart = "sa-trend")

# look at different results (plots)
# S-I ratio
plot(MI018_Var_model$decomposition)
plot(MI027_Var_model$decomposition)
plot(MI032_Var_model$decomposition)
plot(MI033_Var_model$decomposition)

# Basic plot with the original series, the trend and the SA series
plot(MI018_EZ_model, type_chart = "sa-trend")
plot(MI027_EZ_model, type_chart = "sa-trend")
plot(MI032_EZ_model, type_chart = "sa-trend")
plot(MI033_EZ_model, type_chart = "sa-trend")

# look at different results (plots)
# S-I ratio
plot(MI018_EZ_model$decomposition)
plot(MI027_EZ_model$decomposition)
plot(MI032_EZ_model$decomposition)
plot(MI033_EZ_model$decomposition)


# Basic plot with the original series, the trend and the SA series
plot(MI018_VarZ_model, type_chart = "sa-trend")
plot(MI027_VarZ_model, type_chart = "sa-trend")
plot(MI032_VarZ_model, type_chart = "sa-trend")
plot(MI033_VarZ_model, type_chart = "sa-trend")

# look at different results (plots)
# S-I ratio
plot(MI018_EZ_model$decomposition)
plot(MI027_EZ_model$decomposition)
plot(MI032_EZ_model$decomposition)
plot(MI033_EZ_model$decomposition)


# Basic plot with the original series, the trend and the SA series
plot(MI018_Ap_model, type_chart = "sa-trend")
plot(MI027_Ap_model, type_chart = "sa-trend")
plot(MI032_Ap_model, type_chart = "sa-trend")
plot(MI033_Ap_model, type_chart = "sa-trend")

# look at different results (plots)
# S-I ratio
plot(MI018_Ap_model$decomposition)
plot(MI027_Ap_model$decomposition)
plot(MI032_Ap_model$decomposition)
plot(MI033_Ap_model$decomposition)


# Basic plot with the original series, the trend and the SA series
plot(MI018_A0_model, type_chart = "sa-trend")
plot(MI027_A0_model, type_chart = "sa-trend")
plot(MI032_A0_model, type_chart = "sa-trend")
plot(MI033_A0_model, type_chart = "sa-trend")

# look at different results (plots)
# S-I ratio
plot(MI018_A0_model$decomposition)
plot(MI027_A0_model$decomposition)
plot(MI032_A0_model$decomposition)
plot(MI033_A0_model$decomposition)


# Basic plot with the original series, the trend and the SA series
plot(MI018_An_model, type_chart = "sa-trend")
plot(MI027_An_model, type_chart = "sa-trend")
plot(MI032_An_model, type_chart = "sa-trend")
plot(MI033_An_model, type_chart = "sa-trend")

# look at different results (plots)
# S-I ratio
plot(MI018_An_model$decomposition)
plot(MI027_An_model$decomposition)
plot(MI032_An_model$decomposition)
plot(MI033_An_model$decomposition)


par(mfrow=c(1,1))

##########
# OUTPUT #
##########

##### reinclude NA for 1st observation of Z
Z_1 <- append(Z_1, NA, 0)
Z_1


# create one table with all four questions
##########################################

# with seasonal correction

# Question 1
E_1 <- MI018_E_model$final$series[, "sa"]
Var_1 <- MI018_Var_model$final$series[, "sa"]
Z_1 <- MI018_EZ_model$final$series[, "sa"]
Z_1 <- append(Z_1, NA, 0)
Var_Z_1 <- MI018_VarZ_model$final$series[, "sa"]
Var_Z_1 <- append(Var_Z_1, NA, 0)
Ap_1 <- MI018_Ap_model$final$series[, "sa"]
A0_1 <- MI018_A0_model$final$series[, "sa"]
An_1 <- MI018_An_model$final$series[, "sa"]
Z_pp_1 <- MI018$Z_pp_p
Z_p0_1 <- MI018$Z_p0_p
Z_pn_1 <- MI018$Z_pn_p
Z_0p_1 <- MI018$Z_0p_p
Z_00_1 <- MI018$Z_00_p
Z_0n_1 <- MI018$Z_0n_p
Z_np_1 <- MI018$Z_np_p
Z_n0_1 <- MI018$Z_n0_p
Z_nn_1 <- MI018$Z_nn_p


# Question 2
E_2 <- MI027_E_model$final$series[, "sa"]
Var_2 <- MI027_Var_model$final$series[, "sa"]
Z_2 <- MI027_EZ_model$final$series[, "sa"]
Z_2 <- append(Z_2, NA, 0)
Var_Z_2 <- MI027_VarZ_model$final$series[, "sa"]
Var_Z_2 <- append(Var_Z_2, NA, 0)
Ap_2 <- MI027_Ap_model$final$series[, "sa"]
A0_2 <- MI027_A0_model$final$series[, "sa"]
An_2 <- MI027_An_model$final$series[, "sa"]
Z_pp_2 <- MI027$Z_pp_p
Z_p0_2 <- MI027$Z_p0_p
Z_pn_2 <- MI027$Z_pn_p
Z_0p_2 <- MI027$Z_0p_p
Z_00_2 <- MI027$Z_00_p
Z_0n_2 <- MI027$Z_0n_p
Z_np_2 <- MI027$Z_np_p
Z_n0_2 <- MI027$Z_n0_p
Z_nn_2 <- MI027$Z_nn_p  

# Question 3
E_3 <- MI032_E_model$final$series[, "sa"]
Var_3 <- MI032_Var_model$final$series[, "sa"]
Z_3 <- MI032_EZ_model$final$series[, "sa"]
Z_3 <- append(Z_3, NA, 0)
Var_Z_3 <- MI032_VarZ_model$final$series[, "sa"]
Var_Z_3 <- append(Var_Z_3, NA, 0)
Ap_3 <- MI032_Ap_model$final$series[, "sa"]
A0_3 <- MI032_A0_model$final$series[, "sa"]
An_3 <- MI032_An_model$final$series[, "sa"]
Z_pp_3 <- MI032$Z_pp_p
Z_p0_3 <- MI032$Z_p0_p
Z_pn_3 <- MI032$Z_pn_p
Z_0p_3 <- MI032$Z_0p_p
Z_00_3 <- MI032$Z_00_p
Z_0n_3 <- MI032$Z_0n_p
Z_np_3 <- MI032$Z_np_p
Z_n0_3 <- MI032$Z_n0_p
Z_nn_3 <- MI032$Z_nn_p    


# Question 4
E_4 <- MI033_E_model$final$series[, "sa"]
Var_4 <- MI033_Var_model$final$series[, "sa"]
Z_4 <- MI033_EZ_model$final$series[, "sa"]
Z_4 <- append(Z_4, NA, 0)
Var_Z_4 <- MI033_VarZ_model$final$series[, "sa"]
Var_Z_4 <- append(Var_Z_4, NA, 0)
Ap_4 <- MI033_Ap_model$final$series[, "sa"]
A0_4 <- MI033_A0_model$final$series[, "sa"]
An_4 <- MI033_An_model$final$series[, "sa"]
Z_pp_4 <- MI033$Z_pp_p
Z_p0_4 <- MI033$Z_p0_p
Z_pn_4 <- MI033$Z_pn_p
Z_0p_4 <- MI033$Z_0p_p
Z_00_4 <- MI033$Z_00_p
Z_0n_4 <- MI033$Z_0n_p
Z_np_4 <- MI033$Z_np_p
Z_n0_4 <- MI033$Z_n0_p
Z_nn_4 <- MI033$Z_nn_p    

data_sa <- data.frame(period, 
    E_1, Var_1, Z_1, Var_Z_1, Ap_1, A0_1, An_1, Z_pp_1, Z_p0_1, Z_pn_1, Z_0p_1, Z_00_1, Z_0n_1, Z_np_1, Z_n0_1, Z_nn_1,
    E_2, Var_2, Z_2, Var_Z_2, Ap_2, A0_2, An_2, Z_pp_2, Z_p0_2, Z_pn_2, Z_0p_2, Z_00_2, Z_0n_2, Z_np_2, Z_n0_2, Z_nn_2,
    E_3, Var_3, Z_3, Var_Z_3, Ap_3, A0_3, An_3, Z_pp_3, Z_p0_3, Z_pn_3, Z_0p_3, Z_00_3, Z_0n_3, Z_np_3, Z_n0_3, Z_nn_3,
    E_4, Var_4, Z_4, Var_Z_4, Ap_4, A0_4, An_4, Z_pp_4, Z_p0_4, Z_pn_4, Z_0p_4, Z_00_4, Z_0n_4, Z_np_4, Z_n0_4, Z_nn_4)

write.xlsx(data_sa, "C:/Users/Fabrice/Documents/KULeuven/RS975_sa.xlsx") 



# without seasonal correction
# Question 1
E_1 <- MI018$Solde_UW
Var_1 <- MI018$var_UW
Z_1 <- MI018$Solde_Z_UW
Var_Z_1 <- MI018$Var_Z_UW
Ap_1 <- MI018$Ap_p
A0_1 <- MI018$A0_p
An_1 <- MI018$An_p
Z_pp_1 <- MI018$Z_pp_p
Z_p0_1 <- MI018$Z_p0_p
Z_pn_1 <- MI018$Z_pn_p
Z_0p_1 <- MI018$Z_0p_p
Z_00_1 <- MI018$Z_00_p
Z_0n_1 <- MI018$Z_0n_p
Z_np_1 <- MI018$Z_np_p
Z_n0_1 <- MI018$Z_n0_p
Z_nn_1 <- MI018$Z_nn_p


# Question 2
E_2 <- MI027$Solde_UW
Var_2 <- MI027$var_UW
Z_2 <- MI027$Solde_Z_UW
Var_Z_2 <- MI027$Var_Z_UW
Ap_2 <- MI027$Ap_p
A0_2 <- MI027$A0_p
An_2 <- MI027$An_p
Z_pp_2 <- MI027$Z_pp_p
Z_p0_2 <- MI027$Z_p0_p
Z_pn_2 <- MI027$Z_pn_p
Z_0p_2 <- MI027$Z_0p_p
Z_00_2 <- MI027$Z_00_p
Z_0n_2 <- MI027$Z_0n_p
Z_np_2 <- MI027$Z_np_p
Z_n0_2 <- MI027$Z_n0_p
Z_nn_2 <- MI027$Z_nn_p  

# Question 3
E_3 <- MI032$Solde_UW
Var_3 <- MI032$var_UW
Z_3 <- MI032$Solde_Z_UW
Var_Z_3 <- MI032$Var_Z_UW
Ap_3 <- MI032$Ap_p
A0_3 <- MI032$A0_p
An_3 <- MI032$An_p
Z_pp_3 <- MI032$Z_pp_p
Z_p0_3 <- MI032$Z_p0_p
Z_pn_3 <- MI032$Z_pn_p
Z_0p_3 <- MI032$Z_0p_p
Z_00_3 <- MI032$Z_00_p
Z_0n_3 <- MI032$Z_0n_p
Z_np_3 <- MI032$Z_np_p
Z_n0_3 <- MI032$Z_n0_p
Z_nn_3 <- MI032$Z_nn_p    


# Question 4
E_4 <- MI033$Solde_UW
Var_4 <- MI033$var_UW
Z_4 <- MI033$Solde_Z_UW
Var_Z_4 <- MI033$Var_Z_UW
Ap_4 <- MI033$Ap_p
A0_4 <- MI033$A0_p
An_4 <- MI033$An_p
Z_pp_4 <- MI033$Z_pp_p
Z_p0_4 <- MI033$Z_p0_p
Z_pn_4 <- MI033$Z_pn_p
Z_0p_4 <- MI033$Z_0p_p
Z_00_4 <- MI033$Z_00_p
Z_0n_4 <- MI033$Z_0n_p
Z_np_4 <- MI033$Z_np_p
Z_n0_4 <- MI033$Z_n0_p
Z_nn_4 <- MI033$Z_nn_p    

data <- data.frame(period, 
                   E_1, Var_1, Z_1, Var_Z_1, Ap_1, A0_1, An_1, Z_pp_1, Z_p0_1, Z_pn_1, Z_0p_1, Z_00_1, Z_0n_1, Z_np_1, Z_n0_1, Z_nn_1,
                   E_2, Var_2, Z_2, Var_Z_2, Ap_2, A0_2, An_2, Z_pp_2, Z_p0_2, Z_pn_2, Z_0p_2, Z_00_2, Z_0n_2, Z_np_2, Z_n0_2, Z_nn_2,
                   E_3, Var_3, Z_3, Var_Z_3, Ap_3, A0_3, An_3, Z_pp_3, Z_p0_3, Z_pn_3, Z_0p_3, Z_00_3, Z_0n_3, Z_np_3, Z_n0_3, Z_nn_3,
                   E_4, Var_4, Z_4, Var_Z_4, Ap_4, A0_4, An_4, Z_pp_4, Z_p0_4, Z_pn_4, Z_0p_4, Z_00_4, Z_0n_4, Z_np_4, Z_n0_4, Z_nn_4)


write.xlsx(data, "C:/Users/Fabrice/Documents/KULeuven/RS975.xlsx") 



#### seasonal correction of the indicators 

data <- read_excel("GitHub/Master-Thesis/Datasets/RS975_not_sa.xlsx")


# create time series 
data_ts = ts(data, start=c(1988,1), frequency=12)


#################################
# apply seasonal correction X13 #
#################################

# for E(X)
E_I <- data_ts[, "E_I"]
Var_I <- data_ts[, "Var_I"]
Z_I <- data_ts[, "Z_I"]
Var_Z_I <- data_ts[, "Var_Z_I"]


E_I_model <- x13(E_I) # X-13ARIMA method
Var_I_model <- x13(Var_I) # X-13ARIMA method
Z_I_model <- x13(Z_I) # X-13ARIMA method
Var_Z_I_model <- x13(Var_Z_I) # X-13ARIMA method


par(mfrow=c(2,2))

# Basic plot with the original series, the trend and the SA series
plot(E_I_model, type_chart = "sa-trend")
plot(Var_I_model, type_chart = "sa-trend")
plot(Z_I_model, type_chart = "sa-trend")
plot(Var_Z_I_model, type_chart = "sa-trend")

# look at different results (plots)
# S-I ratio
plot(E_I_model$decomposition)
plot(Var_I_model$decomposition)
plot(Z_I_model$decomposition)
plot(Var_Z_I_model$decomposition)







