###########################
### Preparing data        #
###########################




# install.packages("devtools")

## Install rjdemetra and rjdqa
# install.packages("devtools")
# devtools::install_github("jdemetra/rjdemetra")
# devtools::install_github("AQLT/rjdqa", args = "--no-multiarch")

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-11.0.3/")


#import libraries
library(rJava)
library(RJDemetra)
library(rjdqa)
library(tidyverse)
library(readxl)
library(xlsx)
library(ggplot2)
library(ggfortify)
library(zoo)
library(xts)


# upload data
data <- read_excel("Master-Thesis/Datasets/data.xlsx")

# create time series with the 
data_ts = ts(data, start=c(1988,1), frequency=12)



#################################
# apply seasonal correction X13 #
#################################

# for E(X)
data_E <- data_ts[, "E"]
data_E_model <- x13(data_E, spec="RSA0") # X-13ARIMA method

# for E(Z)
data_EZ <- data_ts[, "Z"]
data_EZ_model <- x13(data_EZ, spec="RSA0") # X-13ARIMA method

# for E(Z2)
data_EZ2 <- data_ts[, "Z2"]
data_EZ2_model <- x13(data_EZ2, spec="RSA0") # X-13ARIMA method

# for E(Z3)
data_EZ3 <- data_ts[, "Z3"]
data_EZ3_model <- x13(data_EZ3, spec="RSA0") # X-13ARIMA method

# for Var(X)
data_Var <- data_ts[, "Var"]
data_Var_model <- x13(data_Var, spec="RSA0") # X-13ARIMA method

# for Var(Z)
data_VarZ <- data_ts[, "Var_Z"]
data_VarZ_model <- x13(data_VarZ, spec="RSA0") # X-13ARIMA method

# for Var(Z2)
data_VarZ2 <- data_ts[, "Var_Z2"]
data_VarZ2_model <- x13(data_VarZ2, spec="RSA0") # X-13ARIMA method

# for Var(Z3)
data_VarZ3 <- data_ts[, "Var_Z2"]
data_VarZ3_model <- x13(data_VarZ3, spec="RSA0") # X-13ARIMA method

data$E_sa <- data_E_model$final$series[, "sa"]
data$Var_sa <- data_Var_model$final$series[, "sa"]
data$Z_sa <- data_EZ_model$final$series[, "sa"]
data$Z2_sa <- data_EZ2_model$final$series[, "sa"]
data$Var_Z_sa <- data_VarZ_model$final$series[, "sa"]
data$Var_Z2_sa <- data_VarZ2_model$final$series[, "sa"]
data$Z3_sa <- data_EZ3_model$final$series[, "sa"]
data$Var_Z3_sa <- data_VarZ3_model$final$series[, "sa"]


################################
# plot seasonal corrected data #
################################

par(mfrow=c(4,2))

# Basic plot with the original series, the trend and the SA series
plot(data_E_model, type_chart = "sa-trend", caption="BSI")
plot(data_Var_model, type_chart = "sa-trend", caption="Var(BSI)")
plot(data_EZ_model, type_chart = "sa-trend", caption="EIR1")
plot(data_VarZ_model, type_chart = "sa-trend", caption="Var(EIR1)")
plot(data_EZ2_model, type_chart = "sa-trend", caption="EIR2")
plot(data_VarZ2_model, type_chart = "sa-trend", caption="Var(EIR2)")
plot(data_EZ3_model, type_chart = "sa-trend", caption="EIR3")
plot(data_VarZ3_model, type_chart = "sa-trend", caption="Var(EIR3)")



# look at different results (plots)
# S-I ratio
plot(data_E_model$decomposition)
plot(data_Var_model$decomposition)
plot(data_EZ_model$decomposition)
plot(data_VarZ_model$decomposition)
plot(data_EZ2_model$decomposition)
plot(data_VarZ2_model$decomposition)
par(mfrow=c(1,1))

plot(sa_dashboard(data_E_model), main = "Seasonal Adjustment Dashboard",
     subtitle = "", raw_color = "#33A02C", sa_color = "#E31A1C",
     trend_color = "black")
plot(sa_dashboard(data_Var_model))
plot(sa_dashboard(data_EZ_model))
plot(sa_dashboard(data_VarZ_model))
plot(sa_dashboard(data_EZ2_model))
plot(sa_dashboard(data_VarZ2_model))

autoplot(decompose(data_E))


##########
# OUTPUT #
##########


write.xlsx(data, "Master-Thesis/Datasets/data_sa.xlsx") 


