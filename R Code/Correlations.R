
# Load packages

library(ggplot2)
library(tidyverse)
library(stargazer)
library(GGally)
library(Hmisc)
library(corrplot)
library(readxl)
library(corrplot)


data <- read_excel("GitHub/Master-Thesis/Datasets/RS975_sa.xlsx")

# create time serie dataset

data_ts = ts(data, start=c(1988,1), frequency=12)

#### Tables


stargazer(cor(na.omit(data[c("GDP", "GDP_year", "E_I", "E_1", "E_2","E_3","E_4")])), title="Correlation Matrix")

ggpairs(data[c("GDP", "GDP_year", "E_I", "E_1", "E_2","E_3","E_4")], colour = "cyl", upper = list(continuous = wrap("cor", size = 8)))  + theme_bw()

corrplot(na.omit(data[c("GDP", "GDP_year", "E_I", "E_1", "E_2","E_3","E_4")]), method="circle")



###### Correlation with indicators
stargazer(cor(na.omit(data[c("GDP", "GDP_year", "E_I", "Var_I", "Z_I","Var_Z_I")])), title="Correlation Matrix")

ggpairs(data[c("GDP", "GDP_year", "E_I", "Var_I", "Z_I","Var_Z_I")], upper = list(continuous = wrap("cor", size = 8)))  + theme_bw()







# Correlation Matrix check Question 3 and 4 if true what they say
# Question 3
stargazer(cor(na.omit(data[c("GDP", "GDP_year", "E_3", "E_3_lag1","E_3_lag2","E_3_lag3","E_3_lag4")])), title="Correlation Matrix")

# Question 4
stargazer(cor(na.omit(data[c("GDP", "GDP_year", "E_4", "E_4_lag1","E_4_lag2","E_4_lag3","E_4_lag4")])), title="Correlation Matrix")



corrplot(cor(na.omit(data[c("E_1","E_2","E_3","E_4")])), type="upper", tl.col="black", tl.srt=45)

corrplot.mixed(cor(na.omit(data[c("E_1","E_2","E_3","E_4")])),  tl.col="black", tl.srt=45)

corrplot.mixed(cor(na.omit(data[c("E_1","E_1_lag1","E_1_lag2","E_1_lag3","E_1_lag4")])),  tl.col="black", tl.srt=45)

corrplot.mixed(cor(na.omit(data[c("E_2","E_2_lag1","E_2_lag2","E_2_lag3","E_2_lag4")])),  tl.col="black", tl.srt=45)

corrplot.mixed(cor(na.omit(data[c("E_3","E_3_lag1","E_3_lag2","E_3_lag3","E_3_lag4")])),  tl.col="black", tl.srt=45)

corrplot.mixed(cor(na.omit(data[c("E_4", "E_4_lag1","E_4_lag2","E_4_lag3","E_4_lag4")])),  tl.col="black", tl.srt=45)

corrplot.mixed(cor(na.omit(data[c("E_1","E_2","E_3","E_4",
                                  "E_1_lag1","E_1_lag2","E_1_lag3","E_1_lag4",
                                  "E_2_lag1","E_2_lag2","E_2_lag3","E_2_lag4",
                                  "E_3_lag1","E_3_lag2","E_3_lag3","E_3_lag4",
                                  "E_4_lag1","E_4_lag2","E_4_lag3","E_4_lag4")])),  tl.col="black", tl.srt=45)


corrplot(cor(na.omit(data[c("GDP_year", "E_1","E_2","E_3","E_4",
                                  "E_1_lag1","E_1_lag2","E_1_lag3","E_1_lag4",
                                  "E_2_lag1","E_2_lag2","E_2_lag3","E_2_lag4",
                                  "E_3_lag1","E_3_lag2","E_3_lag3","E_3_lag4",
                                  "E_4_lag1","E_4_lag2","E_4_lag3","E_4_lag4")])), type = "upper", method="number")

#############
### plots ###
#############

### Autocorrelation ACF plots
par(mfrow=c(2,2))
acf(na.omit(data$E_1), main = "Autocorrelation of the Indicator of Question 1")
acf(na.omit(data$E_2), main = "Autocorrelation of the Indicator of Question 2")
acf(na.omit(data$E_3), main = "Autocorrelation of the Indicator of Question 3")
acf(na.omit(data$E_4), main = "Autocorrelation of the Indicator of Question 4")

acf(na.omit(data$Var_1), main = "Autocorrelation of the Variance of Question 1")
acf(na.omit(data$Var_2), main = "Autocorrelation of the Variance of Question 2")
acf(na.omit(data$Var_3), main = "Autocorrelation of the Variance of Question 3")
acf(na.omit(data$Var_4), main = "Autocorrelation of the Variance of Question 4")

acf(na.omit(data$E_I), main = "Autocorrelation of X")
acf(na.omit(data$Var_I), main = "Autocorrelation of Var(X)")
acf(na.omit(data$Z_I), main = "Autocorrelation of Z")
acf(na.omit(data$Var_Z_I), main = "Autocorrelation of Var(Z)")

par(mfrow=c(1,1))

par(mfrow=c(2,1))
acf(na.omit(data$GDP_year), main = "Autocorrelation of the GDP YoY")
acf(na.omit(data$GDP), main = "Autocorrelation of the GDP")
par(mfrow=c(1,1))


### Correlation plots
#ggpairs(data[c("period", "E_1", "Var_1", "Z_1", "Var_Z_1")])
#ggpairs(data[c("period", "E_2", "Var_2", "Z_2", "Var_Z_2")])
#ggpairs(data[c("period", "E_3", "Var_3", "Z_3", "Var_Z_3")])
#ggpairs(data[c("period", "E_4", "Var_4", "Z_4", "Var_Z_4")])

#ggpairs(data[c("GDP_year", "E_1", "E_2", "E_3", "E_4")])

ggpairs(data[c("period", "E_I", "Var_I", "Z_I", "Var_Z_I")])



###############
# Correlation #
###############


