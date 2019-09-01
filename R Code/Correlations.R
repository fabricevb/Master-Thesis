
# Load packages 

library(ggplot2)
library(tidyverse)
library(stargazer)
library(GGally)
library(lattice)
library(survival)
library(Formula)
library(Hmisc)
library(corrplot)
library(readxl)
library(corrplot)
library(xtable)
library(reshape2)
library(tseries)


 
# data <- read_excel("Master-Thesis/Datasets/data_sa.xlsx")
data <- read_excel("~/Master-Thesis/Datasets/data_sa.xlsx")

# number the columns
data$Obs = 1:nrow(data)

###########################
# Plot Variables          #
###########################
data$date <- as.Date(data$period)
tmp <- data[c("date", "GDP_year_plot", "E_sa", "Var_sa", "Z_sa", 
              "Var_Z_sa", "Z2_sa", "Var_Z2_sa", "Z3_sa", "Var_Z3_sa")]

meltdf <- melt(tmp,id="date")
meltdf$GDP_year <- data$GDP_year
meltdf$date <- as.Date(meltdf$date)
levels(meltdf$variable) <- c("YoY GDP", "BSI", "Var(BSI)", "EIR", "Var(EIR)", "EIR2", "Var(EIR2)", "EIR3", "Var(EIR3)")
ggplot(meltdf,aes(x=date,y=value,group=variable)) + 
        geom_line(na.rm=FALSE,colour="dodgerblue") + 
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        facet_grid(rows = vars(variable), scales="free") +
        ylab(" ") +
        xlab(" ") + theme_bw() +   theme(legend.position = "none",axis.text.x = element_text(angle=45, hjust = 1))




###########################
### Attrition and dropout #
###########################

stargazer(cor(na.omit(data[c("Obs", "GDP_year", "E_sa", "Var_sa", "Z_sa","Var_Z_sa")]))
          , title="Correlation Matrix")

tmp <- data[c("GDP_year", "E_sa", "Var_sa", "Z_sa","Var_Z_sa", "Z2_sa","Var_Z2_sa", "Z3_sa","Var_Z3_sa")]

# descriptive table
stargazer(as.data.frame(tmp))

#### Tables

#### GDP corr with the different indicators
stargazer(cor(na.omit(data2[c("GDP_year", "E_I", "E_1", "E_2","E_3","E_4")]))
          , title="Correlation Matrix")

ggpairs(data2[c("GDP_year", "E_1", "E_2","E_3","E_4")], 
         upper = list(continuous = wrap("cor", size = 6))
        , columnLabels = c("YoY GDP", "BSI Q1", "BSI Q2", "BSI Q3", "BSI Q4"))  + theme_bw()

corrplot.mixed(cor(na.omit(data2[c("GDP", "GDP_year", "E_1", "E_2","E_3","E_4")]))
               , tl.col = "black"
               ,lower = "circle", upper = "number")

#### GDP corr with the different indicators
stargazer(cor(na.omit(data[c("GDP_year", "E", "Var", "Z","Var_Z", "Z2","Var_Z2", "Z3","Var_Z3")])), title="Correlation Matrix")

ggpairs(data[c("GDP_year", "E_sa", "Var_sa", "Z_sa","Var_Z_sa", "Z2_sa","Var_Z2_sa", "Z3_sa","Var_Z3_sa")], upper = list(continuous = wrap("cor", size = 6)), 
        columnLabels = c("YoY GDP", "BSI", "Var(BSI)", "EIR1", "Var(EIR1)", "EIR2", "Var(EIR2)", "EIR3", "Var(EIR3)"))  + theme_bw()

ggpairs(data[c("GDP_year", "E_sa", "Var_sa", "Z_sa","Var_Z_sa")], upper = list(continuous = wrap("cor", size = 6)), 
        columnLabels = c("YoY GDP", "BSI", "Var(BSI)", "EIR", "Var(EIR)"))  + theme_bw()

# Correlation Matrix check Question 3 and 4 if true what they say
# Question 3
stargazer(cor(na.omit(data2[c("GDP", "GDP_year", "E_3", "E_3_lag1","E_3_lag2","E_3_lag3","E_3_lag4")])), title="Correlation Matrix")

# Question 4
stargazer(cor(na.omit(data2[c("GDP", "GDP_year", "E_4", "E_4_lag1","E_4_lag2","E_4_lag3","E_4_lag4")])), title="Correlation Matrix")


################################
### participants and variances #
################################

scaleFactor <- max(data$Respondents) / max(data$Var_sa)
ggplot(data, aes(x=date)) +
        geom_line(aes(y=Respondents), col="dodgerblue2") +
        geom_line(aes(y=Var_sa * scaleFactor), col="tomato3") +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        scale_y_continuous(name="Respondents", sec.axis=sec_axis(~./scaleFactor, name="Var(BSI)")) + 
        theme_bw() +   theme(legend.position = "none",axis.text.x = element_text(angle=45, hjust = 1))

scaleFactor <- max(data$Respondents) / max(data$Var_Z_sa)
ggplot(data, aes(x=date)) +
        geom_line(aes(y=Respondents), col="dodgerblue2") +
        geom_line(aes(y=Var_Z_sa * scaleFactor), col="tomato3") +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        scale_y_continuous(name="Respondents", sec.axis=sec_axis(~./scaleFactor, name="Var(EIR)")) +         
        theme_bw() +   theme(legend.position = "none",axis.text.x = element_text(angle=45, hjust = 1))


#####################
### Autocorrelation #
#####################

### ADF test

# create time series with the 
data_ts = ts(data, start=c(1988,1), frequency=12)
E_sa <- data_ts[, "E_sa"]
Var_sa <- data_ts[, "Var_sa"]
Z_sa <- data_ts[, "Z_sa"]
Var_Z_sa <- data_ts[, "Var_Z_sa"]
Z2_sa <- data_ts[, "Z2_sa"]
Var_Z2_sa <- data_ts[, "Var_Z2_sa"]
Z3_sa <- data_ts[, "Z3_sa"]
Var_Z3_sa <- data_ts[, "Var_Z3_sa"]

# apply test
c(adf.test(E_sa)$p.value, adf.test(Var_sa)$p.value, adf.test(Z_sa)$p.value, adf.test(Var_Z_sa)$p.value,
        adf.test(Z2_sa)$p.value, adf.test(Var_Z2_sa)$p.value, adf.test(Z3_sa)$p.value, adf.test(Var_Z3_sa)$p.value)

### Autocorrelation ACF plots

layout(matrix(c(1,1,2,3,4,5,6,7,8,9), 5, 2, byrow = TRUE))
acf(na.omit(data$GDP_year), main = "Autocorrelation of the YoY GDP")
acf(na.omit(data$E_sa), main = "Autocorrelation of the BSI")
acf(na.omit(data$Var_sa), main = "Autocorrelation of Var(BSI)")
acf(na.omit(data$Z_sa), main = "Autocorrelation of the EIR1")
acf(na.omit(data$Var_Z_sa), main = "Autocorrelation of Var(EIR1)")
acf(na.omit(data$Z2_sa), main = "Autocorrelation of the EIR2")
acf(na.omit(data$Var_Z2_sa), main = "Autocorrelation of Var(EIR2)")
acf(na.omit(data$Z3_sa), main = "Autocorrelation of the EIR3")
acf(na.omit(data$Var_Z3_sa), main = "Autocorrelation of Var(EIR3)")

par(mfrow=c(1,1))

