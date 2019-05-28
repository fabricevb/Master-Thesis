
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
 

data2 <- read_excel("Master-Thesis/Datasets/RS975_sa.xlsx")
data <- read_excel("Master-Thesis/Datasets/data_sa.xlsx")


data$Obs = 1:nrow(data)

### ATTRITION

data$date <- as.Date(data$period)
tmp <- data[c("date", "GDP_year_plot", "predicted_model1", "predicted_model2", "predicted_model3")]





meltdf <- melt(tmp,id="date")
meltdf$GDP_year <- data$GDP_year
ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + 
        geom_line(na.rm=FALSE) + 
        scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) +
        geom_point(aes(y = GDP_year))



# Attrition and dropout


stargazer(cor(na.omit(data[c("Obs", "GDP_year", "E_sa", "Var_sa", "Z_sa","Var_Z_sa")]))
          , title="Correlation Matrix")

tmp <- data[c("E_sa", "Var_sa", "Z_sa","Var_Z_sa", "Z2_sa","Var_Z2_sa", "Z3_sa","Var_Z3_sa")]

library(xtable)
library(Hmisc)

describe(tmp)

stargazer(as.data.frame(tmp))
typeof(data)

#### Tables

#### GDP corr with the different indicators
stargazer(cor(na.omit(data2[c("GDP_year", "E_I", "E_1", "E_2","E_3","E_4")]))
          , title="Correlation Matrix")

ggpairs(data2[c("GDP_year", "E_1", "E_2","E_3","E_4")], 
        colour = "cyl", upper = list(continuous = wrap("cor", size = 6))
        , columnLabels = c("YoY GDP", "BSI Q1", "BSI Q2", "BSI Q3", "BSI Q4"))  + theme_bw()

corrplot.mixed(cor(na.omit(data2[c("GDP", "GDP_year", "E_I", "E_1", "E_2","E_3","E_4")]))
               , tl.col = "black"
               ,lower = "circle", upper = "number")

#### GDP corr with the different indicators
stargazer(cor(na.omit(data2[c("GDP", "GDP_year", "E_I", "Var_I", "Z_I","Var_Z_I")])), title="Correlation Matrix")

ggpairs(data2[c("GDP", "GDP_year", "E_I", "Var_I", "Z_I","Var_Z_I")], 
        upper = list(continuous = wrap("cor", size = 6)))  + theme_bw()

ggpairs(data[c("GDP_year", "E", "Var", "Z","Var_Z", "Z2","Var_Z2", "Z3","Var_Z3")], 
        upper = list(continuous = wrap("cor", size = 6)), 
        columnLabels = c("YoY GDP", "BSI", "Var(BSI)", "EIR1", "Var(EIR1)", "EIR2", "Var(EIR2)", "EIR3", "Var(EIR3)"))  + theme_bw()


###### Correlation with indicators
stargazer(cor(na.omit(data2[c("GDP", "GDP_year", "E_I", "Var_I", "Z_I","Var_Z_I")])), title="Correlation Matrix")

ggpairs(data2[c("GDP", "GDP_year", "E_I", "Var_I", "Z_I","Var_Z_I")], 
        upper = list(continuous = wrap("cor", size = 6)))  + theme_bw()



# Correlation Matrix check Question 3 and 4 if true what they say
# Question 3
stargazer(cor(na.omit(data2[c("GDP", "GDP_year", "E_3", "E_3_lag1","E_3_lag2","E_3_lag3","E_3_lag4")])), title="Correlation Matrix")

# Question 4
stargazer(cor(na.omit(data2[c("GDP", "GDP_year", "E_4", "E_4_lag1","E_4_lag2","E_4_lag3","E_4_lag4")])), title="Correlation Matrix")



corrplot(cor(na.omit(data2[c("E_1","E_2","E_3","E_4")])), type="upper", tl.col="black", tl.srt=45)

corrplot.mixed(cor(na.omit(data2[c("E_1","E_2","E_3","E_4")])),  tl.col="black", tl.srt=45)

corrplot.mixed(cor(na.omit(data2[c("E_1","E_1_lag1","E_1_lag2","E_1_lag3","E_1_lag4")])),  tl.col="black", tl.srt=45)

corrplot.mixed(cor(na.omit(data2[c("E_2","E_2_lag1","E_2_lag2","E_2_lag3","E_2_lag4")])),  tl.col="black", tl.srt=45)

corrplot.mixed(cor(na.omit(data2[c("E_3","E_3_lag1","E_3_lag2","E_3_lag3","E_3_lag4")])),  tl.col="black", tl.srt=45)

corrplot.mixed(cor(na.omit(data2[c("E_4", "E_4_lag1","E_4_lag2","E_4_lag3","E_4_lag4")])),  tl.col="black", tl.srt=45)

corrplot.mixed(cor(na.omit(data2[c("E_1","E_2","E_3","E_4",
                                  "E_1_lag1","E_1_lag2","E_1_lag3","E_1_lag4",
                                  "E_2_lag1","E_2_lag2","E_2_lag3","E_2_lag4",
                                  "E_3_lag1","E_3_lag2","E_3_lag3","E_3_lag4",
                                  "E_4_lag1","E_4_lag2","E_4_lag3","E_4_lag4")])),  tl.col="black", tl.srt=45)


corrplot(cor(na.omit(data2[c("GDP_year", "E_1","E_2","E_3","E_4",
                                  "E_1_lag1","E_1_lag2","E_1_lag3","E_1_lag4",
                                  "E_2_lag1","E_2_lag2","E_2_lag3","E_2_lag4",
                                  "E_3_lag1","E_3_lag2","E_3_lag3","E_3_lag4",
                                  "E_4_lag1","E_4_lag2","E_4_lag3","E_4_lag4")])), type = "upper", method="number")

#############
### plots ###
#############

### Autocorrelation ACF plots
par(mfrow=c(2,2))
acf(na.omit(data2$E_1), main = "Autocorrelation of the Indicator of Question 1")
acf(na.omit(data2$E_2), main = "Autocorrelation of the Indicator of Question 2")
acf(na.omit(data2$E_3), main = "Autocorrelation of the Indicator of Question 3")
acf(na.omit(data2$E_4), main = "Autocorrelation of the Indicator of Question 4")

acf(na.omit(data2$Var_1), main = "Autocorrelation of the Variance of Question 1")
acf(na.omit(data2$Var_2), main = "Autocorrelation of the Variance of Question 2")
acf(na.omit(data2$Var_3), main = "Autocorrelation of the Variance of Question 3")
acf(na.omit(data2$Var_4), main = "Autocorrelation of the Variance of Question 4")

acf(na.omit(data2$E_I), main = "Autocorrelation of X")
acf(na.omit(data2$Var_I), main = "Autocorrelation of Var(X)")
acf(na.omit(data2$Z_I), main = "Autocorrelation of Z")
acf(na.omit(data2$Var_Z_I), main = "Autocorrelation of Var(Z)")

par(mfrow=c(1,1))

par(mfrow=c(2,1))
acf(na.omit(data2$GDP_year), main = "Autocorrelation of the GDP YoY")
acf(na.omit(data2$GDP), main = "Autocorrelation of the GDP")
par(mfrow=c(1,1))


### Correlation plots
#ggpairs(data2[c("period", "E_1", "Var_1", "Z_1", "Var_Z_1")])
#ggpairs(data2[c("period", "E_2", "Var_2", "Z_2", "Var_Z_2")])
#ggpairs(data2[c("period", "E_3", "Var_3", "Z_3", "Var_Z_3")])
#ggpairs(data2[c("period", "E_4", "Var_4", "Z_4", "Var_Z_4")])

#ggpairs(data2[c("GDP_year", "E_1", "E_2", "E_3", "E_4")])

