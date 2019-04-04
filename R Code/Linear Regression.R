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



###############
# Correlation #
###############

cor(MI027)

ggpairs(data[, 1:2])

ggpairs(MI027[, 2:4], lower=list(continuous="smooth", params=c(colour="blue")),
        diag=list(continuous="bar"))

library("GGally")
data(iris)
ggpairs(iris[, 1:4], lower=list(continuous="smooth", params=c(colour="blue")),
        diag=list(continuous="bar", params=c(colour="blue")), 
        upper=list(params=list(corSize=6)), axisLabels='show')
#################
# Linear Models #
#################
attach(MI027)

#model <- lm(GDP ~ solde_UW, MI027)

model <- lm(Value ~ ind, MI027)

model <- lm(Value ~ ind + var + ind_Z + var_Z, MI027)

summary(model)

plot(Solde_UW, var_UW)


################################
# Linear Autoregressive Models #
################################

