
library(depmixS4)
library(tidyverse)
library(readxl)
library(ggplot2)
# library(gridExtra)
library(reshape2)
library(tseries)
library(parallel)
library(MSwM)
# TEST


mod <- depmix(list(Solde_UW ~ 1), data = MI027, nstates = 2,
              family = list(gaussian(), multinomial("identity")),
              transition = ~ scale(var_UW), instart = runif(2))
fm <- fit(mod, verbose = FALSE, emc=em.control(rand=FALSE))
hmm1 <- fm
summary(fm)







attach(data)

help(MSwM)

#Model with only intercept
mod<-lm(Solde_UW ~ 1, MI018)

#Fit regime-switching model
mod.mswm=msmFit(mod, k=2, sw=c(T,T), p=0)
plot(mod.mswm)











#####################################################







olsE <- lm(Solde_UW ~ 1, MI027)
olsVar <- lm(var_UW ~ 1, MI027)
ols <- lm(Industry ~ 1, MI027)

summary(olsE)


# MS for Value Stocks (k is number of regimes, 6 is for means of 5 variables
# + 1 for volatility)
msE = msmFit(olsE, k = 2, sw = rep(TRUE, 4), p=2)

msVar = msmFit(olsVar, k = 2, sw = rep(TRUE, 3), p=1)

# p= 1 is the number of AR coefficients that the MS model has to have.

msEAR = msmFit(olsE, k = 2, sw = rep(TRUE, 6), p=1)

summary(msBI)


par(mar=c(3,3,3,3))
plotProb(msE, which=1)

plotProb(msE, which=2)


par(mar=c(3,3,3,3))
plotDiag(msE, regime=1, which=1)

plotDiag(msE, regime=1, which=2)

plotDiag(msE, regime=1, which=3)



par(mar=c(3,3,3,3))
plotProb(msVar, which=1)

plotProb(msVar, which=2)


par(mar=c(3,3,3,3))
plotDiag(msVar, regime=1, which=1)

plotDiag(msVar, regime=1, which=2)

plotDiag(msVar, regime=1, which=3)





#####################################################






