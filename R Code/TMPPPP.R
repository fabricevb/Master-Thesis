

library(MSwM)





ols <- lm(Solde_UW ~ Ap_p + A0_p + An_p, MI027)

summary(ols)


# MS for Value Stocks (k is number of regimes, 6 is for means of 5 variables
# + 1 for volatility)
ms = msmFit(ols, k = 3, sw = rep(TRUE, 7), p=2)

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








library(HiddenMarkov)
demo("norm", package="HiddenMarkov")




