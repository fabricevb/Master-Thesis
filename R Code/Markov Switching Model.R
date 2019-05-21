
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


mod <- depmix(list(GDP_year ~ 1), data = data, nstates = 2,
              family = list(gaussian()),
              transition = ~ scale(E_sa), instart = runif(2))
fm <- fit(mod, verbose = FALSE, emc=em.control(rand=FALSE))
summary(fm)

est.states <- posterior(fm)
head(est.states)


plot.hmm.output <- function(model.output){
  g0 <- (ggplot(model.output$draws, aes(x = roll, y = obs)) + geom_line() +
           theme(axis.ticks = element_blank(), axis.title.y = element_blank())) %>% ggplotGrob
  g1 <- (ggplot(model.output$draws, aes(x = roll, y = state, fill = state, col = state)) + 
           geom_bar(stat = "identity", alpha = I(0.7)) + 
           scale_fill_manual(values = mycols, name = "State:\nPerson that\nrolled the\ndice", labels = c("Alice", "Bob")) +
           scale_color_manual(values = mycols, name = "State:\nPerson that\nrolled the\ndice", labels = c("Alice", "Bob")) +
           theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
           labs(y = "Actual State")) %>% ggplotGrob
  g2 <- (ggplot(model.output$draws, aes(x = roll, y = est.state.labels, fill = est.state.labels, col = est.state.labels)) + 
           geom_bar(stat = "identity", alpha = I(0.7)) +
           scale_fill_manual(values = mycols, name = "State:\nPerson that\nrolled the\ndice", labels = c("Alice", "Bob")) +
           scale_color_manual(values = mycols, name = "State:\nPerson that\nrolled the\ndice", labels = c("Alice", "Bob")) +
           theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + 
           labs(y = "Estimated State")) %>% ggplotGrob
  g3 <- (ggplot(model.output$hmm.post.df, aes(x = roll, y = value, col = variable)) + geom_line() +
           scale_color_manual(values = mycols, name = "State:\nPerson that\nrolled the\ndice", labels = c("Alice", "Bob")) +
           theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + 
           labs(y = "Posterior Prob.")) %>%
    ggplotGrob()
  g0$widths <- g1$widths
  return(grid.arrange(g0, g1, g2, g3, widths = 1, nrow = 4))
}
plot.hmm.output(hmm1)

plot.hmm.output(fm)


attach(data)

help(MSwM)


#Fit regime-switching model
mod.mswm=msmFit(model1, k=2, sw=c(T,T,T,T), p=1)
mod.mswm=msmFit(model2, k=2, sw=c(F,F,F,F,F), p=1)

plotProb(mod.mswm, which = 2)
plotProb(mod.mswm, which = 1)
plotDiag(mod.mswm)



autoplot(model1)
autoplot(model2)
autoplot(model3)
autoplot(model4)
autoplot(model5)



#####################################################





summary(olsE)


# MS for Value Stocks (k is number of regimes, 6 is for means of 5 variables
# + 1 for volatility)
msE = msmFit(model2, k = 2, sw = rep(TRUE, 4), p=0)
plotProb(msE)

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


msp <- depmix(GDP_year ~ 1,nstates=2,data=data)
set.seed(1)
fmsp <- fit(msp)	

# plot posterior state sequence for the 2-state model
plot(ts(posterior(fmsp)[,2], start=c(1988,2),deltat=1/12),ylab="probability",
     main="Posterior probability of state 1",
     frame=FALSE)


