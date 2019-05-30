########################################
### variable relative importance       #
########################################

library(Boruta)
library(mlbench)
# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(GDP_year ~ E_sa + Z_sa + Z2_sa + Z3_sa + Var_sa + Var_Z_sa + Var_Z2_sa + Var_Z3_sa, data=na.omit(data), doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance

# plotZHistory(boruta_output)


library(earth)
marsModel <- earth(GDP_year ~ E_sa + Z_sa + Z2_sa + Z3_sa + Var_sa + Var_Z_sa + Var_Z2_sa + Var_Z3_sa, data=na.omit(data)) # build model
ev <- evimp (marsModel) # estimate variable importance

ev

plot(ev)



library(party)
cf1 <- cforest(GDP_year ~ E_sa + Z_sa + Z2_sa + Z3_sa + Var_sa + Var_Z_sa + Var_Z2_sa + Var_Z3_sa, data=na.omit(data), control=cforest_unbiased(mtry=2,ntree=50)) # fit the random forest
tb <- sort(varimp(cf1)) # get variable importance, based on mean decrease in accuracy

stargazer(tb)


tb2 <- sort(varimp(cf1, conditional=TRUE))  # conditional=True, adjusts for correlations between predictors
stargazer(tb2)

library(relaimpo)
lmMod <- lm(GDP_year ~ E_sa + Z_sa + Z2_sa + Z3_sa + Var_sa + Var_Z_sa + Var_Z2_sa + Var_Z3_sa, data=na.omit(data))  # fit lm() model
relImportance <- calc.relimp(lmMod, type = "lmg", rela = TRUE)  # calculate relative importance scaled to 100
sort(relImportance$lmg, decreasing=TRUE)  # relative importance


