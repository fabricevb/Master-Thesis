# Master Thesis: The Variability of the Belgian Business Survey Indicator - analysis and predictive power

Thesis presented in fulfillment of the requirements for the degree of Master of Science in Statistics, 2018 - 2019.

Co-supervisor: Geert Molenberghs - KU Leuven 

Co-supervisor: Laurent Van Belle - National Bank of Belgium

# Abstract

The Belgian business survey indicator, which is published monthly by the National Bank of Belgium, is a well-known indicator of the evolution of the economy.
Two different measures of its variability are proposed here; the variance of the business survey indicator (BSI) and the variance of a new indicator called the evolution of individual responses (EIR).
The first is understood as the (dis)agreement among respondents, while the second corresponds to the amount of changes in answers.

In the context of the business survey, the data is trichotomous, since there are only three possible answers to the questions of the survey. 
This comes with interesting properties as the mean-variance relation
and the bound of the variance between 0 and 1.

The survey has the particularity of being a panel survey. The new indicator of EIR takes advantage of this. The information of the evolution of each participant is compared to its previous answer.
This comes with new information which is not directly accounted for in the BSI, as the volatility of the business survey, in other words, the magnitude of participants changing their answer.

The variance of the BSI was shown as being a good predictor of GDP when using a linear regression which includes the BSI. A model including the BSI and it's variance outperforms a model only using the information of the BSI.
On the other hand, the EIR and its variance showed potential predictive power.

Aside from the predictive power, the variance of the BSI and the variance of the EIR can be used to diagnose potential bias occurring in a panel survey. A decrease of the variances can be a sign of potential attrition or dropout bias. The new indicator and the variances showed interesting interpretation properties and can be used to study a survey and better understand respondents behaviour.

# R Code

The code used for the processing, analysing and modelling of the data can be found in the folder "R code".




