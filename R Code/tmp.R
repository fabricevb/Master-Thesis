
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-11.0.2/")



library(readxl)

data <- read_excel("MI_2008-2018_T.xlsx", sheet = "MI022")

library(rJava)
install.packages("RJDemetra")

library(RJDemetra)


data <-read_excel("C:/Users/Fabrice/Documents/RS_975.xlsx",
                   sheet="Sheet1")
attach(indu)
plot(Period, Industry)





autoplot(stl(MI027_E, s.window = 'periodic'), ts.colour = 'blue')






