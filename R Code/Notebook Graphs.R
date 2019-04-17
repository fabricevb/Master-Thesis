
# notebook 


# Library
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)
library(dplyr)

# Read the data (hosted on the gallery website)
# data <- read_excel("Y:/TRICONAT/EMOS/Stage2018/Datasets/RS975_sa.xlsx",
#                   sheet="Sheet1")
str(data)

data <- data[1:372,]

# Since my time is currently a factor, I have to convert it to a date-time format!
data$datetime = ymd_hms(data$period)

# Then you can create the xts format, and thus use dygraph
don=xts(x = data$E_I, order.by = data$period)
dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)


library(ggfortify)

E_3 <- data_ts[, "E_3"]

autoplot(E_I)

datatmp <- data[1:372,]
ggplot(data = datatmp, aes(x=period, y=E_I)) + geom_line(color = "steelblue") # +  theme_minimal()



datatmp <- data[1:372,]
p <- ggplot(data = datatmp, aes(period)) +  
                geom_line(aes(y = E_1, colour="E_1" )) +
                geom_line(aes(y = E_2, colour="E_2" )) +
                geom_line(aes(y = E_3, colour="E_3" )) +
                geom_line(aes(y = E_4, colour="E_4" ))  

p

ggsave("mon_graphique.pdf", plot = p, width = 11, height = 8)


tmp <- select(data,-c(datetime, GDP_EU28))
tmp <- na.omit(tmp)

library(ggExtra)

p <- ggplot(data = tmp, aes(x=period, y=GDP)) + geom_line(color = "steelblue") 
p

p <- ggplot(data = tmp, aes(x=period, y=GDP)) + geom_point()
ggMarginal(p, type = "histogram")








