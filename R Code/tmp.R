
library(readxl)

library(rJava)

library(RJDemetra)



write.xlsx(recession, "GitHub/Master-Thesis/Datasets/recession.xlsx") 


# plot
data$GDP_year_plot <- with(data, interp1(Obs, GDP_year, Obs, "linear"))
data$GDP_year_plot_lag <- Lag(data$GDP_year_plot, -1)

recession <- subset(data, GDP_year_plot > GDP_year_plot_lag)


temp <- data.frame(
  start = as.Date(c('2016-11-01', '2017-11-01')), 
  end   = as.Date(c('2017-03-01', '2018-03-01')))

dateRanges <- data.frame(
  start = as.POSIXct(temp [,1], "%Y-%m-%d"),
  end   = as.POSIXct(temp [,2], "%Y-%m-%d"))



ggplot(data) + 
  geom_rect(data = dateRanges, aes(xmin=start, xmax=end, ymin=-10, xmin=10))
 
geom_line(aes(x=period, y=GDP_year_plot))





