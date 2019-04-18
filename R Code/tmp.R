
library(readxl)

library(rJava)

library(RJDemetra)



write.xlsx(recession, "GitHub/Master-Thesis/Datasets/recession.xlsx") 


# plot
data$GDP_year_plot <- with(data, interp1(Obs, GDP_year, Obs, "linear"))
data$GDP_year_plot_lag <- Lag(data$GDP_year_plot, -1)

recession <- subset(data, GDP_year_plot > GDP_year_plot_lag)



ggplot(data) + 
  geom_line(aes(x=period, y=GDP_year_plot)) +
  geom_rect(data = recession, aes(xmin=min(period), xmax=max(period), ymin=-10, xmin=10))






