
library(readxl)

library(rJava)

library(RJDemetra)





# plot
data$GDP_year_plot <- with(data, interp1(Obs, GDP_year, Obs, "linear"))
data$GDP_year_plot_lag <- Lag(data$GDP_year_plot, -1)

recession <- subset(data, GDP_year_plot > GDP_year_plot_lag)

geom_area(mapping = aes(x = ifelse(x>65 & x< 70 , x, 0)), fill = "red")


temp <- data.frame(
  start = as.Date(c('2016-11-01', '2017-11-01')), 
  end   = as.Date(c('2017-03-01', '2018-03-01')))

dateRanges <- data.frame(
  start = as.POSIXct(temp [,1], "%Y-%m-%d"),
  end   = as.POSIXct(temp [,2], "%Y-%m-%d"))

data$date <- as.Date(data$period)

ggplot(data,  mapping = aes(x = date, y = E_I)) + 
  geom_line() +
  geom_area(mapping = aes(x = ifelse(E_I> 0 , E_I, 0)), fill = "red")





par(mfrow=c(1,3))

ggplot(data,aes(x=date,y=E_I)) + 
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=45, hjust = 1))

p + facet_grid(rows = vars(variables))



tmp <- data[c("date", "GDP_year_plot", "E_I", "Var_I", "Z_I", 
              "Var_Z_I")]

meltdf <- melt(tmp,id="date")
meltdf$GDP_year <- data$GDP_year
meltdf$date <- as.Date(meltdf$date)
ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + 
  geom_line(na.rm=FALSE) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  facet_grid(rows = vars(variable), scales="free")









