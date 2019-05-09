

# Load packages

library(ggplot2)
library(tidyverse)
library(stargazer)
library(GGally)
library(lattice)
library(survival)
library(Formula)
library(Hmisc)
library(corrplot)
library(readxl)
library(corrplot)
library(reshape2)


data <- read_excel("GitHub/Master-Thesis/Datasets/RS975_not_sa.xlsx")



lims <- as.POSIXct(strptime(c("1988-01-01 00:00","2018-01-01 00:00"), format = "%Y-%m-%d %H:%M"))    

tmp <- data[c("date", "Ap_I", "A0_I", "An_I")]

meltdf <- melt(tmp,id="date")
meltdf$date <- as.Date(meltdf$date)
group.colors <- c(Ap_I = "green4", A0_I = "grey85", An_I ="red")
ggplot(meltdf,aes(x=date,y=value,fill=variable, color)) + 
  geom_area(aes(y=value)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_x_date(limits = c(min,max), date_breaks = "1 year", date_labels = "%Y") +
#  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_manual(values=group.colors, labels=c("proportion of postive answers","proportion of neutral answers", "proportion of negative answers")) 

