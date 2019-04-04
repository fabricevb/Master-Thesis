
library(readxl)
data <- read_excel("GitHub/Master-Thesis/Datasets/RS975_sa.xlsx")


data_dt <- data[c("E_1", "E_2", "E_3", "E_4")] # remove year and quarter
pigs_dt$profit_group <- as.numeric(pigs_dt$profit > mean(pigs_dt$profit))
qplot(
  time, value,
  data = reshape::melt.data.frame(pigs_dt, "time"),
  geom = c("smooth", "point")
) +
  facet_grid(variable ~ ., scales = "free_y")




