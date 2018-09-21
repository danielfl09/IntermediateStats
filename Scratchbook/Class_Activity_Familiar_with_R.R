library(mosaic)
library(tidyverse)

View(airquality)

x <- 
  na.omit(airquality)

x %>% 
  ggplot(aes(y = Wind, group = Month, color = "steelblue")) +
  geom_boxplot() +
  theme_bw()


boxplot(airquality$Wind)
