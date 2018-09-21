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

airquality %>% 
  ggplot(aes(x = Solar.R)) +
  geom_bar(binwidth = 50, fill = "orange", color = "black") +
  theme_bw()
