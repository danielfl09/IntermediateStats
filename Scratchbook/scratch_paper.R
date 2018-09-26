library(tidyverse)

Rent <- read_csv("./Data/Rent.csv") %>% 
  na.omit()

Rent2 <- 
  Rent %>% 
  