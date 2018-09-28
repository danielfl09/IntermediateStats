library(tidyverse)
library(mosaic)
library(reshape2)

# Create a Correlation matirx
# Following example found here: "http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization"
olympic <- read_csv("./Data/Olympics.csv")

olympic$Medal[is.na(olympic$Medal)] <- "no medal"

ideal <-
  olympic %>%
  select(ID, Sex, Age, Height, Weight, Sport, Medal) %>%
  na.omit() %>%
  mutate(Medal_pts = case_when(
    Medal == "Gold" ~ 3,
    Medal == "Silver" ~ 2,
    Medal == "Bronze" ~ 1,
    TRUE ~ 0
  ))

cor_data <- 
  ideal %>% 
  select(Age, Height, Weight, Medal_pts)

cormat <- round(cor(cor_data),2)
head(cormat)

melted_cormat <- melt(cormat)
head(melted_cormat)

# Get lower triangle of the correlation matrix
get_lower_tri <- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") + 
  geom_text(aes(label = value), color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "lab",
                       name = "Pearson\nCorrelation") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.40, 0.75),
        legend.direction = "horizontal") + 
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  coord_fixed()
  