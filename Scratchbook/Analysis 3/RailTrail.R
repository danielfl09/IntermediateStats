library(tidyverse)
library(mosaic)
library(reshape2)
library(plotly)
library(pander)

options(scipen = 1)

# Add new columns for Weekday and Weekend in the style as the season columns
railtrail <- RailTrail %>% 
  mutate(
    Weekday = case_when(
      dayType == "weekday" ~ 1,
      TRUE                 ~ 0
    ),
    Weekend = case_when(
      dayType == "weekend" ~ 1,
      TRUE                 ~ 0
    )
  ) %>% 
  select(-c(weekday, dayType))

#write.csv(railtrail, "./Data/railtrail.csv")

# Step 1
cormat_trail <- round(cor(railtrail),2)
head(cormat_trail)

# Step 2
melted_cormat <- melt(cormat_trail)
head(melted_cormat)

# Step 3
get_upper_tri <- function(data_set){
  data_set[lower.tri(data_set)] <- NA
  return(data_set)
}

upper_tri_trai <- get_upper_tri(cormat_trail)
melted_upper_tri <- melt(upper_tri_trai, na.rm = TRUE)
head(melted_upper_tri)

# Step 4
ggplot(data = melted_upper_tri, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") + 
  geom_text(aes(label = value), color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "lab",
                       name = "Pearson\nCorrelation") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.40, 0.75),
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  coord_fixed() +
  labs(
    title = "There is a strong positive correlation between high temperatures and volume of park visitors."
  )

rail.scale <- as.data.frame(scale(railtrail))
View(rail.scale)

# Layered Density Plot of High Temperature and Volume of Visitors
plot_1 <- ggplot() +
  geom_density(data = rail.scale,
                 aes(x = hightemp),
               fill = "red",
               alpha = 0.25) +
  geom_density(data = rail.scale,
                 aes(x = volume),
               fill = "blue",
               alpha = 0.25) +
  scale_fill_identity(name = "Legend",
                      guide = 'legend',
                      labels = c("High Temperature",
                                 "Volume of Visitors")) +
  theme_bw()

plot_1


# Student's t-Test
pander(t.test(railtrail$volume, railtrail$hightemp, mu = 0, alternative = "two.sided"))


# Faceted Density Plot
railtrail %>% 
  ggplot(aes(x = volume, y = hightemp)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_y_continuous(breaks = seq(40,100,by = 10)) +
  scale_x_continuous(breaks = seq(100,800,by = 100)) +
  theme_bw()

ggplot() +
  geom_density(data = rail.scale,
               aes(x = volume),
               fill = "blue",
               alpha = 0.25) +
  theme_bw()

# Adjust layout of the graphs
hist(rail.scale$hightemp, breaks=20, xlim=c(-3,3), col=rgb(1,0,0,0.5))
hist(rail.scale$volume, breaks=20, col=rgb(0,0,1,0.5), add=T)
box()


