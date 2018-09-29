library(tidyverse)
library(mosaic)
library(reshape2)
library(plotly)

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

ideal$Medal <- factor(ideal$Medal, levels = c("Gold", "Silver", "Bronze", "no medal"))

#####################################################################################
# Find the most common height among medal winners
#####################################################################################

#write.csv(ideal, "./Data/olympic_ideal.csv")
gold <- 
  ideal %>% 
  filter(Medal == "Gold")

silver <- 
  ideal %>% 
  filter(Medal == "Silver")

bronze <- 
  ideal %>% 
  filter(Medal == "Bronze")

nomedal <- 
  ideal %>% 
  filter(Medal == "no medal")


ggplot() + 
  geom_violin(data = gold, aes(x = Medal, y = Age), fill = "gold")  + 
  geom_violin(data = silver, aes(x = Medal, y = Age), fill = "grey69")  +
  geom_violin(data = bronze, aes(x = Medal, y = Age), fill = "brown")  +
  geom_violin(data = nomedal, aes(x = Medal, y = Age), fill = "white")  +
  facet_wrap(~Sex, ncol = 4) + 
  labs(title = "Medal Winners By Age") +
  theme_bw()

ggplot() + 
  geom_violin(data = gold, aes(x = Medal, y = Height), fill = "gold")  + 
  geom_violin(data = silver, aes(x = Medal, y = Height), fill = "grey69")  +
  geom_violin(data = bronze, aes(x = Medal, y = Height), fill = "brown")  +
  geom_violin(data = nomedal, aes(x = Medal, y = Height), fill = "white")  + 
  facet_wrap(~Sex, ncol = 4) + 
  labs(title = "Medal Winners By Height") +
  theme_bw()

ggplot() + 
  geom_violin(data = gold, aes(x = Medal, y = Weight), fill = "gold")  + 
  geom_violin(data = silver, aes(x = Medal, y = Weight), fill = "grey69")  +
  geom_violin(data = bronze, aes(x = Medal, y = Weight), fill = "brown")  +
  geom_violin(data = nomedal, aes(x = Medal, y = Weight), fill = "white")  + 
  facet_wrap(~Sex, ncol = 4) + 
  labs(title = "Medal Winners By Weight") +
  theme_bw()


ideal %>% 
  plot_ly(
    x = ~Medal,
    y = ~Age, 
    split = ~Medal,
    type = 'violin',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    )
  ) %>% 
  layout(
    xaxis = list(
      title = "Medal"
    ),
    yaxis = list(
      title = "Age",
      zeroline = T
    )
  )

ideal %>% 
  plot_ly(type = 'violin') %>% 
  add_trace(
    x = ~Medal[ideal$Sex == "M"],
    y = ~Age[ideal$Sex == "M"],
    legendgroup = 'Male',
    scalegroup = 'Male',
    name = 'Male',
    side = 'negative',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    )
  ) %>% 
  layout(
    xaxis = list(
      title = "Medal"
    ),
    yaxis = list(
      title = "Age",
      zeroline = T
    )
  ) %>% 
  add_trace(
    x = ~Medal[ideal$Sex == "F"],
    y = ~Age[ideal$Sex == "F"],
    legendgroup = 'Female',
    scalegroup = 'Female',
    name = 'Female',
    side = 'positive',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    )
  ) %>% 
  layout(
    xaxis = list(
      title = "Medal"
    ),
    yaxis = list(
      title = "Age",
      zeroline = T
    )
  )











##########
# Males
##########

cor_data_males <- 
  ideal %>% 
  filter(Sex == "M") %>% 
  select(Age, Height, Weight, Medal)
head(cor_data_males)

cormat_males <- round(cor(cor_data_males, method = "kendall"),2)
head(cormat_males)

melted_cormat_males <- melt(cormat_males)
head(melted_cormat_males)

############
# Triangle Functions
###########

# Get lower triangle of the correlation matrix
get_lower_tri <- function(cormat_males){
  cormat_males[upper.tri(cormat_males)] <- NA
  return(cormat_males)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

upper_tri_males <- get_upper_tri(cormat_males)
melted_cormat_males <- melt(upper_tri_males, na.rm = TRUE)

ggplot(data = melted_cormat_males, aes(Var2, Var1, fill = value)) +
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
  coord_fixed() +
  labs(
    title = "Age, height and weight are positively correlated with Winning Medals"
  )
ggsave(filename = "./Scratchbook/Analysis 2/cor_matrix_males.png")



###########
# Females
###########

cor_data_females <- 
  ideal %>% 
  filter(Sex == "F") %>% 
  select(Age, Height, Weight, Medal_pts)

cormat_females <- round(cor(cor_data_females),2)
head(cormat_females)

melted_cormat_females <- melt(cormat_females)
head(melted_cormat_females)

############
# Triangle Functions
###########

# Get lower triangle of the correlation matrix
get_lower_tri <- function(cormat_females){
  cormat_females[upper.tri(cormat_females)] <- NA
  return(cormat_females)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat_females){
  cormat_females[lower.tri(cormat_females)] <- NA
  return(cormat_females)
}

upper_tri_females <- get_upper_tri(cormat_females)
melted_cormat_females <- melt(upper_tri_females, na.rm = TRUE)

ggplot(data = melted_cormat_females, aes(Var2, Var1, fill = value)) +
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
  coord_fixed() +
  labs(
    title = "Age, height and weight are positively correlated with Winning Medals"
  )
ggsave(filename = "./Scratchbook/Analysis 2/cor_matrix_females.png")



















