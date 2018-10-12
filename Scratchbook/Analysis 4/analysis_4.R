library(tidyverse)
library(car)
library(DT)
library(mosaic)
library(pander)
library(lattice)


# States (and motivates) an interesting question.


# States a null and alternative hypothesis.

# The null hypothesis is there is no difference between the medians of both groups.
# The alternative hypothesis is there is a difference between the medians of both groups.

if FALSE {
  $$
    H_0: \text{median of the differences} = 0
  $$
    
  $$
    H_a: \text{median of the differences} \neq 0
  $$
 
  # States the significance level, alpha.

  $$
    \alpha = 0.05
  $$
}


# Presents a graphic. 
before <- Friendly %>% filter(condition == "Before")
meshed <- Friendly %>%  filter(condition == "Meshed")

par(mfrow = c(1,2))
qqPlot(before$correct, xlab = "Normal quantiles",
       ylab = "correct", main = "Before test group")
qqPlot(meshed$correct, xlab = "Normal quantiles",
       ylab = "correct", main = "Meshed Test group")

stripchart(before$correct, pch = 16, method = "stack")
stripchart(meshed$correct, pch = 16, method = "stack")
# Presents appropriate numerical summaries.


# Performs and presents an appropriate Wilcoxon Test.


# Discusses whether or not the Wilcoxon Test was appropriate for the data.


# Interprets the results in a meaningful way that gives useful insight to the reader.