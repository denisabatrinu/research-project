library(tidyverse)
library(readxl)
library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(Rmisc)
library(DHARMa)
library(car)

# Load the data via a path
data <- read_excel("~/Desktop/impdata.xlsx")

# Calculating the hatching percentage for each clutch at 48 hours post-fertilization
data$hatching_percentage <- (data$hatched / data$live_48h) * 100

# Creating a strip plot comparing hatching percentages
ggplot(data, aes(x = factor(temp), y = hatching_percentage, color = factor(temp))) +
  geom_jitter(position = position_jitter(width = 0.1), size = 3) +
  stat_summary(fun = mean, 
               fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)), 
               fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom = 'errorbar',  width = 0.1, color = "gray14") +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = 'errorbar',  width = 0.2, color = "gray14") +  
  labs(x = "Temperature (°C)", y = "Hatching Percentage (%)") +
  ggtitle("Hatching Percentage by Temperature at 48hrs") +
  scale_color_manual(values = c("28" = "skyblue", "34" = "darkblue"))  # Customizing colors for clarity

# Statistical analysis 1
# Mann-Whitney U
wilcox_test_result <- wilcox.test(hatching_percentage ~ temp, data = data)
wilcox_test_result
