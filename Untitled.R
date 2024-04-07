# Loading libraries and data
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
data <- read_excel("~/Desktop/impdata.xlsx")

# Calculating the survival percentage for each clutch at 24 hours post-fertilization
data$survival_percentage24 <- (data$live_24h / data$total_24h) * 100

# Creating a strip plot comparing survival percentages for each temperature condition
ggplot(data, aes(x = factor(temp), y = survival_percentage24, color = factor(temp))) +
  geom_jitter(position = position_jitter(width = 0.1), size = 3) +
  stat_summary(fun = mean, 
               fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)), 
               fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom = 'errorbar',  width = 0.1, color = "gray14") +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = 'errorbar',  width = 0.2, color = "gray14") +  
  labs(x = "Temperature (°C)", y = "Embryo Survival Percentage (%)") +
  ggtitle("Survival Percentage by Temperature at 24hrs") +
  scale_color_manual(values = c("28" = "skyblue", "34" = "darkblue"))  # Customizing colors for clarity

# Statistical analysis 1
# Mann-Whitney U
wilcox_test_result <- wilcox.test(survival_percentage24 ~ temp, data = data)
wilcox_test_result

# Calculating the survival percentage for each clutch at 48 hours post-fertilization
data$survival_percentage48 <- (data$live_48h / data$total_48h) * 100

# Creating a strip plot comparing survival percentages for each temperature condition
ggplot(data, aes(x = factor(temp), y = survival_percentage48, color = factor(temp))) +
  geom_jitter(position = position_jitter(width = 0.1), size = 3) +
  stat_summary(fun = mean, 
               fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)), 
               fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom = 'errorbar',  width = 0.1, color = "gray14") +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = 'errorbar',  width = 0.2, color = "gray14") +  
  labs(x = "Temperature (°C)", y = "Embryo Survival Percentage (%)") +
  ggtitle("Survival Percentage by Temperature at 48hrs") +
  scale_color_manual(values = c("28" = "skyblue", "34" = "darkblue"))  # Customizing colors for clarity

# Statistical analysis 2
# Mann-Whitney U
wilcox_test_result <- wilcox.test(survival_percentage48 ~ temp, data = data)
wilcox_test_result