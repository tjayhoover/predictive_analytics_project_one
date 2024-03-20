# Adam Rilatt and Tyler Hoover
# 20 March 2024
# Predictive Analytics Group Project 1

library(dplyr)

load("classroom6.RData")

summary(classroom6)

# a.
classroom6 %>%
  group_by(sex) %>%
  summarize(Mean = mean(mathgain),
            SD = sd(mathgain))

# TODO: More of these for level 1, then for level 2