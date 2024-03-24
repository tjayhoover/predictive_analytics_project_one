# Adam Rilatt and Tyler Hoover
# 20 March 2024
# Predictive Analytics Group Project 1

library(dplyr)

load("classroom6.RData")

summary(classroom6)

# a.

# Level 1 fixed factors
classroom6 %>%
  group_by(sex) %>%
  summarize(Mean = mean(mathgain),
            SD = sd(mathgain))
# Difference in means and difference in SD by 3.5% and 9.7% respectively.

classroom6 %>%
  group_by(minority) %>%
  summarize(Mean = mean(mathgain),
            SD = sd(mathgain))
# Difference in means 3.9%, difference in SD 4.3%.

# Level 1 fixed factor interaction terms
classroom6 %>%
  group_by(sex, minority) %>%
  summarize(Mean = mean(mathgain),
            SD = sd(mathgain))

# Level 2 factors
classroom6 %>%
  group_by(sex, minority) %>%
  summarize(CorYearsTaught = cor(yearstea, mathgain),
            CorMathPrep = cor(mathprep, mathgain))
# Note the different slopes for both yearstea, mathprep, and SES-- these should
# all have split coefficients by sex and/or minority until later testing.

# 

# TODO: More of these for level 1(?), then for level 2