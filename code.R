# Adam Rilatt and Tyler Hoover
# 20 March 2024
# Predictive Analytics Group Project 1

library(dplyr)
library(nlme)
library(lattice)
library(readxl)
library(ggplot2)
library(grid)

load("classroom6.RData")

summary(classroom6)

# a.

# Level 1 fixed factors
classroom6 %>%
  group_by(sex) %>%
  summarize(
    'N Obs' = n(),
    Mean = mean(mathgain),
    SD = sd(mathgain),
    Min = min(mathgain),
    Max = max(mathgain)
  )
# Difference in means and difference in SD by 3.5% and 9.7% respectively.

classroom6 %>%
  group_by(minority) %>%
  summarize(
    'N Obs' = n(),
    Mean = mean(mathgain),
    SD = sd(mathgain),
    Min = min(mathgain),
    Max = max(mathgain)
  )
# Difference in means 3.9%, difference in SD 4.3%.

# Level 1 fixed factor interaction terms
classroom6 %>%
  group_by(sex, minority) %>%
  summarize(
    'N Obs' = n(),
    Mean = mean(mathgain),
    SD = sd(mathgain),
    Min = min(mathgain),
    Max = max(mathgain)
  )

# Level 2 factors
classroom6 %>%
  group_by(sex, minority) %>%

  summarize(CorYearsTaught = cor(yearstea, mathgain),
            CorMathPrep = cor(mathprep, mathgain))
# Note the different slopes for both yearstea, mathprep, and SES-- these should
# all have split coefficients by sex and/or minority until later testing.

# 

# TODO: More of these for level 1(?), then for level 2


# Box and whisker plots

bwplot(
  mathgain ~ sex |
    minority,
  data = classroom6,
  aspect = 2,
  ylab = "mathgain",
  xlab = "sex",
  main = "Boxplots of math gain by minority and sex"
)


# 
# class.first8 <- classroom6[classroom6$schoolid <= 8, ]
# par(mfrow = c(4, 2))
# 
# for (i in 1:8)
# {
#   boxplot(class.first8$mathgain[class.first8$schoolid == i] ~ class.first8$classid[class.first8$schoolid == i])
# }
