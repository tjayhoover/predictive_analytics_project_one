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
# Conclusion: include as a fixed factor, possibly as a variance term for
# residuals...

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
# Conclusion: include as a fixed factor.

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

plot(mathgain ~ mathkind, data = classroom6)
# Conclusion: strong fixed factor

plot(mathgain ~ ses, data = classroom6)
# Nothing good...


#  TODO: add the rest from the Word doc.

# Level 2 factors
classroom6 %>%
  group_by(sex, minority) %>%
  summarize(CorYearsTaught = cor(yearstea, mathgain),
            CorMathPrep = cor(mathprep, mathgain),
            CorMathKind = cor(mathkind, mathgain),
            CorSES = cor(ses, mathgain))
# Note the different slopes for both yearstea, mathprep, mathkind, SES-- these
# should all have split coefficients by sex and/or minority until later testing.

# TODO: We should fit some minimal linear regression models using these and see
# how the parameters change; looking at correlation doesn't tell us as much as
# we'd hope.


means_by_class = classroom6 %>%
  group_by(classid) %>%
  mutate(means = mean(mathgain))
plot(means ~ yearstea, data = means_by_class)
plot(means ~ mathprep, data = means_by_class)
# Conclusion: lvl 2 variables have variance, form bins and use as fixed factors.

plot(means ~ classid, data = means_by_class)
# Conclusion: bin classID as a random factor / or residual term
# TODO: which one?

plot(mathgain ~ childid, data = classroom6)
# Conclusion: not much here, but a good candidate for residual structure.
# Looks to have equal variance


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
