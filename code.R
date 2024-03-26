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

# Level 2 factors
classroom6 %>%
  group_by(yearstea, mathprep) %>%
  summarize(Mean = mean(mathgain),
            SD = sd(mathgain))
# Conclusion: hard to read when unbinned, so apply binning.

means_by_class = classroom6 %>%
  group_by(classid) %>%
  mutate(means = mean(mathgain))
plot(means ~ yearstea, data = means_by_class)
plot(means ~ mathprep, data = means_by_class)
# Conclusion: lvl 2 variables have variance, form bins and use as fixed factors.


mutated_classroom6_data = classroom6 %>%
  mutate(
    yearstea_binned = cut(
      yearstea,
      breaks = 3,
      labels = c("Low", "Medium", "High")
    ),
    mathprep_binned = cut(
      mathprep,
      breaks = 3,
      labels = c("Low", "Medium", "High")
    )
  )

mutated_classroom6_data %>%
  group_by(yearstea_binned) %>%
  summarize(Mean = mean(mathgain),
            SD = sd(mathgain))
bwplot(
  mathgain ~ yearstea_binned,
  data = mutated_classroom6_data,
  aspect = 2,
  ylab = "mathgain",
  xlab = "mathprep",
  main = "Boxplots of years taught against math gain"
)
# Conclusion: binned yearstea is significant. Variances are different, and there
# are notable outliers in all groups.

mutated_classroom6_data %>%
  group_by(mathprep_binned) %>%
  summarize(Mean = mean(mathgain),
            SD = sd(mathgain))
bwplot(
  mathgain ~ mathprep_binned,
  data = mutated_classroom6_data,
  aspect = 2,
  ylab = "mathgain",
  xlab = "mathprep",
  main = "Boxplots of math prep against math gain"
)
# Conclusion: binned mathprep looks good. Extremely different variance. Mean is
# different between low / medium and high groups; consider consolidating these
# categories together.

mutated_classroom6_data %>%
  group_by(mathprep_binned, yearstea_binned) %>%
  summarize(Mean = mean(mathgain),
            SD = sd(mathgain),
            N = n())
bwplot(
  mathgain ~ mathprep_binned | yearstea_binned,
  data = mutated_classroom6_data,
  aspect = 2,
  ylab = "mathgain",
  xlab = "mathprep",
  main = "Boxplots of math prep and years taught against math gain"
)
# Conclusion: This could be viable, but note how low the mean is for high
# mathprep and high yearstea-- this is just a really small bin. There's also no
# data for low years taught and high math prep. Combining some of these bins
# would probably be beneficial. Not very strong.

classroom6 %>%
  group_by(sex, minority) %>%
  summarize(
    CorYearsTaught = cor(yearstea, mathgain),
    CorMathPrep = cor(mathprep, mathgain),
    CorMathKind = cor(mathkind, mathgain),
    CorSES = cor(ses, mathgain)
  )
# Conclusion: not much, the correlation actually isn't that descriptive.

plot(means ~ classid, data = means_by_class)
# Conclusion: class means vary by class ID, include classid as a random factor.
# First evidence of an outlier too-- class 37.

plot(mathgain ~ childid, data = classroom6)
# Conclusion: nothing much here, but a good sign of constant variance.

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


# Convert text-labeled bins to numeric (ordinal) values
mutated_classroom6_data$yearstea_binned_num <-
  as.numeric(mutated_classroom6_data$yearstea_binned)
mutated_classroom6_data$mathprep_binned_num <-
  as.numeric(mutated_classroom6_data$mathprep_binned)




# Initial Model (Full)

summary(mutated_classroom6_data)

model1.fit <-
  lme(
    mathgain ~ sex + minority + mathkind + ses + yearstea_binned_num + mathprep_binned_num,
    random = ~ 1 |
      classid,
    data = mutated_classroom6_data,
    method = "REML"
  )
summary(model1.fit)
anova(model1.fit)


# First Hypothesis: Does the random intercept on the class id matter?
# Fit Model 3.1A.
model1a.fit <-
  gls(
    mathgain ~ sex + minority + mathkind + ses + yearstea_binned_num + mathprep_binned_num,
    mutated_classroom6_data
  )
anova(model1.fit, model1a.fit) # Test hypothesis one.

# Conclusion: retain the random classid effect


# Second hypothesis: Does the random intercept on child id matter?

model1b.fit <-
  lme(
    mathgain ~ sex + minority + mathkind + ses + yearstea_binned_num + mathprep_binned_num,
    random = (list(
      classid =  ~ 1, childid =  ~ 1
    )),
    data = mutated_classroom6_data,
    method = "REML"
  )
summary(model1b.fit)
anova(model1.fit, model1b.fit) # Test hypothesis two.

# Conclusion: child ID random intercept should not be included. Model 1 is the current choice.
