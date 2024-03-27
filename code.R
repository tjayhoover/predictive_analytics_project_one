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


# ---------------------- Summary Stats -----------------------------------------

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



# ----------------------- Binning by yearstea and mathprep -------------------------

full_df = classroom6 %>%
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

# Convert text-labeled bins to numeric (ordinal) values
full_df$yearstea_binned_num <-
  as.numeric(full_df$yearstea_binned)
full_df$mathprep_binned_num <-
  as.numeric(full_df$mathprep_binned)

# Note: full_df is the full dataframe with the binned columns, both
# ordinal (numeric) and purely qualitative text-based levels


# ------------------- Summary stats and BW plots -------------------------

full_df %>%
  group_by(yearstea_binned) %>%
  summarize(Mean = mean(mathgain),
            SD = sd(mathgain))
bwplot(
  mathgain ~ yearstea_binned,
  data = full_df,
  aspect = 2,
  ylab = "mathgain",
  xlab = "mathprep",
  main = "Boxplots of years taught against math gain"
)
# Conclusion: binned yearstea is significant. Variances are different, and there
# are notable outliers in all groups.

full_df %>%
  group_by(mathprep_binned) %>%
  summarize(Mean = mean(mathgain),
            SD = sd(mathgain))


bwplot(
  mathgain ~ mathprep_binned,
  data = full_df,
  aspect = 2,
  ylab = "mathgain",
  xlab = "mathprep",
  main = "Boxplots of math prep against math gain"
)
# Conclusion: binned mathprep looks good. Extremely different variance. Mean is
# different between low / medium and high groups; consider consolidating these
# categories together.

full_df %>%
  group_by(mathprep_binned, yearstea_binned) %>%
  summarize(Mean = mean(mathgain),
            SD = sd(mathgain),
            N = n())
bwplot(
  mathgain ~ mathprep_binned | yearstea_binned,
  data = full_df,
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


# ----------- Mathkind vs Mathgain scatterplots by various groups ----------------

# Create a scatterplot with different colors for each level of the third variable
ggplot(classroom6, aes(x = mathkind, y = mathgain, color = classid)) +
  geom_point() +
  labs(x = "Mathkind", y = "Mathgain", color = "classid") +
  theme_minimal()


# Create a scatterplot with different colors for each level of the third variable
ggplot(classroom6, aes(x = mathkind, y = mathgain, color = sex)) +
  geom_point() +
  labs(x = "Mathkind", y = "Mathgain", color = "sex") +
  theme_minimal()

# Create a scatterplot with different colors for each level of the third variable
ggplot(full_df, aes(x = mathkind, y = mathgain, color = yearstea_binned)) +
  geom_point() +
  labs(x = "Mathkind", y = "Mathgain", color = "Years Taught") +
  theme_minimal()


# Create separate scatterplots for each level of the qualitative variable
plots <- lapply(unique(full_df$yearstea_binned), function(level) {
  p <-
    ggplot(full_df[full_df$yearstea_binned == level,], aes(x = mathkind, y = mathgain)) +
    geom_point() +
    labs(x = "Mathkind", y = "Mathgain") +
    ggtitle(paste("Level:", level)) +
    theme_minimal()
  return(p)
})

# Print the plots
plots

# Note: the color-coded scatter plots are of limited utilitity. It is hard to
# deduce differences in the relationship between mathkind and math




# ------------------------ Model Specification and Hypothesis Tests ---------------------


# Initial Model (All fixed factors, random intercept by class ID

model1.fit <-
  lme(
    mathgain ~ sex + minority + mathkind + ses + yearstea_binned_num + mathprep_binned_num:yearstea_binned_num + mathkind:minority + mathprep_binned_num,
    random = ~ 1 |
      classid,
    data = full_df,
    method = "REML"
  )
summary(model1.fit)
anova(model1.fit)


# First Hypothesis: Does the random intercept on the class id matter?
# Fit Model 3.1A.
model1a.fit <-
  gls(
    mathgain ~ sex + minority + mathkind + ses + yearstea_binned_num + mathprep_binned_num:yearstea_binned_num + mathkind:minority + mathprep_binned_num,
    full_df
  )
anova(model1.fit, model1a.fit) # Test hypothesis one.

# Conclusion: retain the random classid effect - it is very significant


# Second hypothesis: Does the random intercept on child id matter?

model1b.fit <-
  lme(
    mathgain ~ sex + minority + mathkind + ses + yearstea_binned_num + mathprep_binned_num:yearstea_binned_num + mathkind:minority + mathprep_binned_num,
    random = (list(
      classid =  ~ 1, childid =  ~ 1
    )),
    data = full_df,
    method = "REML"
  )
summary(model1b.fit)
anova(model1.fit, model1b.fit) # Test hypothesis two.

# Conclusion: child ID random intercept should not be included. Model 1 is the current choice.


# Third hypothesis: Does a random slope effect on mathkind based on class id matter?
model2.fit <-
  lme(
    mathgain ~ sex + minority + mathkind + ses + yearstea_binned_num + mathprep_binned_num:yearstea_binned_num + mathkind:minority + mathprep_binned_num,
    random = ~ mathkind |
      classid,
    data = full_df,
    method = "REML"
  )
summary(model2.fit)
anova(model2.fit)

anova(model1.fit, model2.fit) # Test hypothesis three

# Conclusion: yes, the random effect on the slope of mathkind based on classid is signficant.
# Model 2 is the current best model.