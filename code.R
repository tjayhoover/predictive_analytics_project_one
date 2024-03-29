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

# What should the bins be?
histogram(~ mathprep, data = classroom6)
# Conclusion: 0-2.5 mathprep, 2.5-3 mathprep, 3 and higher mathprep.

histogram(~ yearstea, data = classroom6)
# Conclusion: even thirds is pretty good.

# ----------------------- Binning by yearstea and mathprep -------------------------

full_df = classroom6 %>%
#[classroom6$childid != 41,] %>%
  mutate(
    yearstea_binned = cut(
      yearstea,
      breaks = 3, # even thirds
      labels = c("Low", "Medium", "High")
    ),
    mathprep_binned = cut(
      mathprep,
      breaks = c(0, 2.5, 3, 5),
      labels = c("Low", "Medium", "High")
    )
  )

# Convert text-labeled bins to numeric (ordinal) values
full_df$yearstea_binned_num[full_df$yearstea_binned == "Low"] <- 1
full_df$yearstea_binned_num[full_df$yearstea_binned == "Medium"] <- 2
full_df$yearstea_binned_num[full_df$yearstea_binned == "High"] <- 3
# full_df$yearstea_binned_num <-
#   as.numeric(full_df$yearstea_binned)
# full_df$mathprep_binned_num <-
#   as.numeric(full_df$mathprep_binned)
full_df$mathprep_binned_num[full_df$mathprep_binned == "Low"] <- 1
full_df$mathprep_binned_num[full_df$mathprep_binned == "Medium"] <- 2
full_df$mathprep_binned_num[full_df$mathprep_binned == "High"] <- 3

full_df$mathprep_binned_num = as.factor(full_df$mathprep_binned_num)
full_df$yearstea_binned_num = as.factor(full_df$yearstea_binned_num)

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
  xlab = "yearstea",
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

plot(mathgain ~ childid, data = full_df)
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

ggplot(full_df, aes(x = mathkind, y = mathgain, color = yearstea_binned)) +
  geom_point() +
  labs(x = "Mathkind", y = "Mathgain", color = "Years Taught") +
  theme_minimal()


# Create separate scatterplots for each level of the qualitative variable
plots <- lapply(unique(full_df$yearstea_binned), function(level) {
  p <-
    ggplot(full_df[full_df$yearstea_binned == level, ], aes(x = mathkind, y = mathgain)) +
    geom_point() +
    labs(x = "Mathkind", y = "Mathgain") +
    ggtitle(paste("Level:", level)) +
    theme_minimal()
  return(p)
})

# Print the plots
plots


# Create scatterplots with best fit lines
ggplot(full_df, aes(x = mathkind, y = mathgain, color = classid)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add best fit lines
  labs(x = "Mathkind", y = "Mathgain", title = "Scatterplot with Best Fit Lines") +
  theme_minimal() + 
  facet_wrap(~ classid, scales = "free")

# Note: the color-coded scatter plots are of limited utilitity. It is hard to
# deduce differences in the relationship between mathkind and mathgain.




# ----------------- Model Specification and Hypothesis Tests -------------------

# ------------ Step 1: Fit a model with a loaded mean structure ----------------


# Initial Model (All fixed factors, random intercept by class ID)

model1.fit <-
  lme(
    mathgain ~ sex + minority + mathkind + ses + yearstea_binned_num + mathprep_binned_num:yearstea_binned_num + mathkind:minority + mathprep_binned_num,
    random = ~ 1 | classid,
    data = full_df,
    method = "REML"
  )
summary(model1.fit)
anova(model1.fit)



# ------------ Step 2: Select a structure for the random effects ---------------


# Hypothesis 1: Does the random intercept on the class id matter?
# Fit Model 3.1A (Note: no random effects, so need to use gls.)
model1a.fit <-
  gls(
    mathgain ~ sex + minority + mathkind + ses + yearstea_binned_num + mathprep_binned_num:yearstea_binned_num + mathkind:minority + mathprep_binned_num,
    full_df
  )
summary(model1a.fit)
anova(model1.fit, model1a.fit) # Test hypothesis one.

# Conclusion: retain the random classid effect - it is very significant


# Hypothesis 2: Does a random slope effect on mathkind based on class id matter?
model2.fit <-
  lme(
    mathgain ~ sex + minority + mathkind + ses + yearstea_binned_num + mathprep_binned_num:yearstea_binned_num + mathkind:minority + mathprep_binned_num,
    random = ~ mathkind | classid,
    data = full_df,
    method = "REML"
  )
summary(model2.fit)
anova(model2.fit)


anova(model1.fit, model2.fit) # Test hypothesis three

# Conclusion: yes, the random effect on the slope of mathkind based on classid is significant.
# Model 2 is the current best model.



# ----------- Step 3: Select a covariance structure for the residuals ----------


# Hypothesis 3: Is a four-way residual split by minority and sex helpful?
model3.fit <-
  lme(
    mathgain ~ sex + minority + mathkind + ses + yearstea_binned_num + mathprep_binned_num:yearstea_binned_num + mathkind:minority + mathprep_binned_num,
    random = ~ mathkind | classid,
    data = full_df,
    method = "REML",
    weights = varIdent(form = ~ 1 | minority * sex)
  )
summary(model3.fit)
anova(model3.fit)


anova(model2.fit, model3.fit) # Test hypothesis four


# Hypothesis 4: What if we split off only minority = 1 and sex = 0 from the rest of the pack in the residuals
# (That is where I noticed the most variance)

full_df$f_min_grp[full_df$minority == 1 & full_df$sex == 0] <- 1
full_df$f_min_grp[full_df$minority == 0 | full_df$sex == 1] <- 2

model3a.fit <-
  lme(
    mathgain ~ sex + minority + mathkind + ses + yearstea_binned_num + mathprep_binned_num:yearstea_binned_num + mathkind:minority + mathprep_binned_num,
    random = ~ mathkind | classid,
    data = full_df,
    method = "REML",
    weights = varIdent(form = ~ 1 | f_min_grp)
  )
summary(model3a.fit)
anova(model3a.fit)


anova(model3.fit, model3a.fit) # Test hypothesis five

# The test is not significant - so we select the nested model in this case
# Model 3a with a 2-way split of the residuals between minority - 1, sex - 0
# and the other groups combined is the best model at this point.


# Hypothesis 5: Is a further residual split by years taught high vs med and low
# useful?

full_df$highyt_grp[full_df$yearstea_binned == "Low" |
                     full_df$yearstea_binned == "Medium"] <- 0
full_df$highyt_grp[full_df$yearstea_binned == "High"] <- 1

model3b.fit <-
  lme(
    mathgain ~ sex + minority + mathkind + ses + yearstea_binned_num + mathprep_binned_num:yearstea_binned_num + mathkind:minority + mathprep_binned_num,
    random = ~ mathkind | classid,
    data = full_df,
    method = "REML",
    weights = varIdent(form = ~ 1 | f_min_grp * highyt_grp)
  )
summary(model3b.fit)
anova(model3b.fit)

anova(model3a.fit, model3b.fit) # Test hypothesis six
# Results - not significant. Thus, adding the extra residual variance parameter
# differentiating experienced teachers did not improve our model.
# Model 3a remains the best.



# ------ Step 4: Reduce the model by removing nonsignificant fixed effects -----


# Hypothesis 6: Is the minority:mathkind interaction term significant?
# Test hypothesis 7 (Type I test)
anova(model3a.fit)

# Based on this Type 1 F-test, the minority:mathkind interaction term is
# insignificant. So we drop it.


# Hypothesis 7: Is sex as a fixed factor significant?
# We will use a LRT and ML estimation.

# Reference model (notice we already dropped the minority:mathkind interaction.)
model4.ml.fit <-
  lme(
    mathgain ~ sex + minority + mathkind + ses + yearstea_binned_num + mathprep_binned_num:yearstea_binned_num + mathprep_binned_num,
    random = ~ mathkind | classid,
    data = full_df,
    method = "ML",
    weights = varIdent(form = ~ 1 | f_min_grp)
  )
summary(model4.ml.fit)

# Nested model (no sex fixed factor)
model4a.ml.fit <-
  lme(
    mathgain ~ minority + mathkind + ses + yearstea_binned_num + mathprep_binned_num:yearstea_binned_num + mathprep_binned_num,
    random = ~ mathkind | classid,
    data = full_df,
    method = "ML",
    weights = varIdent(form = ~ 1 | f_min_grp)
  )
summary(model4a.ml.fit)

# Test hypothesis 7
anova(model4.ml.fit, model4a.ml.fit)

# The test is nonsignificant, so we drop the sex fixed effects


# Hypothesis 8: Is mathprep_binned_num as a fixed factor significant?
# We will use a LRT and ML estimation.

# Nested model (No math_prepped_bin_num fixed effect).
model4b.ml.fit <-
  lme(
    mathgain ~ minority + mathkind + ses + yearstea_binned_num + mathprep_binned_num:yearstea_binned_num,
    random = ~ mathkind | classid,
    data = full_df,
    method = "ML",
    weights = varIdent(form = ~ 1 | f_min_grp)
  )
summary(model4b.ml.fit)


# Test hypothesis 8
anova(model4a.ml.fit, model4b.ml.fit)

# The test is nonsignificant, so we drop the sex fixed effects



# ----------------------------- FINAL MODEL ------------------------------------

# Fit the current best model, model4, using REML estimation.
# Is this the final model?
model4.reml.fit <-
  lme(
    mathgain ~ minority + mathkind + ses + yearstea_binned_num + mathprep_binned_num:yearstea_binned_num,
    random = ~ mathkind | classid,
    data = full_df,
    method = "REML",
    weights = varIdent(form = ~ 1 | f_min_grp)
  )

 #-5675.008
summary(model4.reml.fit) # AIC = 11376.02
anova(model4.reml.fit)

# ------------------------------- DIAGNOSTICS ----------------------------------

# Test 1: normality of residuals.
# Test with a histogram of the residuals and a Q-Q plot.
# Our residual matrix is split by a 2-group treatment classification, so we will
# have 2 of each.
full_df_with_residuals <- data.frame(full_df, res = resid(model4.reml.fit))
histogram(~ res | factor(f_min_grp),
          data = full_df_with_residuals,
          layout=c(1,2), aspect = 0.5, breaks = 10)
# Conclusion: normality assumption visually met.

# Bonus points: Shapiro-Wilk test for normality.
shapiro.test(full_df_with_residuals$res)
# These residuals do not appear to be normally distributed according to the
# Shapiro-Wilks test, but this is likely due to the large sample size. Suppose
# we were to select a small group of residuals:
shapiro.test(full_df_with_residuals$res[sample(nrow(full_df_with_residuals), 30)])
# Our residuals are acceptably normal.

qqnorm(model4.reml.fit, ~ resid(.) | factor(f_min_grp),
       layout=c(1,2), aspect = 1)
# Conclusion: Q-Q plots are linear enough. Evidence for an outlier in group 1.

# Test 2: constant variance of the residuals, a lack of which would show
# heteroskedasticity.
plot(model4.reml.fit, resid(.) ~ fitted(.) | factor(f_min_grp), layout=c(2,1), aspect=1)
# Conclusion: relatively constant variance within each group, but there are
# notable leverage points and notable outliers. Studentized conditional residuals
# will give us more information on these.

# Test 3: equal variance between groups, tested with studentized conditional
# residuals.
bwplot(resid(model4.reml.fit, type = "pearson")  ~ factor(f_min_grp), data = full_df)
# Conclusion: equal variance between groups, with a notable negative outlier
# in group 1 (minority 1, sex 0) and a notable positive outlier in group 2.

