library(tidyverse)
library(ggplot2)
library(lmtest)
library(sandwich)
data <- read_csv("cps99_ps1.csv")

ols_1 <- lm(ahe ~ yrseduc, data)
ols_2 <- lm(ahe ~ yrseduc + female, data)



# Task 7

# the coefficient on yrseduc is
ols_2$coefficients[2]
# this means that an additional year of education on average provides
# an increase in average hourly earnings by 1.3414 dollars, holding the gender constant

# T-test of null hypothesis that coefficient on female is zero,
# against the alternative that it is non-zero
coeftest(ols_2, vcov. = vcovHC(ols_2, type = "HC1"))
# t-value is 27.5 > 1.96 hence the null hyppothesis is rejected at 5% significance level.
# In everyday words, this means that there is a difference in income between female and male
# workers who have the same level of education

# The difference between the coefficients is
ols_1$coefficients[2] - ols_2$coefficients[2]
# The difference between coefficients is small in a real-world sense, because in both models
# a year of education provides at least $1.3 of average hourly earnings, so two cents
# is less than 2% difference. The correlation between yrseduc and female is low
cor(data$female, data$yrseduc)
# which means that in ols_2, only a small part of variation in yrseduc is removed 
# when female is held constant, hence the coefficient does not change much



#Task 8

ols_3 <- lm(ahe ~ yrseduc + ba, data)

# The marginal value of ba with yrseduc held constant is
ols_3$coefficients[3]
# Since yrseduc is held constant, this is also the marginal value of ba is for
# people who studied for 16 years. The 95% confidence interval is
ct <- coeftest(ols_3, vcov. = vcovHC(ols_3, type = "HC1"))
confint(ct, 3, 0.95)
