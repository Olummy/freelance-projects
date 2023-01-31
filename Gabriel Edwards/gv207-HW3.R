
# Task 1: Load libraries

library(ggplot2)
library(gmodels)
library(Hmisc)
library(stargazer)
library(effects)
library(gridExtra)

# Task 2: Load dataset

wd <- read.csv("wvs A3.csv")

# Task 3:

nrow(wd)

## Comment: The data set contains 1000 respondents'.


# Task 4:

table(wd$peoptrust)

# Task 5:

paste0((table(wd$peoptrust)[2]/sum(table(wd$peoptrust)))*100, "%")

## Comment: 46.2% of the people are trusting of other people.


# Task 6:

## Comment:

# The independent variable is religiosity
# The dependent variable is trust

# Task 7:

CrossTable(wd$peoptrust, wd$religious, chisq = TRUE, prop.c = TRUE,
           prop.r = FALSE, prop.t = FALSE, prop.chisq = FALSE,
           format = "SPSS", dnn = c("Trust", "Religious"))


# Task 8:

### A.

# The trusting percentage among the religious respondents is 54.47%.

### B.

# The trusting percentage among the non-religious respondents is 47.48%.

### C.

# The trusting percentage among the atheists respondents is 46.67%.


# Task 9:

# There's no significance relationship between trusting and religious going by the 
# chi-square analysis.



# Task 10:

# The appropriate bivariate test is chi-square.

wd$suicide <- ifelse(wd$suicide == "always ", 10,
                     ifelse(wd$suicide == "never", 1, wd$suicide))

wd$suicide <- as.numeric(wd$suicide)

CrossTable(wd$suicide, wd$sex, chisq = TRUE, prop.c = TRUE,
           prop.r = FALSE, prop.t = FALSE, prop.chisq = FALSE,
           format = "SPSS", dnn = c("Suicide", "Sex"))


# Task 11:

# There's a significance relationship between sex and suicide going by the 
# chi square test. The test is significance at 5% level of significance. Females
# tends to be in support of suicide than males going by the result of the 
# chi square test.


# Task 12:

ggplot(data = wd, aes(x = age, y = suicide)) +
  geom_point() +
  labs(x = "Age",
       y = "Suicide")

# No, the older generation does not tends to support suicide.


# Task 13:


cor.test(wd$suicide, wd$age)

# Task 14:

# There's a significance relationship between age and suicide going by the 
# correlation test result above. The test is significance at 5% level of significance. 
# The older generation tends to have a different view on suicide compared to the younger 
# generation.


# Task 15:

model1 <- lm(suicide ~ age, data = wd, na.action = "na.omit")

stargazer(model1, type = "text")


# Task 16:

plot(Effect("age", model1))

# Task 17:

# going by the result of the test and the model above, age and suicide are
# negatively correlated. The relationship is somewhat weak but significant at
# 5% level of significance.

# Task 18:

ggplot(data = wd, aes(x = age, y = suicide, color = sex)) +
  geom_point(show.legend = FALSE) +
  labs(x = "Age",
       y = "Suicide") +
  facet_wrap(. ~ sex, scales = "free")

# Task 19:

model2 <- lm(suicide ~ age + sex, na.action = "na.omit", data = wd)

stargazer(model1, model2, type = "text")


# Task 20:

# The second model i.e. the model with sex as a covariate performed better.
# This is because the residual standard error estimate is lower and the adjusted
# r2 value is higher in the second model than the first model.

# Task 21:

plot(Effect(c("age", "sex"), model2))[1]


# Task 22:

plot(Effect(c("age", "sex"), model2))

# Task 23:

model3 <- lm(suicide ~ age * sex, na.action = "na.omit", data = wd)

stargazer(model3, type = "text")


# Task 24:

plot(allEffects(model3))

# Task 25:

# model Men:

# Suicide = 3.84 -0.016*age - 0.489*male + 0.0033*age:male

# model Women:

# Suicide = 3.84 -0.016*age
