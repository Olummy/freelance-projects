##========== header ==================

# HW 4 GV207 2022




# Q1: Load libraries ===============

library(ggplot2)
library(gmodels)
library(Hmisc)
library(stargazer)
library(effects)
library(gridExtra)

# Q2: Load dataset ==================

td <- read.csv("titanic.csv")

# Q3: rows in td dataframe ===========

nrow(td)

## There are 1309 individual passengers in the dataset.


# Q4: survived frequency table =============

table(td$survived)

# Q5: survived percentage ==================

prop.table(table(td$survived))

# About 38% of the passengers in the dataset survived.

# Q6: passenger class frequency table =============

table(td$pclass)

# Q7: pclass and survival =================

# passenger socio-economic class is the independent variable as this could be manipulated/change to measure it impact on survival.

# passenger survival is the dependent variable as this could only be measured/ascertained after manipulating the passenger socio-economic class.


# Q8: Chi-Square Analysis ================

CrossTable(y = td$pclass,
           x = td$survived,
           prop.c = TRUE,
           prop.r = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE,
           chisq = TRUE)


# Q9: Cross tabulation interpretation ===============

## (a) The survival percentage among the 1st class passengers is 0.619 (62%).

## (b) The survival percentage among the 2nd class passengers is 0.430 (43%).

## (c) The survival percentage among the 3rd class passengers is 0.255 (26%).



# Q10: cross tabulation interpretation; hypothesis =============

## The relationship between survived and pclass is consistent with the hypothesis 1 which states that socio-economic class of passengers is positively associated with passenger survival. This is clear from the percentages of survived passengers from the different socio-economic class.



# Q11: Fill in the gap option a-p ==================

## (j); (n); (g); (m).


# Q12: compare fare paid by male and female =================

## we run an independent samples t-test

t.test(fare ~ female, data = td, alternative = "greater")


# Q13: Interpret the t-test ================

## There's a significant difference (at 5% level of significance) between the fare paid by the male and the female passengers as the p-value from the test above is less than 5%.

## The estimated mean of the fare paid by the female group is more than that of the male group.

## We can reject the null hypothesis and conclude that the female group have a more expensive ticket compared with the male passengers.


# Q14: plot of age vs fare =================

ggplot(data = td, aes(x = age, y = fare)) +
  geom_point()


# Q15: run the linear correlation =============

cor.test(td$age, td$fare)


# Q16: Interpretation of the correlation ====================

## There's a weak positive relationship between age and fare.

## The relationship is significantly different from zero at 5% level of significance.

## It could be concluded that older passengers tend to have a more expensive ticket compared with younger passengers.


# Q17: Regress fare on age ====================

model1 <- lm(fare ~ age, data = td)

stargazer(model1, type = "text")

# Q18: estimated effect based on the model =====================

plot(Effect("age", model1))


# Q19: Interpretation ======================

## Based on the numerical and the graphical results above, it could be concluded that age and fare are positively related. The relationship is significant as the estimate of the variable age is significant in the regression table.


# Q20: plot of age vs fare holding female constant ===========

ggplot(data = td, aes(x = age, y = fare, color = female)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(female ~ ., scales = "free")

# Q21: Regress fare on age and female ===================

model2 <- lm(fare ~ age + female, data = td)

stargazer(model2, type = "text")


# Q22: fill in the gap ===================

## (a); (f); (i); (i).


# Q23: estimated effect of female on fare =================

plot(Effect("female", model2))


# Q24: estimated effect plot of age on fare m/f separately ==========

plot(Effect(c("age", "female"), model2))


# Q25: regress fare on age, female and age*female =========

model3 <- lm(fare ~ age + female + age*female, data = td)

stargazer(model3, type = "text")


# Q26: plot of age vs fare for men and women ==========

ggplot(data = td, aes(x = age, y = fare, color = female)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(female ~ ., scales = "free")

## The graph is different between men and women such that women of older age tends to have a more expensive fare ticket than their male counterparts.


# Q27: Write out the implied equation

## for men: fare = 15.799 + 1.203*age - 1.043 - 0.748*age
## after simplifying the above we have:
## for men:  fare = 14.756 + 0.455*age

## for women: fare = 15.799 + 1.203*age





