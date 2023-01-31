
## Load packages  -------------------------------------------------------------------------------
if(!require(pacman))install.packages("pacman")

pacman::p_load(
  tidyverse,
  scales,
  janitor
)

# Questions

## Q1. Load Data---------------------------------------------------------------------------------

world.data <- read_csv("world.csv")


## Q2. Categorical variable Analysis-------------------------------------------------------------------------------------------------

ft.oecd <- tabyl(world.data, oecd) %>% 
  as.data.frame() %>% 
  rename(OECD = oecd,
         Freq = n,
         Percentage = percent)


#------------------------------------------------------------------------------------------------  

# Q3.
# •	(A). 30 countries in the data are OECD members.
# •	(B). 161 countries in the data are not OECD members.
# •	(C). About 16% of the countries in the data are OECD members.
# •	(D). About 84% of the countries in the data are not OECD members.


## Q4. Bar Graph---------------------------------------------------------------------------------
ggplot(data = ft.oecd, aes(x = OECD, y = Freq)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "OECD membership",
       y = "Number of countries")


## Q5. per capita GDP Summary-----------------------------------------------------------------------------------------------

summary(world.data$gdp_10_thou)

sd(world.data$gdp_10_thou, na.rm = TRUE)



## Q6. Q6. Skewness ---------------------------------------------------------------------------

#•	(B). Positively skewed (skewed to the right).



## Q7. describe per capita GDP graphically-------------------------------------------------------------------------------------------------
ggplot(data = world.data, aes(gdp_10_thou)) +
  geom_histogram() +
  labs(x = "Per capita GDP (in 10,000 US dollars)",
       y = "Number of countries")


## Q8. The standard error of mean-------------------------------------------------------------------------------------------------

sd(world.data$gdp_10_thou, na.rm = TRUE)/sqrt(177)


## Q9. 95% CI
#------------------------------------------------------------------------------------------------

lower <- mean(world.data$gdp_10_thou, na.rm = TRUE) - 1.96*sd(world.data$gdp_10_thou, na.rm = TRUE)/sqrt(177)

upper <- mean(world.data$gdp_10_thou, na.rm = TRUE) + 1.96*sd(world.data$gdp_10_thou, na.rm = TRUE)/sqrt(177)

list("Lower CI" = lower,
     "Upper CI" = upper)


## Q10. Histogram for democracy and non-democracy per capita GDP -------------------------------------------------------------------------------------------------
dem.gdp <- world.data %>% 
  filter(!is.na(gdp_10_thou)) %>% 
  mutate(dem.dum = if_else(democ_regime == "Yes", "Democracy", "Autocracy"))


ggplot(data = dem.gdp, aes(x = gdp_10_thou, fill = dem.dum), color = "white") +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(dem.dum ~ .) +
  labs(x = "Per capita GDP (in 10,000 US dollars)",
       y = "Number of countries")


## Q11. mean and CI for democracy-------------------------------------------------------------------------------------------------
dem.gdp %>% 
  filter(dem.dum == "Democracy") %>% 
  group_by(dem.dum) %>% 
  summarise(Freq = n(),
            Mean = mean(gdp_10_thou),
            stdev = sd(gdp_10_thou)) %>% 
  mutate(lower.95.CI = Mean - 1.96*stdev/sqrt(Freq),
         upper.95.CI = Mean + 1.96*stdev/sqrt(Freq)) %>% 
  select(dem.dum, Mean, lower.95.CI, upper.95.CI)


## Q12. mean nd CI for non-democracy-------------------------------------------------------------------------------------------------
dem.gdp %>% 
  filter(dem.dum == "Autocracy") %>% 
  group_by(dem.dum) %>% 
  summarise(Freq = n(),
            Mean = mean(gdp_10_thou),
            stdev = sd(gdp_10_thou)) %>% 
  mutate(lower.95.CI = Mean - 1.96*stdev/sqrt(Freq),
         upper.95.CI = Mean + 1.96*stdev/sqrt(Freq)) %>% 
  select(dem.dum, Mean, lower.95.CI, upper.95.CI)

