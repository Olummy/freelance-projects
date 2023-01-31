## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(reactable)
library(ggthemes)
library(DescTools)
library(tidymodels)
library(vip)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------


# Load datasets

survival_tbl <- read_csv('Q4_dataset.csv')



## -------------------------------------------------------------------------------------------------------------------------------------------------------------

# structure and data types of the fields

glimpse(survival_tbl)




## -------------------------------------------------------------------------------------------------------------------------------------------------------------


# Convert the dependent variable `survival` to factor

survival_tbl %<>%
  mutate(survival = if_else(survival == 1, "The patient survived 5 years or longer", "The patient died within 5 years"),
         survival = as.factor(survival))
  


## -------------------------------------------------------------------------------------------------------------------------------------------------------------


reactable(survival_tbl, searchable = TRUE, filterable = TRUE, sortable = TRUE, pagination = TRUE)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------


# brief data summary

summary(survival_tbl)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------

# detailed summary

Desc(survival_tbl)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------



# Survival Distribution

survival_tbl %>% 
  group_by(survival) %>%
  summarise(Freq = n()) %>% 
  mutate(prop = Freq/sum(Freq)) %>% 
  filter(Freq != 0) %>% 
  
  ggplot(mapping = aes(x = 2, y = prop, fill = survival))+
  geom_bar(width = 1, color = "white", stat = "identity") +
  xlim(0.5, 2.5) +
  coord_polar(theta = "y", start = 0) +
  theme_void() +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(prop*100, 1), "%")), size = 4, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#fc0394","#03adfc")) +
  #theme(axis.text.x = element_text(angle = 90), legend.position = "top")+
  labs(title = "Patient survival distribution",
       x = "",
       y = "",
       fill = "") +
  theme(legend.position = "top") +
   theme(title = element_text(family = "Sans", face = "bold", size = 16))




## -------------------------------------------------------------------------------------------------------------------------------------------------------------


# Age Distribution

ggplot(survival_tbl, aes(Age)) +
  geom_histogram(fill = "steelblue", color = "white") +
  labs(title = 'Patient age distribution',
       x = "Age",
       y = "Frequency",
       fill = "") +
   theme(title = element_text(family = "Sans", face = "bold", size = 16)) +
  theme_clean()



## -------------------------------------------------------------------------------------------------------------------------------------------------------------


# Distribution of positive auxiliary nodes detected

ggplot(survival_tbl, aes(nr_of_nodes)) +
  geom_histogram(fill = "steelblue", color = "white") +
  labs(title = 'Distribution of positive auxiliary nodes',
       x = "# of Auxiliary Nodes",
       y = "Frequency",
       fill = "") +
   theme(title = element_text(family = "Sans", face = "bold", size = 16)) +
  theme_clean()



## -------------------------------------------------------------------------------------------------------------------------------------------------------------


# Counts of surgery performed yearly

survival_tbl %>% 
  mutate(Operation_year = paste0("19", Operation_year)) %>% 
  group_by(Operation_year) %>% 
  summarise(Count = n()) %>% 
  ggplot(aes(x = Operation_year, y = Count)) +
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue", color = "white") +
  labs(title = 'Count of surgery performed yearly',
       x = "Year of operation") +
  theme(title = element_text(family = "Sans", face = "bold", size = 16),
        axis.title = element_text(family = "sans", size = 10, face = "plain")) +
  theme_clean() +
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label = Count), size = 4)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------


any(is.na(survival_tbl))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------

# split data to train and test set

set.seed(1234)

split <- survival_tbl %>% 
  initial_split(prop = 0.75, strata = survival) # 75% training set | 25% testing set

df_train <- split %>% 
  training()

df_test <- split %>% 
  testing()



## -------------------------------------------------------------------------------------------------------------------------------------------------------------


rec <- recipe(survival ~ ., data = df_train)

# add preprocessing

prepro <- rec %>% 
  step_normalize(all_numeric_predictors()) %>% 
  prep()

prepro




## -------------------------------------------------------------------------------------------------------------------------------------------------------------

## Logistic Regression

lr <- logistic_reg(
  mode = "classification"
) %>% 
  set_engine("glm")



## -------------------------------------------------------------------------------------------------------------------------------------------------------------

## Logistic Regression

lr_wf <- workflow() %>% 
  add_recipe(prepro) %>% 
  add_model(lr)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1234)

## Logistic Regression

lr_wf %>% 
  fit(df_train) %>% 
  tidy()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------

set.seed(1234)

## Logistic Regression

lr_pred <- lr_wf %>% 
  fit(df_train) %>% 
  predict(df_test) %>% 
  bind_cols(df_test)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------



lr_pred %>% 
  conf_mat(truth = survival, estimate = .pred_class) %>% 
  autoplot(type = "heatmap")


lr_pred %>% 
  conf_mat(truth = survival, estimate = .pred_class) %>% 
  summary()




## -------------------------------------------------------------------------------------------------------------------------------------------------------------


prob_preds <- lr_wf %>% 
  fit(df_train) %>% 
  predict(df_test, type = "prob") %>% 
  bind_cols(df_test)


threshold_df <- prob_preds %>% 
  roc_curve(truth = survival, estimate = `.pred_The patient survived 5 years or longer`)

threshold_df %>% 
  autoplot()


roc_auc(prob_preds, truth = survival, estimate = `.pred_The patient survived 5 years or longer`)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------


final_lr_model <-
  lr_wf %>%
  fit(data = df_train)

final_lr_model


final_lr_model %>% 
  extract_fit_parsnip()

## variable importance plot

final_lr_model %>%
  extract_fit_parsnip() %>%
  vip() +
  labs(title = 'Variables relative importance',
       x = "") +
  theme(title = element_text(family = "Sans", face = "bold", size = 16),
        axis.title = element_text(family = "sans", size = 10, face = "plain")) +
  theme_clean() +
  scale_y_continuous(labels = scales::comma)



