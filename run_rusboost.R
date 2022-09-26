library(readr)
# devtools::install_github("steveohh/rusboost")
# library(rusboost)
library(rpart)
library(tidyverse)
library(pROC)

X_vars <- c('act', 'ap', 'at', 'ceq', 'che', 'cogs', 'csho', 'dlc', 
            'dltis', 'dltt', 'dp', 'ib', 'invt', 'ivao', 'ivst', 
            'lct', 'lt', 'ni', 'ppegt', 'pstk', 're', 'rect', 'sale', 
            'sstk', 'txp', 'txt', 'xint', 'prcc_f')

y_var <- "misstate"


df <- 
  read_csv("data_FraudDetection_JAR2020.csv",
           show_col_types = FALSE) %>% 
  mutate(misstate = coalesce(misstate, 0),
         new_p_aaer = coalesce(as.character(new_p_aaer), "")) %>%
  select(one_of(c(y_var, X_vars, "new_p_aaer", "fyear"))) %>%
  na.omit() %>%
  as.data.frame()

for (test_year in 2003:2003) {
  
  gap <- 2
  
  data_test <- 
    df %>%
    filter(fyear == test_year) %>%
    mutate(misstate = factor(misstate))
  
  data_train <- 
    df %>%
    filter(between(fyear, 1991, test_year - gap)) %>%
    mutate(recode = new_p_aaer %in% unique(data_test$new_p_aaer) & new_p_aaer != "",
           misstate = if_else(!recode, misstate, 0)) %>%
    mutate(misstate = factor(misstate))
  
  sum(data_train$recode)
  
  formula <- paste(y_var, "~", paste(X_vars, collapse = " + "))
  
  set.seed(2021)
  fm <- rusboost(formula, data_train, size = 300, learn_rate = 0.1)
  scores <- predict(fm, data_test, type = "prob")
  fit <- predict(fm, data_test, type = "class")
  print(auc(as.numeric(data_test$misstate), as.numeric(scores)))
}

