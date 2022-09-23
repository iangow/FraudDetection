library(readr)
library(rusboost)
library(rpart)
library(tidyverse)

df <- read_csv("data_FraudDetection_JAR2020.csv")
df %>% count(p_aaer=!is.na(p_aaer), new_p_aaer=!is.na(new_p_aaer)) %>% arrange(desc(n))
test_year <- 2003
gap <- 2


data_train

data_test <- 
  df %>%
  filter(fyear == test_year)

data_train <- 
  df %>%
  filter(between(fyear, 1991, test_year - gap)) %>%
  mutate(recode = new_p_aaer %in% unique(data_test$new_p_aaer) & 
           !is.na(new_p_aaer),
         misstate = if_else(recode, 0, misstate))

sum(data_train$recode)

formula <- paste("misstate ~ ", paste(names(data_train[10:37]), collapse = " + "))
formula   
fm <- rusb(formula, as.data.frame(data_train), iters = 300, sampleFraction = 0.5, idx = data_train$misstate == 0)
