library(readr)
library(rpart)
library(tidyverse)
library(pROC)
library(farr)

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

set.seed(2021)

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
  
  fm <- rusboost(formula, data_train, size = 3000, control = rpart::rpart.control(minbucket = 5))
  scores <- predict(fm, data_test, type = "prob")
  fit <- predict(fm, data_test, type = "class")
}

get_stats <- function(response, scores) {
  auc(as.integer(as.character(response)), scores)
}

get_stats(data_test$misstate, scores)

#  calculate metric: NDCG@k
ndcg <- function(response, scores, predictor = NULL, k = 0.01) {

  # Organize data
  if (is.null(predictor)) {
    predictor <- as.numeric(scores > 0.5)
  } else {
    predictor <- as.numeric(as.character(predictor))
  }
  
  ranks <- sort(scores, index.return = TRUE, decreasing = TRUE)$ix
  response <- as.numeric(as.character(response))
  
  kn <- round(length(response)*k)
  kz <- min(kn, sum(response))

  first_best <- c(rep(1, kz), rep(0, kn - kz))
  
  z <- sum((as.integer(first_best) == 1)/log(1:kn + 1, 2))
  dcg_at_k <- sum((response[ranks][1:kn] == 1)/log(1:kn + 1, 2))
  
  res <- ifelse(z > 0, dcg_at_k/z, 0)
  names(res) <- paste0("k_",k)
  res
  
}

ndcg_k <- function(k) {
  c(ndcg(data_test$misstate, scores, k = k))
}

ndcgs <- unlist(lapply(seq(0.01, 0.05, 0.01), ndcg_k))
ndcgs

confusion_stats <- function(response, scores, k = 0.01) {
  
  # Organize data
  predictor <- as.numeric(scores >= quantile(scores, 1-k))
  
  response <- as.numeric(as.character(response))

  tp <- sum(response & predictor)
  fp <- sum(!response & predictor)
  fn <- sum(response & !predictor)
  tn <- sum(!response & !predictor)
  c(sensitivity = tp/(tp + fn),
    precision = tp/(tp + fp))
}

confusion_stats(data_test$misstate, scores, 0.01)
