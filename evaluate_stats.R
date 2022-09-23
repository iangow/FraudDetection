library(readr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

<<<<<<< HEAD
get_stats <- function(csv_file) {

  year <- gsub("^.*_(\\d+)\\.csv$", "\\1", csv_file)
=======
get_stats <- function(year) {
>>>>>>> 60addba (Added evaluation code and data.)

  read_csv(csv_file,
           col_names = c("year", "gvkey", "y_test", "pred"),
           col_types = "iild") %>%
    mutate(y_pred = pred >= quantile(pred, probs=0.99)) %>%
    count(y_test, y_pred) %>%
    pivot_wider(names_from = c(y_test, y_pred), values_from = n) %>%
    mutate(year = year) %>%
    select(year, everything())
}

get_file_list <- function() {
  temp_path <- tempdir()
  unzip("prediction_rusboost28_1999-2014.zip", exdir = temp_path)
  list.files(temp_path, "*.csv", full.names = TRUE)
}

results <-
  bind_rows(lapply(get_file_list(), get_stats)) %>%
  rename(tp = `TRUE_TRUE`,
         tn = `FALSE_FALSE`,
         fp = `FALSE_TRUE`,
         fn = `TRUE_FALSE`) %>%
  mutate(tp = coalesce(tp, 0)) %>%
  mutate(ppv = tp/(tp+fp),
         acc = tp/(tp+fn),
         prec = tp/(tp+fp))

results %>%
  summarize(across(tn:prec, mean))

results %>%
<<<<<<< HEAD
  mutate(ppv = tp/(tp+fp)) %>%
  print(n = Inf)


results %>%
  summarize(across(tn:tp, sum)) %>%
  mutate(ppv = tp/(tp+fp))
=======
  summarize(across(tn:tp, sum)) %>%
  mutate(acc = tp/(tp+fn), prec = tp/(tp+fp))

results_auc <- read_csv("results_rusboost28.csv") %>% filter(year < 2009)
mean(results_auc$auc)
mean(results_auc$ndcg_at_k)
mean(results_auc$sensitivity)
mean(results_auc$precision)
>>>>>>> 60addba (Added evaluation code and data.)
