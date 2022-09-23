library(readr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

get_stats <- function(year) {

  read_csv(paste0("prediction_rusboost28_", year, ".csv"),
             col_names = c("year", "gvkey", "y_test", "pred"),
             col_types = "iild") %>%
    mutate(y_pred = pred >= quantile(pred, probs=0.99)) %>%
    count(y_test, y_pred) %>%
    pivot_wider(names_from = c(y_test, y_pred), values_from = n) %>%
    mutate(year = year) %>%
    select(year, everything())
}

results <-
  bind_rows(lapply(2003:2008L, get_stats)) %>%
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
  summarize(across(tn:tp, sum)) %>%
  mutate(acc = tp/(tp+fn), prec = tp/(tp+fp))

results_auc <- read_csv("results_rusboost28.csv") %>% filter(year < 2009)
mean(results_auc$auc)
mean(results_auc$ndcg_at_k)
mean(results_auc$sensitivity)
mean(results_auc$precision)
