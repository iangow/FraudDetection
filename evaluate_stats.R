library(readr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

# setwd("~/Downloads/prediciton_rusboost28_2003-2008")

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
  mutate(tp = coalesce(tp, 0))

results %>%
  mutate(ppv = tp/(tp+fp))
