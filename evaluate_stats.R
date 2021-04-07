library(readr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

get_stats <- function(csv_file) {

  year <- gsub("^.*_(\\d+)\\.csv$", "\\1", csv_file)

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
  mutate(tp = coalesce(tp, 0))

results %>%
  mutate(ppv = tp/(tp+fp)) %>%
  print(n = Inf)


results %>%
  summarize(across(tn:tp, sum)) %>%
  mutate(ppv = tp/(tp+fp))
