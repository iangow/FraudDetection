library(farr)
library(dplyr, warn.conflicts = FALSE)
library(readr)

uscecchini28 <- read_csv("uscecchini28.csv")

uscecchini28_aaer_years <-
  uscecchini28 %>%
  mutate(aaer_num = paste0("AAER-", p_aaer)) %>%
  left_join(aaer_dates, by = "aaer_num") %>%
  rename(aaer_year = year) %>%
  mutate(aaer_year = if_else(!is.na(p_aaer), coalesce(aaer_year, 1998L), NA_integer_))

uscecchini28_aaer_years %>%
  select(-aaer_num, -aaer_date, -aaer_desc) %>%
  write_csv(file = "uscecchini28_aaer_years.csv", na = "")
