library(tidyverse)

data_path <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv' #nolint
paygap <- readr::read_csv(data_path)

tidytuesdayR::tt_load(x = "2022-06-28")

paygap <- paygap %>%
  janitor::clean_names() %>%
  mutate(across(c(due_date, date_submitted), lubridate::as_datetime),
    employer_nameloyer_name = str_remove_all(employer_name, "\""),
    employer_name = str_replace_all(employer_name, ", |,", ", "))

paygap

ggplot(paygap, aes(y = diff_median_hourly_percent,
                   x = diff_mean_hourly_percent)) +
geom_point()

paygap %>% summary()

paygap %>%
    group_by(employer_id) %>%
    summarise(min = min(due_date),
              max = max(due_date),
              n = n()) %>%
              count(n)

# Can look at distributions of mean/median differences
ggplot(paygap, aes(x = diff_median_hourly_percent)) +
 geom_histogram(bins = 100)

ggplot(paygap, aes(x = diff_median_hourly_percent)) +
 geom_histogram(bins = 100) +
    ggplot2::xlim(-100, 100)

# How many companies are men paid more?

# Can look at change through time by using date (due/submitted)
