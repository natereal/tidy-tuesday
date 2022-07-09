library(tidyverse)

data_path <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv' #nolint
paygap <- readr::read_csv(data_path)

tidytuesdayR::tt_load(x = "2022-06-28")

paygap
