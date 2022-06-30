library(tidyverse)
library(tidytuesdayR)
tt <- tt_load("2022-01-18")

tt

data <- tt$chocolate

data <- data %>%
    mutate(cocoa_percent = as.numeric(str_remove_all(cocoa_percent, "%")))

data %>%
    select(where(is.character)) %>%
    map(fct_count, sort = TRUE, prop = TRUE)

plot1 <- data %>%
    ggplot(aes(y = rating, x = cocoa_percent)) +
    geom_point() +
    geom_smooth()

data %>%
    group_by(specific_bean_origin_or_bar_name) %>%
    mutate(med_rating = median(rating), n = n()) %>%
    arrange(desc(med_rating)) %>%
    select(specific_bean_origin_or_bar_name, med_rating, n)

# Each bean doesn't have a lot of reviews
# Try something more general

top_countries <- data %>%
    group_by(country_of_bean_origin) %>%
    summarise(med_rating = median(rating), n = n()) %>%
    arrange(desc(med_rating)) %>%
    filter(n >= 50) %>%
    slice_max(med_rating, n = 5) %>%
    pull(country_of_bean_origin)

data %>%
    filter(country_of_bean_origin %in% top_countries) %>%
    ggplot(aes(x = country_of_bean_origin, y = rating)) +
    geom_boxplot()

data <- data %>%
    mutate(
        BEANS = if_else(stringr::str_detect(ingredients, "B"), TRUE, FALSE),
        SUGAR = if_else((stringr::str_detect(ingredients, "S") &
                        !stringr::str_detect(ingredients, "\\*") &
                        !stringr::str_detect(ingredients, "\a")), TRUE, FALSE),
        SWEET = if_else(stringr::str_detect(ingredients, "S\\*"), TRUE, FALSE),
        COCOA = if_else(stringr::str_detect(ingredients, "C"), TRUE, FALSE),
        VANILLA = if_else(stringr::str_detect(ingredients, "V"), TRUE, FALSE),
        LEC = if_else(stringr::str_detect(ingredients, "L"), TRUE, FALSE),
        SALT = if_else(stringr::str_detect(ingredients, "Sa"), TRUE, FALSE),
    ) %>%
    mutate(sweetness = case_when(
        SUGAR == FALSE & SWEET == FALSE ~ "NONE",
        SUGAR == TRUE & SWEET == FALSE ~ "SUGAR",
        SUGAR == FALSE & SWEET == TRUE ~ "OTHER"
    ))

data %>%
    ggplot(aes(y = rating, x = sweetness)) +
    geom_boxplot()

data %>%
    ggplot(aes(y = rating, x = BEANS)) +
    geom_boxplot()

data %>%
    ggplot(aes(y = rating, x = COCOA)) +
    geom_boxplot()

data %>%
    ggplot(aes(y = rating, x = VANILLA)) +
    geom_boxplot()

data %>%
    ggplot(aes(y = rating, x = LEC)) +
    geom_boxplot()

data %>%
    ggplot(aes(y = rating, x = SALT)) +
    geom_boxplot()

data %>%
    ggplot(aes(y = rating, x = reorder(reorder(ingredients, rating, FUN = var), rating, FUN = median))) + # nolint
    geom_boxplot()

data %>%
 count(ingredients, sort = TRUE) %>%
 print(n = 22)

data %>%
    count(ingredients, BEANS, sweetness, COCOA, VANILLA, LEC, SALT) %>%
    view

data %>%
    select(BEANS, sweetness, COCOA, VANILLA, LEC, SALT) %>%
    map(~ fct_count(as.factor(.x)))
