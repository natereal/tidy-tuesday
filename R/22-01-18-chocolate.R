library(tidyverse)
library(tidytuesdayR)
library(ggforce)
library(colorspace)
library(ggblanket)


# Functions ----
add_notches_as_box <- function(plot, nudge = 0, alpha = 0.5) {
    gg_info <- ggplot2::ggplot_build(plot)$data[[1]]

    plot + ggplot2::geom_rect(data = gg_info, aes(
        xmin = xmin - nudge, xmax = xmax + nudge,
        ymin = notchlower, ymax = notchupper
        #,alpha = 1
    ), fill = "red", color = "transparent", alpha = alpha)
}

add_notches_as_lines <- function(plot, alpha = 1, size = 1, color = "red") {
    gg_info <- ggplot2::ggplot_build(plot)$data[[1]]

    plot + ggplot2::geom_segment(data = gg_info, aes(
        x = xmin, xend = (xmin + xid) / 2, y = notchupper, yend = middle,
        alpha = alpha
    ), colour = color, size = size) +
        ggplot2::geom_segment(data = gg_info, aes(
            x = xmin, xend = (xmin + xid) / 2, y = notchlower, yend = middle,
            alpha = alpha
        ), colour = color, size = size) +
        ggplot2::geom_segment(data = gg_info, aes(
            x = xmax, xend = (xmax + xid) / 2, y = notchupper, yend = middle,
            alpha = alpha
        ), colour = color, size = size) +
        ggplot2::geom_segment(data = gg_info, aes(
            x = xmax, xend = (xmax + xid) / 2, y = notchlower, yend = middle,
            alpha = alpha
        ), colour = color, size = size)
}

custom_boxplot <- function(data, x, y, nudge = 0.04, alpha = 0.2, xlab = NULL, ylab = NULL, title = NULL) { #nolint
    plot <- ggplot(data) +
        geom_boxplot(aes_string(x = x, y = y)) +
        xlab({{xlab}}) +
        ylab({{ylab}}) +
        ggtitle({{title}})

    plot <- add_notches_as_box(plot, nudge = nudge, alpha = alpha) +
        theme_minimal()

    return(plot)
}

# Load data ----
# tt <- tt_load("2022-01-18")

# tt

data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

# Data cleaning ----
data <- data %>%
    mutate(cocoa_percent = as.numeric(str_remove_all(cocoa_percent, "%")))

data <- data %>%
    mutate(
        BEANS = if_else(stringr::str_detect(ingredients, "B"), "Contains", "Does Not Contain"),
        SUGAR = if_else((stringr::str_detect(ingredients, "S") &
                             !stringr::str_detect(ingredients, "\\*") &
                             !stringr::str_detect(ingredients, "\a")), TRUE, FALSE), # nolint
        SWEET = if_else(stringr::str_detect(ingredients, "S\\*"), TRUE, FALSE),
        COCOA = if_else(stringr::str_detect(ingredients, "C"), "Contains", "Does Not Contain"),
        VANILLA = if_else(stringr::str_detect(ingredients, "V"), "Contains", "Does Not Contain"),
        LECITHIN = if_else(stringr::str_detect(ingredients, "L"), "Contains", "Does Not Contain"),
        SALT = if_else(stringr::str_detect(ingredients, "Sa"), "Contains", "Does Not Contain"),
    ) %>%
    mutate(sweetness = case_when(
        SUGAR == FALSE & SWEET == FALSE ~ "No Sweetener",
        SUGAR == TRUE & SWEET == FALSE ~ "Sugar",
        SUGAR == FALSE & SWEET == TRUE ~ "Other Sweetener"
    ))

data_no_na <- data %>%
    drop_na()


data %>%
    select(where(is.character)) %>%
    map(fct_count, sort = TRUE, prop = TRUE)

data %>%
    ggplot(aes(y = rating, x = cocoa_percent)) +
    geom_point() +
    geom_smooth()

plot1_cocoa <- data %>%
    ggplot(aes(y = rating, x = cocoa_percent)) +
    geom_jitter(alpha = 0.5) +
    geom_smooth() +
    xlab("Cocoa Level (%)") +
    ylab("Review Rating") +
    ggtitle("How Does Cocoa Affect Review Rating?")

data %>%
    ggplot(aes(y = rating, x = cocoa_percent)) +
    geom_bin2d(bins = 12, alpha = 0.8) +
    scale_fill_continuous(type = "viridis") +
    # geom_jitter(alpha = 0.5, width = 1, height = 0.2) +
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

plot2 <- data_no_na %>%
    ggplot() +
    geom_boxplot(
        aes(y = rating, x = factor(sweetness)),
        fill = "#85e4ff", colour = "#000000",
        # varwidth = TRUE,
        outlier.shape = 21, outlier.size = 3, notch = FALSE)

add_notches_as_box(plot2, alpha = 0.4, nudge = 0)

p1 <- data_no_na %>%
    ggplot() +
    geom_boxplot(aes(y = rating, x = sweetness))

add_notches_as_lines(p1)

add_notches_as_box(p1, nudge = 0.04, alpha = 0.2) +
    theme_minimal()

R <- map(
    .x = list("sweetness", "VANILLA", "LECITHIN", "SALT"),
    .f = ~ custom_boxplot(data_no_na, y = "rating", x = .x,
        ylab = "Review Rating",
        xlab = stringr::str_to_title(string = .x)))

gridExtra::grid.arrange(grobs = R, ncols = 2)


plot2_sweet <- custom_boxplot(data_no_na, x = "sweetness", y = "rating",
    xlab = "Type of Sweetnener", ylab = "Review Rating",
    title = "How Does Sweetener Affect Review Rating?")

p1 <- custom_boxplot(data_no_na, x = "VANILLA", y = "rating",
    xlab = "Vanilla", ylab = "Review Rating")

p2 <- custom_boxplot(data_no_na, x = "LECITHIN", y = "rating",
    xlab = "Lecithin")

p3 <- custom_boxplot(data_no_na, x = "SALT", y = "rating",
    xlab = "Salt")

gridExtra::grid.arrange(p1, p2, p3, nrow = 1, ncol = 3)
# p <- data_no_na %>%
#     ggplot(aes(y = rating, x = sweetness, fill = sweetness)) +
#     geom_boxplot() +
#     scale_fill_brewer(palette = "Blues") +
#     theme_minimal()

# p2 <- data_no_na %>%
#     ggplot(aes(y = rating, x = BEANS)) +
#     geom_boxplot()

# p3 <- data_no_na %>%
#     ggplot(aes(y = rating, x = COCOA)) +
#     geom_boxplot()

# p4 <- data_no_na %>%
#     ggplot(aes(y = rating, x = VANILLA)) +
#     geom_boxplot()

# p5 <- data_no_na %>%
#     ggplot(aes(y = rating, x = LECITHIN)) +
#     geom_boxplot()

# p6 <- data_no_na %>%
#     ggplot(aes(y = rating, x = SALT)) +
#     geom_boxplot()

# gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)
# # Not much info in BEANS, so removing that plot;
# # also not a lot of info in COCOA, and if its removed we will
# # have a nice 2x2 grid
# gridExtra::grid.arrange(p1, p4, p5, p6, nrow = 2)

data_no_na %>%
    ggplot(aes(y = rating, x = sweetness)) +
    geom_boxplot(aes(fill = sweetness)) +
    scale_fill_discrete_sequential("Peach")
    theme_bw()

data %>%
    ggplot(aes(y = rating)) +
    geom_boxplot(aes(y = .panel_y, x = .panel_x)) +
    facet_matrix(rows = vars(sweetness, BEANS), cols = vars(rating))

ggplot(data) +
    geom_boxplot(aes(x = .panel_x, y = .panel_y, group = .panel_x)) +
    facet_matrix(rows = vars(rating), cols = vars(COCOA, VANILLA, LECITHIN))

data %>%
    drop_na() %>%
    ggplot(aes(y = rating, x = reorder(reorder(ingredients, rating, FUN = var), rating, FUN = median))) + # nolint
    geom_boxplot()

data %>%
 count(ingredients, sort = TRUE) %>%
 print(n = 22)

data %>%
    count(ingredients, BEANS, sweetness, COCOA, VANILLA, LECITHIN, SALT)

data %>%
    select(BEANS, sweetness, COCOA, VANILLA, LECITHIN, SALT) %>%
    map(~ fct_count(as.factor(.x)))



 data %>%
    tidyr::drop_na() %>%
    rename(SWEETNESS = sweetness) %>%
    select(rating, BEANS, SWEETNESS, COCOA, VANILLA, LECITHIN, SALT) %>%
    mutate(across(!rating, .fns = as.character)) %>%
    pivot_longer(!rating, names_to = "INGR", values_to = "value") %>%
    gg_boxplot(x = value, y = rating, facet = INGR)

path <- "plots/2022-01-18/"
ggplot2::ggsave(filename = paste0(path, "cocoa.pdf"), plot = plot1_cocoa)
ggplot2::ggsave(filename = paste0(path, "sweetness.pdf"), plot = plot2_sweet)
pl3 <- gridExtra::arrangeGrob(p1, p2, p3, nrow = 1, ncol = 3, top = "Effect of Various Ingredients on Review Rating")
ggplot2::ggsave(filename = paste0(path, "ingredients.pdf"), plot = pl3)

for(name in c("cocoa", "sweetness", "ingredients")) {
    pdftools::pdf_convert(
        pdf = glue::glue("{path}{name}.pdf"), 
        filenames = glue::glue("{path}{name}.png"),
        format = "png", dpi = 450)
}
