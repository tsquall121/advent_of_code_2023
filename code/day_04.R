library(tidyverse)
(df <- read_delim("data/day_04_data.csv",
    delim = "|", col_names = FALSE
) %>%
    separate_wider_delim(cols = X1, delim = ":", names = c("card", "win_num")) %>%
    rename(my_num = X2) %>%
    mutate(win_num = str_trim(win_num), my_num = str_trim(my_num)))

function(str_num, num_length) {
    if (num_length == 1) {
        any(str_detect(str_num, pattern = ))
    }
}

(all_matches <- df %>%
    mutate(
        my_num = str_split(my_num, pattern = " ")
    ) %>%
    unnest(my_num) %>%
    mutate(my_num = str_squish(my_num)) %>%
    mutate(my_num_len = str_length(my_num)) %>%
    filter(my_num_len > 0) %>%
    group_by(card) %>%
    mutate(
        match = case_when(
            my_num_len == 1 ~ map_lgl(my_num, ~ any(str_detect(win_num, paste("\\b", .x, "\\b", "|", .x, "\\b")))),
            my_num_len == 2 ~ map_lgl(my_num, ~ any(str_detect(win_num, .x)))
        )
    ))

(all_points <- all_matches %>%
    filter(match == TRUE) %>%
    count(match) %>%
    mutate(points = 2^(n - 1)) %>%
    ungroup())

all_points %>%
    pull(points) %>%
    sum()
