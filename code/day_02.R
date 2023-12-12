library(tidyverse)

df <- read_delim("data/day_two_data.csv", delim = ":", col_names = FALSE) %>%
    select(game = X1, game_info = X2) %>%
    mutate(
        game_info = str_squish(game_info),
        game_id = parse_number(game)
    )

# Part I
(problem_cases <- df %>%
    mutate(red_count_str = str_extract_all(game_info, "\\d+(?=\\s+red)")) %>%
    mutate(red_count_num = map(red_count_str, as.numeric)) %>%
    mutate(red_test = map(red_count_num, ~ .x <= 12)) %>%
    mutate(green_count_str = str_extract_all(game_info, "\\d+(?=\\s+green)")) %>%
    mutate(green_count_num = map(green_count_str, as.numeric)) %>%
    mutate(green_test = map(green_count_num, ~ .x <= 13)) %>%
    mutate(blue_count_str = str_extract_all(game_info, "\\d+(?=\\s+blue)")) %>%
    mutate(blue_count_num = map(blue_count_str, as.numeric)) %>%
    mutate(blue_test = map(blue_count_num, ~ .x <= 14)) %>%
    unnest(red_test) %>%
    unnest(green_test) %>%
    unnest(blue_test) %>%
    filter(red_test == FALSE | green_test == FALSE | blue_test == FALSE) %>%
    distinct(game_id) %>%
    pull(game_id))

df %>%
    filter(!game_id %in% problem_cases) %>%
    pull(game_id) %>%
    sum()

# Part II
df %>%
    mutate(red_count_str = str_extract_all(game_info, "\\d+(?=\\s+red)")) %>%
    mutate(red_count_num = map(red_count_str, as.numeric)) %>%
    mutate(red_max = map(red_count_num, ~ max(.x))) %>%
    mutate(green_count_str = str_extract_all(game_info, "\\d+(?=\\s+green)")) %>%
    mutate(green_count_num = map(green_count_str, as.numeric)) %>%
    mutate(green_max = map(green_count_num, ~ max(.x))) %>%
    mutate(blue_count_str = str_extract_all(game_info, "\\d+(?=\\s+blue)")) %>%
    mutate(blue_count_num = map(blue_count_str, as.numeric)) %>%
    mutate(blue_max = map(blue_count_num, ~ max(.x))) %>%
    unnest(red_max) %>%
    unnest(green_max) %>%
    unnest(blue_max) %>%
    mutate(power = red_max * green_max * blue_max) %>%
    pull(power) %>%
    sum()
