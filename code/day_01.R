library(tidyverse)
# Part I
df <- readxl::read_excel("data/day_one_data.xlsx", col_names = FALSE) %>%
    janitor::clean_names()

(final_df <- df %>%
    mutate(num_only = str_replace_all(x1, "[^\\d]", "")) %>%
    mutate(num_count = nchar(num_only)) %>%
    mutate(two_digits_str = case_when(
        num_count == 2 ~ num_only,
        num_count < 2 ~ str_dup(num_only, times = 2),
        num_count > 2 ~ str_c(str_sub(num_only,
            start = 1,
            end = 1
        ), str_sub(num_only, -1, -1))
    )) %>%
    mutate(two_digits_num = parse_number(two_digits_str)))

final_df %>%
    summarize(sum_calibration = sum(two_digits_num)) %>%
    pull(sum_calibration)

# Part II
pattern <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
replacement <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")

(spelled_out_df <- df %>%
    mutate(spelled_out = map_chr(x1, ~ str_replace_all(.x, setNames(replacement, pattern)))) %>%
    mutate(num_only = str_replace_all(spelled_out, "[^\\d]", "")) %>%
    mutate(num_count = nchar(num_only)) %>%
    mutate(two_digits_str = case_when(
        num_count == 2 ~ num_only,
        num_count < 2 ~ str_dup(num_only, times = 2),
        num_count > 2 ~ str_c(str_sub(num_only,
            start = 1,
            end = 1
        ), str_sub(num_only, -1, -1))
    )) %>%
    mutate(two_digits_num = parse_number(two_digits_str)))

spelled_out_df %>%
    summarize(sum_calibration = sum(two_digits_num)) %>%
    pull(sum_calibration)
