library(tidyverse)
# Part I
(df <- read_delim("data/day_04_data.csv",
    delim = "|", col_names = FALSE
) %>%
    separate_wider_delim(cols = X1, delim = ":", names = c("card", "win_num")) %>%
    rename(my_num = X2) %>%
    mutate(win_num = str_squish(win_num), my_num = str_squish(my_num)))


(win_num_list <- df %>%
    pull(win_num) %>%
    str_extract_all("\\d+") %>%
    lapply(as.numeric))

(my_num_list <- df %>%
    pull(my_num) %>%
    str_extract_all("\\d+") %>%
    lapply(as.numeric))

(matches_list <- map2(win_num_list, my_num_list, .f = ~ .x %in% .y))

(num_matches_list <- map(matches_list, sum))

score <- function(x) {
    if (x == 0) {
        0
    } else {
        2^(x - 1)
    }
}

map_int(num_matches_list, score) %>%
    sum()
# correct anwer is 23941


# (all_matches <- df %>%
#     mutate(
#         my_num = str_split(my_num, pattern = " ")
#     ) %>%
#     unnest(my_num) %>%
#     mutate(my_num_len = str_length(my_num)) %>%
#     filter(my_num_len > 0) %>%
#     group_by(card) %>%
#     mutate(
#         match = case_when(
#             my_num_len == 1 ~ map_lgl(my_num, ~ any(str_detect(win_num, paste("\\b", .x, "\\b", "|", .x, "\\b")))),
#             my_num_len == 2 ~ map_lgl(my_num, ~ any(str_detect(win_num, .x)))
#         )
#     ))

# (all_points <- all_matches %>%
#     filter(match == TRUE) %>%
#     count(match) %>%
#     mutate(points = 2^(n - 1)) %>%
#     ungroup())

# all_points %>%
#     pull(points) %>%
#     sum()
# something is wrong with this significantly more complicated code, which generate a lower number at 21375



# Part II

(cards <- rep(1, nrow(df))) # all cards start with one

for (i in seq_along(cards)) {
    if (i == 0) next
    n <- cards[i]

    wins <- sum(win_num_list[[i]] %in% my_num_list[[i]])

    cards[seq_len(wins) + i] <- cards[seq_len(wins) + i] + n
}

sum(cards)


# tibble(
#     card = 1:length(num_matches_list %>% unlist()),
#     matches = num_matches_list %>% unlist()
# ) %>%
#     mutate(card_copy = map2(card + 1, matches, ~ seq(
#         from = .x, by = 1,
#         length.out = .y
#     ))) %>%
#     unnest(card_copy) %>%
#     group_by(card_copy) %>%
#     mutate(card_copy_2 = map2())
# Look at how the data was structured
