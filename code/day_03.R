library(tidyverse)
# Part I

input_string_1 <- paste(readLines("data/day_03_data.txt"), collapse = "")

matches <- unique(unlist(str_extract_all(input_string_1, "[^0-9.]")))

matrix_data <- matrix(
    data = str_split(input_string_1, "")[[1]],
    nrow = length(readLines("data/day_03_data.txt")),
    ncol = nchar(input_string_1) / length(readLines("data/day_03_data.txt")),
    byrow = TRUE
)

extract_adjacent_numbers <- function(matrix_data) {
    # Initialize a vector to store the extracted numbers
    extracted_numbers <- numeric(0)
    extracted_locations <- character(0)

    # Get the dimensions of the matrix
    n_rows <- nrow(matrix_data)
    n_cols <- ncol(matrix_data)

    # Loop through each element in the matrix
    for (i in 1:n_rows) {
        for (j in 1:n_cols) {
            # Check if the element is a symbol and extract adjacent numbers
            if (matrix_data[i, j] %in% matches) {
                adjacent_numbers <- c(
                    matrix_data[i - 1, j - 1], matrix_data[i - 1, j], matrix_data[i - 1, j + 1],
                    matrix_data[i, j - 1], matrix_data[i, j + 1],
                    matrix_data[i + 1, j - 1], matrix_data[i + 1, j], matrix_data[i + 1, j + 1]
                )
                adjacent_numbers_position <- c(
                    paste(i - 1, j - 1, sep = ", "), paste(i - 1, j, sep = ", "),
                    paste(i - 1, j + 1, sep = ", "), paste(i, j - 1, sep = ", "),
                    paste(i, j + 1, sep = ", "), paste(i + 1, j - 1, sep = ", "),
                    paste(i + 1, j, sep = ", "), paste(i + 1, j + 1, sep = ", ")
                )
                adjacent_numbers_final <- adjacent_numbers[grep("[0-9]", adjacent_numbers)]
                extracted_numbers <- c(extracted_numbers, adjacent_numbers_final)
                adjacent_numbers_position <- adjacent_numbers_position[grep("[0-9]", adjacent_numbers)]
                extracted_locations <- c(extracted_locations, adjacent_numbers_position)
            }
        }
    }
    return(list(numbers = extracted_numbers, locations = extracted_locations))
}

adjacent_numbers_results <- extract_adjacent_numbers(matrix_data)

# Convert strings to numeric indices
indices <- sapply(
    strsplit(adjacent_numbers_results$locations, ","),
    function(x) as.numeric(x)
)
row_indices <- indices[1, ]
col_indices <- indices[2, ]

result_matrix <- mapply(function(i, j) {
    new_matrix <- c(
        matrix_data[row_indices[i], col_indices[j] - 2],
        matrix_data[row_indices[i], col_indices[j] - 1],
        matrix_data[row_indices[i], col_indices[j]],
        matrix_data[row_indices[i], col_indices[j] + 1],
        matrix_data[row_indices[i], col_indices[j] + 2]
    )
    return(new_matrix)
}, i = seq_along(row_indices), j = seq_along(col_indices))

# Print the result
print(result_matrix)


final_extracted <- function(result_matrix) {
    pasted_numbers <- c()

    for (i in 1:dim(result_matrix)[2]) {
        row_char <- paste(result_matrix[, i], collapse = "")
        if (sum(str_count(row_char, "\\d")) <= 3) {
            list_num <- str_extract_all(row_char, "\\d+")
            if (length(list_num[[1]]) == 1) {
                only_num <- list_num[[1]][1]
            } else if (length(list_num[[1]] == 2) &
                sapply(list_num, str_count)[1] > sapply(list_num, str_count)[2]) {
                only_num <- list_num[[1]][1]
            } else {
                only_num <- list_num[[1]][2]
            }
            pasted_numbers <- c(pasted_numbers, only_num)
        } else {
            list_num <- str_extract_all(row_char, "\\d+")
            if (sapply(list_num, str_count)[1] > sapply(list_num, str_count)[2]) {
                only_num <- list_num[[1]][1]
            } else {
                only_num <- list_num[[1]][2]
            }
            pasted_numbers <- c(pasted_numbers, only_num)
        }
    }
    final_numbers <- as.numeric(pasted_numbers)
    return(list(
        numbers = final_numbers,
        sum = sum(final_numbers)
    ))
}

final_extracted(result_matrix) # We can see these matched numbers have duplications caused by the digit-level matching algorithm from last section. A symbol may match multiple digits in a numbers so that the number will be extracted for several times.

duplicated_vector <- final_extracted(result_matrix)$numbers

final_vec <- c()

for (i in 1:length(duplicated_vector)) {
    if (i == 1 || duplicated_vector[i] != duplicated_vector[i - 1]) {
        final_vec <- c(final_vec, duplicated_vector[i])
    } else if (i == length(duplicated_vector)) {
        final_vec <- c(final_vec, duplicated_vector[i])
    }
}

print(final_vec)

sum(final_vec)
