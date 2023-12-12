# Your input string
input_string <- "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"

# Function to calculate the sum of part numbers
calculate_part_number_sum <- function(input_string) {
    # Replace non-numeric characters (excluding periods) with whitespace
    cleaned_string <- gsub("[^0-9.]+", " ", input_string)

    # Split the cleaned string into numbers
    numbers <- as.numeric(unlist(strsplit(cleaned_string, " ")))

    # Get the indices of periods in the input string
    period_indices <- which(substr(input_string, 1, nchar(input_string)) == ".")

    # Identify the rows and columns corresponding to the period indices
    row_indices <- (period_indices - 1) %/% nchar(strsplit(input_string, "\n")[[1]]) + 1
    col_indices <- (period_indices - 1) %% nchar(strsplit(input_string, "\n")[[1]]) + 1

    # Check if each number is adjacent to a symbol
    is_adjacent <- sapply(1:length(numbers), function(i) {
        any(abs(row_indices - (i - 1) %/% nchar(strsplit(input_string, "\n")[[1]]) - 1) <= 1 &
            abs(col_indices - (i - 1) %% nchar(strsplit(input_string, "\n")[[1]]) - 1) <= 1)
    })

    # Filter out numbers that are not adjacent to a symbol
    part_numbers <- numbers[is_adjacent]

    # Calculate the sum of part numbers
    sum_part_numbers <- sum(part_numbers, na.rm = TRUE)

    return(sum_part_numbers)
}

# Call the function with the input string
result <- calculate_part_number_sum(input_string)

# Print the result
cat("Sum of part numbers:", result, "\n")
