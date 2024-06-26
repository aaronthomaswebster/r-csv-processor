# Function to read a CSV file into a dataframe
read_csv_data <- function(file_path) {
  # read a CSV file into a dataframe
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  print("CSV file has been read successfully.")
  return(data)
}

# Function to print the column names in the data
print_column_names <- function(data) {
  print("Column names in the data:")
  # for loop that iterates over the column names
  for (column_name in names(data)) {
    print(column_name)
  }
}


# Function to convert a column to numeric, some columns may not be read as numeric, so we need to convert them
convert_column_to_numeric <- function(data, column_name) {
  
  if (!column_name %in% names(data)) {
    print(paste("The column", column_name, "does not exist in the data."))
    return()
  }

  data[[column_name]] <- as.numeric(as.character(data[[column_name]]))

  # Check for any conversion issues
  if (any(is.na(data$Test1))) {
    warning("NAs introduced by coercion in 'Test1'")
  }
  return (data)
}


# Function to display the summary options
column_summerization_menu <- function() {
  print('Enert 1 to get Max value')
  print('Enter 2 to get Min value')
  print('Enter 3 to get Mean value')
  print('Enter 4 to get Median value')
  print('Enter 5 to get Standard Deviation value')
  print('Enter 6 to get the Sum of column values')
  print('Enter 0 to exit')
}

# Function to summarize a single column
summerize_column <-function(data){
  summerize_selection <- -1
  column_name <- readline(prompt = "Enter the name of the column to summarize: ")  
  data <- convert_column_to_numeric(data, column_name)

  while (summerize_selection != 0) {
    column_summerization_menu()
    summerize_selection <- as.integer(readline(prompt = "Enter your selection: "))
    switch(summerize_selection,
      `1` = print(paste("Max value of column", column_name, "is", max(data[[column_name]], na.rm = TRUE))),
      `2` = print(paste("Min value of column", column_name, "is", min(data[[column_name]], na.rm = TRUE))),
      `3` = print(paste("Mean value of column", column_name, "is", mean(data[[column_name]], na.rm = TRUE))),
      `4` = print(paste("Median value of column", column_name, "is", median(data[[column_name]], na.rm = TRUE))),
      `5` = print(paste("Standard Deviation value of column", column_name, "is", sd(data[[column_name]], na.rm = TRUE))),
      `6` = print(paste("Sum of column", column_name, "is", sum(data[[column_name]], na.rm = TRUE))),
      `0` = print("Goodbye!"),
      print("Invalid selection. Please try again.")
    )
  }
}

calculate_new_column_menu <- function() {
  print("Enter 1 to add two columns")
  print("Enter 2 to subtract two columns")
  print("Enter 3 to multiply two columns")
  print("Enter 4 to divide two columns")
  print("Enter 5 to save the new column to file")
  print("Enter 0 to exit")
}

calculate_new_column <- function(data) {
  column_name <- readline(prompt = "Enter the name of the new column: ")
  column1 <- readline(prompt = "Enter the name of the first column: ")
  column2 <- readline(prompt = "Enter the name of the second column: ")
  
  data <- convert_column_to_numeric(data, column1)
  data <- convert_column_to_numeric(data, column2)
  new_column_selection <- -1
  while(new_column_selection != 0) {
    calculate_new_column_menu()
    new_column_selection <- as.integer(readline(prompt = "Enter your selection: "))
    switch(new_column_selection,
      `1` = data[[column_name]] <- data[[column1]] + data[[column2]],
      `2` = data[[column_name]] <- data[[column1]] - data[[column2]],
      `3` = data[[column_name]] <- data[[column1]] * data[[column2]],
      `4` = data[[column_name]] <- data[[column1]] / data[[column2]],
      `5` = {
        output_path <- readline(prompt = "Enter the name of the file to save the new column: ")
        write.csv(data, output_path)
        print(paste("New column saved to", output_path))
      },
      `0` = print("Goodbye!"),
      print("Invalid selection. Please try again.")
    )
  }
}

# Function to display the main menu
display_main_menu <- function() {
  print('Welcome to the data processor!')
  print('Enter 1 to summerize a single column')
  print('Enter 2 to generate a new calculated column.')
  print('Enter 3 to display all the column names in the data.')
  print('Enter 0 to exit.')
  print('What would you like to do next?')
}

# Main function to run the data processing
main <- function(input_path, output_path) {
  # get column name from user
  input_path <- readline(prompt = "Enter the name of the file to process: ")
  data <- read_csv_data(input_path)
  selection <- -1
  while (selection != 0) {
    display_main_menu()
    selection <- as.integer(readline(prompt = "Enter your selection: "))
    if (selection == 1) {
      summerize_column(data)
    } else if (selection == 2) {
      calculate_new_column(data)
    } else if (selection == 3) {
      print_column_names()
    } else if (selection == 0) {
      print("Goodbye!")
    } else {
      print("Invalid selection. Please try again.")
    }
  }
}

# Example usage
main()
