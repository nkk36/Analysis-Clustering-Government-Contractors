Transform_to_Percentage = function(df){
  
  # Load packages
  library(dplyr)
  library(magrittr)
    
  # Calculate the sum of each row
  row_sum = df %>%
    group_by(row) %>%
    summarise(total = sum(values)) %>%
    arrange(row)
    
  # Join row sums with data and convert to percentages by each row ordering the data by row
  df = left_join(x = df, y = row_sum, by = "row")
  df = df %>%
    mutate(percentage = round(values/total,2)) %>%
    arrange(row) %>%
    dplyr::select(row, col, percentage, Desc)
  
  # Rename columns
  colnames(df)[3] = "values"
  
  # Return
  return(df)
  
}