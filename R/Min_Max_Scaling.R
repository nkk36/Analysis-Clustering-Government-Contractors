Min_Max_Scaling = function(df){
  
  # Get the minimum of each column
  col_min = df %>% 
    group_by(col) %>%
    summarise(min = min(values)) %>%
    arrange(col)
  
  # Get the maximum of each column
  col_max = df %>% 
    group_by(col) %>%
    summarise(max = max(values)) %>%
    arrange(col)
  
  # Join columns min/max with data
  df = left_join(x = df, y = col_min, by = "col")
  df = left_join(x = df, y = col_max, by = "col")
  
  # Perform min-max scaling
  df = df %>%
    mutate(values_hat = (values-min)/(max - min)) %>%
    select(row, col, values_hat)
  
  # Rename column
  colnames(df)[3] = "values"

  # Return
  return(df)
  
}