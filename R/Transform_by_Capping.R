Transform_by_Capping = function(df, lower_bound, upper_bound, type){
  
  # Cap values above a given percentile 
  if (type == "Above"){
    quantile_upper_bound = quantile(df$values, probs = upper_bound)
    df$values[df$values > quantile_upper_bound] = quantile_upper_bound
  }
  
  # Cap values below a given percent 
  else if (type == "Below"){
    df$values[df$values < lower_bound] = 0
    df = df[df$values != 0, ]
    rownames(df) = 1:nrow(df)
  }
  
  # Return
  return(df)
  
}