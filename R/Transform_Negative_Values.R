Transform_Negative_Values = function(df, type){
  
  # Make all negative values 0
  if (type == "zero"){
    # Set all negative values to 0
    df = df[df$values > 0,]
  }
  # Make all negative values positive
  else if (type == "absolute value"){
    # Make all values positive
    df$values = abs(df$values)
  }
  
  # Return
  return(df)
  
}