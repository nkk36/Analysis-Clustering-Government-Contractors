Get_Revenue_Actions = function(company) {
  
  # Setup ====
  
  # Load packages
  library(formattable)
  library(dplyr)
  library(DT)
  
  # Load data
  vendors = read.csv("data/total_dollars_obligated_total_actions_duns_vendor_names.csv",
                      colClasses = "character")
  vendors$total_dobl = currency(vendors$total_dobl)
  vendors$total_actions = as.numeric(vendors$total_actions)
  
  # Main ====
  Output = vendors[vendors$vendorname %in% company,]
  colnames(Output) = c("DUNS", "Company", "Revenue", "Actions")
  rownames(Output) = 1:nrow(Output)
  Output = datatable(Output)
  formatCurrency(table = Output, columns = "Revenue")
  formatCurrency(table = Output, columns = "Actions", currency = "", digits = 0)
                                                       
    
  # Return ====
  return(Output)
  
}