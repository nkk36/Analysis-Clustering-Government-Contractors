Find_NN = function(k = 5, company, minRev, maxRev, minAct, maxAct, includeAct = FALSE) {
  
  # Setup ====
  
  # Load packages
  library(data.table)
  library(dbscan)
  library(dplyr)
  library(DT)
  library(formattable)
  library(magrittr)
  library(Matrix)

  # Load data
  vendors = read.csv("data/total_dollars_obligated_total_actions_duns_vendor_names.csv",
                     colClasses = "character")
  
  # NAICS ====
  
  naics6d = read.csv("data/percentages_naics6D_indices_values.csv")
  
  # Make into matrix and convert to data frame
  matrix = sparseMatrix(i = naics6d$row, j = naics6d$col, x = naics6d$values)
  matrix = as.matrix(matrix)
  df_for_cp = as.data.frame(matrix)
  
  # Read in duns and vendor names lookup table for naics6D
  vendors = read.csv("data/total_dollars_obligated_naics6D_duns_vendor_names.csv",
                     colClasses = "character")
  df_for_cp = cbind(vendors, df_for_cp )
  df_for_cp  = as.data.frame(df_for_cp )
  # df_for_cp  = df_for_cp [df_for_cp $parentdunsnumber %in% companies$parentdunsnumber,]
  temp = fread("data/total_dollars_obligated_naics6D_clustering_data.csv",
               colClasses = "character",
               header = TRUE)
  colnames(df_for_cp)[3:ncol(df_for_cp)] = colnames(temp)
  
  naics6d = df_for_cp
  remove(matrix, df_for_cp, temp, vendors)
  
  # PSC ====
  
  psc = read.csv("data/percentages_psc_indices_values.csv")
  
  # Make into matrix and convert to data frame
  matrix = sparseMatrix(i = psc$row, j = psc$col, x = psc$values)
  matrix = as.matrix(matrix)
  df_for_cp = as.data.frame(matrix)
  
  # Read in duns and vendor names lookup table for naics6D
  vendors = read.csv("data/total_dollars_obligated_psc_duns_vendor_names.csv",
                     colClasses = "character")
  df_for_cp = cbind(vendors, df_for_cp )
  df_for_cp  = as.data.frame(df_for_cp )
  # df_for_cp  = df_for_cp [df_for_cp $parentdunsnumber %in% companies$parentdunsnumber,]
  temp = fread("data/total_dollars_obligated_psc_clustering_data.csv",
               colClasses = "character",
               header = TRUE)
  colnames(df_for_cp)[3:ncol(df_for_cp)] = colnames(temp)
  
  psc = df_for_cp
  remove(matrix, df_for_cp, temp, vendors)
  
  
  # Funding Agency ====
  
  funding_agency = read.csv("data/percentages_funding_agency_indices_values.csv")
  
  # Make into matrix and convert to data frame
  matrix = sparseMatrix(i = funding_agency$row, j = funding_agency$col, x = funding_agency$values)
  matrix = as.matrix(matrix)
  df_for_cp = as.data.frame(matrix)
  
  # Read in duns and vendor names lookup table for naics6D
  vendors = read.csv("data/total_dollars_obligated_funding_agency_duns_vendor_names.csv",
                     colClasses = "character")
  df_for_cp = cbind(vendors, df_for_cp )
  df_for_cp  = as.data.frame(df_for_cp )
  # df_for_cp  = df_for_cp [df_for_cp $parentdunsnumber %in% companies$parentdunsnumber,]
  temp = fread("data/total_dollars_obligated_funding_agency_clustering_data.csv",
               colClasses = "character",
               header = TRUE)
  colnames(df_for_cp)[3:ncol(df_for_cp)] = colnames(temp)
  
  funding_agency = df_for_cp
  
  remove(matrix, df_for_cp, temp, vendors)
  
  # Rename columns ====
  
  colnames(funding_agency)[3:ncol(funding_agency)] = sapply(X = colnames(funding_agency)[3:ncol(funding_agency)],
                                                            FUN = function(x){
                                                              return(paste("f",x, sep = ""))
                                                            })
  
  colnames(naics6d)[3:ncol(naics6d)] = sapply(X = colnames(naics6d)[3:ncol(naics6d)],
                                              FUN = function(x){
                                                return(paste("n",x, sep = ""))
                                              })
  
  colnames(psc)[3:ncol(psc)] = sapply(X = colnames(psc)[3:ncol(psc)],
                                      FUN = function(x){
                                        return(paste("p",x, sep = ""))
                                      })
  
  # Join ====
  
  df = naics6d %>%
    inner_join(y = psc, by = c("parentdunsnumber", "vendorname")) %>%
    inner_join(y = funding_agency, by = c("parentdunsnumber", "vendorname"))
  
  remove(funding_agency, naics6d, psc)
  
  # Join with total revenue and actions ====
  
  # Join companies info with their total revenue and actions 
  n = fread("data/dobl_actions_by_duns.csv", colClasses = "character")
  df = left_join(x = df,
                 y = n,
                 by = "parentdunsnumber")
  df = df[,c("parentdunsnumber", "vendorname","total_dobl","total_actions",
              sort(setdiff(colnames(df),c("parentdunsnumber", "vendorname","total_dobl","total_actions"))))]
  
  df$total_dobl = as.numeric(df$total_dobl)
  df$total_actions = as.numeric(df$total_actions)
  
  remove(n)
  
  # Subset ====
  
  if (includeAct == TRUE) {
    Index = which(df$total_dobl >= minRev & df$total_dobl <= maxRev & df$total_actions >= minAct & df$total_actions <= maxAct)
  } else {
    Index = which(df$total_dobl >= minRev & df$total_dobl <= maxRev)
  }
  
  df_sub = df[Index,setdiff(colnames(df), c("parentdunsnumber", "vendorname", "total_dobl", "total_actions"))]
  rownames(df_sub) = 1:nrow(df_sub)
  
  Vendors = df[,c("parentdunsnumber", "vendorname", "total_dobl", "total_actions")]
  Vendors_sub = Vendors[Index,]
  rownames(Vendors_sub) = 1:nrow(Vendors_sub)
  colnames(Vendors_sub) = c("DUNS", "Company", "Revenue", "Actions")
  
  # Nearest neighbors ====
  
  NN = kNN(x = df_sub, k = k, sort = TRUE)
  NN2 = NN$id[which(Vendors_sub$Company == company),]
  Output = Vendors_sub[NN2,]
  rownames(Output) = 1:nrow(Output)
  Output = datatable(Output) %>%
    formatCurrency(columns = "Revenue") %>%
    formatCurrency(columns = "Actions", currency = "", digits = 0)
  
  # Return ====
  return(Output)
  
}