# Setup ====

# Load packages
library(bit64)
library(data.table)
library(DBI)
library(dplyr)
library(ggplot2)
library(irlba)
library(magrittr)
library(MASS)
library(Matrix)
library(matrixStats)
library(progress)
library(RPostgreSQL)
library(stringi)
library(stringr)

# Load functions
source("R/set_environment_variables.R", chdir = T)

# Run and Set environment variables
set_environment_variables(TRUE)

# Connect to database ====

# Load the PostgreSQL driver
drv =dbDriver("PostgreSQL")

# Create a connection to the database
con =dbConnect(drv, dbname = Sys.getenv("db_name"),
               host = Sys.getenv("db_host"), port = Sys.getenv("db_port"),
               user = Sys.getenv("db_username"), password = Sys.getenv("db_password"))

df = dbGetQuery(con,
'SELECT federal_action_obligation AS dollarsobligated,
        award_type_code AS contractactiontype,
        type_of_contract_pricing_code AS typeofcontractpricing,
        recipient_name AS vendorname,
        recipient_duns AS dunsnumber,
        recipient_parent_duns AS parentdunsnumber,
        LEFT(product_or_service_code,2) AS psc_cat,
        product_or_service_code_description AS psc_code,
        naics_code AS naics_code,
        award_id_piid AS piid,
        modification_number AS modnumber,
        funding_sub_agency_code AS funding_agency_code
FROM public."FPDS_FY17"')

# Read in data ====

#################### Read in data


#df2 = fread("data/2_clustering_data_before_dcast.csv", colClasses = "character")
df$dollarsobligated = as.numeric(df$dollarsobligated)


# Get dollars obligated and actions by DUNS number ====

dobl_actions_duns = df %>%
  group_by(dunsnumber) %>%
  summarise(total_dobl = sum(dollarsobligated),
            total_actions = n_distinct(piid)) %>%
  filter(!is.na(dunsnumber))

write.csv(dobl_actions_duns, "data/dobl_actions_by_duns.csv", row.names = FALSE)

# Get one vendor name for each unique parent DUNS number ====

# For each parent DUNS number count the number of times each of its vendor names appear.
# Arrange the rows so that vendor names appear in decreasing order by the number of times they appear in the data
# Goal is to find the most frequent vendor name for each parent DUNS number
Vendorname_Counts = df %>%
  group_by(parentdunsnumber, vendorname) %>%
  summarise(count = n()) %>%
  arrange(parentdunsnumber, desc(count))

# Remove rows where parent DUNS number is blank
Vendorname_Counts = subset(Vendorname_Counts, parentdunsnumber != "")

# For each parent DUNS number we associate with it the most frequent vendor name
Vendornames = Vendorname_Counts %>%
  group_by(parentdunsnumber) %>%
  summarise(vendorname = head(vendorname,1))

# Find the parent DUNS number that have a blank vendor name
MissingNames = data.frame(vendorname = Vendornames$parentdunsnumber[which(Vendornames$vendorname == "")])

# Remove the DUNS numbers associated with a blank vendor name 
# We now have one vendor name for each unique parent DUNS number. 
Vendornames = subset(Vendornames, !(parentdunsnumber %in% MissingNames$vendorname))

remove(Vendorname_Counts)

# Get one vendor name for each unique DUNS number ====

# For each parent DUNS number count the number of times each of its vendor names appear.
# Arrange the rows so that vendor names appear in decreasing order by the number of times they appear in the data
# Goal is to find the most frequent vendor name for each parent DUNS number
Vendorname_Counts = df %>%
  group_by(dunsnumber, vendorname) %>%
  summarise(count = n()) %>%
  arrange(dunsnumber, desc(count))

# Remove rows where parent DUNS number is blank
Vendorname_Counts = subset(Vendorname_Counts, dunsnumber != "")

# For each parent DUNS number we associate with it the most frequent vendor name
Vendornames = Vendorname_Counts %>%
  group_by(dunsnumber) %>%
  summarise(vendorname = head(vendorname,1))

# Find the parent DUNS number that have a blank vendor name
MissingNames = data.frame(vendorname = Vendornames$dunsnumber[which(Vendornames$vendorname == "")])

# Remove the DUNS numbers associated with a blank vendor name 
# We now have one vendor name for each unique parent DUNS number. 
Vendornames = subset(Vendornames, !(dunsnumber %in% MissingNames$vendorname))

remove(Vendorname_Counts)


# Make Tables for Clustering ====


# Total Dollars Obligated --> NAICS 6D ====

# Sum dollars obligated for each parent DUNS number and for each NAICS code
Total_Dollars_Obligated  = df %>% 
  group_by(dunsnumber, naics_code) %>% 
  summarise(total = sum(dollarsobligated)) %>%
  ungroup()

#remove(df)

# Remove rows where parent DUNS is blank or the NAICS code is blank
Total_Dollars_Obligated = Total_Dollars_Obligated[Total_Dollars_Obligated$dunsnumber != "",]
Total_Dollars_Obligated = Total_Dollars_Obligated[Total_Dollars_Obligated$naics_code != "",]

# Convert to data table
Total_Dollars_Obligated = data.table(Total_Dollars_Obligated)

# Cast the data into long form using the sum of dollars obligated as the value. If no value is known, fill with 0
Total_Dollars_Obligated = data.table::dcast(data = Total_Dollars_Obligated, 
                                            formula = dunsnumber ~ naics_code, 
                                            fill = 0, 
                                            value.var = "total"
                                            )

# Remove parent DUNS numbers that have a missing vendor name
Total_Dollars_Obligated = subset(Total_Dollars_Obligated, !(dunsnumber %in% MissingNames$vendorname))

# Join parent DUNS numbers with vendor names that we got from before
Total_Dollars_Obligated = left_join(Total_Dollars_Obligated, Vendornames, by = "dunsnumber")

# Rearrange the columns
Total_Dollars_Obligated = Total_Dollars_Obligated[,c(1,ncol(Total_Dollars_Obligated),2:(ncol(Total_Dollars_Obligated) - 1))]

# Remove rows that are all zeroes
Total_Dollars_Obligated = Total_Dollars_Obligated[which(rowSums(Total_Dollars_Obligated[,3:ncol(Total_Dollars_Obligated)]) != 0),1:ncol(Total_Dollars_Obligated)] 
rownames(Total_Dollars_Obligated) = 1:nrow(Total_Dollars_Obligated) # Contains data for clustering using count of unique piids

# Save duns and vendor names
duns_vendor_names_total_dollars_obligated_naics6D = Total_Dollars_Obligated[,1:2]
write.csv(duns_vendor_names_total_dollars_obligated_naics6D, "data/total_dollars_obligated_naics6D_duns_vendor_names.csv", row.names = FALSE)

# Remove NA columns
Total_Dollars_Obligated = Total_Dollars_Obligated[,colnames(Total_Dollars_Obligated) != "NA"]

# Data for clustering - total dollars obligated
Total_Dollars_Obligated = Total_Dollars_Obligated[,3:ncol(Total_Dollars_Obligated)]
write.csv(Total_Dollars_Obligated, "data/total_dollars_obligated_naics6D_clustering_data.csv", row.names = FALSE)

#################### Make sparse matrix and save it to compress the data
Total_Dollars_Obligated = fread("data/total_dollars_obligated_naics6D_clustering_data.csv")
indices = data.frame(which(Total_Dollars_Obligated != 0,arr.ind = T))
Total_Dollars_Obligated = data.frame(Total_Dollars_Obligated)

values = data.frame(values = Total_Dollars_Obligated[indices$row[1], indices$col[1]])
write.table(values, file = "data/total_dollars_obligated_naics6D_indices_values.csv", sep = ",", row.names = FALSE)


for (i in 2:nrow(indices)){
  
  values = data.frame(values = Total_Dollars_Obligated[indices$row[i], indices$col[i]])
  write.table(values, 
              file = "data/total_dollars_obligated_naics6D_indices_values.csv", 
              sep = ",", 
              row.names = FALSE, 
              append = TRUE, 
              col.names = FALSE)

}
remove(i)

values = read.csv("data/total_dollars_obligated_naics6D_indices_values.csv")
indices$values = values$values
temp = data.frame(col = 1:ncol(Total_Dollars_Obligated),
                  NAICS6D = substr(colnames(Total_Dollars_Obligated),2,7))
indices = left_join(x = indices,
                    y = temp,
                    by = "col")
colnames(indices)[4] = "Desc"
Sparse_Matrix = sparseMatrix(i = indices$row, j = indices$col, x = indices$values)

# Save the sparse data as a sparse matrix and as a CSV
writeMM(Sparse_Matrix, "data/total_dollars_obligated_naics6D_sparse_matrix.rua")
write.csv(indices, "data/total_dollars_obligated_naics6D_indices_values.csv", row.names = FALSE)

# Remove data
remove(temp, values, Sparse_Matrix, indices, duns_vendor_names_total_dollars_obligated_naics6D, Total_Dollars_Obligated)

# Dollars Obligated --> NAICS 2D ====

# Read in 2D NAICS lookup table
naics2D = fread("data/lookup_naics2D.csv")

# Make a new column for the 2D NAICS codes
df$naics_code_2D = as.integer(substr(df$naics_code, 1,2))

# Join description of naics 2D codes with data
df3 = left_join(x = df,
                y = naics2D[,c("Code", "Desc")],
                by = c("naics_code_2D" = "Code"))

# Sum dollars obligated for each parent DUNS number and for each 2D naics code
# If there are multiple codes for one category (i.e. manufacturing) we aggregate them
Total_Dollars_Obligated  = df3 %>% 
  group_by(dunsnumber, Desc) %>% 
  summarise(total = sum(dollarsobligated)) %>%
  ungroup()

# Remove rows where parent DUNS is blank or the NAICS code is blank
Total_Dollars_Obligated = Total_Dollars_Obligated[Total_Dollars_Obligated$dunsnumber != "",] 
Total_Dollars_Obligated = Total_Dollars_Obligated[is.na(Total_Dollars_Obligated$Desc) != T,] 

# Convert to data table
Total_Dollars_Obligated = data.table(Total_Dollars_Obligated)

# Cast the data into long form using the sum of dollars obligated as the value. If no value is known, fill with 0
Total_Dollars_Obligated = data.table::dcast(data = Total_Dollars_Obligated, 
                                            formula = dunsnumber ~ Desc, 
                                            fill = 0, 
                                            value.var = "total"
)

# Remove parent DUNS numbers that have a missing vendor name
Total_Dollars_Obligated = subset(Total_Dollars_Obligated, !(dunsnumber %in% MissingNames$vendorname))

# Join parent DUNS numbers with vendor names that we got from before
Total_Dollars_Obligated = left_join(Total_Dollars_Obligated, 
                                    Vendornames, 
                                    by = "dunsnumber")

# Rearrange columns
Total_Dollars_Obligated = Total_Dollars_Obligated[,c(1,ncol(Total_Dollars_Obligated),2:(ncol(Total_Dollars_Obligated) - 1))]

# Save the data used for clustering by removing the columns for the DUNS and vendor names
Total_Dollars_Obligated = Total_Dollars_Obligated[which(rowSums(Total_Dollars_Obligated[,3:ncol(Total_Dollars_Obligated)]) != 0),1:ncol(Total_Dollars_Obligated)] 
rownames(Total_Dollars_Obligated) = 1:nrow(Total_Dollars_Obligated) # Contains data for clustering using total dollars obligated to 2D naics codes

# Save duns and vendor names
total_dollars_obligated_naics2D_duns_vendor_names = Total_Dollars_Obligated[,1:2]
write.csv(total_dollars_obligated_naics2D_duns_vendor_names, "data/total_dollars_obligated_naics2D_duns_vendor_names.csv", row.names = FALSE)

# Data for clustering - total dollars obligated
Total_Dollars_Obligated = Total_Dollars_Obligated[,3:ncol(Total_Dollars_Obligated)]
write.csv(Total_Dollars_Obligated, "data/total_dollars_obligated_naics2D_clustering_data.csv", row.names = FALSE)

#################### Make sparse matrix and save it to compress the data
Total_Dollars_Obligated = fread("data/total_dollars_obligated_naics2D_clustering_data.csv")
indices = data.frame(which(Total_Dollars_Obligated != 0,arr.ind = T))
Total_Dollars_Obligated = data.frame(Total_Dollars_Obligated)

values = data.frame(values = Total_Dollars_Obligated[indices$row[1], indices$col[1]])
write.table(values, file = "data/total_dollars_obligated_naics2D_indices_values.csv", sep = ",", row.names = FALSE)


for (i in 2:nrow(indices)){
  
  values = data.frame(values = Total_Dollars_Obligated[indices$row[i], indices$col[i]])
  write.table(values, 
              file = "data/total_dollars_obligated_naics2D_indices_values.csv", 
              sep = ",", 
              row.names = FALSE, 
              append = TRUE, 
              col.names = FALSE)
  
}
remove(i)


values = read.csv("data/total_dollars_obligated_naics2D_indices_values.csv")
indices$values = values$values
temp = data.frame(col = 1:ncol(Total_Dollars_Obligated), 
                  NAICS2D = colnames(Total_Dollars_Obligated))
indices = left_join(x = indices,
                    y = temp,
                    by = "col")
colnames(indices)[4] = "Desc"
Sparse_Matrix = sparseMatrix(i = indices$row, j = indices$col, x = indices$values)

# Save the sparse data as a sparse matrix and as a CSV
writeMM(Sparse_Matrix, "data/total_dollars_obligated_naics2D_sparse_matrix.rua")
write.csv(indices, "data/total_dollars_obligated_naics2D_indices_values.csv", row.names = FALSE)

# Remove data
remove(temp, values, Sparse_Matrix, indices, total_dollars_obligated_naics2D_duns_vendor_names, 
       Total_Dollars_Obligated, naics2D, df3)

# Total Dollars Obligated --> PSC ====

# Sum dollars obligated for each parent DUNS number and for each PSC code
Total_Dollars_Obligated  = df %>% 
  group_by(dunsnumber, psc_code) %>% 
  summarise(total = sum(dollarsobligated)) %>%
  ungroup()

# Remove
#remove(df)

# Remove rows where parent DUNS is blank or the PSC code is blank
Total_Dollars_Obligated = Total_Dollars_Obligated[Total_Dollars_Obligated$dunsnumber != "",]  
Total_Dollars_Obligated = Total_Dollars_Obligated[Total_Dollars_Obligated$psc_code != "",]

# Convert to data table
Total_Dollars_Obligated = data.table(Total_Dollars_Obligated)

# Cast the data into long form using the sum of dollars obligated as the value. If no value is known, fill with 0
Total_Dollars_Obligated = data.table::dcast(data = Total_Dollars_Obligated, 
                                            formula = dunsnumber ~ psc_code, 
                                            fill = 0, 
                                            value.var = "total"
)

# Remove parent DUNS numbers that have a missing vendor name
Total_Dollars_Obligated = subset(Total_Dollars_Obligated, !(dunsnumber %in% MissingNames$vendorname))

# Join parent DUNS numbers with vendor names that we got from before
Total_Dollars_Obligated = left_join(Total_Dollars_Obligated, 
                                    Vendornames, 
                                    by = "dunsnumber")

# Rearrange the columns
Total_Dollars_Obligated = Total_Dollars_Obligated[,c(1,ncol(Total_Dollars_Obligated),2:(ncol(Total_Dollars_Obligated) - 1))]

# Save the data used for clustering by removing the columns for the DUNS and vendor names
Total_Dollars_Obligated = Total_Dollars_Obligated[which(rowSums(Total_Dollars_Obligated[,3:ncol(Total_Dollars_Obligated)]) != 0),1:ncol(Total_Dollars_Obligated)] 
rownames(Total_Dollars_Obligated) = 1:nrow(Total_Dollars_Obligated) # Contains data for clustering using count of unique piids

# Save duns and vendor names
total_dollars_obligated_psc_duns_vendor_names = Total_Dollars_Obligated[,1:2]
write.csv(total_dollars_obligated_psc_duns_vendor_names, "data/total_dollars_obligated_psc_duns_vendor_names.csv", row.names = FALSE)

# Data for clustering - total dollars obligated
Total_Dollars_Obligated = Total_Dollars_Obligated[,3:ncol(Total_Dollars_Obligated)]
write.csv(Total_Dollars_Obligated, "data/total_dollars_obligated_psc_clustering_data.csv", row.names = FALSE)

#################### Make sparse matrix and save it to compress the data
Total_Dollars_Obligated = fread("data/total_dollars_obligated_psc_clustering_data.csv")
indices = data.frame(which(Total_Dollars_Obligated != 0,arr.ind = T))
Total_Dollars_Obligated = data.frame(Total_Dollars_Obligated)

values = data.frame(values = Total_Dollars_Obligated[indices$row[1], indices$col[1]])
write.table(values, file = "data/total_dollars_obligated_psc_indices_values.csv", sep = ",", row.names = FALSE)


for (i in 2:nrow(indices)){
  
  values = data.frame(values = Total_Dollars_Obligated[indices$row[i], indices$col[i]])
  write.table(values, 
              file = "data/total_dollars_obligated_psc_indices_values.csv", 
              sep = ",", 
              row.names = FALSE, 
              append = TRUE, 
              col.names = FALSE)
  
}

remove(i)

values = read.csv("data/total_dollars_obligated_psc_indices_values.csv")
indices$values = values$values
temp = data.frame(col = 1:ncol(Total_Dollars_Obligated), 
                  PSC = colnames(Total_Dollars_Obligated))
indices = left_join(x = indices,
                    y = temp,
                    by = "col")
colnames(indices)[4] = "Desc"


Sparse_Matrix = sparseMatrix(i = indices$row, j = indices$col, x = indices$values)

# Save the sparse data as a sparse matrix and as a CSV
writeMM(Sparse_Matrix, "data/total_dollars_obligated_psc_sparse_matrix.rua")
write.csv(indices, "data/total_dollars_obligated_psc_indices_values.csv", row.names = FALSE)


# Remove data
remove(temp, values, Sparse_Matrix, indices, total_dollars_obligated_psc_duns_vendor_names, 
       Total_Dollars_Obligated)

# Total Dollars Obligated --> Funding Agency ====

# Sum dollars obligated for each parent DUNS number and for each funding agency code
Total_Dollars_Obligated  = df %>% 
  group_by(dunsnumber, funding_agency_code) %>% 
  summarise(total = sum(dollarsobligated)) %>%
  ungroup()

# Remove
#remove(df)

# Remove rows where parent DUNS is blank or the NAICS code is blank
Total_Dollars_Obligated = Total_Dollars_Obligated[Total_Dollars_Obligated$dunsnumber != "" | 
                                                    Total_Dollars_Obligated$funding_agency_code != "",]


# Convert to data table
Total_Dollars_Obligated = data.table(Total_Dollars_Obligated)


# Cast the data into long form using the sum of dollars obligated as the value. If no value is known, fill with 0
Total_Dollars_Obligated = data.table::dcast(data = Total_Dollars_Obligated, 
                                            formula = dunsnumber ~ funding_agency_code, 
                                            fill = 0, 
                                            value.var = "total"
)
# Remove rows with unknown parent DUNS number 
#(seems as if the cast produced some unknown DUNS numbers - not sure why that occurred...should look into that)
Total_Dollars_Obligated = Total_Dollars_Obligated[Total_Dollars_Obligated$dunsnumber != "", ]

# Remove parent DUNS numbers that have a missing vendor name
Total_Dollars_Obligated = subset(Total_Dollars_Obligated, !(dunsnumber %in% MissingNames$vendorname))
Total_Dollars_Obligated$dunsnumber = as.character(Total_Dollars_Obligated$dunsnumber)

# Join parent DUNS numbers with vendor names that we got from before
Total_Dollars_Obligated = left_join(Total_Dollars_Obligated, Vendornames, by = "dunsnumber")

# Rearrange the columns...the 2nd column is removed because that was for unknown NAICS codes
Total_Dollars_Obligated = Total_Dollars_Obligated[,c(1,ncol(Total_Dollars_Obligated),3:(ncol(Total_Dollars_Obligated) - 1))]


# Save the data used for clustering by removing the columns for the DUNS and vendor names
Total_Dollars_Obligated = Total_Dollars_Obligated[which(rowSums(Total_Dollars_Obligated[,3:ncol(Total_Dollars_Obligated)]) != 0),1:ncol(Total_Dollars_Obligated)] 
rownames(Total_Dollars_Obligated) = 1:nrow(Total_Dollars_Obligated) # Contains data for clustering using count of unique piids

# Save duns and vendor names
duns_vendor_names = Total_Dollars_Obligated[,1:2]
write.csv(duns_vendor_names, "data/total_dollars_obligated_funding_agency_duns_vendor_names.csv", row.names = FALSE)

# Data for clustering - total dollars obligated
Total_Dollars_Obligated = Total_Dollars_Obligated[,3:ncol(Total_Dollars_Obligated)]
write.csv(Total_Dollars_Obligated, "data/total_dollars_obligated_funding_agency_clustering_data.csv", row.names = FALSE)

#################### Make sparse matrix and save it to compress the data
Total_Dollars_Obligated = fread("data/total_dollars_obligated_funding_agency_clustering_data.csv")
indices = data.frame(which(Total_Dollars_Obligated != 0,arr.ind = T))
Total_Dollars_Obligated = data.frame(Total_Dollars_Obligated)

values = data.frame(values = Total_Dollars_Obligated[indices$row[1], indices$col[1]])
write.table(values, file = "data/total_dollars_obligated_funding_agency_indices_values.csv", sep = ",", row.names = FALSE)


for (i in 2:nrow(indices)){
  
  values = data.frame(values = Total_Dollars_Obligated[indices$row[i], indices$col[i]])
  write.table(values, 
              file = "data/total_dollars_obligated_funding_agency_indices_values.csv", 
              sep = ",", 
              row.names = FALSE, 
              append = TRUE, 
              col.names = FALSE)
  
}

values = read.csv("data/total_dollars_obligated_funding_agency_indices_values.csv")
indices$values = values$values
temp = data.frame(col = 1:ncol(Total_Dollars_Obligated),
                  funding_agency = substr(colnames(Total_Dollars_Obligated),1,5))
indices = left_join(x = indices,
                    y = temp,
                    by = "col")
colnames(indices)[4] = "Desc"
Sparse_Matrix = sparseMatrix(i = indices$row, j = indices$col, x = indices$values)

# Save the sparse data as a sparse matrix and as a CSV
writeMM(Sparse_Matrix, "data/total_dollars_obligated_funding_agency_sparse_matrix.rua")
write.csv(indices, "data/total_dollars_obligated_funding_agency_indices_values.csv", row.names = FALSE)


# Number of Unique PIIDS --> NAICS 6D ====


#################### Count NAICS by distinct PIID

# Count the number of unique PIIDs for each parent DUNS number and for each NAICS code
Unique_PIIDs  = df %>% 
  group_by(dunsnumber, naics_code) %>% 
  summarise(n = n_distinct(piid)) %>%
  ungroup()

# Remove
#remove(df)

# Remove rows where parent DUNS is blank or the NAICS code is blank
Unique_PIIDs = Unique_PIIDs[Unique_PIIDs$dunsnumber != "" | Unique_PIIDs$naics_code != "",]

# Convert to data table
Unique_PIIDs = data.table(Unique_PIIDs)

# Cast the data into long form using the distinct PIID count value as the value. If no value is known, fill with 0
Unique_PIIDs = data.table::dcast(data = Unique_PIIDs, 
                                 formula = dunsnumber ~ naics_code, 
                                 fill = 0, 
                                 value.var = "n"
)

# Remove rows with unknown parent DUNS number 
#(seems as if the cast produced some unknown DUNS numbers - not sure why that occurred...should look into that)
Unique_PIIDs = Unique_PIIDs[Unique_PIIDs$dunsnumber != "", ]

# Remove parent DUNS numbers that have a missing vendor name
Unique_PIIDs = subset(Unique_PIIDs, !(dunsnumber %in% MissingNames$vendorname))
Unique_PIIDs$dunsnumber = as.character(Unique_PIIDs$dunsnumber)

# Join parent DUNS numbers with vendor names that we got from before
Unique_PIIDs = left_join(Unique_PIIDs, Vendornames, by = "dunsnumber")

# Rearrange the columns...the 2nd column is removed because that was for unknown NAICS codes
Unique_PIIDs = Unique_PIIDs[,c(1,ncol(Unique_PIIDs),3:(ncol(Unique_PIIDs) - 1))]

# Save the DUNS and vendor names
# Only save them for the rows where the row sum is non-zero (since we don't want companies with 0 contracts)
# Also not sure why a company would have 0 contracts in a year - should look into why this happens
duns_vendornames_Unique_PIIDs = Unique_PIIDs[which(rowSums(Unique_PIIDs[,3:1219]) != 0),1:2] # Remove rows of entire 0s
rownames(duns_vendornames_Unique_PIIDs) = 1:nrow(duns_vendornames_Unique_PIIDs)

# Save the data used for clustering by removing the columns for the DUNS and vendor names
Unique_PIIDs = Unique_PIIDs[which(rowSums(Unique_PIIDs[,3:1219]) != 0),3:ncol(Unique_PIIDs)] 
rownames(Unique_PIIDs) = 1:nrow(Unique_PIIDs) # Contains data for clustering using count of unique piids

#################### Make sparse matrix and save it to compress the data
indices = data.frame(which(Unique_PIIDs != 0,arr.ind = T))
Unique_PIIDs = data.frame(Unique_PIIDs)

values = data.frame(values = Unique_PIIDs[indices$row[1], indices$col[1]])
write.table(values, file = "data/unique_piids_indices_values.csv", sep = ",", row.names = FALSE)

for (i in 2:nrow(indices)){
  
  values = data.frame(values = Unique_PIIDs[indices$row[i], indices$col[i]])
  write.table(values, file = "data/unique_piids_indices_values.csv", sep = ",", row.names = FALSE, append = TRUE, col.names = FALSE)
  
}

values = read.csv("data/unique_piids_indices_values.csv")
indices$values = values$values
Sparse_Matrix = sparseMatrix(i = indices$row, j = indices$col, x = indices$values)

# Save the sparse data as a sparse matrix and as a CSV
writeMM(Sparse_Matrix, "data/unique_piids.rua")
write.csv(indices, "data/unique_piids_indices_values.csv", row.names = FALSE)

# Binary Indicators --> NAICS 6D ====

#################### Binary count
Unique_PIIDs = read.csv("data/unique_piids_indices_values.csv")
Binary_Counts = Unique_PIIDs
Binary_Counts$values = rep(1, nrow(Binary_Counts)) # Contains binary data for clustering using just naics codes

#################### Make sparse matrix and save it to compress the data
Binary_Counts = read.csv("data/binary_counts_naics_indices_values.csv")
Sparse_Matrix = sparseMatrix(i = Binary_Counts$row, j = Binary_Counts$col, x = Binary_Counts$values)

# Save the sparse data as a sparse matrix and as a CSV
writeMM(Sparse_Matrix, "data/binary_counts_naics.rua")
write.csv(Binary_Counts, "data/binary_counts_naics_indices_values.csv", row.names = FALSE)
