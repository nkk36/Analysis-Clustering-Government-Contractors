# Setup ====

# Load packages
library(data.table)
library(dbscan)
library(dplyr)
library(magrittr)
library(stringi)
library(Matrix)
library(irlba)
library(ggplot2)
library(matrixStats)
library(MASS)

# Load functions
source("R/Transform_to_Percentage.R")
source("R/Transform_by_Capping.R")
source("R/Transform_Negative_Values.R")

# Load data
load("data/NAICS_2D_Clusters_2_50.RData")
naics2D = read.csv("data/lookup_naics2D.csv")
naics2D_vendors = read.csv("data/total_dollars_obligated_naics2D_duns_vendor_names.csv",
                           colClasses = "character")


# Set parameters ====

DUNS = "964725688" #"077815736"#"118330203"
print(naics2D_vendors[naics2D_vendors$parentdunsnumber == DUNS,])
N_Clusters = 13
#Cluster = unique(naics2D$Column[naics2D$Desc == Industry])
Subcategory = "NAICS"
N_Clusters_subcategory = 15
Revenue_Limits = c(500000000,5000000000)
Action_Limits = c(500,3000)

# Data Processing ====

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

remove(df_for_cp, temp, vendors, matrix)

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

df = naics6d %>%
  inner_join(y = psc, by = c("parentdunsnumber", "vendorname")) %>%
  inner_join(y = funding_agency, by = c("parentdunsnumber", "vendorname"))

remove(funding_agency, naics6d, psc)

# Find the row of the company in the data and determine its cluster/group
Row = which(naics2D_vendors$parentdunsnumber == DUNS)
Group = Cluster_KMeans[[N_Clusters - 1]]$cluster[Row]
Industry = names(which.max(Cluster_KMeans[[N_Clusters - 1]]$centers[Group,]))

# Assign vendor to cluster and get list of vendors in cluster/group
naics2D_vendors$cluster = Cluster_KMeans[[N_Clusters - 1]]$cluster
companies = naics2D_vendors[naics2D_vendors$cluster == Group,]
rownames(companies) = 1:nrow(companies)

# Join companies info with their total revenue and actions 
n = fread("data/dobl_actions_by_duns.csv", colClasses = "character")
companies = left_join(x = companies,
                      y = n,
                      by = "parentdunsnumber")

companies$total_dobl = as.numeric(companies$total_dobl)
companies$total_actions = as.numeric(companies$total_actions)
# Print company info ====
print(companies[companies$parentdunsnumber == DUNS,])

# Nearest Neighbors ====

df_sub = df[df$parentdunsnumber %in% companies$parentdunsnumber,]
df_companies = df_sub[,c("parentdunsnumber", "vendorname")]
df_companies = df_companies %>%
  left_join(y = companies, by = c("parentdunsnumber", "vendorname"))

Index = which(df_companies$total_dobl >= Revenue_Limits[1] & df_companies$total_dobl <= Revenue_Limits[2] 
              & df_companies$total_actions >= Action_Limits[1] & df_companies$total_actions <= Action_Limits[2])

df_sub2 = df_sub[Index,3:ncol(df_sub)]
df_companies2 = df_companies[Index,]
rownames(df_sub2) = 1:nrow(df_sub2)
rownames(df_companies2) = 1:nrow(df_companies2)


# Cluster ====

t = kNN(x = df_sub2, k = 5, sort = TRUE)
which(df_companies2$parentdunsnumber == DUNS)
t$id[which(df_companies2$parentdunsnumber == DUNS),]
df_companies2[t$id[which(df_companies2$parentdunsnumber == DUNS),],]
