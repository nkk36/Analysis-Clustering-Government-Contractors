# Setup ====

# Load packages
library(data.table)
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

DUNS = "118330203"
print(naics2D_vendors[naics2D_vendors$parentdunsnumber == DUNS,])
N_Clusters = 13
#Cluster = unique(naics2D$Column[naics2D$Desc == Industry])
Subcategory = "NAICS"
N_Clusters_subcategory = 15
Revenue_Limits = c(1000000,10000000)
Action_Limits = c(50,150)

# Data Processing ====

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

# Cluster ====

if (Subcategory == "NAICS"){
  
  # Read pre-processed data
  df = read.csv("data/percentages_naics6D_indices_values.csv")
  
  # Make into matrix and convert to data frame
  matrix = sparseMatrix(i = df$row, j = df$col, x = df$values)
  matrix = as.matrix(matrix)
  df_for_cp = as.data.frame(matrix)
  
  # Read in duns and vendor names lookup table for naics6D
  vendors = read.csv("data/total_dollars_obligated_naics6D_duns_vendor_names.csv",
                     colClasses = "character")
  df_for_cp = cbind(vendors, df_for_cp )
  df_for_cp  = as.data.frame(df_for_cp )
  df_for_cp  = df_for_cp [df_for_cp $parentdunsnumber %in% companies$parentdunsnumber,]
  temp = fread("data/total_dollars_obligated_naics6D_clustering_data.csv",
               colClasses = "character")
  colnames(df_for_cp)[3:ncol(df_for_cp)] = colnames(temp)
  df_for_cp = df_for_cp[,3:ncol(df_for_cp)]
  
  Clusters = 2:50
  Cluster_KMeans_naics6D = list()
  
  for (i in Clusters) {
    # Set seed
    set.seed(123)
    
    # Perform K-Means clustering with 13 groups
    temp_kmeans = KMeans_rcpp(data = df_for_cp,
                              clusters = i)
    #temp_kmeans = kmeans(x = df_for_cp, centers = i, nstart=50, iter.max = 15)
    Cluster_KMeans_naics6D[[i - 1]] = temp_kmeans
  }
  
  Filename = paste("data/NAICS_6D_", Industry, ".RData", sep = "")
  load(Filename)
  
  clusters = Cluster_KMeans_naics6D[[N_Clusters_subcategory - 1]]$cluster
  companies$subcluster = clusters
  companies[which(substr(companies$vendorname,1,4) == "BOOZ"),]
  companies[which(companies$vendorname == "ANALYTIC SERVICES INC"),]
  Subcluster = 10 # Choose subcluster based on the two above lines
  c = Cluster_KMeans_naics6D[[N_Clusters_subcategory-1]]
  t = as.data.frame(c$centers)
  t2  = t[Subcluster,which(t[Subcluster,] != 0)]
  plot(1:length(t2), sort(t2))
  t3 = melt(t2)
  t3 = arrange(t3, desc(value))
  
  remove(df, matrix, temp, vendors, centers, Cluster_KMeans, naics2D, naics2D_vendors)
  
  
  # Clusters = 2:50
  # Cluster_KMeans_naics6D = list()
  # 
  # for (i in Clusters) {
  #   # Set seed
  #   set.seed(123)
  #   
  #   # Perform K-Means clustering with 13 groups
  #   temp_kmeans = kmeans(x = df_for_cp, centers = i, nstart=50, iter.max = 15)
  #   Cluster_KMeans_naics6D[[i - 1]] = temp_kmeans
  # }
  # 
  # remove(temp_kmeans)
  
}else if (Subcategory == "PSC") {
  # Read pre-processed data
  df = read.csv("data/percentages_psc_indices_values.csv")
  
  # Make into matrix and convert to data frame
  matrix = sparseMatrix(i = df$row, j = df$col, x = df$values)
  matrix = as.matrix(matrix)
  df_for_cp = as.data.frame(matrix)
  
  # Read in duns and vendor names lookup table for naics6D
  vendors = read.csv("data/total_dollars_obligated_psc_duns_vendor_names.csv",
                     colClasses = "character")
  df_for_cp = cbind(vendors, df_for_cp )
  df_for_cp  = as.data.frame(df_for_cp )
  df_for_cp  = df_for_cp [df_for_cp $parentdunsnumber %in% companies$parentdunsnumber,]
  temp = fread("data/total_dollars_obligated_psc_clustering_data.csv",
               colClasses = "character")
  colnames(df_for_cp)[3:ncol(df_for_cp)] = colnames(temp)
  df_for_cp = df_for_cp[,3:ncol(df_for_cp)]
  
  remove(df, matrix, temp, vendors, centers, Cluster_KMeans, naics2D, naics2D_vendors)
  
  # Filename = paste("data/NAICS_6D_", Industry, ".RData", sep = "")
  # load(Filename)
  
  # Clusters = 2:50
  # Cluster_KMeans_naics6D = list()
  # 
  # for (i in Clusters) {
  #   # Set seed
  #   set.seed(123)
  #   
  #   # Perform K-Means clustering with 13 groups
  #   temp_kmeans = kmeans(x = df_for_cp, centers = i, nstart=50, iter.max = 15)
  #   Cluster_KMeans_naics6D[[i - 1]] = temp_kmeans
  # }
  # 
  # remove(temp_kmeans) 
}




