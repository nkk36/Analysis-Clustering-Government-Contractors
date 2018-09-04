# Setup ====

# Load packages
library(clValid)
library(corrplot)
library(data.table)
library(DBI)
library(dbplyr)
library(dplyr)
library(fossil)
library(ggplot2)
library(knitr)
library(magrittr)
library(Matrix)
library(plotly)
library(RColorBrewer)
library(RMySQL)

# Load data
df_original_naics2d = fread("data/percentages_naics2D_indices_values.csv")
df_original_naics6d = fread("data/percentages_naics6D_indices_values.csv")
df_original_psc = fread("data/percentages_psc_indices_values.csv")
df_original_funding_agency = fread("data/percentages_funding_agency_indices_values.csv")


# Set parameters for pre-processing ====
Param_Table = "naics2d" # Options: "naics2d" or "naics6d" or "psc" or "funding_agency"


# Pre-Process ====

# Create a copy of the table
if (Param_Table == "naics6d") {
  df = df_original_naics6d
  #remove(df_original_naics2d, df_original_psc)
}else if (Param_Table == "naics2d") {
  df = df_original_naics2d
  #remove(df_original_psc, df_original_naics6d)
}else if (Param_Table == "psc") {
  df = df_original_psc
  #remove(df_original_naics2d, df_original_naics6d)
}else if (Param_Table == "funding_agency") {
  df = df_original_funding_agency
  #remove(df_original_naics2d, df_original_naics6d)
}

# Convert data to a data frame
matrix = sparseMatrix(i = df$row, j = df$col, x = df$values)
matrix = as.matrix(matrix)
df_for_cp = as.data.frame(matrix)


# Get descriptions of the columns
Column_Descriptions = data.frame(unique(df[,c("col", "Desc")]))
Column_Descriptions = arrange(Column_Descriptions, col)
# Rename columns to keep track of their meaning
colnames(df_for_cp) = Column_Descriptions$Desc

remove(matrix, df)

# Clustering ====

#load("data/NAICS_2D_Clusters_2_50.RData")


# Remove
remove(list=setdiff(ls(), "df_for_cp"))
gc()

Clusters = 2:50
Cluster_KMeans = list()

for (i in Clusters) {
  # Set seed
  set.seed(123)

  # Perform K-Means clustering with 13 groups
  temp_kmeans = kmeans(x = df_for_cp, centers = i, nstart=50, iter.max = 15)
  Cluster_KMeans[[i - 1]] = temp_kmeans
}

remove(temp_kmeans)



# Plot ====
t = c()
for (i in 1:49){
  t[i] = Cluster_KMeans[[i]]$tot.withinss
}

plot(2:50, t, col = ifelse(2:50 == 15, "red", "black"))














