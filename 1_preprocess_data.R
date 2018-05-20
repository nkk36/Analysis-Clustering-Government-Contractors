# Setup ====

# Load packages
library(data.table)
library(ggplot2)
library(irlba)
library(Matrix)
library(matrixStats)
library(subspace)

# Load functions
source("R/Transform_to_Percentage.R")
source("R/Transform_by_Capping.R")
source("R/Min_Max_Scaling.R")
source("R/Transform_Negative_Values.R")

# Load data
df_original_naics6d = fread("data/total_dollars_obligated_naics6D_indices_values.csv") # NAICS 6D
df_original_naics2d = fread("data/total_dollars_obligated_naics2D_indices_values.csv") # NAICS 2D
df_original_psc = fread("data/total_dollars_obligated_psc_indices_values.csv") # PSC
df_original_funding_agency = fread("data/total_dollars_obligated_funding_agency_indices_values.csv") # Funding agency


# Data Processing ====

# Set Parameters for pre-processing ====
Param_Table = "funding_agency" # Options: "naics6d" or "naics2d" or "psc" or "funding_agency"
Param_Negative_Values = "absolute value" # Options: "absolute value" or "zero"
Param_Row_Percentages = T # Options: T or F
Param_Capping_Type = "Below" # Options: "Below" or "Above"
Param_Capping_Lower_Bound = 0.1
Param_Capping_Upper_Bound = 0.9
Filename_CSV = paste("data/percentages_", Param_Table, "_indices_values.csv", sep = "")


# Transform data ====

# Create a copy of the table
if (Param_Table == "naics6d") {
  df = df_original_naics6d
  remove(df_original_naics2d, df_original_psc, df_original_funding_agency)
}else if (Param_Table == "naics2d") {
  df = df_original_naics2d
  remove(df_original_psc, df_original_naics6d, df_original_funding_agency)
}else if (Param_Table == "psc") {
  df = df_original_psc
  remove(df_original_naics2d, df_original_naics6d, df_original_funding_agency)
}else if (Param_Table == "funding_agency") {
  df = df_original_funding_agency
  remove(df_original_naics2d, df_original_naics6d, df_original_psc)
}

# Transform negative values as specified
df = Transform_Negative_Values(df, 
                               type = Param_Negative_Values)

# Transform to percentages if desired
if (Param_Row_Percentages == T){
  df = Transform_to_Percentage(df)
}

# Cap the data as specified
df = Transform_by_Capping(df = df, 
                          lower_bound = Param_Capping_Lower_Bound, 
                          upper_bound = Param_Capping_Upper_Bound, 
                          type = Param_Capping_Type)

# Write to CSV
write.csv(df, Filename_CSV, row.names = FALSE)

rm(list = ls())


# Other Code ====
# # Perform SVD ====
# matrix = sparseMatrix(i = df$row, j = df$col, x = df$values)
# 
# svd = irlba(A = matrix,
#             nv = 2, 
#             nu = 2)
# 
# svd2 = svd(x = matrix, nu = 1203, nv = 1203)
# 
# # Approximate with SVD 
# 
# s = load("notebooks/20180129 - Dimensionality Reduction/svd_percent_capped_below_1_percent.rda")
# 
# d = diag(SVD$d[1:355])
# approx = SVD$u[,1:355] %*% d %*% t(SVD$v[1:355,1:355])
# 
# # Visualize SVD ====
# 
# plot(svd$u[,1], svd$u[,2])
# u = data.frame(svd$u)
# d = diag(svd$d)
# x = svd$u %*% d
# x2 = data.frame(x)
# 
# g = ggplot(data = x2) + 
#   geom_point(mapping = aes(x = X1,
#                            y = X2)) + 
#   xlab("First Singular Vector") + 
#   ylab("Second Singular Vector") +
#   ggtitle("Top Two Singular Vectors") + 
#   theme_light() + 
#   theme(text = element_text(size = 16))
# 
# png(filename = "figs/top_2_singular_vectors_unique_piids.png", width = 784, height = 557)
# g
# dev.off()
# 
# png(filename = "figs/all_5_singular_vectors_unique_piids.png", width = 784, height = 557)
# pairs(x2, labels = c("First Singular Vector", 
#                      "Second Singular Vector", 
#                      "Third Singular Vector", 
#                      "Fourth Singular Vector", 
#                      "Fifth Singular Vector"))
# dev.off()
